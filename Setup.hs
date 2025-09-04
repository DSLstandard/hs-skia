{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad.IO.Class
import Data.Bool
import Data.Foldable
import Debug.Trace
import Distribution.Compiler qualified as Compiler
import Distribution.PackageDescription qualified as PackageDescription
import Distribution.Simple qualified as Simple
import Distribution.Simple.Flag qualified as MonoidFlag
import Distribution.Simple.LocalBuildInfo qualified as LocalBuildInfo
import Distribution.Simple.Program qualified as Program
import Distribution.Simple.Setup qualified as Setup
import Distribution.Simple.Utils qualified as Utils
import Distribution.Types.BuildInfo qualified as BuildInfo
import Distribution.Types.Flag qualified as Flag
import Distribution.Verbosity qualified as Verbosity
import Optics.Core as Optics
import Optics.TH as Optics
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

makeFieldLabelsNoPrefix ''LocalBuildInfo.LocalBuildInfo
makeFieldLabelsNoPrefix ''PackageDescription.PackageDescription

-- TIP: See https://github.com/google/btls/blob/master/Setup.hs
-- TIP: See https://github.com/deech/fltkhs/blob/master/Setup.hs

cmakeProgram = Program.simpleProgram "cmake"
ninjaProgram = Program.simpleProgram "ninja"

main :: IO ()
main = do
  let h = Simple.simpleUserHooks
  Simple.defaultMainWithHooks h
    { Simple.hookedPrograms =
        [ cmakeProgram
        , ninjaProgram
        ]
    , Simple.confHook = \info confFlags -> do
        -- We want "Make it possible to directly link a haskell library with an
        -- external static C library(.a)"
        -- https://github.com/haskell/cabal/issues/4042, but currently we don't
        -- know how. We will follow how
        -- https://github.com/jakubfijalkowski/hlibsass is set up.

        -- Here, we build the '.a' library of 'skia_capi' with the source under
        -- './cbits/'.

        lbi <- Simple.confHook h info confFlags

        cwd <- System.Directory.getCurrentDirectory
        let tmpBuildDir = cwd </> LocalBuildInfo.buildDir lbi

        -- Build the static lib
        let programDb = view #withPrograms lbi
        Program.runDbProgram Verbosity.verbose cmakeProgram programDb $
          [ "-G", "Ninja"
          , "-S", "cbits_cmake/"
          , "-B", tmpBuildDir </> "skia_capi-BUILD"
          ]

        -- NOTE: This builds ./skia_capi-BUILD/libskia_capi_{static.a,shared.so}
        Program.runDbProgram Verbosity.verbose ninjaProgram programDb $
          [ "-C", tmpBuildDir </> "skia_capi-BUILD"
          , "libskia_capi_shared.so"
          , "libskia_capi_static.a" 
          ]

        let lbi' = lbi & over
              (#localPkgDescr % #library % _Just % #libBuildInfo % #extraLibDirs)
              ([ tmpBuildDir </> "skia_capi-BUILD" ] <>)
        pure lbi'
    , Simple.preBuild = \_args buildFlags -> do
        cwd <- getCurrentDirectory
        let tmpBuildDir = cwd </> MonoidFlag.fromFlag (buildFlags ^. #buildDistPref)
        let bi = BuildInfo.emptyBuildInfo & over
              #extraLibDirs
              ([ tmpBuildDir </> "skia_capi-BUILD" ] <> )
        return (Just bi, [])
    , Simple.postCopy = \_args flags pkgdescr lbi -> do
        cwd <- getCurrentDirectory
        let tmpBuildDir = cwd </> LocalBuildInfo.buildDir lbi
        let libPref = view #libdir $ LocalBuildInfo.absoluteInstallDirs pkgdescr lbi (MonoidFlag.fromFlag flags.copyDest)

        for_ ["libskia_capi_shared.so",  "libskia_capi_static.a"] \libname -> do
          Utils.installExecutableFile Verbosity.verbose
            (tmpBuildDir </> "skia_capi-BUILD" </> libname)
            (libPref </> libname)
    }

mustLookupFlag :: Flag.FlagAssignment -> Flag.FlagName -> IO Bool
mustLookupFlag flags name = do
  case Flag.lookupFlagAssignment name flags of
    Nothing -> do
      Utils.dieNoVerbosity $ "IMPOSSIBLE: unknown flag: " <> Flag.unFlagName name
    Just ison -> do
      pure ison
