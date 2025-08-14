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

import Distribution.PackageDescription qualified as PackageDescription
import Distribution.Simple qualified as Simple
import Distribution.Simple.LocalBuildInfo qualified as LocalBuildInfo
import Distribution.Simple.Program qualified as Program
import Distribution.Simple.Setup qualified as Setup
import Distribution.Simple.Utils qualified as Utils
import Distribution.Verbosity qualified as Verbosity
import Optics.Core
import Optics.TH
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))

makeFieldLabelsNoPrefix ''LocalBuildInfo.LocalBuildInfo
makeFieldLabelsNoPrefix ''PackageDescription.PackageDescription

-- TIP: See https://github.com/google/btls/blob/master/Setup.hs

cmakeProgram = Program.simpleProgram "cmake"
ninjaProgram = Program.simpleProgram "ninja"

main :: IO ()
main = do
  let h = Simple.simpleUserHooks
  Simple.defaultMainWithHooks
    h
      { Simple.hookedPrograms =
          [ cmakeProgram
          , ninjaProgram
          ]
      , Simple.confHook = \info flags -> do
          localBuildInfo <- Simple.confHook h info flags
          let programDb = view #withPrograms localBuildInfo
          let tmpBuildDir = LocalBuildInfo.buildDir localBuildInfo </> "cmake_build"
          Program.runDbProgram Verbosity.verbose cmakeProgram programDb ["-G", "Ninja", "-S", "cbits_cmake", "-B", tmpBuildDir]
          Program.runDbProgram Verbosity.verbose ninjaProgram programDb ["-C", tmpBuildDir, "libskia_capi_shared.so"]

          let lensBuildInfo = #localPkgDescr % #library % _Just % #libBuildInfo
          let localBuildInfo' = localBuildInfo & over (lensBuildInfo % #extraLibDirs) (tmpBuildDir :)
          pure localBuildInfo'
      }
