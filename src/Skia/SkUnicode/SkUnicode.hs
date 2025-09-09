module Skia.SkUnicode.SkUnicode where

import Skia.Internal.Prelude
import Skia.SkRefCnt qualified as SkRefCnt
import Skia.SkUnicode.Objects
import Language.C.Inline.Cpp qualified as C
import Language.C.Inline.Internal qualified as C
import NeatInterpolation
import Data.Text qualified as T

C.context $ C.cppCtx <> cppSkiaObjectTypes <> cppSkUnicodeObjectTypes

C.include "modules/skunicode/include/SkUnicode.h"
C.include "modules/skunicode/include/SkUnicode_icu.h"
C.include "modules/skunicode/include/SkUnicode_icu4x.h"
C.include "modules/skunicode/include/SkUnicode_libgrapheme.h"

-- Using weak symbols as a trick to allow skunicode.cpp to be compiled with
-- unsolved ::Make() references if Skia is not compiled to support the
-- corresponding SkUnicode backend.
$(C.emitVerbatim $
  T.unpack [text|
    __attribute__((weak)) sk_sp<SkUnicode> SkUnicodes::ICU::Make();
    __attribute__((weak)) sk_sp<SkUnicode> SkUnicodes::ICU4X::Make();
    __attribute__((weak)) sk_sp<SkUnicode> SkUnicodes::Libgrapheme::Make();
  |]
 )

-- * Creating SkUnicode instances

createICU :: (MonadResource m) => m (ReleaseKey, SkUnicode)
createICU =
  allocateSkObjectNeverNull'
    ( do
        unlessM isICUAvailable do
          throwIO $ SkiaError "ICU SkUnicode backend is not available"
        [C.exp| SkUnicode* { SkUnicodes::ICU::Make().release() } |]
    )
    SkRefCnt.decrement

createICU4X :: (MonadResource m) => m (ReleaseKey, SkUnicode)
createICU4X =
  allocateSkObjectNeverNull'
    ( do
        unlessM isICU4XAvailable do
          throwIO $ SkiaError "ICU4X SkUnicode backend is not available"
        [C.exp| SkUnicode* { SkUnicodes::ICU4X::Make().release() } |]
    )
    SkRefCnt.decrement

createLibgrapheme :: (MonadResource m) => m (ReleaseKey, SkUnicode)
createLibgrapheme =
  allocateSkObjectNeverNull'
    ( do
        unlessM isLibgraphemeAvailable do
          throwIO $ SkiaError "Libgrapheme SkUnicode backend is not available"
        [C.exp| SkUnicode* { SkUnicodes::Libgrapheme::Make().release() } |]
    )
    SkRefCnt.decrement

-- * Checking if a particular backend is available

isICUAvailable :: (MonadIO m) => m Bool
isICUAvailable = toBool <$> liftIO [C.exp|bool { SkUnicodes::ICU::Make != nullptr }|]

isICU4XAvailable :: (MonadIO m) => m Bool
isICU4XAvailable = toBool <$> liftIO [C.exp|bool { SkUnicodes::ICU4X::Make != nullptr }|]

isLibgraphemeAvailable :: (MonadIO m) => m Bool
isLibgraphemeAvailable = toBool <$> liftIO [C.exp|bool { SkUnicodes::Libgrapheme::Make != nullptr }|]

-- * Miscellaneous utils

{- | Conveience function. Tries 'SKUnicode' backends ICU, ICU4X, and libgrapheme
(in that order) and returns the first available 'SKUnicode' backend, or throws a
'SkiaError' if no backend is available.
-}
createAutoDetect :: (MonadResource m) => m (ReleaseKey, SkUnicode)
createAutoDetect =
  go
    [ (isICUAvailable, createICU)
    , (isICU4XAvailable, createICU4X)
    , (isLibgraphemeAvailable, createLibgrapheme)
    ]
 where
  go [] = do
    liftIO $ throwIO $ SkiaError "No SKUnicode backend available"
  go ((isBackendAvailable, createBackend) : entries') = do
    available <- isBackendAvailable
    if available
      then createBackend
      else go entries'
