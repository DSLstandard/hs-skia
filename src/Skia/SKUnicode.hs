module Skia.SKUnicode where

import Skia.Bindings.Skunicode
import Skia.Internal.Prelude
import Skia.SKRefCnt qualified as SKRefCnt

-- * Creating SKUnicode

createICU :: (MonadResource m) => m (ReleaseKey, SKUnicode)
createICU =
  allocateSKObjectOrErrorIfNull'
    "Failed to create SKUnicode with ICU as backend"
    skunicode_unicode_new_icu
    SKRefCnt.decrement

createICU4X :: (MonadResource m) => m (ReleaseKey, SKUnicode)
createICU4X =
  allocateSKObjectOrErrorIfNull'
    "Failed to create SKUnicode with ICU4X as backend"
    skunicode_unicode_new_icu4x
    SKRefCnt.decrement

createLibgrapheme :: (MonadResource m) => m (ReleaseKey, SKUnicode)
createLibgrapheme =
  allocateSKObjectOrErrorIfNull'
    "Failed to create SKUnicode with Libgrapheme as backend"
    skunicode_unicode_new_libgrapheme
    SKRefCnt.decrement

-- * Checking if a particular backend is supported

isICUSupported :: (MonadIO m) => m Bool
isICUSupported = liftIO $ toBool <$> skunicode_unicode_is_supported_icu

isICU4XSupported :: (MonadIO m) => m Bool
isICU4XSupported = liftIO $ toBool <$> skunicode_unicode_is_supported_icu4x

isLibgraphemeSupported :: (MonadIO m) => m Bool
isLibgraphemeSupported = liftIO $ toBool <$> skunicode_unicode_is_supported_libgrapheme

-- * Miscellaneous utils

{- | Conveience function. Tries 'SKUnicode' backends ICU, ICU4X, and libgrapheme
(in that order) and returns the first supported 'SKUnicode' backend, or throws a
'SkiaError' if no backend is supported.
-}
createAutoDetect :: (MonadResource m) => m (ReleaseKey, SKUnicode)
createAutoDetect =
  go
    [ (isICUSupported, createICU)
    , (isICU4XSupported, createICU4X)
    , (isLibgraphemeSupported, createLibgrapheme)
    ]
 where
  go [] = do
    liftIO $ Control.Exception.throwIO $ SkiaError "No SKUnicode backend available"
  go ((isBackendSupported, createBackend) : entries') = do
    supported <- isBackendSupported
    if supported
      then createBackend
      else go entries'
