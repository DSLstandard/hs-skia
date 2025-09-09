module Skia.SkFileStream where

import Skia.Internal.Prelude
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkStream.h"

{- | Creates a new 'SkFileStream' that wraps the file with the specified path.

You need to use 'isValid' to check validity of the returned 'SkFileStream'.
-}
createFromFile :: (MonadResource m) => FilePath -> m (ReleaseKey, SkFileStream)
createFromFile path =
  allocateSkObjectNeverNull
    ( evalManaged do
        path' <- managed $ withCString path
        liftIO [C.exp| SkFILEStream* { new SkFILEStream($(const char* path')) } |]
    )
    (\p -> [C.block| void { delete $(SkFILEStream* p); } |])

-- | Returns true if the current path could be opened.
isValid :: (MonadIO m) => SkFileStream -> m Bool
isValid (ptr -> stream) = liftIO do
  toBool <$> [C.exp| bool { $(SkFILEStream* stream)->isValid() } |]

-- | Close this 'SkFileStream'
close :: (MonadIO m) => SkFileStream -> m ()
close (ptr -> stream) = liftIO do
  [C.block| void { $(SkFILEStream* stream)->close(); } |]
