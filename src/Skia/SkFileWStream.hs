module Skia.SkFileWStream where

import Skia.Internal.Prelude
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkStream.h"

-- | Creates a new 'SkFileWStream' by opening the given file path.
open :: (MonadResource m) => FilePath -> m (ReleaseKey, SkFileWStream)
open path =
  allocateSkObjectNeverNull
    (withCString path \path' -> do
      [C.block|SkFILEWStream* {
        return new SkFILEWStream($(const char* path'));
      }|]
    )
    (\p -> [C.block|void {
      delete $(SkFILEWStream* p);
    }|])

-- | Returns true if the current path could be opened.
isValid :: (MonadIO m) => SkFileWStream -> m Bool
isValid (ptr -> stream) = liftIO $ toBool <$> 
  [C.block|bool {
    return $(SkFILEWStream* stream)->isValid();
  }|]
