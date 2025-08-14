module Skia.SKFileWStream where

import Skia.Bindings.Sk_stream
import Skia.Internal.Prelude

-- | Creates a new 'SKFileWStream' by opening the given file path.
open :: (MonadResource m) => FilePath -> m (ReleaseKey, SKFileWStream)
open path =
  allocateSKObjectNeverNull
    ( withCString path \path' -> do
        sk_filewstream_new path'
    )
    sk_filewstream_destroy

-- | Returns true if the current path could be opened.
isValid :: (MonadIO m) => SKFileWStream -> m Bool
isValid stream = liftIO do
  toBool <$> sk_filewstream_is_valid (ptr stream)
