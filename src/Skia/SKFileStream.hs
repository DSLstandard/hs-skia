module Skia.SKFileStream where

import Skia.Bindings.Sk_stream
import Skia.Internal.Prelude

{- | Creates a new 'SKFileStream' that wraps the file with the specified path.

You need to use 'isValid' to check validity of the returned 'SKFileStream'.
-}
createFromFile :: (MonadResource m) => FilePath -> m (ReleaseKey, SKFileStream)
createFromFile path =
  allocateSKObjectNeverNull
    (withCString path sk_filestream_new)
    sk_filestream_destroy

-- | Returns true if the current path could be opened.
isValid :: (MonadIO m) => SKFileStream -> m Bool
isValid stream = liftIO do
  liftIO $ toBool <$> sk_filestream_is_valid (ptr stream)
