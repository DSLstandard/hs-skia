module Skia.SKVersion where

import Skia.Internal.Prelude
import Skia.Bindings.Sk_general
import System.IO.Unsafe
import Foreign.C.String (peekCString)

getMilestone :: (MonadIO m) => m Int
getMilestone = liftIO $ fromIntegral <$> sk_version_get_milestone

getIncrement :: (MonadIO m) => m Int
getIncrement = liftIO $ fromIntegral <$> sk_version_get_increment

getVersionCString :: (MonadIO m) => m CString
getVersionCString = liftIO $ sk_version_get_string

getVersionString :: (MonadIO m) => m String
getVersionString = liftIO do
  cs <- sk_version_get_string
  peekCString cs