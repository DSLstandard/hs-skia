{- | SKString

Light weight class for managing strings. Uses reference counting to make
string assignments and copies very fast with no extra RAM cost. Assumes UTF8
encoding.
-}
module Skia.SKString where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSBuilder
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Foreign qualified as T
import Skia.Bindings.Sk_string
import Skia.Internal.Prelude

-- | Creates an empty SKString
createEmpty :: (MonadResource m) => m (ReleaseKey, SKString)
createEmpty =
  allocateSKObjectNeverNull
    sk_string_new_empty
    sk_string_destructor

{- | O(n). Creates an SKString by copying from a 'BS.ByteString'. Assumes UTF-8
encoding.
-}
createFromByteString :: (MonadResource m) => BS.ByteString -> m (ReleaseKey, SKString)
createFromByteString bs =
  allocateSKObjectNeverNull
    ( evalManaged do
        (ptr, len) <- storableByteStringLen bs
        liftIO $ sk_string_new_with_copy ptr (fromIntegral len)
    )
    sk_string_destructor

-- | O(n). Creates an SKString by copying from a 'T.Text'.
createFromText :: (MonadResource m) => T.Text -> m (ReleaseKey, SKString)
createFromText txt =
  -- TODO: Make it more efficient if possible. T.encodeUtf8 is awful, but
  -- technically T.Text is meant to be an opaque blob of text data...
  createFromByteString $ T.encodeUtf8 txt

-- | O(n). Creates an SKString by copying from a 'String'.
createFromString :: (MonadResource m) => String -> m (ReleaseKey, SKString)
createFromString string = do
  -- NOTE: Do not use Foreign.C.String.withCString, because it has no
  -- guarantee on what the marshalled output even is.
  createFromByteString $ BS.toStrict $ BSBuilder.toLazyByteString $ BSBuilder.stringUtf8 string

-- | Returns the number of bytes of the SKString.
getSize :: (MonadIO m) => SKString -> m Int
getSize str =
  liftIO $ fromIntegral <$> sk_string_get_size (ptr str)

{- | Exposes the **read-only** data pointer of the SKString. The encoding is
UTF-8.
-}
withData :: SKString -> (Ptr CChar -> IO r) -> IO r
withData str f = do
  p <- sk_string_get_c_str (ptr str)
  f p

-- | O(n). Builds a 'T.Text' by copying from the input 'SKString'.
getAsText :: (MonadIO m) => SKString -> m T.Text
getAsText str = liftIO do
  withData str \ptr -> do
    len <- getSize str
    T.peekCStringLen (ptr, len)

-- | Like 'getAsText' but returns 'String'.
getAsString :: (MonadIO m) => SKString -> m String
getAsString str = T.unpack <$> getAsText str

-- | O(n). Builds a ByteString by copying from the input SKString.
getAsByteStringUtf8 :: (MonadIO m) => SKString -> m BS.ByteString
getAsByteStringUtf8 str = liftIO do
  withData str \ptr -> do
    len <- getSize str
    BS.packCStringLen (ptr, len)
