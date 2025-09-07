module Skia.SkString where

import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BSBuilder
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Foreign qualified as T
import Skia.Internal.Prelude
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkString.h"

-- | Creates an empty SkString
createEmpty :: (MonadResource m) => m (ReleaseKey, SkString)
createEmpty =
  allocateSkObjectNeverNull
    [C.exp| SkString* { new SkString() } |]
    (\p -> [C.block| void { delete $(SkString* p); } |])

{- | O(n). Creates an SkString by copying from a 'BS.ByteString'. Assumes UTF-8
encoding.
-}

createFromByteString :: (MonadResource m) => BS.ByteString -> m (ReleaseKey, SkString)
createFromByteString bs =
  allocateSkObjectNeverNull
    ( evalManaged do
        (ptr, fromIntegral -> len) <- storableByteStringLen bs
        liftIO [C.exp| SkString* { new SkString($(const char* ptr), $(size_t len)) } |]
    )
    (\p -> [C.block| void { delete $(SkString* p); } |])

-- | O(n). Creates an SkString by copying from a 'T.Text'.
createFromText :: (MonadResource m) => T.Text -> m (ReleaseKey, SkString)
createFromText txt =
  -- TODO: Make it more efficient if possible. T.encodeUtf8 is awful, but
  -- technically T.Text is meant to be an opaque blob of text data...
  createFromByteString $ T.encodeUtf8 txt

-- | O(n). Creates an SkString by copying from a 'String'.
createFromString :: (MonadResource m) => String -> m (ReleaseKey, SkString)
createFromString string = do
  -- NOTE: Do not use Foreign.C.String.withCString, because it has no
  -- guarantee on what the marshalled output even is.
  createFromByteString $ BS.toStrict $ BSBuilder.toLazyByteString $ BSBuilder.stringUtf8 string

-- | Returns the number of bytes of the SkString.
getSize :: (MonadIO m) => SkString -> m Int
getSize (ptr -> str) = fromIntegral <$> liftIO [C.exp| size_t { $(SkString* str)->size() } |]

{- | Exposes the **read-only** data pointer of the SkString. The encoding is
UTF-8.
-}
withData :: SkString -> (Ptr CChar -> IO r) -> IO r
withData (ptr -> str) f = do
  p <- [C.exp| const char* { $(SkString* str)->c_str() } |]
  f p

-- | O(n). Builds a 'T.Text' by copying from the input 'SkString'.
getAsText :: (MonadIO m) => SkString -> m T.Text
getAsText str = liftIO do
  withData str \ptr -> do
    len <- getSize str
    T.peekCStringLen (ptr, len)

-- | Like 'getAsText' but returns 'String'.
getAsString :: (MonadIO m) => SkString -> m String
getAsString str = T.unpack <$> getAsText str

-- | O(n). Builds a ByteString by copying from the input SkString.
getAsByteStringUtf8 :: (MonadIO m) => SkString -> m BS.ByteString
getAsByteStringUtf8 str = liftIO do
  withData str \ptr -> do
    len <- getSize str
    BS.packCStringLen (ptr, len)
