module Skia.SKMemoryStream where

import Control.Monad.Trans.Resource
import Data.Text qualified as T
import Skia.Bindings.Sk_stream
import Skia.Internal.Prelude

create :: (MonadResource m) => m (ReleaseKey, SKMemoryStream)
create =
  allocateSKObjectOrErrorIfNull
    "Failed to create SKMemoryStream"
    sk_memorystream_new
    sk_memorystream_destroy

createWithLength ::
  (MonadResource m) =>
  -- | Byte length
  Int ->
  m (ReleaseKey, SKMemoryStream)
createWithLength len =
  allocateSKObjectOrErrorIfNull
    ("Failed to create SKMemoryStream with length " <> T.pack (show len))
    (sk_memorystream_new_with_length (fromIntegral len))
    sk_memorystream_destroy

createWithData ::
  (MonadResource m) =>
  -- | Buffer
  Ptr Word8 ->
  -- | Buffer size
  Int ->
  -- | Copy data?
  Bool ->
  m (ReleaseKey, SKMemoryStream)
createWithData buffer size copyData =
  allocateSKObjectOrErrorIfNull
    "Failed to create SKMemoryStream with input data"
    (sk_memorystream_new_with_data (castPtr buffer) (fromIntegral size) (fromBool copyData))
    sk_memorystream_destroy

createWithSKData :: (MonadResource m) => SKData -> m (ReleaseKey, SKMemoryStream)
createWithSKData dat =
  allocateSKObjectOrErrorIfNull
    "Failed to create SKMemoryStream with input SKData"
    (sk_memorystream_new_with_skdata (ptr dat))
    sk_memorystream_destroy

setMemory ::
  (MonadIO m) =>
  SKMemoryStream ->
  -- | Buffer
  Ptr Word8 ->
  -- | Buffer size
  Int ->
  -- | Copy data?
  Bool ->
  m ()
setMemory stream buffer size copyData = liftIO do
  sk_memorystream_set_memory
    (ptr stream)
    (castPtr buffer)
    (fromIntegral size)
    (fromBool copyData)
