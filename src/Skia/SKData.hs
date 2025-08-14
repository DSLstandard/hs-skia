module Skia.SKData where

import Control.Exception
import Data.ByteString qualified as BS
import Skia.Bindings.Sk_data
import Skia.Internal.Prelude
import Skia.SKRefCnt qualified as SKRefCnt

-- * Creating 'SKData'

-- | Returns a new empty dataref (or a reference to a shared empty dataref).
createEmpty :: (MonadResource m) => m (ReleaseKey, SKData)
createEmpty = allocateSKObjectNeverNull' sk_data_new_empty SKRefCnt.decrementNV

-- | O(n). Create a new dataref by copying the input 'BS.ByteString'.
createCopy :: (MonadResource m) => BS.ByteString -> m (ReleaseKey, SKData)
createCopy bytestring =
  allocateSKObjectNeverNull'
    {-
        TODO: Possible O(1) implementation?

        -- The StablePtr keeps the ByteString's alive, which in turn keeps the
        -- 'ForeignPtr' data pointer alive, until 'createWithRelease' is done.
        bsStablePtr :: StablePtr BS.ByteString <- newStablePtr bs
        let onRelease = freeStablePtr bsStablePtr
        let (dataptr, len) = BS.toForeignPtr0 bs
        createWithRelease (unsafeForeignPtrToPtr dataptr) (fromIntegral len) onRelease
    -}
    ( evalManaged do
        (ptr, count) <- managed $ BS.useAsCStringLen bytestring
        liftIO $ sk_data_new_with_copy (castPtr ptr) (fromIntegral count)
    )
    SKRefCnt.decrementNV

{- | Create a new data with uninitialized contents.

The returned 'SKData' is unreferenced when 'Acquire' releases.
-}
createUninitialized ::
  (MonadResource m) =>
  -- | Number of bytes to allocate. The content will be uninitialized.
  Int ->
  m (ReleaseKey, SKData)
createUninitialized size =
  allocateSKObjectNeverNull'
    (sk_data_new_uninitialized (fromIntegral size))
    SKRefCnt.decrementNV

{- | Create a new dataref using a subset of the data in the specified src
dataref.

The returned 'SKData' is unreferenced when 'Acquire' releases.
-}
createSubset ::
  (MonadResource m) =>
  SKData ->
  -- | Byte offset
  Int ->
  -- | Byte length
  Int ->
  m (ReleaseKey, SKData)
createSubset dat offset len =
  allocateSKObjectNeverNull'
    (sk_data_new_subset (ptr dat) (fromIntegral offset) (fromIntegral len))
    SKRefCnt.decrementNV

{- | Create a new dataref by reading from a file given its 'FilePath'.

Raises 'SkiaError' on failure.

The returned 'SKData' is unreferenced when 'Acquire' releases.
-}
createFromFile :: (MonadResource m) => FilePath -> m (ReleaseKey, SKData)
createFromFile path =
  allocateSKObjectNeverNull'
    ( evalManaged do
        path' <- managed $ withCString path
        dat' <- liftIO $ sk_data_new_from_file path'
        when (dat' == nullPtr) do
          liftIO $ throwIO $ SkiaError $ "Cannot create SKData from file path: " <> show path
        pure dat'
    )
    SKRefCnt.decrementNV

{- | Attempt to read size bytes into a 'SkData'.

If the read succeeds, return the data, else an 'SkiaError' is raised. Either way
the stream's cursor may have been changed as a result of calling read().

The returned 'SKData' is unreferenced when 'Acquire' releases.
-}
createFromStream ::
  (IsSKStream stream, MonadResource m) =>
  stream ->
  -- | Number of bytes to read from stream
  Int ->
  m (ReleaseKey, SKData)
createFromStream (toA SKStream -> stream) len =
  allocateSKObjectOrErrorIfNull'
    "Cannot read data from input stream"
    (sk_data_new_from_stream (ptr stream) (fromIntegral len))
    SKRefCnt.decrementNV

-- * Misc utilities

-- | Returns the number of bytes stored.
getSize :: (MonadIO m) => SKData -> m Int
getSize dat = evalManaged do
  liftIO $ fromIntegral <$> sk_data_get_size (ptr dat)

{- | O(1). Exposes the **read-only** pointer to the data.

You must not modify the content under the pointer.
-}
withData :: SKData -> (Ptr Word8 -> IO r) -> IO r
withData dat f = do
  dat <- liftIO $ sk_data_get_bytes (ptr dat)
  f dat

{- | O(n). Convenience function. Builds a 'BS.ByteString' by copying from the
input 'SKData'.
-}
getByteString :: (MonadIO m) => SKData -> m BS.ByteString
getByteString dat = liftIO do
  -- TODO: Make this O(1) with epic ByteString + ForeignPtr + holding the
  -- SKData's refcnt hostage hacks?
  withData dat \ptr -> do
    sz <- getSize dat
    BS.packCStringLen (castPtr ptr, fromIntegral sz)
