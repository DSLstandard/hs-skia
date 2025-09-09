module Skia.SkData where

import Data.ByteString qualified as BS
import Language.C.Inline.Cpp qualified as C
import Skia.Internal.Prelude
import Skia.SkRefCnt qualified as SkRefCnt

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkData.h"

-- * Creating 'SkData'

-- | Returns a new empty dataref (or a reference to a shared empty dataref).
createEmpty :: (MonadResource m) => m (ReleaseKey, SkData)
createEmpty =
  allocateSkObjectNeverNull'
    [C.exp| SkData* {
      SkData::MakeEmpty().release()
    }|]
    SkRefCnt.decrementNV

-- | O(n). Create a new dataref by copying the input 'BS.ByteString'.
createCopy :: (MonadResource m) => BS.ByteString -> m (ReleaseKey, SkData)
createCopy bytestring =
  allocateSkObjectNeverNull'
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
        (castPtr -> ptr, fromIntegral -> count) <- managed $ BS.useAsCStringLen bytestring
        liftIO
          [C.exp| SkData* {
            SkData::MakeWithCopy($(void* ptr), $(size_t count)).release()
          }|]
    )
    SkRefCnt.decrementNV

{- | Create a new data with uninitialized contents.

The returned 'SkData' is unreferenced when 'Acquire' releases.
-}
createUninitialized ::
  (MonadResource m) =>
  -- | Number of bytes to allocate. The content will be uninitialized.
  Int ->
  m (ReleaseKey, SkData)
createUninitialized (fromIntegral -> size) =
  allocateSkObjectNeverNull'
    [C.exp| SkData* {
      SkData::MakeUninitialized($(size_t size)).release()
    }|]
    SkRefCnt.decrementNV

{- | Create a new dataref using a subset of the data in the specified src
dataref.

The returned 'SkData' is unreferenced when 'Acquire' releases.
-}
createSubset ::
  (MonadResource m) =>
  SkData ->
  -- | Byte offset
  Int ->
  -- | Byte length
  Int ->
  m (ReleaseKey, SkData)
createSubset (ptr -> dat) (fromIntegral -> offset) (fromIntegral -> len) =
  allocateSkObjectNeverNull'
    [C.exp| SkData* {
      SkData::MakeSubset($(SkData* dat), $(size_t offset), $(size_t len)).release()
    }|]
    SkRefCnt.decrementNV

{- | Create a new dataref by reading from a file given its 'FilePath'.

Raises 'SkiaError' on failure.

The returned 'SkData' is unreferenced when 'Acquire' releases.
-}
createFromFile :: (MonadResource m) => FilePath -> m (ReleaseKey, SkData)
createFromFile path =
  allocateSkObjectNeverNull'
    ( evalManaged do
      path' <- managed $ withCString path
      liftIO
        [C.exp| SkData* {
          SkData::MakeFromFileName($(const char* path')).release()
        }|]
    )
    SkRefCnt.decrementNV

{- | Attempt to read size bytes into a 'SkData'.

If the read succeeds, return the data, else an 'SkiaError' is raised. Either way
the stream's cursor may have been changed as a result of calling read().

The returned 'SkData' is unreferenced when 'Acquire' releases.
-}
createFromStream ::
  (IsSkStream stream, MonadResource m) =>
  stream ->
  -- | Number of bytes to read from stream
  Int ->
  m (ReleaseKey, SkData)
createFromStream (ptr . toA SkStream -> stream) (fromIntegral -> len) =
  allocateSkObjectOrErrorIfNull'
    "Cannot read data from input stream"
    [C.exp| SkData* {
      SkData::MakeFromStream($(SkStream* stream), $(size_t len)).release()
    }|]
    SkRefCnt.decrementNV

-- * Misc utilities

-- | Returns the number of bytes stored.
getSize :: (MonadIO m) => SkData -> m Int
getSize (ptr -> dat) = liftIO do
  fromIntegral <$> [C.exp| size_t { $(SkData* dat)->size() }|]

{- | O(1). Exposes the **read-only** pointer to the data.

You must not modify the content under the pointer.
-}
withData :: SkData -> (Ptr Word8 -> IO r) -> IO r
withData (ptr -> dat) f = do
  dat <- liftIO $ [C.exp| const void* { $(SkData* dat)->data() }|]
  f (castPtr dat)

{- | O(n). Convenience function. Builds a 'BS.ByteString' by copying from the
input 'SkData'.
-}
getByteString :: (MonadIO m) => SkData -> m BS.ByteString
getByteString dat = liftIO do
  -- TODO: Make this O(1) with epic ByteString + ForeignPtr + holding the
  -- SkData's refcnt hostage hacks?
  withData dat \ptr -> do
    sz <- getSize dat
    BS.packCStringLen (castPtr ptr, fromIntegral sz)
