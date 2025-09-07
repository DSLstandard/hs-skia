module Skia.SkMemoryStream where

import Data.Text qualified as T
import Skia.Internal.Prelude
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkStream.h"

create :: (MonadResource m) => m (ReleaseKey, SkMemoryStream)
create =
  allocateSkObjectOrErrorIfNull
    "Failed to create SkMemoryStream"
    [C.exp| SkMemoryStream* {
      new SkMemoryStream()
    }|]
    (\p -> [C.block| void { delete $(SkMemoryStream* p); } |])

createWithLength ::
  (MonadResource m) =>
  -- | Byte length
  Int ->
  m (ReleaseKey, SkMemoryStream)
createWithLength (fromIntegral -> len) =
  allocateSkObjectOrErrorIfNull
    ("Failed to create SkMemoryStream with length " <> T.pack (show len))
    [C.exp| SkMemoryStream* {
      new SkMemoryStream($(size_t len))
    }|]
    (\p -> [C.block| void { delete $(SkMemoryStream* p); } |])

createWithData ::
  (MonadResource m) =>
  -- | Buffer
  Ptr Word8 ->
  -- | Buffer size
  Int ->
  -- | Copy data?
  Bool ->
  m (ReleaseKey, SkMemoryStream)
createWithData (castPtr -> buffer) (fromIntegral -> size) (fromBool -> copyData) =
  allocateSkObjectOrErrorIfNull
    "Failed to create SkMemoryStream with input data"
    [C.exp| SkMemoryStream* {
      new SkMemoryStream($(void* buffer), $(size_t size), $(bool copyData))
    }|]
    (\p -> [C.block| void { delete $(SkMemoryStream* p); } |])

createWithSkData :: (MonadResource m) => SkData -> m (ReleaseKey, SkMemoryStream)
createWithSkData (ptr -> dat) =
  allocateSkObjectOrErrorIfNull
    "Failed to create SkMemoryStream with input SkData"
    [C.exp| SkMemoryStream* {
      new SkMemoryStream(sk_ref_sp($(SkData* dat)))
    }|]
    (\p -> [C.block| void { delete $(SkMemoryStream* p); } |])

setMemory ::
  (MonadIO m) =>
  SkMemoryStream ->
  -- | Buffer
  Ptr Word8 ->
  -- | Buffer size
  Int ->
  -- | Copy data?
  Bool ->
  m ()
setMemory (ptr -> stream) (castPtr -> buffer) (fromIntegral -> size) (fromBool -> copyData) = liftIO $
  [C.block| void {
    $(SkMemoryStream* stream)->setMemory($(void* buffer), $(size_t size), $(bool copyData));
  }|]
