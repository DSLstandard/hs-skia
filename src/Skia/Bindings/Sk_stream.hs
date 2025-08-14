{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This is an auto-generated FFI binding module.
module Skia.Bindings.Sk_stream where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void sk_stream_asset_destroy(sk_stream_asset_t *cstream)
@
-}
foreign import ccall "sk_stream_asset_destroy" sk_stream_asset_destroy ::
  Ptr (Sk_stream_asset) -- ^ C argument @"sk_stream_asset_t * cstream"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_stream_asset_destroy'
foreign import ccall "&sk_stream_asset_destroy" p'sk_stream_asset_destroy ::
  FunPtr (Ptr (Sk_stream_asset) -> IO (()))

{- | C function signature:

@
sk_stream_filestream_t *sk_filestream_new(const char *path)
@
-}
foreign import ccall "sk_filestream_new" sk_filestream_new ::
  Ptr (CChar) -- ^ C argument @"const char * path"@
  -> IO (Ptr (Sk_stream_filestream)) -- ^ C return type: @"sk_stream_filestream_t *"@

-- | Function pointer to 'sk_filestream_new'
foreign import ccall "&sk_filestream_new" p'sk_filestream_new ::
  FunPtr (Ptr (CChar) -> IO (Ptr (Sk_stream_filestream)))

{- | C function signature:

@
void sk_filestream_destroy(sk_stream_filestream_t *cstream)
@
-}
foreign import ccall "sk_filestream_destroy" sk_filestream_destroy ::
  Ptr (Sk_stream_filestream) -- ^ C argument @"sk_stream_filestream_t * cstream"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_filestream_destroy'
foreign import ccall "&sk_filestream_destroy" p'sk_filestream_destroy ::
  FunPtr (Ptr (Sk_stream_filestream) -> IO (()))

{- | C function signature:

@
_Bool sk_filestream_is_valid(sk_stream_filestream_t *cstream)
@
-}
foreign import ccall "sk_filestream_is_valid" sk_filestream_is_valid ::
  Ptr (Sk_stream_filestream) -- ^ C argument @"sk_stream_filestream_t * cstream"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_filestream_is_valid'
foreign import ccall "&sk_filestream_is_valid" p'sk_filestream_is_valid ::
  FunPtr (Ptr (Sk_stream_filestream) -> IO (CBool))

{- | C function signature:

@
sk_stream_memorystream_t *sk_memorystream_new(void)
@
-}
foreign import ccall "sk_memorystream_new" sk_memorystream_new ::
  IO (Ptr (Sk_stream_memorystream)) -- ^ C return type: @"sk_stream_memorystream_t *"@

-- | Function pointer to 'sk_memorystream_new'
foreign import ccall "&sk_memorystream_new" p'sk_memorystream_new ::
  FunPtr (IO (Ptr (Sk_stream_memorystream)))

{- | C function signature:

@
sk_stream_memorystream_t *sk_memorystream_new_with_length(size_t length)
@
-}
foreign import ccall "sk_memorystream_new_with_length" sk_memorystream_new_with_length ::
  CSize -- ^ C argument @"size_t length"@
  -> IO (Ptr (Sk_stream_memorystream)) -- ^ C return type: @"sk_stream_memorystream_t *"@

-- | Function pointer to 'sk_memorystream_new_with_length'
foreign import ccall "&sk_memorystream_new_with_length" p'sk_memorystream_new_with_length ::
  FunPtr (CSize -> IO (Ptr (Sk_stream_memorystream)))

{- | C function signature:

@
sk_stream_memorystream_t *sk_memorystream_new_with_data(const void *data, size_t length, _Bool copyData)
@
-}
foreign import ccall "sk_memorystream_new_with_data" sk_memorystream_new_with_data ::
  Ptr (()) -- ^ C argument @"const void * data"@
  -> CSize -- ^ C argument @"size_t length"@
  -> CBool -- ^ C argument @"_Bool copyData"@
  -> IO (Ptr (Sk_stream_memorystream)) -- ^ C return type: @"sk_stream_memorystream_t *"@

-- | Function pointer to 'sk_memorystream_new_with_data'
foreign import ccall "&sk_memorystream_new_with_data" p'sk_memorystream_new_with_data ::
  FunPtr (Ptr (()) -> CSize -> CBool -> IO (Ptr (Sk_stream_memorystream)))

{- | C function signature:

@
sk_stream_memorystream_t *sk_memorystream_new_with_skdata(sk_data_t *data)
@
-}
foreign import ccall "sk_memorystream_new_with_skdata" sk_memorystream_new_with_skdata ::
  Ptr (Sk_data) -- ^ C argument @"sk_data_t * data"@
  -> IO (Ptr (Sk_stream_memorystream)) -- ^ C return type: @"sk_stream_memorystream_t *"@

-- | Function pointer to 'sk_memorystream_new_with_skdata'
foreign import ccall "&sk_memorystream_new_with_skdata" p'sk_memorystream_new_with_skdata ::
  FunPtr (Ptr (Sk_data) -> IO (Ptr (Sk_stream_memorystream)))

{- | C function signature:

@
void sk_memorystream_set_memory(sk_stream_memorystream_t *cmemorystream, const void *data, size_t length, _Bool copyData)
@
-}
foreign import ccall "sk_memorystream_set_memory" sk_memorystream_set_memory ::
  Ptr (Sk_stream_memorystream) -- ^ C argument @"sk_stream_memorystream_t * cmemorystream"@
  -> Ptr (()) -- ^ C argument @"const void * data"@
  -> CSize -- ^ C argument @"size_t length"@
  -> CBool -- ^ C argument @"_Bool copyData"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_memorystream_set_memory'
foreign import ccall "&sk_memorystream_set_memory" p'sk_memorystream_set_memory ::
  FunPtr (Ptr (Sk_stream_memorystream) -> Ptr (()) -> CSize -> CBool -> IO (()))

{- | C function signature:

@
void sk_memorystream_destroy(sk_stream_memorystream_t *cstream)
@
-}
foreign import ccall "sk_memorystream_destroy" sk_memorystream_destroy ::
  Ptr (Sk_stream_memorystream) -- ^ C argument @"sk_stream_memorystream_t * cstream"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_memorystream_destroy'
foreign import ccall "&sk_memorystream_destroy" p'sk_memorystream_destroy ::
  FunPtr (Ptr (Sk_stream_memorystream) -> IO (()))

{- | C function signature:

@
size_t sk_stream_read(sk_stream_t *cstream, void *buffer, size_t size)
@
-}
foreign import ccall "sk_stream_read" sk_stream_read ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> Ptr (()) -- ^ C argument @"void * buffer"@
  -> CSize -- ^ C argument @"size_t size"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_stream_read'
foreign import ccall "&sk_stream_read" p'sk_stream_read ::
  FunPtr (Ptr (Sk_stream) -> Ptr (()) -> CSize -> IO (CSize))

{- | C function signature:

@
size_t sk_stream_peek(sk_stream_t *cstream, void *buffer, size_t size)
@
-}
foreign import ccall "sk_stream_peek" sk_stream_peek ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> Ptr (()) -- ^ C argument @"void * buffer"@
  -> CSize -- ^ C argument @"size_t size"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_stream_peek'
foreign import ccall "&sk_stream_peek" p'sk_stream_peek ::
  FunPtr (Ptr (Sk_stream) -> Ptr (()) -> CSize -> IO (CSize))

{- | C function signature:

@
size_t sk_stream_skip(sk_stream_t *cstream, size_t size)
@
-}
foreign import ccall "sk_stream_skip" sk_stream_skip ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> CSize -- ^ C argument @"size_t size"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_stream_skip'
foreign import ccall "&sk_stream_skip" p'sk_stream_skip ::
  FunPtr (Ptr (Sk_stream) -> CSize -> IO (CSize))

{- | C function signature:

@
_Bool sk_stream_is_at_end(sk_stream_t *cstream)
@
-}
foreign import ccall "sk_stream_is_at_end" sk_stream_is_at_end ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_stream_is_at_end'
foreign import ccall "&sk_stream_is_at_end" p'sk_stream_is_at_end ::
  FunPtr (Ptr (Sk_stream) -> IO (CBool))

{- | C function signature:

@
_Bool sk_stream_read_s8(sk_stream_t *cstream, int8_t *buffer)
@
-}
foreign import ccall "sk_stream_read_s8" sk_stream_read_s8 ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> Ptr (Int8) -- ^ C argument @"int8_t * buffer"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_stream_read_s8'
foreign import ccall "&sk_stream_read_s8" p'sk_stream_read_s8 ::
  FunPtr (Ptr (Sk_stream) -> Ptr (Int8) -> IO (CBool))

{- | C function signature:

@
_Bool sk_stream_read_s16(sk_stream_t *cstream, int16_t *buffer)
@
-}
foreign import ccall "sk_stream_read_s16" sk_stream_read_s16 ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> Ptr (Int16) -- ^ C argument @"int16_t * buffer"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_stream_read_s16'
foreign import ccall "&sk_stream_read_s16" p'sk_stream_read_s16 ::
  FunPtr (Ptr (Sk_stream) -> Ptr (Int16) -> IO (CBool))

{- | C function signature:

@
_Bool sk_stream_read_s32(sk_stream_t *cstream, int32_t *buffer)
@
-}
foreign import ccall "sk_stream_read_s32" sk_stream_read_s32 ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> Ptr (Int32) -- ^ C argument @"int32_t * buffer"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_stream_read_s32'
foreign import ccall "&sk_stream_read_s32" p'sk_stream_read_s32 ::
  FunPtr (Ptr (Sk_stream) -> Ptr (Int32) -> IO (CBool))

{- | C function signature:

@
_Bool sk_stream_read_u8(sk_stream_t *cstream, uint8_t *buffer)
@
-}
foreign import ccall "sk_stream_read_u8" sk_stream_read_u8 ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> Ptr (Word8) -- ^ C argument @"uint8_t * buffer"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_stream_read_u8'
foreign import ccall "&sk_stream_read_u8" p'sk_stream_read_u8 ::
  FunPtr (Ptr (Sk_stream) -> Ptr (Word8) -> IO (CBool))

{- | C function signature:

@
_Bool sk_stream_read_u16(sk_stream_t *cstream, uint16_t *buffer)
@
-}
foreign import ccall "sk_stream_read_u16" sk_stream_read_u16 ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> Ptr (Word16) -- ^ C argument @"uint16_t * buffer"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_stream_read_u16'
foreign import ccall "&sk_stream_read_u16" p'sk_stream_read_u16 ::
  FunPtr (Ptr (Sk_stream) -> Ptr (Word16) -> IO (CBool))

{- | C function signature:

@
_Bool sk_stream_read_u32(sk_stream_t *cstream, uint32_t *buffer)
@
-}
foreign import ccall "sk_stream_read_u32" sk_stream_read_u32 ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> Ptr (Word32) -- ^ C argument @"uint32_t * buffer"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_stream_read_u32'
foreign import ccall "&sk_stream_read_u32" p'sk_stream_read_u32 ::
  FunPtr (Ptr (Sk_stream) -> Ptr (Word32) -> IO (CBool))

{- | C function signature:

@
_Bool sk_stream_read_bool(sk_stream_t *cstream, _Bool *buffer)
@
-}
foreign import ccall "sk_stream_read_bool" sk_stream_read_bool ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> Ptr (CBool) -- ^ C argument @"_Bool * buffer"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_stream_read_bool'
foreign import ccall "&sk_stream_read_bool" p'sk_stream_read_bool ::
  FunPtr (Ptr (Sk_stream) -> Ptr (CBool) -> IO (CBool))

{- | C function signature:

@
_Bool sk_stream_rewind(sk_stream_t *cstream)
@
-}
foreign import ccall "sk_stream_rewind" sk_stream_rewind ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_stream_rewind'
foreign import ccall "&sk_stream_rewind" p'sk_stream_rewind ::
  FunPtr (Ptr (Sk_stream) -> IO (CBool))

{- | C function signature:

@
_Bool sk_stream_has_position(sk_stream_t *cstream)
@
-}
foreign import ccall "sk_stream_has_position" sk_stream_has_position ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_stream_has_position'
foreign import ccall "&sk_stream_has_position" p'sk_stream_has_position ::
  FunPtr (Ptr (Sk_stream) -> IO (CBool))

{- | C function signature:

@
size_t sk_stream_get_position(sk_stream_t *cstream)
@
-}
foreign import ccall "sk_stream_get_position" sk_stream_get_position ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_stream_get_position'
foreign import ccall "&sk_stream_get_position" p'sk_stream_get_position ::
  FunPtr (Ptr (Sk_stream) -> IO (CSize))

{- | C function signature:

@
_Bool sk_stream_seek(sk_stream_t *cstream, size_t position)
@
-}
foreign import ccall "sk_stream_seek" sk_stream_seek ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> CSize -- ^ C argument @"size_t position"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_stream_seek'
foreign import ccall "&sk_stream_seek" p'sk_stream_seek ::
  FunPtr (Ptr (Sk_stream) -> CSize -> IO (CBool))

{- | C function signature:

@
_Bool sk_stream_move(sk_stream_t *cstream, long offset)
@
-}
foreign import ccall "sk_stream_move" sk_stream_move ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> CLong -- ^ C argument @"long offset"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_stream_move'
foreign import ccall "&sk_stream_move" p'sk_stream_move ::
  FunPtr (Ptr (Sk_stream) -> CLong -> IO (CBool))

{- | C function signature:

@
_Bool sk_stream_has_length(sk_stream_t *cstream)
@
-}
foreign import ccall "sk_stream_has_length" sk_stream_has_length ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_stream_has_length'
foreign import ccall "&sk_stream_has_length" p'sk_stream_has_length ::
  FunPtr (Ptr (Sk_stream) -> IO (CBool))

{- | C function signature:

@
size_t sk_stream_get_length(sk_stream_t *cstream)
@
-}
foreign import ccall "sk_stream_get_length" sk_stream_get_length ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_stream_get_length'
foreign import ccall "&sk_stream_get_length" p'sk_stream_get_length ::
  FunPtr (Ptr (Sk_stream) -> IO (CSize))

{- | C function signature:

@
const void *sk_stream_get_memory_base(sk_stream_t *cstream)
@
-}
foreign import ccall "sk_stream_get_memory_base" sk_stream_get_memory_base ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> IO (Ptr (())) -- ^ C return type: @"const void *"@

-- | Function pointer to 'sk_stream_get_memory_base'
foreign import ccall "&sk_stream_get_memory_base" p'sk_stream_get_memory_base ::
  FunPtr (Ptr (Sk_stream) -> IO (Ptr (())))

{- | C function signature:

@
sk_stream_t *sk_stream_fork(sk_stream_t *cstream)
@
-}
foreign import ccall "sk_stream_fork" sk_stream_fork ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> IO (Ptr (Sk_stream)) -- ^ C return type: @"sk_stream_t *"@

-- | Function pointer to 'sk_stream_fork'
foreign import ccall "&sk_stream_fork" p'sk_stream_fork ::
  FunPtr (Ptr (Sk_stream) -> IO (Ptr (Sk_stream)))

{- | C function signature:

@
sk_stream_t *sk_stream_duplicate(sk_stream_t *cstream)
@
-}
foreign import ccall "sk_stream_duplicate" sk_stream_duplicate ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> IO (Ptr (Sk_stream)) -- ^ C return type: @"sk_stream_t *"@

-- | Function pointer to 'sk_stream_duplicate'
foreign import ccall "&sk_stream_duplicate" p'sk_stream_duplicate ::
  FunPtr (Ptr (Sk_stream) -> IO (Ptr (Sk_stream)))

{- | C function signature:

@
void sk_stream_destroy(sk_stream_t *cstream)
@
-}
foreign import ccall "sk_stream_destroy" sk_stream_destroy ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * cstream"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_stream_destroy'
foreign import ccall "&sk_stream_destroy" p'sk_stream_destroy ::
  FunPtr (Ptr (Sk_stream) -> IO (()))

{- | C function signature:

@
sk_wstream_filestream_t *sk_filewstream_new(const char *path)
@
-}
foreign import ccall "sk_filewstream_new" sk_filewstream_new ::
  Ptr (CChar) -- ^ C argument @"const char * path"@
  -> IO (Ptr (Sk_wstream_filestream)) -- ^ C return type: @"sk_wstream_filestream_t *"@

-- | Function pointer to 'sk_filewstream_new'
foreign import ccall "&sk_filewstream_new" p'sk_filewstream_new ::
  FunPtr (Ptr (CChar) -> IO (Ptr (Sk_wstream_filestream)))

{- | C function signature:

@
void sk_filewstream_destroy(sk_wstream_filestream_t *cstream)
@
-}
foreign import ccall "sk_filewstream_destroy" sk_filewstream_destroy ::
  Ptr (Sk_wstream_filestream) -- ^ C argument @"sk_wstream_filestream_t * cstream"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_filewstream_destroy'
foreign import ccall "&sk_filewstream_destroy" p'sk_filewstream_destroy ::
  FunPtr (Ptr (Sk_wstream_filestream) -> IO (()))

{- | C function signature:

@
_Bool sk_filewstream_is_valid(sk_wstream_filestream_t *cstream)
@
-}
foreign import ccall "sk_filewstream_is_valid" sk_filewstream_is_valid ::
  Ptr (Sk_wstream_filestream) -- ^ C argument @"sk_wstream_filestream_t * cstream"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_filewstream_is_valid'
foreign import ccall "&sk_filewstream_is_valid" p'sk_filewstream_is_valid ::
  FunPtr (Ptr (Sk_wstream_filestream) -> IO (CBool))

{- | C function signature:

@
sk_wstream_dynamicmemorystream_t *sk_dynamicmemorywstream_new(void)
@
-}
foreign import ccall "sk_dynamicmemorywstream_new" sk_dynamicmemorywstream_new ::
  IO (Ptr (Sk_wstream_dynamicmemorystream)) -- ^ C return type: @"sk_wstream_dynamicmemorystream_t *"@

-- | Function pointer to 'sk_dynamicmemorywstream_new'
foreign import ccall "&sk_dynamicmemorywstream_new" p'sk_dynamicmemorywstream_new ::
  FunPtr (IO (Ptr (Sk_wstream_dynamicmemorystream)))

{- | C function signature:

@
sk_stream_asset_t *sk_dynamicmemorywstream_detach_as_stream(sk_wstream_dynamicmemorystream_t *cstream)
@
-}
foreign import ccall "sk_dynamicmemorywstream_detach_as_stream" sk_dynamicmemorywstream_detach_as_stream ::
  Ptr (Sk_wstream_dynamicmemorystream) -- ^ C argument @"sk_wstream_dynamicmemorystream_t * cstream"@
  -> IO (Ptr (Sk_stream_asset)) -- ^ C return type: @"sk_stream_asset_t *"@

-- | Function pointer to 'sk_dynamicmemorywstream_detach_as_stream'
foreign import ccall "&sk_dynamicmemorywstream_detach_as_stream" p'sk_dynamicmemorywstream_detach_as_stream ::
  FunPtr (Ptr (Sk_wstream_dynamicmemorystream) -> IO (Ptr (Sk_stream_asset)))

{- | C function signature:

@
sk_data_t *sk_dynamicmemorywstream_detach_as_data(sk_wstream_dynamicmemorystream_t *cstream)
@
-}
foreign import ccall "sk_dynamicmemorywstream_detach_as_data" sk_dynamicmemorywstream_detach_as_data ::
  Ptr (Sk_wstream_dynamicmemorystream) -- ^ C argument @"sk_wstream_dynamicmemorystream_t * cstream"@
  -> IO (Ptr (Sk_data)) -- ^ C return type: @"sk_data_t *"@

-- | Function pointer to 'sk_dynamicmemorywstream_detach_as_data'
foreign import ccall "&sk_dynamicmemorywstream_detach_as_data" p'sk_dynamicmemorywstream_detach_as_data ::
  FunPtr (Ptr (Sk_wstream_dynamicmemorystream) -> IO (Ptr (Sk_data)))

{- | C function signature:

@
void sk_dynamicmemorywstream_copy_to(sk_wstream_dynamicmemorystream_t *cstream, void *data)
@
-}
foreign import ccall "sk_dynamicmemorywstream_copy_to" sk_dynamicmemorywstream_copy_to ::
  Ptr (Sk_wstream_dynamicmemorystream) -- ^ C argument @"sk_wstream_dynamicmemorystream_t * cstream"@
  -> Ptr (()) -- ^ C argument @"void * data"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_dynamicmemorywstream_copy_to'
foreign import ccall "&sk_dynamicmemorywstream_copy_to" p'sk_dynamicmemorywstream_copy_to ::
  FunPtr (Ptr (Sk_wstream_dynamicmemorystream) -> Ptr (()) -> IO (()))

{- | C function signature:

@
_Bool sk_dynamicmemorywstream_write_to_stream(sk_wstream_dynamicmemorystream_t *cstream, sk_wstream_t *dst)
@
-}
foreign import ccall "sk_dynamicmemorywstream_write_to_stream" sk_dynamicmemorywstream_write_to_stream ::
  Ptr (Sk_wstream_dynamicmemorystream) -- ^ C argument @"sk_wstream_dynamicmemorystream_t * cstream"@
  -> Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * dst"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_dynamicmemorywstream_write_to_stream'
foreign import ccall "&sk_dynamicmemorywstream_write_to_stream" p'sk_dynamicmemorywstream_write_to_stream ::
  FunPtr (Ptr (Sk_wstream_dynamicmemorystream) -> Ptr (Sk_wstream) -> IO (CBool))

{- | C function signature:

@
void sk_dynamicmemorywstream_destroy(sk_wstream_dynamicmemorystream_t *cstream)
@
-}
foreign import ccall "sk_dynamicmemorywstream_destroy" sk_dynamicmemorywstream_destroy ::
  Ptr (Sk_wstream_dynamicmemorystream) -- ^ C argument @"sk_wstream_dynamicmemorystream_t * cstream"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_dynamicmemorywstream_destroy'
foreign import ccall "&sk_dynamicmemorywstream_destroy" p'sk_dynamicmemorywstream_destroy ::
  FunPtr (Ptr (Sk_wstream_dynamicmemorystream) -> IO (()))

{- | C function signature:

@
_Bool sk_wstream_write(sk_wstream_t *cstream, const void *buffer, size_t size)
@
-}
foreign import ccall "sk_wstream_write" sk_wstream_write ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * cstream"@
  -> Ptr (()) -- ^ C argument @"const void * buffer"@
  -> CSize -- ^ C argument @"size_t size"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_wstream_write'
foreign import ccall "&sk_wstream_write" p'sk_wstream_write ::
  FunPtr (Ptr (Sk_wstream) -> Ptr (()) -> CSize -> IO (CBool))

{- | C function signature:

@
_Bool sk_wstream_newline(sk_wstream_t *cstream)
@
-}
foreign import ccall "sk_wstream_newline" sk_wstream_newline ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * cstream"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_wstream_newline'
foreign import ccall "&sk_wstream_newline" p'sk_wstream_newline ::
  FunPtr (Ptr (Sk_wstream) -> IO (CBool))

{- | C function signature:

@
void sk_wstream_flush(sk_wstream_t *cstream)
@
-}
foreign import ccall "sk_wstream_flush" sk_wstream_flush ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * cstream"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_wstream_flush'
foreign import ccall "&sk_wstream_flush" p'sk_wstream_flush ::
  FunPtr (Ptr (Sk_wstream) -> IO (()))

{- | C function signature:

@
size_t sk_wstream_bytes_written(sk_wstream_t *cstream)
@
-}
foreign import ccall "sk_wstream_bytes_written" sk_wstream_bytes_written ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * cstream"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_wstream_bytes_written'
foreign import ccall "&sk_wstream_bytes_written" p'sk_wstream_bytes_written ::
  FunPtr (Ptr (Sk_wstream) -> IO (CSize))

{- | C function signature:

@
_Bool sk_wstream_write_8(sk_wstream_t *cstream, uint8_t value)
@
-}
foreign import ccall "sk_wstream_write_8" sk_wstream_write_8 ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * cstream"@
  -> Word8 -- ^ C argument @"uint8_t value"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_wstream_write_8'
foreign import ccall "&sk_wstream_write_8" p'sk_wstream_write_8 ::
  FunPtr (Ptr (Sk_wstream) -> Word8 -> IO (CBool))

{- | C function signature:

@
_Bool sk_wstream_write_16(sk_wstream_t *cstream, uint16_t value)
@
-}
foreign import ccall "sk_wstream_write_16" sk_wstream_write_16 ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * cstream"@
  -> Word16 -- ^ C argument @"uint16_t value"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_wstream_write_16'
foreign import ccall "&sk_wstream_write_16" p'sk_wstream_write_16 ::
  FunPtr (Ptr (Sk_wstream) -> Word16 -> IO (CBool))

{- | C function signature:

@
_Bool sk_wstream_write_32(sk_wstream_t *cstream, uint32_t value)
@
-}
foreign import ccall "sk_wstream_write_32" sk_wstream_write_32 ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * cstream"@
  -> Word32 -- ^ C argument @"uint32_t value"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_wstream_write_32'
foreign import ccall "&sk_wstream_write_32" p'sk_wstream_write_32 ::
  FunPtr (Ptr (Sk_wstream) -> Word32 -> IO (CBool))

{- | C function signature:

@
_Bool sk_wstream_write_text(sk_wstream_t *cstream, const char *value)
@
-}
foreign import ccall "sk_wstream_write_text" sk_wstream_write_text ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * cstream"@
  -> Ptr (CChar) -- ^ C argument @"const char * value"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_wstream_write_text'
foreign import ccall "&sk_wstream_write_text" p'sk_wstream_write_text ::
  FunPtr (Ptr (Sk_wstream) -> Ptr (CChar) -> IO (CBool))

{- | C function signature:

@
_Bool sk_wstream_write_dec_as_text(sk_wstream_t *cstream, int32_t value)
@
-}
foreign import ccall "sk_wstream_write_dec_as_text" sk_wstream_write_dec_as_text ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * cstream"@
  -> Int32 -- ^ C argument @"int32_t value"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_wstream_write_dec_as_text'
foreign import ccall "&sk_wstream_write_dec_as_text" p'sk_wstream_write_dec_as_text ::
  FunPtr (Ptr (Sk_wstream) -> Int32 -> IO (CBool))

{- | C function signature:

@
_Bool sk_wstream_write_bigdec_as_text(sk_wstream_t *cstream, int64_t value, int minDigits)
@
-}
foreign import ccall "sk_wstream_write_bigdec_as_text" sk_wstream_write_bigdec_as_text ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * cstream"@
  -> Int64 -- ^ C argument @"int64_t value"@
  -> CInt -- ^ C argument @"int minDigits"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_wstream_write_bigdec_as_text'
foreign import ccall "&sk_wstream_write_bigdec_as_text" p'sk_wstream_write_bigdec_as_text ::
  FunPtr (Ptr (Sk_wstream) -> Int64 -> CInt -> IO (CBool))

{- | C function signature:

@
_Bool sk_wstream_write_hex_as_text(sk_wstream_t *cstream, uint32_t value, int minDigits)
@
-}
foreign import ccall "sk_wstream_write_hex_as_text" sk_wstream_write_hex_as_text ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * cstream"@
  -> Word32 -- ^ C argument @"uint32_t value"@
  -> CInt -- ^ C argument @"int minDigits"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_wstream_write_hex_as_text'
foreign import ccall "&sk_wstream_write_hex_as_text" p'sk_wstream_write_hex_as_text ::
  FunPtr (Ptr (Sk_wstream) -> Word32 -> CInt -> IO (CBool))

{- | C function signature:

@
_Bool sk_wstream_write_scalar_as_text(sk_wstream_t *cstream, float value)
@
-}
foreign import ccall "sk_wstream_write_scalar_as_text" sk_wstream_write_scalar_as_text ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * cstream"@
  -> CFloat -- ^ C argument @"float value"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_wstream_write_scalar_as_text'
foreign import ccall "&sk_wstream_write_scalar_as_text" p'sk_wstream_write_scalar_as_text ::
  FunPtr (Ptr (Sk_wstream) -> CFloat -> IO (CBool))

{- | C function signature:

@
_Bool sk_wstream_write_bool(sk_wstream_t *cstream, _Bool value)
@
-}
foreign import ccall "sk_wstream_write_bool" sk_wstream_write_bool ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * cstream"@
  -> CBool -- ^ C argument @"_Bool value"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_wstream_write_bool'
foreign import ccall "&sk_wstream_write_bool" p'sk_wstream_write_bool ::
  FunPtr (Ptr (Sk_wstream) -> CBool -> IO (CBool))

{- | C function signature:

@
_Bool sk_wstream_write_scalar(sk_wstream_t *cstream, float value)
@
-}
foreign import ccall "sk_wstream_write_scalar" sk_wstream_write_scalar ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * cstream"@
  -> CFloat -- ^ C argument @"float value"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_wstream_write_scalar'
foreign import ccall "&sk_wstream_write_scalar" p'sk_wstream_write_scalar ::
  FunPtr (Ptr (Sk_wstream) -> CFloat -> IO (CBool))

{- | C function signature:

@
_Bool sk_wstream_write_packed_uint(sk_wstream_t *cstream, size_t value)
@
-}
foreign import ccall "sk_wstream_write_packed_uint" sk_wstream_write_packed_uint ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * cstream"@
  -> CSize -- ^ C argument @"size_t value"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_wstream_write_packed_uint'
foreign import ccall "&sk_wstream_write_packed_uint" p'sk_wstream_write_packed_uint ::
  FunPtr (Ptr (Sk_wstream) -> CSize -> IO (CBool))

{- | C function signature:

@
_Bool sk_wstream_write_stream(sk_wstream_t *cstream, sk_stream_t *input, size_t length)
@
-}
foreign import ccall "sk_wstream_write_stream" sk_wstream_write_stream ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * cstream"@
  -> Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * input"@
  -> CSize -- ^ C argument @"size_t length"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_wstream_write_stream'
foreign import ccall "&sk_wstream_write_stream" p'sk_wstream_write_stream ::
  FunPtr (Ptr (Sk_wstream) -> Ptr (Sk_stream) -> CSize -> IO (CBool))

{- | C function signature:

@
int sk_wstream_get_size_of_packed_uint(size_t value)
@
-}
foreign import ccall "sk_wstream_get_size_of_packed_uint" sk_wstream_get_size_of_packed_uint ::
  CSize -- ^ C argument @"size_t value"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_wstream_get_size_of_packed_uint'
foreign import ccall "&sk_wstream_get_size_of_packed_uint" p'sk_wstream_get_size_of_packed_uint ::
  FunPtr (CSize -> IO (CInt))
