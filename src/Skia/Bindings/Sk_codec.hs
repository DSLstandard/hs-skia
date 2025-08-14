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
module Skia.Bindings.Sk_codec where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
size_t sk_codec_min_buffered_bytes_needed(void)
@
-}
foreign import ccall "sk_codec_min_buffered_bytes_needed" sk_codec_min_buffered_bytes_needed ::
  IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_codec_min_buffered_bytes_needed'
foreign import ccall "&sk_codec_min_buffered_bytes_needed" p'sk_codec_min_buffered_bytes_needed ::
  FunPtr (IO (CSize))

{- | C function signature:

@
sk_codec_t *sk_codec_new_from_stream(sk_stream_t *stream, sk_codec_result_t *result)
@
-}
foreign import ccall "sk_codec_new_from_stream" sk_codec_new_from_stream ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * stream"@
  -> Ptr (Sk_codec_result) -- ^ C argument @"sk_codec_result_t * result"@
  -> IO (Ptr (Sk_codec)) -- ^ C return type: @"sk_codec_t *"@

-- | Function pointer to 'sk_codec_new_from_stream'
foreign import ccall "&sk_codec_new_from_stream" p'sk_codec_new_from_stream ::
  FunPtr (Ptr (Sk_stream) -> Ptr (Sk_codec_result) -> IO (Ptr (Sk_codec)))

{- | C function signature:

@
sk_codec_t *sk_codec_new_from_data(sk_data_t *data)
@
-}
foreign import ccall "sk_codec_new_from_data" sk_codec_new_from_data ::
  Ptr (Sk_data) -- ^ C argument @"sk_data_t * data"@
  -> IO (Ptr (Sk_codec)) -- ^ C return type: @"sk_codec_t *"@

-- | Function pointer to 'sk_codec_new_from_data'
foreign import ccall "&sk_codec_new_from_data" p'sk_codec_new_from_data ::
  FunPtr (Ptr (Sk_data) -> IO (Ptr (Sk_codec)))

{- | C function signature:

@
void sk_codec_destroy(sk_codec_t *codec)
@
-}
foreign import ccall "sk_codec_destroy" sk_codec_destroy ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_codec_destroy'
foreign import ccall "&sk_codec_destroy" p'sk_codec_destroy ::
  FunPtr (Ptr (Sk_codec) -> IO (()))

{- | C function signature:

@
void sk_codec_get_info(sk_codec_t *codec, sk_imageinfo_t *info)
@
-}
foreign import ccall "sk_codec_get_info" sk_codec_get_info ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> Ptr (Sk_imageinfo) -- ^ C argument @"sk_imageinfo_t * info"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_codec_get_info'
foreign import ccall "&sk_codec_get_info" p'sk_codec_get_info ::
  FunPtr (Ptr (Sk_codec) -> Ptr (Sk_imageinfo) -> IO (()))

{- | C function signature:

@
sk_encodedorigin_t sk_codec_get_origin(sk_codec_t *codec)
@
-}
foreign import ccall "sk_codec_get_origin" sk_codec_get_origin ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> IO (Sk_encodedorigin) -- ^ C return type: @"sk_encodedorigin_t"@

-- | Function pointer to 'sk_codec_get_origin'
foreign import ccall "&sk_codec_get_origin" p'sk_codec_get_origin ::
  FunPtr (Ptr (Sk_codec) -> IO (Sk_encodedorigin))

{- | C function signature:

@
void sk_codec_get_scaled_dimensions(sk_codec_t *codec, float desiredScale, sk_isize_t *dimensions)
@
-}
foreign import ccall "sk_codec_get_scaled_dimensions" sk_codec_get_scaled_dimensions ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> CFloat -- ^ C argument @"float desiredScale"@
  -> Ptr (Sk_isize) -- ^ C argument @"sk_isize_t * dimensions"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_codec_get_scaled_dimensions'
foreign import ccall "&sk_codec_get_scaled_dimensions" p'sk_codec_get_scaled_dimensions ::
  FunPtr (Ptr (Sk_codec) -> CFloat -> Ptr (Sk_isize) -> IO (()))

{- | C function signature:

@
_Bool sk_codec_get_valid_subset(sk_codec_t *codec, sk_irect_t *desiredSubset)
@
-}
foreign import ccall "sk_codec_get_valid_subset" sk_codec_get_valid_subset ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> Ptr (Sk_irect) -- ^ C argument @"sk_irect_t * desiredSubset"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_codec_get_valid_subset'
foreign import ccall "&sk_codec_get_valid_subset" p'sk_codec_get_valid_subset ::
  FunPtr (Ptr (Sk_codec) -> Ptr (Sk_irect) -> IO (CBool))

{- | C function signature:

@
sk_encoded_image_format_t sk_codec_get_encoded_format(sk_codec_t *codec)
@
-}
foreign import ccall "sk_codec_get_encoded_format" sk_codec_get_encoded_format ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> IO (Sk_encoded_image_format) -- ^ C return type: @"sk_encoded_image_format_t"@

-- | Function pointer to 'sk_codec_get_encoded_format'
foreign import ccall "&sk_codec_get_encoded_format" p'sk_codec_get_encoded_format ::
  FunPtr (Ptr (Sk_codec) -> IO (Sk_encoded_image_format))

{- | C function signature:

@
sk_codec_result_t sk_codec_get_pixels(sk_codec_t *codec, const sk_imageinfo_t *info, void *pixels, size_t rowBytes, const sk_codec_options_t *options)
@
-}
foreign import ccall "sk_codec_get_pixels" sk_codec_get_pixels ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> Ptr (Sk_imageinfo) -- ^ C argument @"const sk_imageinfo_t * info"@
  -> Ptr (()) -- ^ C argument @"void * pixels"@
  -> CSize -- ^ C argument @"size_t rowBytes"@
  -> Ptr (Sk_codec_options) -- ^ C argument @"const sk_codec_options_t * options"@
  -> IO (Sk_codec_result) -- ^ C return type: @"sk_codec_result_t"@

-- | Function pointer to 'sk_codec_get_pixels'
foreign import ccall "&sk_codec_get_pixels" p'sk_codec_get_pixels ::
  FunPtr (Ptr (Sk_codec) -> Ptr (Sk_imageinfo) -> Ptr (()) -> CSize -> Ptr (Sk_codec_options) -> IO (Sk_codec_result))

{- | C function signature:

@
sk_codec_result_t sk_codec_start_incremental_decode(sk_codec_t *codec, const sk_imageinfo_t *info, void *pixels, size_t rowBytes, const sk_codec_options_t *options)
@
-}
foreign import ccall "sk_codec_start_incremental_decode" sk_codec_start_incremental_decode ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> Ptr (Sk_imageinfo) -- ^ C argument @"const sk_imageinfo_t * info"@
  -> Ptr (()) -- ^ C argument @"void * pixels"@
  -> CSize -- ^ C argument @"size_t rowBytes"@
  -> Ptr (Sk_codec_options) -- ^ C argument @"const sk_codec_options_t * options"@
  -> IO (Sk_codec_result) -- ^ C return type: @"sk_codec_result_t"@

-- | Function pointer to 'sk_codec_start_incremental_decode'
foreign import ccall "&sk_codec_start_incremental_decode" p'sk_codec_start_incremental_decode ::
  FunPtr (Ptr (Sk_codec) -> Ptr (Sk_imageinfo) -> Ptr (()) -> CSize -> Ptr (Sk_codec_options) -> IO (Sk_codec_result))

{- | C function signature:

@
sk_codec_result_t sk_codec_incremental_decode(sk_codec_t *codec, int *rowsDecoded)
@
-}
foreign import ccall "sk_codec_incremental_decode" sk_codec_incremental_decode ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> Ptr (CInt) -- ^ C argument @"int * rowsDecoded"@
  -> IO (Sk_codec_result) -- ^ C return type: @"sk_codec_result_t"@

-- | Function pointer to 'sk_codec_incremental_decode'
foreign import ccall "&sk_codec_incremental_decode" p'sk_codec_incremental_decode ::
  FunPtr (Ptr (Sk_codec) -> Ptr (CInt) -> IO (Sk_codec_result))

{- | C function signature:

@
sk_codec_result_t sk_codec_start_scanline_decode(sk_codec_t *codec, const sk_imageinfo_t *info, const sk_codec_options_t *options)
@
-}
foreign import ccall "sk_codec_start_scanline_decode" sk_codec_start_scanline_decode ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> Ptr (Sk_imageinfo) -- ^ C argument @"const sk_imageinfo_t * info"@
  -> Ptr (Sk_codec_options) -- ^ C argument @"const sk_codec_options_t * options"@
  -> IO (Sk_codec_result) -- ^ C return type: @"sk_codec_result_t"@

-- | Function pointer to 'sk_codec_start_scanline_decode'
foreign import ccall "&sk_codec_start_scanline_decode" p'sk_codec_start_scanline_decode ::
  FunPtr (Ptr (Sk_codec) -> Ptr (Sk_imageinfo) -> Ptr (Sk_codec_options) -> IO (Sk_codec_result))

{- | C function signature:

@
int sk_codec_get_scanlines(sk_codec_t *codec, void *dst, int countLines, size_t rowBytes)
@
-}
foreign import ccall "sk_codec_get_scanlines" sk_codec_get_scanlines ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> Ptr (()) -- ^ C argument @"void * dst"@
  -> CInt -- ^ C argument @"int countLines"@
  -> CSize -- ^ C argument @"size_t rowBytes"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_codec_get_scanlines'
foreign import ccall "&sk_codec_get_scanlines" p'sk_codec_get_scanlines ::
  FunPtr (Ptr (Sk_codec) -> Ptr (()) -> CInt -> CSize -> IO (CInt))

{- | C function signature:

@
_Bool sk_codec_skip_scanlines(sk_codec_t *codec, int countLines)
@
-}
foreign import ccall "sk_codec_skip_scanlines" sk_codec_skip_scanlines ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> CInt -- ^ C argument @"int countLines"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_codec_skip_scanlines'
foreign import ccall "&sk_codec_skip_scanlines" p'sk_codec_skip_scanlines ::
  FunPtr (Ptr (Sk_codec) -> CInt -> IO (CBool))

{- | C function signature:

@
sk_codec_scanline_order_t sk_codec_get_scanline_order(sk_codec_t *codec)
@
-}
foreign import ccall "sk_codec_get_scanline_order" sk_codec_get_scanline_order ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> IO (Sk_codec_scanline_order) -- ^ C return type: @"sk_codec_scanline_order_t"@

-- | Function pointer to 'sk_codec_get_scanline_order'
foreign import ccall "&sk_codec_get_scanline_order" p'sk_codec_get_scanline_order ::
  FunPtr (Ptr (Sk_codec) -> IO (Sk_codec_scanline_order))

{- | C function signature:

@
int sk_codec_next_scanline(sk_codec_t *codec)
@
-}
foreign import ccall "sk_codec_next_scanline" sk_codec_next_scanline ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_codec_next_scanline'
foreign import ccall "&sk_codec_next_scanline" p'sk_codec_next_scanline ::
  FunPtr (Ptr (Sk_codec) -> IO (CInt))

{- | C function signature:

@
int sk_codec_output_scanline(sk_codec_t *codec, int inputScanline)
@
-}
foreign import ccall "sk_codec_output_scanline" sk_codec_output_scanline ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> CInt -- ^ C argument @"int inputScanline"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_codec_output_scanline'
foreign import ccall "&sk_codec_output_scanline" p'sk_codec_output_scanline ::
  FunPtr (Ptr (Sk_codec) -> CInt -> IO (CInt))

{- | C function signature:

@
int sk_codec_get_frame_count(sk_codec_t *codec)
@
-}
foreign import ccall "sk_codec_get_frame_count" sk_codec_get_frame_count ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_codec_get_frame_count'
foreign import ccall "&sk_codec_get_frame_count" p'sk_codec_get_frame_count ::
  FunPtr (Ptr (Sk_codec) -> IO (CInt))

{- | C function signature:

@
void sk_codec_get_frame_info(sk_codec_t *codec, sk_codec_frameinfo_t *frameInfo)
@
-}
foreign import ccall "sk_codec_get_frame_info" sk_codec_get_frame_info ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> Ptr (Sk_codec_frameinfo) -- ^ C argument @"sk_codec_frameinfo_t * frameInfo"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_codec_get_frame_info'
foreign import ccall "&sk_codec_get_frame_info" p'sk_codec_get_frame_info ::
  FunPtr (Ptr (Sk_codec) -> Ptr (Sk_codec_frameinfo) -> IO (()))

{- | C function signature:

@
_Bool sk_codec_get_frame_info_for_index(sk_codec_t *codec, int index, sk_codec_frameinfo_t *frameInfo)
@
-}
foreign import ccall "sk_codec_get_frame_info_for_index" sk_codec_get_frame_info_for_index ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> CInt -- ^ C argument @"int index"@
  -> Ptr (Sk_codec_frameinfo) -- ^ C argument @"sk_codec_frameinfo_t * frameInfo"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_codec_get_frame_info_for_index'
foreign import ccall "&sk_codec_get_frame_info_for_index" p'sk_codec_get_frame_info_for_index ::
  FunPtr (Ptr (Sk_codec) -> CInt -> Ptr (Sk_codec_frameinfo) -> IO (CBool))

{- | C function signature:

@
int sk_codec_get_repetition_count(sk_codec_t *codec)
@
-}
foreign import ccall "sk_codec_get_repetition_count" sk_codec_get_repetition_count ::
  Ptr (Sk_codec) -- ^ C argument @"sk_codec_t * codec"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_codec_get_repetition_count'
foreign import ccall "&sk_codec_get_repetition_count" p'sk_codec_get_repetition_count ::
  FunPtr (Ptr (Sk_codec) -> IO (CInt))
