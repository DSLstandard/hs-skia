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
module Skia.Bindings.Sk_pixmap where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void sk_pixmap_destructor(sk_pixmap_t *cpixmap)
@
-}
foreign import ccall "sk_pixmap_destructor" sk_pixmap_destructor ::
  Ptr (Sk_pixmap) -- ^ C argument @"sk_pixmap_t * cpixmap"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_pixmap_destructor'
foreign import ccall "&sk_pixmap_destructor" p'sk_pixmap_destructor ::
  FunPtr (Ptr (Sk_pixmap) -> IO (()))

{- | C function signature:

@
sk_pixmap_t *sk_pixmap_new(void)
@
-}
foreign import ccall "sk_pixmap_new" sk_pixmap_new ::
  IO (Ptr (Sk_pixmap)) -- ^ C return type: @"sk_pixmap_t *"@

-- | Function pointer to 'sk_pixmap_new'
foreign import ccall "&sk_pixmap_new" p'sk_pixmap_new ::
  FunPtr (IO (Ptr (Sk_pixmap)))

{- | C function signature:

@
sk_pixmap_t *sk_pixmap_new_with_params(const sk_imageinfo_t *cinfo, const void *addr, size_t rowBytes)
@
-}
foreign import ccall "sk_pixmap_new_with_params" sk_pixmap_new_with_params ::
  Ptr (Sk_imageinfo) -- ^ C argument @"const sk_imageinfo_t * cinfo"@
  -> Ptr (()) -- ^ C argument @"const void * addr"@
  -> CSize -- ^ C argument @"size_t rowBytes"@
  -> IO (Ptr (Sk_pixmap)) -- ^ C return type: @"sk_pixmap_t *"@

-- | Function pointer to 'sk_pixmap_new_with_params'
foreign import ccall "&sk_pixmap_new_with_params" p'sk_pixmap_new_with_params ::
  FunPtr (Ptr (Sk_imageinfo) -> Ptr (()) -> CSize -> IO (Ptr (Sk_pixmap)))

{- | C function signature:

@
void sk_pixmap_reset(sk_pixmap_t *cpixmap)
@
-}
foreign import ccall "sk_pixmap_reset" sk_pixmap_reset ::
  Ptr (Sk_pixmap) -- ^ C argument @"sk_pixmap_t * cpixmap"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_pixmap_reset'
foreign import ccall "&sk_pixmap_reset" p'sk_pixmap_reset ::
  FunPtr (Ptr (Sk_pixmap) -> IO (()))

{- | C function signature:

@
void sk_pixmap_reset_with_params(sk_pixmap_t *cpixmap, const sk_imageinfo_t *cinfo, const void *addr, size_t rowBytes)
@
-}
foreign import ccall "sk_pixmap_reset_with_params" sk_pixmap_reset_with_params ::
  Ptr (Sk_pixmap) -- ^ C argument @"sk_pixmap_t * cpixmap"@
  -> Ptr (Sk_imageinfo) -- ^ C argument @"const sk_imageinfo_t * cinfo"@
  -> Ptr (()) -- ^ C argument @"const void * addr"@
  -> CSize -- ^ C argument @"size_t rowBytes"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_pixmap_reset_with_params'
foreign import ccall "&sk_pixmap_reset_with_params" p'sk_pixmap_reset_with_params ::
  FunPtr (Ptr (Sk_pixmap) -> Ptr (Sk_imageinfo) -> Ptr (()) -> CSize -> IO (()))

{- | C function signature:

@
void sk_pixmap_set_colorspace(sk_pixmap_t *cpixmap, sk_colorspace_t *colorspace)
@
-}
foreign import ccall "sk_pixmap_set_colorspace" sk_pixmap_set_colorspace ::
  Ptr (Sk_pixmap) -- ^ C argument @"sk_pixmap_t * cpixmap"@
  -> Ptr (Sk_colorspace) -- ^ C argument @"sk_colorspace_t * colorspace"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_pixmap_set_colorspace'
foreign import ccall "&sk_pixmap_set_colorspace" p'sk_pixmap_set_colorspace ::
  FunPtr (Ptr (Sk_pixmap) -> Ptr (Sk_colorspace) -> IO (()))

{- | C function signature:

@
_Bool sk_pixmap_extract_subset(const sk_pixmap_t *cpixmap, sk_pixmap_t *result, const sk_irect_t *subset)
@
-}
foreign import ccall "sk_pixmap_extract_subset" sk_pixmap_extract_subset ::
  Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * cpixmap"@
  -> Ptr (Sk_pixmap) -- ^ C argument @"sk_pixmap_t * result"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * subset"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_pixmap_extract_subset'
foreign import ccall "&sk_pixmap_extract_subset" p'sk_pixmap_extract_subset ::
  FunPtr (Ptr (Sk_pixmap) -> Ptr (Sk_pixmap) -> Ptr (Sk_irect) -> IO (CBool))

{- | C function signature:

@
void sk_pixmap_get_info(const sk_pixmap_t *cpixmap, sk_imageinfo_t *cinfo)
@
-}
foreign import ccall "sk_pixmap_get_info" sk_pixmap_get_info ::
  Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * cpixmap"@
  -> Ptr (Sk_imageinfo) -- ^ C argument @"sk_imageinfo_t * cinfo"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_pixmap_get_info'
foreign import ccall "&sk_pixmap_get_info" p'sk_pixmap_get_info ::
  FunPtr (Ptr (Sk_pixmap) -> Ptr (Sk_imageinfo) -> IO (()))

{- | C function signature:

@
size_t sk_pixmap_get_row_bytes(const sk_pixmap_t *cpixmap)
@
-}
foreign import ccall "sk_pixmap_get_row_bytes" sk_pixmap_get_row_bytes ::
  Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * cpixmap"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_pixmap_get_row_bytes'
foreign import ccall "&sk_pixmap_get_row_bytes" p'sk_pixmap_get_row_bytes ::
  FunPtr (Ptr (Sk_pixmap) -> IO (CSize))

{- | C function signature:

@
sk_colorspace_t *sk_pixmap_get_colorspace(const sk_pixmap_t *cpixmap)
@
-}
foreign import ccall "sk_pixmap_get_colorspace" sk_pixmap_get_colorspace ::
  Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * cpixmap"@
  -> IO (Ptr (Sk_colorspace)) -- ^ C return type: @"sk_colorspace_t *"@

-- | Function pointer to 'sk_pixmap_get_colorspace'
foreign import ccall "&sk_pixmap_get_colorspace" p'sk_pixmap_get_colorspace ::
  FunPtr (Ptr (Sk_pixmap) -> IO (Ptr (Sk_colorspace)))

{- | C function signature:

@
_Bool sk_pixmap_compute_is_opaque(const sk_pixmap_t *cpixmap)
@
-}
foreign import ccall "sk_pixmap_compute_is_opaque" sk_pixmap_compute_is_opaque ::
  Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * cpixmap"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_pixmap_compute_is_opaque'
foreign import ccall "&sk_pixmap_compute_is_opaque" p'sk_pixmap_compute_is_opaque ::
  FunPtr (Ptr (Sk_pixmap) -> IO (CBool))

{- | C function signature:

@
sk_color_t sk_pixmap_get_pixel_color(const sk_pixmap_t *cpixmap, int x, int y)
@
-}
foreign import ccall "sk_pixmap_get_pixel_color" sk_pixmap_get_pixel_color ::
  Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * cpixmap"@
  -> CInt -- ^ C argument @"int x"@
  -> CInt -- ^ C argument @"int y"@
  -> IO (Sk_color) -- ^ C return type: @"sk_color_t"@

-- | Function pointer to 'sk_pixmap_get_pixel_color'
foreign import ccall "&sk_pixmap_get_pixel_color" p'sk_pixmap_get_pixel_color ::
  FunPtr (Ptr (Sk_pixmap) -> CInt -> CInt -> IO (Sk_color))

{- | C function signature:

@
void sk_pixmap_get_pixel_color4f(const sk_pixmap_t *cpixmap, int x, int y, sk_color4f_t *color)
@
-}
foreign import ccall "sk_pixmap_get_pixel_color4f" sk_pixmap_get_pixel_color4f ::
  Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * cpixmap"@
  -> CInt -- ^ C argument @"int x"@
  -> CInt -- ^ C argument @"int y"@
  -> Ptr (Sk_color4f) -- ^ C argument @"sk_color4f_t * color"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_pixmap_get_pixel_color4f'
foreign import ccall "&sk_pixmap_get_pixel_color4f" p'sk_pixmap_get_pixel_color4f ::
  FunPtr (Ptr (Sk_pixmap) -> CInt -> CInt -> Ptr (Sk_color4f) -> IO (()))

{- | C function signature:

@
float sk_pixmap_get_pixel_alphaf(const sk_pixmap_t *cpixmap, int x, int y)
@
-}
foreign import ccall "sk_pixmap_get_pixel_alphaf" sk_pixmap_get_pixel_alphaf ::
  Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * cpixmap"@
  -> CInt -- ^ C argument @"int x"@
  -> CInt -- ^ C argument @"int y"@
  -> IO (CFloat) -- ^ C return type: @"float"@

-- | Function pointer to 'sk_pixmap_get_pixel_alphaf'
foreign import ccall "&sk_pixmap_get_pixel_alphaf" p'sk_pixmap_get_pixel_alphaf ::
  FunPtr (Ptr (Sk_pixmap) -> CInt -> CInt -> IO (CFloat))

{- | C function signature:

@
void *sk_pixmap_get_writable_addr(const sk_pixmap_t *cpixmap)
@
-}
foreign import ccall "sk_pixmap_get_writable_addr" sk_pixmap_get_writable_addr ::
  Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * cpixmap"@
  -> IO (Ptr (())) -- ^ C return type: @"void *"@

-- | Function pointer to 'sk_pixmap_get_writable_addr'
foreign import ccall "&sk_pixmap_get_writable_addr" p'sk_pixmap_get_writable_addr ::
  FunPtr (Ptr (Sk_pixmap) -> IO (Ptr (())))

{- | C function signature:

@
void *sk_pixmap_get_writeable_addr_with_xy(const sk_pixmap_t *cpixmap, int x, int y)
@
-}
foreign import ccall "sk_pixmap_get_writeable_addr_with_xy" sk_pixmap_get_writeable_addr_with_xy ::
  Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * cpixmap"@
  -> CInt -- ^ C argument @"int x"@
  -> CInt -- ^ C argument @"int y"@
  -> IO (Ptr (())) -- ^ C return type: @"void *"@

-- | Function pointer to 'sk_pixmap_get_writeable_addr_with_xy'
foreign import ccall "&sk_pixmap_get_writeable_addr_with_xy" p'sk_pixmap_get_writeable_addr_with_xy ::
  FunPtr (Ptr (Sk_pixmap) -> CInt -> CInt -> IO (Ptr (())))

{- | C function signature:

@
_Bool sk_pixmap_read_pixels(const sk_pixmap_t *cpixmap, const sk_imageinfo_t *dstInfo, void *dstPixels, size_t dstRowBytes, int srcX, int srcY)
@
-}
foreign import ccall "sk_pixmap_read_pixels" sk_pixmap_read_pixels ::
  Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * cpixmap"@
  -> Ptr (Sk_imageinfo) -- ^ C argument @"const sk_imageinfo_t * dstInfo"@
  -> Ptr (()) -- ^ C argument @"void * dstPixels"@
  -> CSize -- ^ C argument @"size_t dstRowBytes"@
  -> CInt -- ^ C argument @"int srcX"@
  -> CInt -- ^ C argument @"int srcY"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_pixmap_read_pixels'
foreign import ccall "&sk_pixmap_read_pixels" p'sk_pixmap_read_pixels ::
  FunPtr (Ptr (Sk_pixmap) -> Ptr (Sk_imageinfo) -> Ptr (()) -> CSize -> CInt -> CInt -> IO (CBool))

{- | C function signature:

@
_Bool sk_pixmap_scale_pixels(const sk_pixmap_t *cpixmap, const sk_pixmap_t *dst, const sk_sampling_options_t *sampling)
@
-}
foreign import ccall "sk_pixmap_scale_pixels" sk_pixmap_scale_pixels ::
  Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * cpixmap"@
  -> Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * dst"@
  -> Ptr (Sk_sampling_options) -- ^ C argument @"const sk_sampling_options_t * sampling"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_pixmap_scale_pixels'
foreign import ccall "&sk_pixmap_scale_pixels" p'sk_pixmap_scale_pixels ::
  FunPtr (Ptr (Sk_pixmap) -> Ptr (Sk_pixmap) -> Ptr (Sk_sampling_options) -> IO (CBool))

{- | C function signature:

@
_Bool sk_pixmap_erase_color(const sk_pixmap_t *cpixmap, sk_color_t color, const sk_irect_t *subset)
@
-}
foreign import ccall "sk_pixmap_erase_color" sk_pixmap_erase_color ::
  Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * cpixmap"@
  -> Sk_color -- ^ C argument @"sk_color_t color"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * subset"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_pixmap_erase_color'
foreign import ccall "&sk_pixmap_erase_color" p'sk_pixmap_erase_color ::
  FunPtr (Ptr (Sk_pixmap) -> Sk_color -> Ptr (Sk_irect) -> IO (CBool))

{- | C function signature:

@
_Bool sk_pixmap_erase_color4f(const sk_pixmap_t *cpixmap, const sk_color4f_t *color, const sk_irect_t *subset)
@
-}
foreign import ccall "sk_pixmap_erase_color4f" sk_pixmap_erase_color4f ::
  Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * cpixmap"@
  -> Ptr (Sk_color4f) -- ^ C argument @"const sk_color4f_t * color"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * subset"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_pixmap_erase_color4f'
foreign import ccall "&sk_pixmap_erase_color4f" p'sk_pixmap_erase_color4f ::
  FunPtr (Ptr (Sk_pixmap) -> Ptr (Sk_color4f) -> Ptr (Sk_irect) -> IO (CBool))

{- | C function signature:

@
_Bool sk_webpencoder_encode(sk_wstream_t *dst, const sk_pixmap_t *src, const sk_webpencoder_options_t *options)
@
-}
foreign import ccall "sk_webpencoder_encode" sk_webpencoder_encode ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * dst"@
  -> Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * src"@
  -> Ptr (Sk_webpencoder_options) -- ^ C argument @"const sk_webpencoder_options_t * options"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_webpencoder_encode'
foreign import ccall "&sk_webpencoder_encode" p'sk_webpencoder_encode ::
  FunPtr (Ptr (Sk_wstream) -> Ptr (Sk_pixmap) -> Ptr (Sk_webpencoder_options) -> IO (CBool))

{- | C function signature:

@
_Bool sk_jpegencoder_encode(sk_wstream_t *dst, const sk_pixmap_t *src, const sk_jpegencoder_options_t *options)
@
-}
foreign import ccall "sk_jpegencoder_encode" sk_jpegencoder_encode ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * dst"@
  -> Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * src"@
  -> Ptr (Sk_jpegencoder_options) -- ^ C argument @"const sk_jpegencoder_options_t * options"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_jpegencoder_encode'
foreign import ccall "&sk_jpegencoder_encode" p'sk_jpegencoder_encode ::
  FunPtr (Ptr (Sk_wstream) -> Ptr (Sk_pixmap) -> Ptr (Sk_jpegencoder_options) -> IO (CBool))

{- | C function signature:

@
_Bool sk_pngencoder_encode(sk_wstream_t *dst, const sk_pixmap_t *src, const sk_pngencoder_options_t *options)
@
-}
foreign import ccall "sk_pngencoder_encode" sk_pngencoder_encode ::
  Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * dst"@
  -> Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * src"@
  -> Ptr (Sk_pngencoder_options) -- ^ C argument @"const sk_pngencoder_options_t * options"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_pngencoder_encode'
foreign import ccall "&sk_pngencoder_encode" p'sk_pngencoder_encode ::
  FunPtr (Ptr (Sk_wstream) -> Ptr (Sk_pixmap) -> Ptr (Sk_pngencoder_options) -> IO (CBool))

{- | C function signature:

@
void sk_swizzle_swap_rb(uint32_t *dest, const uint32_t *src, int count)
@
-}
foreign import ccall "sk_swizzle_swap_rb" sk_swizzle_swap_rb ::
  Ptr (Word32) -- ^ C argument @"uint32_t * dest"@
  -> Ptr (Word32) -- ^ C argument @"const uint32_t * src"@
  -> CInt -- ^ C argument @"int count"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_swizzle_swap_rb'
foreign import ccall "&sk_swizzle_swap_rb" p'sk_swizzle_swap_rb ::
  FunPtr (Ptr (Word32) -> Ptr (Word32) -> CInt -> IO (()))

{- | C function signature:

@
sk_color_t sk_color_unpremultiply(const sk_pmcolor_t pmcolor)
@
-}
foreign import ccall "sk_color_unpremultiply" sk_color_unpremultiply ::
  Sk_pmcolor -- ^ C argument @"const sk_pmcolor_t pmcolor"@
  -> IO (Sk_color) -- ^ C return type: @"sk_color_t"@

-- | Function pointer to 'sk_color_unpremultiply'
foreign import ccall "&sk_color_unpremultiply" p'sk_color_unpremultiply ::
  FunPtr (Sk_pmcolor -> IO (Sk_color))

{- | C function signature:

@
sk_pmcolor_t sk_color_premultiply(const sk_color_t color)
@
-}
foreign import ccall "sk_color_premultiply" sk_color_premultiply ::
  Sk_color -- ^ C argument @"const sk_color_t color"@
  -> IO (Sk_pmcolor) -- ^ C return type: @"sk_pmcolor_t"@

-- | Function pointer to 'sk_color_premultiply'
foreign import ccall "&sk_color_premultiply" p'sk_color_premultiply ::
  FunPtr (Sk_color -> IO (Sk_pmcolor))

{- | C function signature:

@
void sk_color_unpremultiply_array(const sk_pmcolor_t *pmcolors, int size, sk_color_t *colors)
@
-}
foreign import ccall "sk_color_unpremultiply_array" sk_color_unpremultiply_array ::
  Ptr (Sk_pmcolor) -- ^ C argument @"const sk_pmcolor_t * pmcolors"@
  -> CInt -- ^ C argument @"int size"@
  -> Ptr (Sk_color) -- ^ C argument @"sk_color_t * colors"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_color_unpremultiply_array'
foreign import ccall "&sk_color_unpremultiply_array" p'sk_color_unpremultiply_array ::
  FunPtr (Ptr (Sk_pmcolor) -> CInt -> Ptr (Sk_color) -> IO (()))

{- | C function signature:

@
void sk_color_premultiply_array(const sk_color_t *colors, int size, sk_pmcolor_t *pmcolors)
@
-}
foreign import ccall "sk_color_premultiply_array" sk_color_premultiply_array ::
  Ptr (Sk_color) -- ^ C argument @"const sk_color_t * colors"@
  -> CInt -- ^ C argument @"int size"@
  -> Ptr (Sk_pmcolor) -- ^ C argument @"sk_pmcolor_t * pmcolors"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_color_premultiply_array'
foreign import ccall "&sk_color_premultiply_array" p'sk_color_premultiply_array ::
  FunPtr (Ptr (Sk_color) -> CInt -> Ptr (Sk_pmcolor) -> IO (()))

{- | C function signature:

@
void sk_color_get_bit_shift(int *a, int *r, int *g, int *b)
@
-}
foreign import ccall "sk_color_get_bit_shift" sk_color_get_bit_shift ::
  Ptr (CInt) -- ^ C argument @"int * a"@
  -> Ptr (CInt) -- ^ C argument @"int * r"@
  -> Ptr (CInt) -- ^ C argument @"int * g"@
  -> Ptr (CInt) -- ^ C argument @"int * b"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_color_get_bit_shift'
foreign import ccall "&sk_color_get_bit_shift" p'sk_color_get_bit_shift ::
  FunPtr (Ptr (CInt) -> Ptr (CInt) -> Ptr (CInt) -> Ptr (CInt) -> IO (()))
