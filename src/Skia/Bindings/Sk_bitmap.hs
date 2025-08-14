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
module Skia.Bindings.Sk_bitmap where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void sk_bitmap_destructor(sk_bitmap_t *cbitmap)
@
-}
foreign import ccall "sk_bitmap_destructor" sk_bitmap_destructor ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_bitmap_destructor'
foreign import ccall "&sk_bitmap_destructor" p'sk_bitmap_destructor ::
  FunPtr (Ptr (Sk_bitmap) -> IO (()))

{- | C function signature:

@
sk_bitmap_t *sk_bitmap_new(void)
@
-}
foreign import ccall "sk_bitmap_new" sk_bitmap_new ::
  IO (Ptr (Sk_bitmap)) -- ^ C return type: @"sk_bitmap_t *"@

-- | Function pointer to 'sk_bitmap_new'
foreign import ccall "&sk_bitmap_new" p'sk_bitmap_new ::
  FunPtr (IO (Ptr (Sk_bitmap)))

{- | C function signature:

@
void sk_bitmap_get_info(sk_bitmap_t *cbitmap, sk_imageinfo_t *info)
@
-}
foreign import ccall "sk_bitmap_get_info" sk_bitmap_get_info ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> Ptr (Sk_imageinfo) -- ^ C argument @"sk_imageinfo_t * info"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_bitmap_get_info'
foreign import ccall "&sk_bitmap_get_info" p'sk_bitmap_get_info ::
  FunPtr (Ptr (Sk_bitmap) -> Ptr (Sk_imageinfo) -> IO (()))

{- | C function signature:

@
void *sk_bitmap_get_pixels(sk_bitmap_t *cbitmap, size_t *length)
@
-}
foreign import ccall "sk_bitmap_get_pixels" sk_bitmap_get_pixels ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> Ptr (CSize) -- ^ C argument @"size_t * length"@
  -> IO (Ptr (())) -- ^ C return type: @"void *"@

-- | Function pointer to 'sk_bitmap_get_pixels'
foreign import ccall "&sk_bitmap_get_pixels" p'sk_bitmap_get_pixels ::
  FunPtr (Ptr (Sk_bitmap) -> Ptr (CSize) -> IO (Ptr (())))

{- | C function signature:

@
size_t sk_bitmap_get_row_bytes(sk_bitmap_t *cbitmap)
@
-}
foreign import ccall "sk_bitmap_get_row_bytes" sk_bitmap_get_row_bytes ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_bitmap_get_row_bytes'
foreign import ccall "&sk_bitmap_get_row_bytes" p'sk_bitmap_get_row_bytes ::
  FunPtr (Ptr (Sk_bitmap) -> IO (CSize))

{- | C function signature:

@
size_t sk_bitmap_get_byte_count(sk_bitmap_t *cbitmap)
@
-}
foreign import ccall "sk_bitmap_get_byte_count" sk_bitmap_get_byte_count ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_bitmap_get_byte_count'
foreign import ccall "&sk_bitmap_get_byte_count" p'sk_bitmap_get_byte_count ::
  FunPtr (Ptr (Sk_bitmap) -> IO (CSize))

{- | C function signature:

@
void sk_bitmap_reset(sk_bitmap_t *cbitmap)
@
-}
foreign import ccall "sk_bitmap_reset" sk_bitmap_reset ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_bitmap_reset'
foreign import ccall "&sk_bitmap_reset" p'sk_bitmap_reset ::
  FunPtr (Ptr (Sk_bitmap) -> IO (()))

{- | C function signature:

@
_Bool sk_bitmap_is_null(sk_bitmap_t *cbitmap)
@
-}
foreign import ccall "sk_bitmap_is_null" sk_bitmap_is_null ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_bitmap_is_null'
foreign import ccall "&sk_bitmap_is_null" p'sk_bitmap_is_null ::
  FunPtr (Ptr (Sk_bitmap) -> IO (CBool))

{- | C function signature:

@
_Bool sk_bitmap_is_immutable(sk_bitmap_t *cbitmap)
@
-}
foreign import ccall "sk_bitmap_is_immutable" sk_bitmap_is_immutable ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_bitmap_is_immutable'
foreign import ccall "&sk_bitmap_is_immutable" p'sk_bitmap_is_immutable ::
  FunPtr (Ptr (Sk_bitmap) -> IO (CBool))

{- | C function signature:

@
void sk_bitmap_set_immutable(sk_bitmap_t *cbitmap)
@
-}
foreign import ccall "sk_bitmap_set_immutable" sk_bitmap_set_immutable ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_bitmap_set_immutable'
foreign import ccall "&sk_bitmap_set_immutable" p'sk_bitmap_set_immutable ::
  FunPtr (Ptr (Sk_bitmap) -> IO (()))

{- | C function signature:

@
void sk_bitmap_erase(sk_bitmap_t *cbitmap, sk_color_t color)
@
-}
foreign import ccall "sk_bitmap_erase" sk_bitmap_erase ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> Sk_color -- ^ C argument @"sk_color_t color"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_bitmap_erase'
foreign import ccall "&sk_bitmap_erase" p'sk_bitmap_erase ::
  FunPtr (Ptr (Sk_bitmap) -> Sk_color -> IO (()))

{- | C function signature:

@
void sk_bitmap_erase_rect(sk_bitmap_t *cbitmap, sk_color_t color, sk_irect_t *rect)
@
-}
foreign import ccall "sk_bitmap_erase_rect" sk_bitmap_erase_rect ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> Sk_color -- ^ C argument @"sk_color_t color"@
  -> Ptr (Sk_irect) -- ^ C argument @"sk_irect_t * rect"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_bitmap_erase_rect'
foreign import ccall "&sk_bitmap_erase_rect" p'sk_bitmap_erase_rect ::
  FunPtr (Ptr (Sk_bitmap) -> Sk_color -> Ptr (Sk_irect) -> IO (()))

{- | C function signature:

@
uint8_t *sk_bitmap_get_addr_8(sk_bitmap_t *cbitmap, int x, int y)
@
-}
foreign import ccall "sk_bitmap_get_addr_8" sk_bitmap_get_addr_8 ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> CInt -- ^ C argument @"int x"@
  -> CInt -- ^ C argument @"int y"@
  -> IO (Ptr (Word8)) -- ^ C return type: @"uint8_t *"@

-- | Function pointer to 'sk_bitmap_get_addr_8'
foreign import ccall "&sk_bitmap_get_addr_8" p'sk_bitmap_get_addr_8 ::
  FunPtr (Ptr (Sk_bitmap) -> CInt -> CInt -> IO (Ptr (Word8)))

{- | C function signature:

@
uint16_t *sk_bitmap_get_addr_16(sk_bitmap_t *cbitmap, int x, int y)
@
-}
foreign import ccall "sk_bitmap_get_addr_16" sk_bitmap_get_addr_16 ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> CInt -- ^ C argument @"int x"@
  -> CInt -- ^ C argument @"int y"@
  -> IO (Ptr (Word16)) -- ^ C return type: @"uint16_t *"@

-- | Function pointer to 'sk_bitmap_get_addr_16'
foreign import ccall "&sk_bitmap_get_addr_16" p'sk_bitmap_get_addr_16 ::
  FunPtr (Ptr (Sk_bitmap) -> CInt -> CInt -> IO (Ptr (Word16)))

{- | C function signature:

@
uint32_t *sk_bitmap_get_addr_32(sk_bitmap_t *cbitmap, int x, int y)
@
-}
foreign import ccall "sk_bitmap_get_addr_32" sk_bitmap_get_addr_32 ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> CInt -- ^ C argument @"int x"@
  -> CInt -- ^ C argument @"int y"@
  -> IO (Ptr (Word32)) -- ^ C return type: @"uint32_t *"@

-- | Function pointer to 'sk_bitmap_get_addr_32'
foreign import ccall "&sk_bitmap_get_addr_32" p'sk_bitmap_get_addr_32 ::
  FunPtr (Ptr (Sk_bitmap) -> CInt -> CInt -> IO (Ptr (Word32)))

{- | C function signature:

@
void *sk_bitmap_get_addr(sk_bitmap_t *cbitmap, int x, int y)
@
-}
foreign import ccall "sk_bitmap_get_addr" sk_bitmap_get_addr ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> CInt -- ^ C argument @"int x"@
  -> CInt -- ^ C argument @"int y"@
  -> IO (Ptr (())) -- ^ C return type: @"void *"@

-- | Function pointer to 'sk_bitmap_get_addr'
foreign import ccall "&sk_bitmap_get_addr" p'sk_bitmap_get_addr ::
  FunPtr (Ptr (Sk_bitmap) -> CInt -> CInt -> IO (Ptr (())))

{- | C function signature:

@
sk_color_t sk_bitmap_get_pixel_color(sk_bitmap_t *cbitmap, int x, int y)
@
-}
foreign import ccall "sk_bitmap_get_pixel_color" sk_bitmap_get_pixel_color ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> CInt -- ^ C argument @"int x"@
  -> CInt -- ^ C argument @"int y"@
  -> IO (Sk_color) -- ^ C return type: @"sk_color_t"@

-- | Function pointer to 'sk_bitmap_get_pixel_color'
foreign import ccall "&sk_bitmap_get_pixel_color" p'sk_bitmap_get_pixel_color ::
  FunPtr (Ptr (Sk_bitmap) -> CInt -> CInt -> IO (Sk_color))

{- | C function signature:

@
_Bool sk_bitmap_ready_to_draw(sk_bitmap_t *cbitmap)
@
-}
foreign import ccall "sk_bitmap_ready_to_draw" sk_bitmap_ready_to_draw ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_bitmap_ready_to_draw'
foreign import ccall "&sk_bitmap_ready_to_draw" p'sk_bitmap_ready_to_draw ::
  FunPtr (Ptr (Sk_bitmap) -> IO (CBool))

{- | C function signature:

@
void sk_bitmap_get_pixel_colors(sk_bitmap_t *cbitmap, sk_color_t *colors)
@
-}
foreign import ccall "sk_bitmap_get_pixel_colors" sk_bitmap_get_pixel_colors ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> Ptr (Sk_color) -- ^ C argument @"sk_color_t * colors"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_bitmap_get_pixel_colors'
foreign import ccall "&sk_bitmap_get_pixel_colors" p'sk_bitmap_get_pixel_colors ::
  FunPtr (Ptr (Sk_bitmap) -> Ptr (Sk_color) -> IO (()))

{- | C function signature:

@
_Bool sk_bitmap_install_pixels(sk_bitmap_t *cbitmap, const sk_imageinfo_t *cinfo, void *pixels, size_t rowBytes, const sk_bitmap_release_proc releaseProc, void *context)
@
-}
foreign import ccall "sk_bitmap_install_pixels" sk_bitmap_install_pixels ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> Ptr (Sk_imageinfo) -- ^ C argument @"const sk_imageinfo_t * cinfo"@
  -> Ptr (()) -- ^ C argument @"void * pixels"@
  -> CSize -- ^ C argument @"size_t rowBytes"@
  -> FunPtr Sk_bitmap_release_proc -- ^ C argument @"const sk_bitmap_release_proc releaseProc"@
  -> Ptr (()) -- ^ C argument @"void * context"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_bitmap_install_pixels'
foreign import ccall "&sk_bitmap_install_pixels" p'sk_bitmap_install_pixels ::
  FunPtr (Ptr (Sk_bitmap) -> Ptr (Sk_imageinfo) -> Ptr (()) -> CSize -> FunPtr Sk_bitmap_release_proc -> Ptr (()) -> IO (CBool))

{- | C function signature:

@
_Bool sk_bitmap_install_pixels_with_pixmap(sk_bitmap_t *cbitmap, const sk_pixmap_t *cpixmap)
@
-}
foreign import ccall "sk_bitmap_install_pixels_with_pixmap" sk_bitmap_install_pixels_with_pixmap ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * cpixmap"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_bitmap_install_pixels_with_pixmap'
foreign import ccall "&sk_bitmap_install_pixels_with_pixmap" p'sk_bitmap_install_pixels_with_pixmap ::
  FunPtr (Ptr (Sk_bitmap) -> Ptr (Sk_pixmap) -> IO (CBool))

{- | C function signature:

@
_Bool sk_bitmap_try_alloc_pixels(sk_bitmap_t *cbitmap, const sk_imageinfo_t *requestedInfo, size_t rowBytes)
@
-}
foreign import ccall "sk_bitmap_try_alloc_pixels" sk_bitmap_try_alloc_pixels ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> Ptr (Sk_imageinfo) -- ^ C argument @"const sk_imageinfo_t * requestedInfo"@
  -> CSize -- ^ C argument @"size_t rowBytes"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_bitmap_try_alloc_pixels'
foreign import ccall "&sk_bitmap_try_alloc_pixels" p'sk_bitmap_try_alloc_pixels ::
  FunPtr (Ptr (Sk_bitmap) -> Ptr (Sk_imageinfo) -> CSize -> IO (CBool))

{- | C function signature:

@
_Bool sk_bitmap_try_alloc_pixels_with_flags(sk_bitmap_t *cbitmap, const sk_imageinfo_t *requestedInfo, uint32_t flags)
@
-}
foreign import ccall "sk_bitmap_try_alloc_pixels_with_flags" sk_bitmap_try_alloc_pixels_with_flags ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> Ptr (Sk_imageinfo) -- ^ C argument @"const sk_imageinfo_t * requestedInfo"@
  -> Word32 -- ^ C argument @"uint32_t flags"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_bitmap_try_alloc_pixels_with_flags'
foreign import ccall "&sk_bitmap_try_alloc_pixels_with_flags" p'sk_bitmap_try_alloc_pixels_with_flags ::
  FunPtr (Ptr (Sk_bitmap) -> Ptr (Sk_imageinfo) -> Word32 -> IO (CBool))

{- | C function signature:

@
void sk_bitmap_set_pixels(sk_bitmap_t *cbitmap, void *pixels)
@
-}
foreign import ccall "sk_bitmap_set_pixels" sk_bitmap_set_pixels ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> Ptr (()) -- ^ C argument @"void * pixels"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_bitmap_set_pixels'
foreign import ccall "&sk_bitmap_set_pixels" p'sk_bitmap_set_pixels ::
  FunPtr (Ptr (Sk_bitmap) -> Ptr (()) -> IO (()))

{- | C function signature:

@
_Bool sk_bitmap_peek_pixels(sk_bitmap_t *cbitmap, sk_pixmap_t *cpixmap)
@
-}
foreign import ccall "sk_bitmap_peek_pixels" sk_bitmap_peek_pixels ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> Ptr (Sk_pixmap) -- ^ C argument @"sk_pixmap_t * cpixmap"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_bitmap_peek_pixels'
foreign import ccall "&sk_bitmap_peek_pixels" p'sk_bitmap_peek_pixels ::
  FunPtr (Ptr (Sk_bitmap) -> Ptr (Sk_pixmap) -> IO (CBool))

{- | C function signature:

@
_Bool sk_bitmap_extract_subset(sk_bitmap_t *cbitmap, sk_bitmap_t *dst, sk_irect_t *subset)
@
-}
foreign import ccall "sk_bitmap_extract_subset" sk_bitmap_extract_subset ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * dst"@
  -> Ptr (Sk_irect) -- ^ C argument @"sk_irect_t * subset"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_bitmap_extract_subset'
foreign import ccall "&sk_bitmap_extract_subset" p'sk_bitmap_extract_subset ::
  FunPtr (Ptr (Sk_bitmap) -> Ptr (Sk_bitmap) -> Ptr (Sk_irect) -> IO (CBool))

{- | C function signature:

@
_Bool sk_bitmap_extract_alpha(sk_bitmap_t *cbitmap, sk_bitmap_t *dst, const sk_paint_t *paint, sk_ipoint_t *offset)
@
-}
foreign import ccall "sk_bitmap_extract_alpha" sk_bitmap_extract_alpha ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * dst"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * paint"@
  -> Ptr (Sk_ipoint) -- ^ C argument @"sk_ipoint_t * offset"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_bitmap_extract_alpha'
foreign import ccall "&sk_bitmap_extract_alpha" p'sk_bitmap_extract_alpha ::
  FunPtr (Ptr (Sk_bitmap) -> Ptr (Sk_bitmap) -> Ptr (Sk_paint) -> Ptr (Sk_ipoint) -> IO (CBool))

{- | C function signature:

@
void sk_bitmap_notify_pixels_changed(sk_bitmap_t *cbitmap)
@
-}
foreign import ccall "sk_bitmap_notify_pixels_changed" sk_bitmap_notify_pixels_changed ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_bitmap_notify_pixels_changed'
foreign import ccall "&sk_bitmap_notify_pixels_changed" p'sk_bitmap_notify_pixels_changed ::
  FunPtr (Ptr (Sk_bitmap) -> IO (()))

{- | C function signature:

@
void sk_bitmap_swap(sk_bitmap_t *cbitmap, sk_bitmap_t *cother)
@
-}
foreign import ccall "sk_bitmap_swap" sk_bitmap_swap ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cother"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_bitmap_swap'
foreign import ccall "&sk_bitmap_swap" p'sk_bitmap_swap ::
  FunPtr (Ptr (Sk_bitmap) -> Ptr (Sk_bitmap) -> IO (()))

{- | C function signature:

@
sk_shader_t *sk_bitmap_make_shader(sk_bitmap_t *cbitmap, sk_shader_tilemode_t tmx, sk_shader_tilemode_t tmy, sk_sampling_options_t *sampling, const sk_matrix_t *cmatrix)
@
-}
foreign import ccall "sk_bitmap_make_shader" sk_bitmap_make_shader ::
  Ptr (Sk_bitmap) -- ^ C argument @"sk_bitmap_t * cbitmap"@
  -> Sk_shader_tilemode -- ^ C argument @"sk_shader_tilemode_t tmx"@
  -> Sk_shader_tilemode -- ^ C argument @"sk_shader_tilemode_t tmy"@
  -> Ptr (Sk_sampling_options) -- ^ C argument @"sk_sampling_options_t * sampling"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * cmatrix"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_bitmap_make_shader'
foreign import ccall "&sk_bitmap_make_shader" p'sk_bitmap_make_shader ::
  FunPtr (Ptr (Sk_bitmap) -> Sk_shader_tilemode -> Sk_shader_tilemode -> Ptr (Sk_sampling_options) -> Ptr (Sk_matrix) -> IO (Ptr (Sk_shader)))
