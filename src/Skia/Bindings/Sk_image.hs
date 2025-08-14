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
module Skia.Bindings.Sk_image where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void sk_image_ref(const sk_image_t *cimage)
@
-}
foreign import ccall "sk_image_ref" sk_image_ref ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * cimage"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_image_ref'
foreign import ccall "&sk_image_ref" p'sk_image_ref ::
  FunPtr (Ptr (Sk_image) -> IO (()))

{- | C function signature:

@
void sk_image_unref(const sk_image_t *cimage)
@
-}
foreign import ccall "sk_image_unref" sk_image_unref ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * cimage"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_image_unref'
foreign import ccall "&sk_image_unref" p'sk_image_unref ::
  FunPtr (Ptr (Sk_image) -> IO (()))

{- | C function signature:

@
sk_image_t *sk_image_new_raster_copy(const sk_imageinfo_t *cinfo, const void *pixels, size_t rowBytes)
@
-}
foreign import ccall "sk_image_new_raster_copy" sk_image_new_raster_copy ::
  Ptr (Sk_imageinfo) -- ^ C argument @"const sk_imageinfo_t * cinfo"@
  -> Ptr (()) -- ^ C argument @"const void * pixels"@
  -> CSize -- ^ C argument @"size_t rowBytes"@
  -> IO (Ptr (Sk_image)) -- ^ C return type: @"sk_image_t *"@

-- | Function pointer to 'sk_image_new_raster_copy'
foreign import ccall "&sk_image_new_raster_copy" p'sk_image_new_raster_copy ::
  FunPtr (Ptr (Sk_imageinfo) -> Ptr (()) -> CSize -> IO (Ptr (Sk_image)))

{- | C function signature:

@
sk_image_t *sk_image_new_raster_copy_with_pixmap(const sk_pixmap_t *pixmap)
@
-}
foreign import ccall "sk_image_new_raster_copy_with_pixmap" sk_image_new_raster_copy_with_pixmap ::
  Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * pixmap"@
  -> IO (Ptr (Sk_image)) -- ^ C return type: @"sk_image_t *"@

-- | Function pointer to 'sk_image_new_raster_copy_with_pixmap'
foreign import ccall "&sk_image_new_raster_copy_with_pixmap" p'sk_image_new_raster_copy_with_pixmap ::
  FunPtr (Ptr (Sk_pixmap) -> IO (Ptr (Sk_image)))

{- | C function signature:

@
sk_image_t *sk_image_new_raster_data(const sk_imageinfo_t *cinfo, sk_data_t *pixels, size_t rowBytes)
@
-}
foreign import ccall "sk_image_new_raster_data" sk_image_new_raster_data ::
  Ptr (Sk_imageinfo) -- ^ C argument @"const sk_imageinfo_t * cinfo"@
  -> Ptr (Sk_data) -- ^ C argument @"sk_data_t * pixels"@
  -> CSize -- ^ C argument @"size_t rowBytes"@
  -> IO (Ptr (Sk_image)) -- ^ C return type: @"sk_image_t *"@

-- | Function pointer to 'sk_image_new_raster_data'
foreign import ccall "&sk_image_new_raster_data" p'sk_image_new_raster_data ::
  FunPtr (Ptr (Sk_imageinfo) -> Ptr (Sk_data) -> CSize -> IO (Ptr (Sk_image)))

{- | C function signature:

@
sk_image_t *sk_image_new_raster(const sk_pixmap_t *pixmap, sk_image_raster_release_proc releaseProc, void *context)
@
-}
foreign import ccall "sk_image_new_raster" sk_image_new_raster ::
  Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * pixmap"@
  -> FunPtr Sk_image_raster_release_proc -- ^ C argument @"sk_image_raster_release_proc releaseProc"@
  -> Ptr (()) -- ^ C argument @"void * context"@
  -> IO (Ptr (Sk_image)) -- ^ C return type: @"sk_image_t *"@

-- | Function pointer to 'sk_image_new_raster'
foreign import ccall "&sk_image_new_raster" p'sk_image_new_raster ::
  FunPtr (Ptr (Sk_pixmap) -> FunPtr Sk_image_raster_release_proc -> Ptr (()) -> IO (Ptr (Sk_image)))

{- | C function signature:

@
sk_image_t *sk_image_new_from_bitmap(const sk_bitmap_t *cbitmap)
@
-}
foreign import ccall "sk_image_new_from_bitmap" sk_image_new_from_bitmap ::
  Ptr (Sk_bitmap) -- ^ C argument @"const sk_bitmap_t * cbitmap"@
  -> IO (Ptr (Sk_image)) -- ^ C return type: @"sk_image_t *"@

-- | Function pointer to 'sk_image_new_from_bitmap'
foreign import ccall "&sk_image_new_from_bitmap" p'sk_image_new_from_bitmap ::
  FunPtr (Ptr (Sk_bitmap) -> IO (Ptr (Sk_image)))

{- | C function signature:

@
sk_image_t *sk_image_new_from_encoded(const sk_data_t *cdata)
@
-}
foreign import ccall "sk_image_new_from_encoded" sk_image_new_from_encoded ::
  Ptr (Sk_data) -- ^ C argument @"const sk_data_t * cdata"@
  -> IO (Ptr (Sk_image)) -- ^ C return type: @"sk_image_t *"@

-- | Function pointer to 'sk_image_new_from_encoded'
foreign import ccall "&sk_image_new_from_encoded" p'sk_image_new_from_encoded ::
  FunPtr (Ptr (Sk_data) -> IO (Ptr (Sk_image)))

{- | C function signature:

@
sk_image_t *sk_image_new_from_texture(gr_recording_context_t *context, const gr_backendtexture_t *texture, gr_surfaceorigin_t origin, sk_colortype_t colorType, sk_alphatype_t alpha, const sk_colorspace_t *colorSpace, const sk_image_texture_release_proc releaseProc, void *releaseContext)
@
-}
foreign import ccall "sk_image_new_from_texture" sk_image_new_from_texture ::
  Ptr (Gr_recording_context) -- ^ C argument @"gr_recording_context_t * context"@
  -> Ptr (Gr_backendtexture) -- ^ C argument @"const gr_backendtexture_t * texture"@
  -> Gr_surfaceorigin -- ^ C argument @"gr_surfaceorigin_t origin"@
  -> Sk_colortype -- ^ C argument @"sk_colortype_t colorType"@
  -> Sk_alphatype -- ^ C argument @"sk_alphatype_t alpha"@
  -> Ptr (Sk_colorspace) -- ^ C argument @"const sk_colorspace_t * colorSpace"@
  -> FunPtr Sk_image_texture_release_proc -- ^ C argument @"const sk_image_texture_release_proc releaseProc"@
  -> Ptr (()) -- ^ C argument @"void * releaseContext"@
  -> IO (Ptr (Sk_image)) -- ^ C return type: @"sk_image_t *"@

-- | Function pointer to 'sk_image_new_from_texture'
foreign import ccall "&sk_image_new_from_texture" p'sk_image_new_from_texture ::
  FunPtr (Ptr (Gr_recording_context) -> Ptr (Gr_backendtexture) -> Gr_surfaceorigin -> Sk_colortype -> Sk_alphatype -> Ptr (Sk_colorspace) -> FunPtr Sk_image_texture_release_proc -> Ptr (()) -> IO (Ptr (Sk_image)))

{- | C function signature:

@
sk_image_t *sk_image_new_from_adopted_texture(gr_recording_context_t *context, const gr_backendtexture_t *texture, gr_surfaceorigin_t origin, sk_colortype_t colorType, sk_alphatype_t alpha, const sk_colorspace_t *colorSpace)
@
-}
foreign import ccall "sk_image_new_from_adopted_texture" sk_image_new_from_adopted_texture ::
  Ptr (Gr_recording_context) -- ^ C argument @"gr_recording_context_t * context"@
  -> Ptr (Gr_backendtexture) -- ^ C argument @"const gr_backendtexture_t * texture"@
  -> Gr_surfaceorigin -- ^ C argument @"gr_surfaceorigin_t origin"@
  -> Sk_colortype -- ^ C argument @"sk_colortype_t colorType"@
  -> Sk_alphatype -- ^ C argument @"sk_alphatype_t alpha"@
  -> Ptr (Sk_colorspace) -- ^ C argument @"const sk_colorspace_t * colorSpace"@
  -> IO (Ptr (Sk_image)) -- ^ C return type: @"sk_image_t *"@

-- | Function pointer to 'sk_image_new_from_adopted_texture'
foreign import ccall "&sk_image_new_from_adopted_texture" p'sk_image_new_from_adopted_texture ::
  FunPtr (Ptr (Gr_recording_context) -> Ptr (Gr_backendtexture) -> Gr_surfaceorigin -> Sk_colortype -> Sk_alphatype -> Ptr (Sk_colorspace) -> IO (Ptr (Sk_image)))

{- | C function signature:

@
sk_image_t *sk_image_new_from_picture(sk_picture_t *picture, const sk_isize_t *dimensions, const sk_matrix_t *cmatrix, const sk_paint_t *paint, _Bool useFloatingPointBitDepth, const sk_colorspace_t *colorSpace, const sk_surfaceprops_t *props)
@
-}
foreign import ccall "sk_image_new_from_picture" sk_image_new_from_picture ::
  Ptr (Sk_picture) -- ^ C argument @"sk_picture_t * picture"@
  -> Ptr (Sk_isize) -- ^ C argument @"const sk_isize_t * dimensions"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * cmatrix"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * paint"@
  -> CBool -- ^ C argument @"_Bool useFloatingPointBitDepth"@
  -> Ptr (Sk_colorspace) -- ^ C argument @"const sk_colorspace_t * colorSpace"@
  -> Ptr (Sk_surfaceprops) -- ^ C argument @"const sk_surfaceprops_t * props"@
  -> IO (Ptr (Sk_image)) -- ^ C return type: @"sk_image_t *"@

-- | Function pointer to 'sk_image_new_from_picture'
foreign import ccall "&sk_image_new_from_picture" p'sk_image_new_from_picture ::
  FunPtr (Ptr (Sk_picture) -> Ptr (Sk_isize) -> Ptr (Sk_matrix) -> Ptr (Sk_paint) -> CBool -> Ptr (Sk_colorspace) -> Ptr (Sk_surfaceprops) -> IO (Ptr (Sk_image)))

{- | C function signature:

@
int sk_image_get_width(const sk_image_t *cimage)
@
-}
foreign import ccall "sk_image_get_width" sk_image_get_width ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * cimage"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_image_get_width'
foreign import ccall "&sk_image_get_width" p'sk_image_get_width ::
  FunPtr (Ptr (Sk_image) -> IO (CInt))

{- | C function signature:

@
int sk_image_get_height(const sk_image_t *cimage)
@
-}
foreign import ccall "sk_image_get_height" sk_image_get_height ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * cimage"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_image_get_height'
foreign import ccall "&sk_image_get_height" p'sk_image_get_height ::
  FunPtr (Ptr (Sk_image) -> IO (CInt))

{- | C function signature:

@
uint32_t sk_image_get_unique_id(const sk_image_t *cimage)
@
-}
foreign import ccall "sk_image_get_unique_id" sk_image_get_unique_id ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * cimage"@
  -> IO (Word32) -- ^ C return type: @"uint32_t"@

-- | Function pointer to 'sk_image_get_unique_id'
foreign import ccall "&sk_image_get_unique_id" p'sk_image_get_unique_id ::
  FunPtr (Ptr (Sk_image) -> IO (Word32))

{- | C function signature:

@
sk_alphatype_t sk_image_get_alpha_type(const sk_image_t *image)
@
-}
foreign import ccall "sk_image_get_alpha_type" sk_image_get_alpha_type ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * image"@
  -> IO (Sk_alphatype) -- ^ C return type: @"sk_alphatype_t"@

-- | Function pointer to 'sk_image_get_alpha_type'
foreign import ccall "&sk_image_get_alpha_type" p'sk_image_get_alpha_type ::
  FunPtr (Ptr (Sk_image) -> IO (Sk_alphatype))

{- | C function signature:

@
sk_colortype_t sk_image_get_color_type(const sk_image_t *image)
@
-}
foreign import ccall "sk_image_get_color_type" sk_image_get_color_type ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * image"@
  -> IO (Sk_colortype) -- ^ C return type: @"sk_colortype_t"@

-- | Function pointer to 'sk_image_get_color_type'
foreign import ccall "&sk_image_get_color_type" p'sk_image_get_color_type ::
  FunPtr (Ptr (Sk_image) -> IO (Sk_colortype))

{- | C function signature:

@
sk_colorspace_t *sk_image_get_colorspace(const sk_image_t *image)
@
-}
foreign import ccall "sk_image_get_colorspace" sk_image_get_colorspace ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * image"@
  -> IO (Ptr (Sk_colorspace)) -- ^ C return type: @"sk_colorspace_t *"@

-- | Function pointer to 'sk_image_get_colorspace'
foreign import ccall "&sk_image_get_colorspace" p'sk_image_get_colorspace ::
  FunPtr (Ptr (Sk_image) -> IO (Ptr (Sk_colorspace)))

{- | C function signature:

@
_Bool sk_image_is_alpha_only(const sk_image_t *image)
@
-}
foreign import ccall "sk_image_is_alpha_only" sk_image_is_alpha_only ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * image"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_image_is_alpha_only'
foreign import ccall "&sk_image_is_alpha_only" p'sk_image_is_alpha_only ::
  FunPtr (Ptr (Sk_image) -> IO (CBool))

{- | C function signature:

@
sk_shader_t *sk_image_make_shader(const sk_image_t *image, sk_shader_tilemode_t tileX, sk_shader_tilemode_t tileY, const sk_sampling_options_t *sampling, const sk_matrix_t *cmatrix)
@
-}
foreign import ccall "sk_image_make_shader" sk_image_make_shader ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * image"@
  -> Sk_shader_tilemode -- ^ C argument @"sk_shader_tilemode_t tileX"@
  -> Sk_shader_tilemode -- ^ C argument @"sk_shader_tilemode_t tileY"@
  -> Ptr (Sk_sampling_options) -- ^ C argument @"const sk_sampling_options_t * sampling"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * cmatrix"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_image_make_shader'
foreign import ccall "&sk_image_make_shader" p'sk_image_make_shader ::
  FunPtr (Ptr (Sk_image) -> Sk_shader_tilemode -> Sk_shader_tilemode -> Ptr (Sk_sampling_options) -> Ptr (Sk_matrix) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_shader_t *sk_image_make_raw_shader(const sk_image_t *image, sk_shader_tilemode_t tileX, sk_shader_tilemode_t tileY, const sk_sampling_options_t *sampling, const sk_matrix_t *cmatrix)
@
-}
foreign import ccall "sk_image_make_raw_shader" sk_image_make_raw_shader ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * image"@
  -> Sk_shader_tilemode -- ^ C argument @"sk_shader_tilemode_t tileX"@
  -> Sk_shader_tilemode -- ^ C argument @"sk_shader_tilemode_t tileY"@
  -> Ptr (Sk_sampling_options) -- ^ C argument @"const sk_sampling_options_t * sampling"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * cmatrix"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_image_make_raw_shader'
foreign import ccall "&sk_image_make_raw_shader" p'sk_image_make_raw_shader ::
  FunPtr (Ptr (Sk_image) -> Sk_shader_tilemode -> Sk_shader_tilemode -> Ptr (Sk_sampling_options) -> Ptr (Sk_matrix) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
_Bool sk_image_peek_pixels(const sk_image_t *image, sk_pixmap_t *pixmap)
@
-}
foreign import ccall "sk_image_peek_pixels" sk_image_peek_pixels ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * image"@
  -> Ptr (Sk_pixmap) -- ^ C argument @"sk_pixmap_t * pixmap"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_image_peek_pixels'
foreign import ccall "&sk_image_peek_pixels" p'sk_image_peek_pixels ::
  FunPtr (Ptr (Sk_image) -> Ptr (Sk_pixmap) -> IO (CBool))

{- | C function signature:

@
_Bool sk_image_is_texture_backed(const sk_image_t *image)
@
-}
foreign import ccall "sk_image_is_texture_backed" sk_image_is_texture_backed ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * image"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_image_is_texture_backed'
foreign import ccall "&sk_image_is_texture_backed" p'sk_image_is_texture_backed ::
  FunPtr (Ptr (Sk_image) -> IO (CBool))

{- | C function signature:

@
_Bool sk_image_is_lazy_generated(const sk_image_t *image)
@
-}
foreign import ccall "sk_image_is_lazy_generated" sk_image_is_lazy_generated ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * image"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_image_is_lazy_generated'
foreign import ccall "&sk_image_is_lazy_generated" p'sk_image_is_lazy_generated ::
  FunPtr (Ptr (Sk_image) -> IO (CBool))

{- | C function signature:

@
_Bool sk_image_is_valid(const sk_image_t *image, gr_recording_context_t *context)
@
-}
foreign import ccall "sk_image_is_valid" sk_image_is_valid ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * image"@
  -> Ptr (Gr_recording_context) -- ^ C argument @"gr_recording_context_t * context"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_image_is_valid'
foreign import ccall "&sk_image_is_valid" p'sk_image_is_valid ::
  FunPtr (Ptr (Sk_image) -> Ptr (Gr_recording_context) -> IO (CBool))

{- | C function signature:

@
_Bool sk_image_read_pixels(const sk_image_t *image, const sk_imageinfo_t *dstInfo, void *dstPixels, size_t dstRowBytes, int srcX, int srcY, sk_image_caching_hint_t cachingHint)
@
-}
foreign import ccall "sk_image_read_pixels" sk_image_read_pixels ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * image"@
  -> Ptr (Sk_imageinfo) -- ^ C argument @"const sk_imageinfo_t * dstInfo"@
  -> Ptr (()) -- ^ C argument @"void * dstPixels"@
  -> CSize -- ^ C argument @"size_t dstRowBytes"@
  -> CInt -- ^ C argument @"int srcX"@
  -> CInt -- ^ C argument @"int srcY"@
  -> Sk_image_caching_hint -- ^ C argument @"sk_image_caching_hint_t cachingHint"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_image_read_pixels'
foreign import ccall "&sk_image_read_pixels" p'sk_image_read_pixels ::
  FunPtr (Ptr (Sk_image) -> Ptr (Sk_imageinfo) -> Ptr (()) -> CSize -> CInt -> CInt -> Sk_image_caching_hint -> IO (CBool))

{- | C function signature:

@
_Bool sk_image_read_pixels_into_pixmap(const sk_image_t *image, const sk_pixmap_t *dst, int srcX, int srcY, sk_image_caching_hint_t cachingHint)
@
-}
foreign import ccall "sk_image_read_pixels_into_pixmap" sk_image_read_pixels_into_pixmap ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * image"@
  -> Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * dst"@
  -> CInt -- ^ C argument @"int srcX"@
  -> CInt -- ^ C argument @"int srcY"@
  -> Sk_image_caching_hint -- ^ C argument @"sk_image_caching_hint_t cachingHint"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_image_read_pixels_into_pixmap'
foreign import ccall "&sk_image_read_pixels_into_pixmap" p'sk_image_read_pixels_into_pixmap ::
  FunPtr (Ptr (Sk_image) -> Ptr (Sk_pixmap) -> CInt -> CInt -> Sk_image_caching_hint -> IO (CBool))

{- | C function signature:

@
_Bool sk_image_scale_pixels(const sk_image_t *image, const sk_pixmap_t *dst, const sk_sampling_options_t *sampling, sk_image_caching_hint_t cachingHint)
@
-}
foreign import ccall "sk_image_scale_pixels" sk_image_scale_pixels ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * image"@
  -> Ptr (Sk_pixmap) -- ^ C argument @"const sk_pixmap_t * dst"@
  -> Ptr (Sk_sampling_options) -- ^ C argument @"const sk_sampling_options_t * sampling"@
  -> Sk_image_caching_hint -- ^ C argument @"sk_image_caching_hint_t cachingHint"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_image_scale_pixels'
foreign import ccall "&sk_image_scale_pixels" p'sk_image_scale_pixels ::
  FunPtr (Ptr (Sk_image) -> Ptr (Sk_pixmap) -> Ptr (Sk_sampling_options) -> Sk_image_caching_hint -> IO (CBool))

{- | C function signature:

@
sk_data_t *sk_image_ref_encoded(const sk_image_t *cimage)
@
-}
foreign import ccall "sk_image_ref_encoded" sk_image_ref_encoded ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * cimage"@
  -> IO (Ptr (Sk_data)) -- ^ C return type: @"sk_data_t *"@

-- | Function pointer to 'sk_image_ref_encoded'
foreign import ccall "&sk_image_ref_encoded" p'sk_image_ref_encoded ::
  FunPtr (Ptr (Sk_image) -> IO (Ptr (Sk_data)))

{- | C function signature:

@
sk_image_t *sk_image_make_subset_raster(const sk_image_t *cimage, const sk_irect_t *subset)
@
-}
foreign import ccall "sk_image_make_subset_raster" sk_image_make_subset_raster ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * cimage"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * subset"@
  -> IO (Ptr (Sk_image)) -- ^ C return type: @"sk_image_t *"@

-- | Function pointer to 'sk_image_make_subset_raster'
foreign import ccall "&sk_image_make_subset_raster" p'sk_image_make_subset_raster ::
  FunPtr (Ptr (Sk_image) -> Ptr (Sk_irect) -> IO (Ptr (Sk_image)))

{- | C function signature:

@
sk_image_t *sk_image_make_subset(const sk_image_t *cimage, gr_direct_context_t *context, const sk_irect_t *subset)
@
-}
foreign import ccall "sk_image_make_subset" sk_image_make_subset ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * cimage"@
  -> Ptr (Gr_direct_context) -- ^ C argument @"gr_direct_context_t * context"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * subset"@
  -> IO (Ptr (Sk_image)) -- ^ C return type: @"sk_image_t *"@

-- | Function pointer to 'sk_image_make_subset'
foreign import ccall "&sk_image_make_subset" p'sk_image_make_subset ::
  FunPtr (Ptr (Sk_image) -> Ptr (Gr_direct_context) -> Ptr (Sk_irect) -> IO (Ptr (Sk_image)))

{- | C function signature:

@
sk_image_t *sk_image_make_texture_image(const sk_image_t *cimage, gr_direct_context_t *context, _Bool mipmapped, _Bool budgeted)
@
-}
foreign import ccall "sk_image_make_texture_image" sk_image_make_texture_image ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * cimage"@
  -> Ptr (Gr_direct_context) -- ^ C argument @"gr_direct_context_t * context"@
  -> CBool -- ^ C argument @"_Bool mipmapped"@
  -> CBool -- ^ C argument @"_Bool budgeted"@
  -> IO (Ptr (Sk_image)) -- ^ C return type: @"sk_image_t *"@

-- | Function pointer to 'sk_image_make_texture_image'
foreign import ccall "&sk_image_make_texture_image" p'sk_image_make_texture_image ::
  FunPtr (Ptr (Sk_image) -> Ptr (Gr_direct_context) -> CBool -> CBool -> IO (Ptr (Sk_image)))

{- | C function signature:

@
sk_image_t *sk_image_make_non_texture_image(const sk_image_t *cimage)
@
-}
foreign import ccall "sk_image_make_non_texture_image" sk_image_make_non_texture_image ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * cimage"@
  -> IO (Ptr (Sk_image)) -- ^ C return type: @"sk_image_t *"@

-- | Function pointer to 'sk_image_make_non_texture_image'
foreign import ccall "&sk_image_make_non_texture_image" p'sk_image_make_non_texture_image ::
  FunPtr (Ptr (Sk_image) -> IO (Ptr (Sk_image)))

{- | C function signature:

@
sk_image_t *sk_image_make_raster_image(const sk_image_t *cimage)
@
-}
foreign import ccall "sk_image_make_raster_image" sk_image_make_raster_image ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * cimage"@
  -> IO (Ptr (Sk_image)) -- ^ C return type: @"sk_image_t *"@

-- | Function pointer to 'sk_image_make_raster_image'
foreign import ccall "&sk_image_make_raster_image" p'sk_image_make_raster_image ::
  FunPtr (Ptr (Sk_image) -> IO (Ptr (Sk_image)))

{- | C function signature:

@
sk_image_t *sk_image_make_with_filter_raster(const sk_image_t *cimage, const sk_imagefilter_t *filter, const sk_irect_t *subset, const sk_irect_t *clipBounds, sk_irect_t *outSubset, sk_ipoint_t *outOffset)
@
-}
foreign import ccall "sk_image_make_with_filter_raster" sk_image_make_with_filter_raster ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * cimage"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * filter"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * subset"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * clipBounds"@
  -> Ptr (Sk_irect) -- ^ C argument @"sk_irect_t * outSubset"@
  -> Ptr (Sk_ipoint) -- ^ C argument @"sk_ipoint_t * outOffset"@
  -> IO (Ptr (Sk_image)) -- ^ C return type: @"sk_image_t *"@

-- | Function pointer to 'sk_image_make_with_filter_raster'
foreign import ccall "&sk_image_make_with_filter_raster" p'sk_image_make_with_filter_raster ::
  FunPtr (Ptr (Sk_image) -> Ptr (Sk_imagefilter) -> Ptr (Sk_irect) -> Ptr (Sk_irect) -> Ptr (Sk_irect) -> Ptr (Sk_ipoint) -> IO (Ptr (Sk_image)))

{- | C function signature:

@
sk_image_t *sk_image_make_with_filter(const sk_image_t *cimage, gr_recording_context_t *context, const sk_imagefilter_t *filter, const sk_irect_t *subset, const sk_irect_t *clipBounds, sk_irect_t *outSubset, sk_ipoint_t *outOffset)
@
-}
foreign import ccall "sk_image_make_with_filter" sk_image_make_with_filter ::
  Ptr (Sk_image) -- ^ C argument @"const sk_image_t * cimage"@
  -> Ptr (Gr_recording_context) -- ^ C argument @"gr_recording_context_t * context"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * filter"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * subset"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * clipBounds"@
  -> Ptr (Sk_irect) -- ^ C argument @"sk_irect_t * outSubset"@
  -> Ptr (Sk_ipoint) -- ^ C argument @"sk_ipoint_t * outOffset"@
  -> IO (Ptr (Sk_image)) -- ^ C return type: @"sk_image_t *"@

-- | Function pointer to 'sk_image_make_with_filter'
foreign import ccall "&sk_image_make_with_filter" p'sk_image_make_with_filter ::
  FunPtr (Ptr (Sk_image) -> Ptr (Gr_recording_context) -> Ptr (Sk_imagefilter) -> Ptr (Sk_irect) -> Ptr (Sk_irect) -> Ptr (Sk_irect) -> Ptr (Sk_ipoint) -> IO (Ptr (Sk_image)))
