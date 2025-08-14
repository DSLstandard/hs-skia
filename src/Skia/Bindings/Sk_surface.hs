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
module Skia.Bindings.Sk_surface where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
sk_surface_t *sk_surface_new_null(int width, int height)
@
-}
foreign import ccall "sk_surface_new_null" sk_surface_new_null ::
  CInt -- ^ C argument @"int width"@
  -> CInt -- ^ C argument @"int height"@
  -> IO (Ptr (Sk_surface)) -- ^ C return type: @"sk_surface_t *"@

-- | Function pointer to 'sk_surface_new_null'
foreign import ccall "&sk_surface_new_null" p'sk_surface_new_null ::
  FunPtr (CInt -> CInt -> IO (Ptr (Sk_surface)))

{- | C function signature:

@
sk_surface_t *sk_surface_new_raster(const sk_imageinfo_t *, size_t rowBytes, const sk_surfaceprops_t *)
@
-}
foreign import ccall "sk_surface_new_raster" sk_surface_new_raster ::
  Ptr (Sk_imageinfo) -- ^ C argument type: @"const sk_imageinfo_t *"@
  -> CSize -- ^ C argument @"size_t rowBytes"@
  -> Ptr (Sk_surfaceprops) -- ^ C argument type: @"const sk_surfaceprops_t *"@
  -> IO (Ptr (Sk_surface)) -- ^ C return type: @"sk_surface_t *"@

-- | Function pointer to 'sk_surface_new_raster'
foreign import ccall "&sk_surface_new_raster" p'sk_surface_new_raster ::
  FunPtr (Ptr (Sk_imageinfo) -> CSize -> Ptr (Sk_surfaceprops) -> IO (Ptr (Sk_surface)))

{- | C function signature:

@
sk_surface_t *sk_surface_new_raster_direct(const sk_imageinfo_t *, void *pixels, size_t rowBytes, const sk_surface_raster_release_proc releaseProc, void *context, const sk_surfaceprops_t *props)
@
-}
foreign import ccall "sk_surface_new_raster_direct" sk_surface_new_raster_direct ::
  Ptr (Sk_imageinfo) -- ^ C argument type: @"const sk_imageinfo_t *"@
  -> Ptr (()) -- ^ C argument @"void * pixels"@
  -> CSize -- ^ C argument @"size_t rowBytes"@
  -> FunPtr Sk_surface_raster_release_proc -- ^ C argument @"const sk_surface_raster_release_proc releaseProc"@
  -> Ptr (()) -- ^ C argument @"void * context"@
  -> Ptr (Sk_surfaceprops) -- ^ C argument @"const sk_surfaceprops_t * props"@
  -> IO (Ptr (Sk_surface)) -- ^ C return type: @"sk_surface_t *"@

-- | Function pointer to 'sk_surface_new_raster_direct'
foreign import ccall "&sk_surface_new_raster_direct" p'sk_surface_new_raster_direct ::
  FunPtr (Ptr (Sk_imageinfo) -> Ptr (()) -> CSize -> FunPtr Sk_surface_raster_release_proc -> Ptr (()) -> Ptr (Sk_surfaceprops) -> IO (Ptr (Sk_surface)))

{- | C function signature:

@
sk_surface_t *sk_surface_new_backend_texture(gr_recording_context_t *context, const gr_backendtexture_t *texture, gr_surfaceorigin_t origin, int samples, sk_colortype_t colorType, sk_colorspace_t *colorspace, const sk_surfaceprops_t *props)
@
-}
foreign import ccall "sk_surface_new_backend_texture" sk_surface_new_backend_texture ::
  Ptr (Gr_recording_context) -- ^ C argument @"gr_recording_context_t * context"@
  -> Ptr (Gr_backendtexture) -- ^ C argument @"const gr_backendtexture_t * texture"@
  -> Gr_surfaceorigin -- ^ C argument @"gr_surfaceorigin_t origin"@
  -> CInt -- ^ C argument @"int samples"@
  -> Sk_colortype -- ^ C argument @"sk_colortype_t colorType"@
  -> Ptr (Sk_colorspace) -- ^ C argument @"sk_colorspace_t * colorspace"@
  -> Ptr (Sk_surfaceprops) -- ^ C argument @"const sk_surfaceprops_t * props"@
  -> IO (Ptr (Sk_surface)) -- ^ C return type: @"sk_surface_t *"@

-- | Function pointer to 'sk_surface_new_backend_texture'
foreign import ccall "&sk_surface_new_backend_texture" p'sk_surface_new_backend_texture ::
  FunPtr (Ptr (Gr_recording_context) -> Ptr (Gr_backendtexture) -> Gr_surfaceorigin -> CInt -> Sk_colortype -> Ptr (Sk_colorspace) -> Ptr (Sk_surfaceprops) -> IO (Ptr (Sk_surface)))

{- | C function signature:

@
sk_surface_t *sk_surface_new_backend_render_target(gr_recording_context_t *context, const gr_backendrendertarget_t *target, gr_surfaceorigin_t origin, sk_colortype_t colorType, sk_colorspace_t *colorspace, const sk_surfaceprops_t *props)
@
-}
foreign import ccall "sk_surface_new_backend_render_target" sk_surface_new_backend_render_target ::
  Ptr (Gr_recording_context) -- ^ C argument @"gr_recording_context_t * context"@
  -> Ptr (Gr_backendrendertarget) -- ^ C argument @"const gr_backendrendertarget_t * target"@
  -> Gr_surfaceorigin -- ^ C argument @"gr_surfaceorigin_t origin"@
  -> Sk_colortype -- ^ C argument @"sk_colortype_t colorType"@
  -> Ptr (Sk_colorspace) -- ^ C argument @"sk_colorspace_t * colorspace"@
  -> Ptr (Sk_surfaceprops) -- ^ C argument @"const sk_surfaceprops_t * props"@
  -> IO (Ptr (Sk_surface)) -- ^ C return type: @"sk_surface_t *"@

-- | Function pointer to 'sk_surface_new_backend_render_target'
foreign import ccall "&sk_surface_new_backend_render_target" p'sk_surface_new_backend_render_target ::
  FunPtr (Ptr (Gr_recording_context) -> Ptr (Gr_backendrendertarget) -> Gr_surfaceorigin -> Sk_colortype -> Ptr (Sk_colorspace) -> Ptr (Sk_surfaceprops) -> IO (Ptr (Sk_surface)))

{- | C function signature:

@
sk_surface_t *sk_surface_new_render_target(gr_recording_context_t *context, _Bool budgeted, const sk_imageinfo_t *cinfo, int sampleCount, gr_surfaceorigin_t origin, const sk_surfaceprops_t *props, _Bool shouldCreateWithMips)
@
-}
foreign import ccall "sk_surface_new_render_target" sk_surface_new_render_target ::
  Ptr (Gr_recording_context) -- ^ C argument @"gr_recording_context_t * context"@
  -> CBool -- ^ C argument @"_Bool budgeted"@
  -> Ptr (Sk_imageinfo) -- ^ C argument @"const sk_imageinfo_t * cinfo"@
  -> CInt -- ^ C argument @"int sampleCount"@
  -> Gr_surfaceorigin -- ^ C argument @"gr_surfaceorigin_t origin"@
  -> Ptr (Sk_surfaceprops) -- ^ C argument @"const sk_surfaceprops_t * props"@
  -> CBool -- ^ C argument @"_Bool shouldCreateWithMips"@
  -> IO (Ptr (Sk_surface)) -- ^ C return type: @"sk_surface_t *"@

-- | Function pointer to 'sk_surface_new_render_target'
foreign import ccall "&sk_surface_new_render_target" p'sk_surface_new_render_target ::
  FunPtr (Ptr (Gr_recording_context) -> CBool -> Ptr (Sk_imageinfo) -> CInt -> Gr_surfaceorigin -> Ptr (Sk_surfaceprops) -> CBool -> IO (Ptr (Sk_surface)))

{- | C function signature:

@
sk_surface_t *sk_surface_new_metal_layer(gr_recording_context_t *context, const void *layer, gr_surfaceorigin_t origin, int sampleCount, sk_colortype_t colorType, sk_colorspace_t *colorspace, const sk_surfaceprops_t *props, const void **drawable)
@
-}
foreign import ccall "sk_surface_new_metal_layer" sk_surface_new_metal_layer ::
  Ptr (Gr_recording_context) -- ^ C argument @"gr_recording_context_t * context"@
  -> Ptr (()) -- ^ C argument @"const void * layer"@
  -> Gr_surfaceorigin -- ^ C argument @"gr_surfaceorigin_t origin"@
  -> CInt -- ^ C argument @"int sampleCount"@
  -> Sk_colortype -- ^ C argument @"sk_colortype_t colorType"@
  -> Ptr (Sk_colorspace) -- ^ C argument @"sk_colorspace_t * colorspace"@
  -> Ptr (Sk_surfaceprops) -- ^ C argument @"const sk_surfaceprops_t * props"@
  -> Ptr (Ptr (())) -- ^ C argument @"const void ** drawable"@
  -> IO (Ptr (Sk_surface)) -- ^ C return type: @"sk_surface_t *"@

-- | Function pointer to 'sk_surface_new_metal_layer'
foreign import ccall "&sk_surface_new_metal_layer" p'sk_surface_new_metal_layer ::
  FunPtr (Ptr (Gr_recording_context) -> Ptr (()) -> Gr_surfaceorigin -> CInt -> Sk_colortype -> Ptr (Sk_colorspace) -> Ptr (Sk_surfaceprops) -> Ptr (Ptr (())) -> IO (Ptr (Sk_surface)))

{- | C function signature:

@
sk_surface_t *sk_surface_new_metal_view(gr_recording_context_t *context, const void *mtkView, gr_surfaceorigin_t origin, int sampleCount, sk_colortype_t colorType, sk_colorspace_t *colorspace, const sk_surfaceprops_t *props)
@
-}
foreign import ccall "sk_surface_new_metal_view" sk_surface_new_metal_view ::
  Ptr (Gr_recording_context) -- ^ C argument @"gr_recording_context_t * context"@
  -> Ptr (()) -- ^ C argument @"const void * mtkView"@
  -> Gr_surfaceorigin -- ^ C argument @"gr_surfaceorigin_t origin"@
  -> CInt -- ^ C argument @"int sampleCount"@
  -> Sk_colortype -- ^ C argument @"sk_colortype_t colorType"@
  -> Ptr (Sk_colorspace) -- ^ C argument @"sk_colorspace_t * colorspace"@
  -> Ptr (Sk_surfaceprops) -- ^ C argument @"const sk_surfaceprops_t * props"@
  -> IO (Ptr (Sk_surface)) -- ^ C return type: @"sk_surface_t *"@

-- | Function pointer to 'sk_surface_new_metal_view'
foreign import ccall "&sk_surface_new_metal_view" p'sk_surface_new_metal_view ::
  FunPtr (Ptr (Gr_recording_context) -> Ptr (()) -> Gr_surfaceorigin -> CInt -> Sk_colortype -> Ptr (Sk_colorspace) -> Ptr (Sk_surfaceprops) -> IO (Ptr (Sk_surface)))

{- | C function signature:

@
void sk_surface_unref(sk_surface_t *)
@
-}
foreign import ccall "sk_surface_unref" sk_surface_unref ::
  Ptr (Sk_surface) -- ^ C argument type: @"sk_surface_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_surface_unref'
foreign import ccall "&sk_surface_unref" p'sk_surface_unref ::
  FunPtr (Ptr (Sk_surface) -> IO (()))

{- | C function signature:

@
sk_canvas_t *sk_surface_get_canvas(sk_surface_t *)
@
-}
foreign import ccall "sk_surface_get_canvas" sk_surface_get_canvas ::
  Ptr (Sk_surface) -- ^ C argument type: @"sk_surface_t *"@
  -> IO (Ptr (Sk_canvas)) -- ^ C return type: @"sk_canvas_t *"@

-- | Function pointer to 'sk_surface_get_canvas'
foreign import ccall "&sk_surface_get_canvas" p'sk_surface_get_canvas ::
  FunPtr (Ptr (Sk_surface) -> IO (Ptr (Sk_canvas)))

{- | C function signature:

@
sk_image_t *sk_surface_new_image_snapshot(sk_surface_t *)
@
-}
foreign import ccall "sk_surface_new_image_snapshot" sk_surface_new_image_snapshot ::
  Ptr (Sk_surface) -- ^ C argument type: @"sk_surface_t *"@
  -> IO (Ptr (Sk_image)) -- ^ C return type: @"sk_image_t *"@

-- | Function pointer to 'sk_surface_new_image_snapshot'
foreign import ccall "&sk_surface_new_image_snapshot" p'sk_surface_new_image_snapshot ::
  FunPtr (Ptr (Sk_surface) -> IO (Ptr (Sk_image)))

{- | C function signature:

@
sk_image_t *sk_surface_new_image_snapshot_with_crop(sk_surface_t *surface, const sk_irect_t *bounds)
@
-}
foreign import ccall "sk_surface_new_image_snapshot_with_crop" sk_surface_new_image_snapshot_with_crop ::
  Ptr (Sk_surface) -- ^ C argument @"sk_surface_t * surface"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * bounds"@
  -> IO (Ptr (Sk_image)) -- ^ C return type: @"sk_image_t *"@

-- | Function pointer to 'sk_surface_new_image_snapshot_with_crop'
foreign import ccall "&sk_surface_new_image_snapshot_with_crop" p'sk_surface_new_image_snapshot_with_crop ::
  FunPtr (Ptr (Sk_surface) -> Ptr (Sk_irect) -> IO (Ptr (Sk_image)))

{- | C function signature:

@
void sk_surface_draw(sk_surface_t *surface, sk_canvas_t *canvas, float x, float y, const sk_paint_t *paint)
@
-}
foreign import ccall "sk_surface_draw" sk_surface_draw ::
  Ptr (Sk_surface) -- ^ C argument @"sk_surface_t * surface"@
  -> Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * canvas"@
  -> CFloat -- ^ C argument @"float x"@
  -> CFloat -- ^ C argument @"float y"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * paint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_surface_draw'
foreign import ccall "&sk_surface_draw" p'sk_surface_draw ::
  FunPtr (Ptr (Sk_surface) -> Ptr (Sk_canvas) -> CFloat -> CFloat -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
_Bool sk_surface_peek_pixels(sk_surface_t *surface, sk_pixmap_t *pixmap)
@
-}
foreign import ccall "sk_surface_peek_pixels" sk_surface_peek_pixels ::
  Ptr (Sk_surface) -- ^ C argument @"sk_surface_t * surface"@
  -> Ptr (Sk_pixmap) -- ^ C argument @"sk_pixmap_t * pixmap"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_surface_peek_pixels'
foreign import ccall "&sk_surface_peek_pixels" p'sk_surface_peek_pixels ::
  FunPtr (Ptr (Sk_surface) -> Ptr (Sk_pixmap) -> IO (CBool))

{- | C function signature:

@
_Bool sk_surface_read_pixels(sk_surface_t *surface, sk_imageinfo_t *dstInfo, void *dstPixels, size_t dstRowBytes, int srcX, int srcY)
@
-}
foreign import ccall "sk_surface_read_pixels" sk_surface_read_pixels ::
  Ptr (Sk_surface) -- ^ C argument @"sk_surface_t * surface"@
  -> Ptr (Sk_imageinfo) -- ^ C argument @"sk_imageinfo_t * dstInfo"@
  -> Ptr (()) -- ^ C argument @"void * dstPixels"@
  -> CSize -- ^ C argument @"size_t dstRowBytes"@
  -> CInt -- ^ C argument @"int srcX"@
  -> CInt -- ^ C argument @"int srcY"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_surface_read_pixels'
foreign import ccall "&sk_surface_read_pixels" p'sk_surface_read_pixels ::
  FunPtr (Ptr (Sk_surface) -> Ptr (Sk_imageinfo) -> Ptr (()) -> CSize -> CInt -> CInt -> IO (CBool))

{- | C function signature:

@
const sk_surfaceprops_t *sk_surface_get_props(sk_surface_t *surface)
@
-}
foreign import ccall "sk_surface_get_props" sk_surface_get_props ::
  Ptr (Sk_surface) -- ^ C argument @"sk_surface_t * surface"@
  -> IO (Ptr (Sk_surfaceprops)) -- ^ C return type: @"const sk_surfaceprops_t *"@

-- | Function pointer to 'sk_surface_get_props'
foreign import ccall "&sk_surface_get_props" p'sk_surface_get_props ::
  FunPtr (Ptr (Sk_surface) -> IO (Ptr (Sk_surfaceprops)))

{- | C function signature:

@
gr_recording_context_t *sk_surface_get_recording_context(sk_surface_t *surface)
@
-}
foreign import ccall "sk_surface_get_recording_context" sk_surface_get_recording_context ::
  Ptr (Sk_surface) -- ^ C argument @"sk_surface_t * surface"@
  -> IO (Ptr (Gr_recording_context)) -- ^ C return type: @"gr_recording_context_t *"@

-- | Function pointer to 'sk_surface_get_recording_context'
foreign import ccall "&sk_surface_get_recording_context" p'sk_surface_get_recording_context ::
  FunPtr (Ptr (Sk_surface) -> IO (Ptr (Gr_recording_context)))

{- | C function signature:

@
sk_surfaceprops_t *sk_surfaceprops_new(uint32_t flags, sk_pixelgeometry_t geometry)
@
-}
foreign import ccall "sk_surfaceprops_new" sk_surfaceprops_new ::
  Word32 -- ^ C argument @"uint32_t flags"@
  -> Sk_pixelgeometry -- ^ C argument @"sk_pixelgeometry_t geometry"@
  -> IO (Ptr (Sk_surfaceprops)) -- ^ C return type: @"sk_surfaceprops_t *"@

-- | Function pointer to 'sk_surfaceprops_new'
foreign import ccall "&sk_surfaceprops_new" p'sk_surfaceprops_new ::
  FunPtr (Word32 -> Sk_pixelgeometry -> IO (Ptr (Sk_surfaceprops)))

{- | C function signature:

@
void sk_surfaceprops_delete(sk_surfaceprops_t *props)
@
-}
foreign import ccall "sk_surfaceprops_delete" sk_surfaceprops_delete ::
  Ptr (Sk_surfaceprops) -- ^ C argument @"sk_surfaceprops_t * props"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_surfaceprops_delete'
foreign import ccall "&sk_surfaceprops_delete" p'sk_surfaceprops_delete ::
  FunPtr (Ptr (Sk_surfaceprops) -> IO (()))

{- | C function signature:

@
uint32_t sk_surfaceprops_get_flags(sk_surfaceprops_t *props)
@
-}
foreign import ccall "sk_surfaceprops_get_flags" sk_surfaceprops_get_flags ::
  Ptr (Sk_surfaceprops) -- ^ C argument @"sk_surfaceprops_t * props"@
  -> IO (Word32) -- ^ C return type: @"uint32_t"@

-- | Function pointer to 'sk_surfaceprops_get_flags'
foreign import ccall "&sk_surfaceprops_get_flags" p'sk_surfaceprops_get_flags ::
  FunPtr (Ptr (Sk_surfaceprops) -> IO (Word32))

{- | C function signature:

@
sk_pixelgeometry_t sk_surfaceprops_get_pixel_geometry(sk_surfaceprops_t *props)
@
-}
foreign import ccall "sk_surfaceprops_get_pixel_geometry" sk_surfaceprops_get_pixel_geometry ::
  Ptr (Sk_surfaceprops) -- ^ C argument @"sk_surfaceprops_t * props"@
  -> IO (Sk_pixelgeometry) -- ^ C return type: @"sk_pixelgeometry_t"@

-- | Function pointer to 'sk_surfaceprops_get_pixel_geometry'
foreign import ccall "&sk_surfaceprops_get_pixel_geometry" p'sk_surfaceprops_get_pixel_geometry ::
  FunPtr (Ptr (Sk_surfaceprops) -> IO (Sk_pixelgeometry))
