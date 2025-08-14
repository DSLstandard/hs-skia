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
module Skia.Bindings.Sk_imagefilter where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void sk_imagefilter_unref(sk_imagefilter_t *cfilter)
@
-}
foreign import ccall "sk_imagefilter_unref" sk_imagefilter_unref ::
  Ptr (Sk_imagefilter) -- ^ C argument @"sk_imagefilter_t * cfilter"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_imagefilter_unref'
foreign import ccall "&sk_imagefilter_unref" p'sk_imagefilter_unref ::
  FunPtr (Ptr (Sk_imagefilter) -> IO (()))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_arithmetic(float k1, float k2, float k3, float k4, _Bool enforcePMColor, const sk_imagefilter_t *background, const sk_imagefilter_t *foreground, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_arithmetic" sk_imagefilter_new_arithmetic ::
  CFloat -- ^ C argument @"float k1"@
  -> CFloat -- ^ C argument @"float k2"@
  -> CFloat -- ^ C argument @"float k3"@
  -> CFloat -- ^ C argument @"float k4"@
  -> CBool -- ^ C argument @"_Bool enforcePMColor"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * background"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * foreground"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_arithmetic'
foreign import ccall "&sk_imagefilter_new_arithmetic" p'sk_imagefilter_new_arithmetic ::
  FunPtr (CFloat -> CFloat -> CFloat -> CFloat -> CBool -> Ptr (Sk_imagefilter) -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_blend(sk_blendmode_t mode, const sk_imagefilter_t *background, const sk_imagefilter_t *foreground, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_blend" sk_imagefilter_new_blend ::
  Sk_blendmode -- ^ C argument @"sk_blendmode_t mode"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * background"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * foreground"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_blend'
foreign import ccall "&sk_imagefilter_new_blend" p'sk_imagefilter_new_blend ::
  FunPtr (Sk_blendmode -> Ptr (Sk_imagefilter) -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_blender(sk_blender_t *blender, const sk_imagefilter_t *background, const sk_imagefilter_t *foreground, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_blender" sk_imagefilter_new_blender ::
  Ptr (Sk_blender) -- ^ C argument @"sk_blender_t * blender"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * background"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * foreground"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_blender'
foreign import ccall "&sk_imagefilter_new_blender" p'sk_imagefilter_new_blender ::
  FunPtr (Ptr (Sk_blender) -> Ptr (Sk_imagefilter) -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_blur(float sigmaX, float sigmaY, sk_shader_tilemode_t tileMode, const sk_imagefilter_t *input, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_blur" sk_imagefilter_new_blur ::
  CFloat -- ^ C argument @"float sigmaX"@
  -> CFloat -- ^ C argument @"float sigmaY"@
  -> Sk_shader_tilemode -- ^ C argument @"sk_shader_tilemode_t tileMode"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * input"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_blur'
foreign import ccall "&sk_imagefilter_new_blur" p'sk_imagefilter_new_blur ::
  FunPtr (CFloat -> CFloat -> Sk_shader_tilemode -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_color_filter(sk_colorfilter_t *cf, const sk_imagefilter_t *input, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_color_filter" sk_imagefilter_new_color_filter ::
  Ptr (Sk_colorfilter) -- ^ C argument @"sk_colorfilter_t * cf"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * input"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_color_filter'
foreign import ccall "&sk_imagefilter_new_color_filter" p'sk_imagefilter_new_color_filter ::
  FunPtr (Ptr (Sk_colorfilter) -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_compose(const sk_imagefilter_t *outer, const sk_imagefilter_t *inner)
@
-}
foreign import ccall "sk_imagefilter_new_compose" sk_imagefilter_new_compose ::
  Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * outer"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * inner"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_compose'
foreign import ccall "&sk_imagefilter_new_compose" p'sk_imagefilter_new_compose ::
  FunPtr (Ptr (Sk_imagefilter) -> Ptr (Sk_imagefilter) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_displacement_map_effect(sk_color_channel_t xChannelSelector, sk_color_channel_t yChannelSelector, float scale, const sk_imagefilter_t *displacement, const sk_imagefilter_t *color, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_displacement_map_effect" sk_imagefilter_new_displacement_map_effect ::
  Sk_color_channel -- ^ C argument @"sk_color_channel_t xChannelSelector"@
  -> Sk_color_channel -- ^ C argument @"sk_color_channel_t yChannelSelector"@
  -> CFloat -- ^ C argument @"float scale"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * displacement"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * color"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_displacement_map_effect'
foreign import ccall "&sk_imagefilter_new_displacement_map_effect" p'sk_imagefilter_new_displacement_map_effect ::
  FunPtr (Sk_color_channel -> Sk_color_channel -> CFloat -> Ptr (Sk_imagefilter) -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_drop_shadow(float dx, float dy, float sigmaX, float sigmaY, sk_color_t color, const sk_imagefilter_t *input, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_drop_shadow" sk_imagefilter_new_drop_shadow ::
  CFloat -- ^ C argument @"float dx"@
  -> CFloat -- ^ C argument @"float dy"@
  -> CFloat -- ^ C argument @"float sigmaX"@
  -> CFloat -- ^ C argument @"float sigmaY"@
  -> Sk_color -- ^ C argument @"sk_color_t color"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * input"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_drop_shadow'
foreign import ccall "&sk_imagefilter_new_drop_shadow" p'sk_imagefilter_new_drop_shadow ::
  FunPtr (CFloat -> CFloat -> CFloat -> CFloat -> Sk_color -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_drop_shadow_only(float dx, float dy, float sigmaX, float sigmaY, sk_color_t color, const sk_imagefilter_t *input, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_drop_shadow_only" sk_imagefilter_new_drop_shadow_only ::
  CFloat -- ^ C argument @"float dx"@
  -> CFloat -- ^ C argument @"float dy"@
  -> CFloat -- ^ C argument @"float sigmaX"@
  -> CFloat -- ^ C argument @"float sigmaY"@
  -> Sk_color -- ^ C argument @"sk_color_t color"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * input"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_drop_shadow_only'
foreign import ccall "&sk_imagefilter_new_drop_shadow_only" p'sk_imagefilter_new_drop_shadow_only ::
  FunPtr (CFloat -> CFloat -> CFloat -> CFloat -> Sk_color -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_image(sk_image_t *image, const sk_rect_t *srcRect, const sk_rect_t *dstRect, const sk_sampling_options_t *sampling)
@
-}
foreign import ccall "sk_imagefilter_new_image" sk_imagefilter_new_image ::
  Ptr (Sk_image) -- ^ C argument @"sk_image_t * image"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * srcRect"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * dstRect"@
  -> Ptr (Sk_sampling_options) -- ^ C argument @"const sk_sampling_options_t * sampling"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_image'
foreign import ccall "&sk_imagefilter_new_image" p'sk_imagefilter_new_image ::
  FunPtr (Ptr (Sk_image) -> Ptr (Sk_rect) -> Ptr (Sk_rect) -> Ptr (Sk_sampling_options) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_image_simple(sk_image_t *image, const sk_sampling_options_t *sampling)
@
-}
foreign import ccall "sk_imagefilter_new_image_simple" sk_imagefilter_new_image_simple ::
  Ptr (Sk_image) -- ^ C argument @"sk_image_t * image"@
  -> Ptr (Sk_sampling_options) -- ^ C argument @"const sk_sampling_options_t * sampling"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_image_simple'
foreign import ccall "&sk_imagefilter_new_image_simple" p'sk_imagefilter_new_image_simple ::
  FunPtr (Ptr (Sk_image) -> Ptr (Sk_sampling_options) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_magnifier(const sk_rect_t *lensBounds, float zoomAmount, float inset, const sk_sampling_options_t *sampling, const sk_imagefilter_t *input, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_magnifier" sk_imagefilter_new_magnifier ::
  Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * lensBounds"@
  -> CFloat -- ^ C argument @"float zoomAmount"@
  -> CFloat -- ^ C argument @"float inset"@
  -> Ptr (Sk_sampling_options) -- ^ C argument @"const sk_sampling_options_t * sampling"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * input"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_magnifier'
foreign import ccall "&sk_imagefilter_new_magnifier" p'sk_imagefilter_new_magnifier ::
  FunPtr (Ptr (Sk_rect) -> CFloat -> CFloat -> Ptr (Sk_sampling_options) -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_matrix_convolution(const sk_isize_t *kernelSize, const float kernel[], float gain, float bias, const sk_ipoint_t *kernelOffset, sk_shader_tilemode_t ctileMode, _Bool convolveAlpha, const sk_imagefilter_t *input, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_matrix_convolution" sk_imagefilter_new_matrix_convolution ::
  Ptr (Sk_isize) -- ^ C argument @"const sk_isize_t * kernelSize"@
  -> Ptr (CFloat) -- ^ C argument @"const float [] kernel"@
  -> CFloat -- ^ C argument @"float gain"@
  -> CFloat -- ^ C argument @"float bias"@
  -> Ptr (Sk_ipoint) -- ^ C argument @"const sk_ipoint_t * kernelOffset"@
  -> Sk_shader_tilemode -- ^ C argument @"sk_shader_tilemode_t ctileMode"@
  -> CBool -- ^ C argument @"_Bool convolveAlpha"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * input"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_matrix_convolution'
foreign import ccall "&sk_imagefilter_new_matrix_convolution" p'sk_imagefilter_new_matrix_convolution ::
  FunPtr (Ptr (Sk_isize) -> Ptr (CFloat) -> CFloat -> CFloat -> Ptr (Sk_ipoint) -> Sk_shader_tilemode -> CBool -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_matrix_transform(const sk_matrix_t *cmatrix, const sk_sampling_options_t *sampling, const sk_imagefilter_t *input)
@
-}
foreign import ccall "sk_imagefilter_new_matrix_transform" sk_imagefilter_new_matrix_transform ::
  Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * cmatrix"@
  -> Ptr (Sk_sampling_options) -- ^ C argument @"const sk_sampling_options_t * sampling"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * input"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_matrix_transform'
foreign import ccall "&sk_imagefilter_new_matrix_transform" p'sk_imagefilter_new_matrix_transform ::
  FunPtr (Ptr (Sk_matrix) -> Ptr (Sk_sampling_options) -> Ptr (Sk_imagefilter) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_merge(const sk_imagefilter_t *cfilters[], int count, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_merge" sk_imagefilter_new_merge ::
  Ptr (Ptr (Sk_imagefilter)) -- ^ C argument @"const sk_imagefilter_t *[] cfilters"@
  -> CInt -- ^ C argument @"int count"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_merge'
foreign import ccall "&sk_imagefilter_new_merge" p'sk_imagefilter_new_merge ::
  FunPtr (Ptr (Ptr (Sk_imagefilter)) -> CInt -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_merge_simple(const sk_imagefilter_t *first, const sk_imagefilter_t *second, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_merge_simple" sk_imagefilter_new_merge_simple ::
  Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * first"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * second"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_merge_simple'
foreign import ccall "&sk_imagefilter_new_merge_simple" p'sk_imagefilter_new_merge_simple ::
  FunPtr (Ptr (Sk_imagefilter) -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_offset(float dx, float dy, const sk_imagefilter_t *input, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_offset" sk_imagefilter_new_offset ::
  CFloat -- ^ C argument @"float dx"@
  -> CFloat -- ^ C argument @"float dy"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * input"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_offset'
foreign import ccall "&sk_imagefilter_new_offset" p'sk_imagefilter_new_offset ::
  FunPtr (CFloat -> CFloat -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_picture(const sk_picture_t *picture)
@
-}
foreign import ccall "sk_imagefilter_new_picture" sk_imagefilter_new_picture ::
  Ptr (Sk_picture) -- ^ C argument @"const sk_picture_t * picture"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_picture'
foreign import ccall "&sk_imagefilter_new_picture" p'sk_imagefilter_new_picture ::
  FunPtr (Ptr (Sk_picture) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_picture_with_rect(const sk_picture_t *picture, const sk_rect_t *targetRect)
@
-}
foreign import ccall "sk_imagefilter_new_picture_with_rect" sk_imagefilter_new_picture_with_rect ::
  Ptr (Sk_picture) -- ^ C argument @"const sk_picture_t * picture"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * targetRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_picture_with_rect'
foreign import ccall "&sk_imagefilter_new_picture_with_rect" p'sk_imagefilter_new_picture_with_rect ::
  FunPtr (Ptr (Sk_picture) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_shader(const sk_shader_t *shader, _Bool dither, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_shader" sk_imagefilter_new_shader ::
  Ptr (Sk_shader) -- ^ C argument @"const sk_shader_t * shader"@
  -> CBool -- ^ C argument @"_Bool dither"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_shader'
foreign import ccall "&sk_imagefilter_new_shader" p'sk_imagefilter_new_shader ::
  FunPtr (Ptr (Sk_shader) -> CBool -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_tile(const sk_rect_t *src, const sk_rect_t *dst, const sk_imagefilter_t *input)
@
-}
foreign import ccall "sk_imagefilter_new_tile" sk_imagefilter_new_tile ::
  Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * src"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * dst"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * input"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_tile'
foreign import ccall "&sk_imagefilter_new_tile" p'sk_imagefilter_new_tile ::
  FunPtr (Ptr (Sk_rect) -> Ptr (Sk_rect) -> Ptr (Sk_imagefilter) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_dilate(float radiusX, float radiusY, const sk_imagefilter_t *input, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_dilate" sk_imagefilter_new_dilate ::
  CFloat -- ^ C argument @"float radiusX"@
  -> CFloat -- ^ C argument @"float radiusY"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * input"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_dilate'
foreign import ccall "&sk_imagefilter_new_dilate" p'sk_imagefilter_new_dilate ::
  FunPtr (CFloat -> CFloat -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_erode(float radiusX, float radiusY, const sk_imagefilter_t *input, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_erode" sk_imagefilter_new_erode ::
  CFloat -- ^ C argument @"float radiusX"@
  -> CFloat -- ^ C argument @"float radiusY"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * input"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_erode'
foreign import ccall "&sk_imagefilter_new_erode" p'sk_imagefilter_new_erode ::
  FunPtr (CFloat -> CFloat -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_distant_lit_diffuse(const sk_point3_t *direction, sk_color_t lightColor, float surfaceScale, float kd, const sk_imagefilter_t *input, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_distant_lit_diffuse" sk_imagefilter_new_distant_lit_diffuse ::
  Ptr (Sk_point3) -- ^ C argument @"const sk_point3_t * direction"@
  -> Sk_color -- ^ C argument @"sk_color_t lightColor"@
  -> CFloat -- ^ C argument @"float surfaceScale"@
  -> CFloat -- ^ C argument @"float kd"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * input"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_distant_lit_diffuse'
foreign import ccall "&sk_imagefilter_new_distant_lit_diffuse" p'sk_imagefilter_new_distant_lit_diffuse ::
  FunPtr (Ptr (Sk_point3) -> Sk_color -> CFloat -> CFloat -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_point_lit_diffuse(const sk_point3_t *location, sk_color_t lightColor, float surfaceScale, float kd, const sk_imagefilter_t *input, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_point_lit_diffuse" sk_imagefilter_new_point_lit_diffuse ::
  Ptr (Sk_point3) -- ^ C argument @"const sk_point3_t * location"@
  -> Sk_color -- ^ C argument @"sk_color_t lightColor"@
  -> CFloat -- ^ C argument @"float surfaceScale"@
  -> CFloat -- ^ C argument @"float kd"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * input"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_point_lit_diffuse'
foreign import ccall "&sk_imagefilter_new_point_lit_diffuse" p'sk_imagefilter_new_point_lit_diffuse ::
  FunPtr (Ptr (Sk_point3) -> Sk_color -> CFloat -> CFloat -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_spot_lit_diffuse(const sk_point3_t *location, const sk_point3_t *target, float specularExponent, float cutoffAngle, sk_color_t lightColor, float surfaceScale, float kd, const sk_imagefilter_t *input, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_spot_lit_diffuse" sk_imagefilter_new_spot_lit_diffuse ::
  Ptr (Sk_point3) -- ^ C argument @"const sk_point3_t * location"@
  -> Ptr (Sk_point3) -- ^ C argument @"const sk_point3_t * target"@
  -> CFloat -- ^ C argument @"float specularExponent"@
  -> CFloat -- ^ C argument @"float cutoffAngle"@
  -> Sk_color -- ^ C argument @"sk_color_t lightColor"@
  -> CFloat -- ^ C argument @"float surfaceScale"@
  -> CFloat -- ^ C argument @"float kd"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * input"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_spot_lit_diffuse'
foreign import ccall "&sk_imagefilter_new_spot_lit_diffuse" p'sk_imagefilter_new_spot_lit_diffuse ::
  FunPtr (Ptr (Sk_point3) -> Ptr (Sk_point3) -> CFloat -> CFloat -> Sk_color -> CFloat -> CFloat -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_distant_lit_specular(const sk_point3_t *direction, sk_color_t lightColor, float surfaceScale, float ks, float shininess, const sk_imagefilter_t *input, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_distant_lit_specular" sk_imagefilter_new_distant_lit_specular ::
  Ptr (Sk_point3) -- ^ C argument @"const sk_point3_t * direction"@
  -> Sk_color -- ^ C argument @"sk_color_t lightColor"@
  -> CFloat -- ^ C argument @"float surfaceScale"@
  -> CFloat -- ^ C argument @"float ks"@
  -> CFloat -- ^ C argument @"float shininess"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * input"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_distant_lit_specular'
foreign import ccall "&sk_imagefilter_new_distant_lit_specular" p'sk_imagefilter_new_distant_lit_specular ::
  FunPtr (Ptr (Sk_point3) -> Sk_color -> CFloat -> CFloat -> CFloat -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_point_lit_specular(const sk_point3_t *location, sk_color_t lightColor, float surfaceScale, float ks, float shininess, const sk_imagefilter_t *input, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_point_lit_specular" sk_imagefilter_new_point_lit_specular ::
  Ptr (Sk_point3) -- ^ C argument @"const sk_point3_t * location"@
  -> Sk_color -- ^ C argument @"sk_color_t lightColor"@
  -> CFloat -- ^ C argument @"float surfaceScale"@
  -> CFloat -- ^ C argument @"float ks"@
  -> CFloat -- ^ C argument @"float shininess"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * input"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_point_lit_specular'
foreign import ccall "&sk_imagefilter_new_point_lit_specular" p'sk_imagefilter_new_point_lit_specular ::
  FunPtr (Ptr (Sk_point3) -> Sk_color -> CFloat -> CFloat -> CFloat -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_imagefilter_t *sk_imagefilter_new_spot_lit_specular(const sk_point3_t *location, const sk_point3_t *target, float specularExponent, float cutoffAngle, sk_color_t lightColor, float surfaceScale, float ks, float shininess, const sk_imagefilter_t *input, const sk_rect_t *cropRect)
@
-}
foreign import ccall "sk_imagefilter_new_spot_lit_specular" sk_imagefilter_new_spot_lit_specular ::
  Ptr (Sk_point3) -- ^ C argument @"const sk_point3_t * location"@
  -> Ptr (Sk_point3) -- ^ C argument @"const sk_point3_t * target"@
  -> CFloat -- ^ C argument @"float specularExponent"@
  -> CFloat -- ^ C argument @"float cutoffAngle"@
  -> Sk_color -- ^ C argument @"sk_color_t lightColor"@
  -> CFloat -- ^ C argument @"float surfaceScale"@
  -> CFloat -- ^ C argument @"float ks"@
  -> CFloat -- ^ C argument @"float shininess"@
  -> Ptr (Sk_imagefilter) -- ^ C argument @"const sk_imagefilter_t * input"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cropRect"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_imagefilter_new_spot_lit_specular'
foreign import ccall "&sk_imagefilter_new_spot_lit_specular" p'sk_imagefilter_new_spot_lit_specular ::
  FunPtr (Ptr (Sk_point3) -> Ptr (Sk_point3) -> CFloat -> CFloat -> Sk_color -> CFloat -> CFloat -> CFloat -> Ptr (Sk_imagefilter) -> Ptr (Sk_rect) -> IO (Ptr (Sk_imagefilter)))
