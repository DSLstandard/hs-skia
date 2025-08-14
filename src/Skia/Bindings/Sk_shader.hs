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
module Skia.Bindings.Sk_shader where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void sk_shader_ref(sk_shader_t *shader)
@
-}
foreign import ccall "sk_shader_ref" sk_shader_ref ::
  Ptr (Sk_shader) -- ^ C argument @"sk_shader_t * shader"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_shader_ref'
foreign import ccall "&sk_shader_ref" p'sk_shader_ref ::
  FunPtr (Ptr (Sk_shader) -> IO (()))

{- | C function signature:

@
void sk_shader_unref(sk_shader_t *shader)
@
-}
foreign import ccall "sk_shader_unref" sk_shader_unref ::
  Ptr (Sk_shader) -- ^ C argument @"sk_shader_t * shader"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_shader_unref'
foreign import ccall "&sk_shader_unref" p'sk_shader_unref ::
  FunPtr (Ptr (Sk_shader) -> IO (()))

{- | C function signature:

@
sk_shader_t *sk_shader_with_local_matrix(const sk_shader_t *shader, const sk_matrix_t *localMatrix)
@
-}
foreign import ccall "sk_shader_with_local_matrix" sk_shader_with_local_matrix ::
  Ptr (Sk_shader) -- ^ C argument @"const sk_shader_t * shader"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * localMatrix"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_shader_with_local_matrix'
foreign import ccall "&sk_shader_with_local_matrix" p'sk_shader_with_local_matrix ::
  FunPtr (Ptr (Sk_shader) -> Ptr (Sk_matrix) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_shader_t *sk_shader_with_color_filter(const sk_shader_t *shader, const sk_colorfilter_t *filter)
@
-}
foreign import ccall "sk_shader_with_color_filter" sk_shader_with_color_filter ::
  Ptr (Sk_shader) -- ^ C argument @"const sk_shader_t * shader"@
  -> Ptr (Sk_colorfilter) -- ^ C argument @"const sk_colorfilter_t * filter"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_shader_with_color_filter'
foreign import ccall "&sk_shader_with_color_filter" p'sk_shader_with_color_filter ::
  FunPtr (Ptr (Sk_shader) -> Ptr (Sk_colorfilter) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_shader_t *sk_shader_new_empty(void)
@
-}
foreign import ccall "sk_shader_new_empty" sk_shader_new_empty ::
  IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_shader_new_empty'
foreign import ccall "&sk_shader_new_empty" p'sk_shader_new_empty ::
  FunPtr (IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_shader_t *sk_shader_new_color(sk_color_t color)
@
-}
foreign import ccall "sk_shader_new_color" sk_shader_new_color ::
  Sk_color -- ^ C argument @"sk_color_t color"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_shader_new_color'
foreign import ccall "&sk_shader_new_color" p'sk_shader_new_color ::
  FunPtr (Sk_color -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_shader_t *sk_shader_new_color4f(const sk_color4f_t *color, const sk_colorspace_t *colorspace)
@
-}
foreign import ccall "sk_shader_new_color4f" sk_shader_new_color4f ::
  Ptr (Sk_color4f) -- ^ C argument @"const sk_color4f_t * color"@
  -> Ptr (Sk_colorspace) -- ^ C argument @"const sk_colorspace_t * colorspace"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_shader_new_color4f'
foreign import ccall "&sk_shader_new_color4f" p'sk_shader_new_color4f ::
  FunPtr (Ptr (Sk_color4f) -> Ptr (Sk_colorspace) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_shader_t *sk_shader_new_blend(sk_blendmode_t mode, const sk_shader_t *dst, const sk_shader_t *src)
@
-}
foreign import ccall "sk_shader_new_blend" sk_shader_new_blend ::
  Sk_blendmode -- ^ C argument @"sk_blendmode_t mode"@
  -> Ptr (Sk_shader) -- ^ C argument @"const sk_shader_t * dst"@
  -> Ptr (Sk_shader) -- ^ C argument @"const sk_shader_t * src"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_shader_new_blend'
foreign import ccall "&sk_shader_new_blend" p'sk_shader_new_blend ::
  FunPtr (Sk_blendmode -> Ptr (Sk_shader) -> Ptr (Sk_shader) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_shader_t *sk_shader_new_blender(sk_blender_t *blender, const sk_shader_t *dst, const sk_shader_t *src)
@
-}
foreign import ccall "sk_shader_new_blender" sk_shader_new_blender ::
  Ptr (Sk_blender) -- ^ C argument @"sk_blender_t * blender"@
  -> Ptr (Sk_shader) -- ^ C argument @"const sk_shader_t * dst"@
  -> Ptr (Sk_shader) -- ^ C argument @"const sk_shader_t * src"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_shader_new_blender'
foreign import ccall "&sk_shader_new_blender" p'sk_shader_new_blender ::
  FunPtr (Ptr (Sk_blender) -> Ptr (Sk_shader) -> Ptr (Sk_shader) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_shader_t *sk_shader_new_linear_gradient(const sk_point_t points[2], const sk_color_t colors[], const float colorPos[], int colorCount, sk_shader_tilemode_t tileMode, const sk_matrix_t *localMatrix)
@
-}
foreign import ccall "sk_shader_new_linear_gradient" sk_shader_new_linear_gradient ::
  Ptr (Sk_point) -- ^ C argument @"const sk_point_t [2] points"@
  -> Ptr (Sk_color) -- ^ C argument @"const sk_color_t [] colors"@
  -> Ptr (CFloat) -- ^ C argument @"const float [] colorPos"@
  -> CInt -- ^ C argument @"int colorCount"@
  -> Sk_shader_tilemode -- ^ C argument @"sk_shader_tilemode_t tileMode"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * localMatrix"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_shader_new_linear_gradient'
foreign import ccall "&sk_shader_new_linear_gradient" p'sk_shader_new_linear_gradient ::
  FunPtr (Ptr (Sk_point) -> Ptr (Sk_color) -> Ptr (CFloat) -> CInt -> Sk_shader_tilemode -> Ptr (Sk_matrix) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_shader_t *sk_shader_new_linear_gradient_color4f(const sk_point_t points[2], const sk_color4f_t *colors, const sk_colorspace_t *colorspace, const float colorPos[], int colorCount, sk_shader_tilemode_t tileMode, const sk_matrix_t *localMatrix)
@
-}
foreign import ccall "sk_shader_new_linear_gradient_color4f" sk_shader_new_linear_gradient_color4f ::
  Ptr (Sk_point) -- ^ C argument @"const sk_point_t [2] points"@
  -> Ptr (Sk_color4f) -- ^ C argument @"const sk_color4f_t * colors"@
  -> Ptr (Sk_colorspace) -- ^ C argument @"const sk_colorspace_t * colorspace"@
  -> Ptr (CFloat) -- ^ C argument @"const float [] colorPos"@
  -> CInt -- ^ C argument @"int colorCount"@
  -> Sk_shader_tilemode -- ^ C argument @"sk_shader_tilemode_t tileMode"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * localMatrix"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_shader_new_linear_gradient_color4f'
foreign import ccall "&sk_shader_new_linear_gradient_color4f" p'sk_shader_new_linear_gradient_color4f ::
  FunPtr (Ptr (Sk_point) -> Ptr (Sk_color4f) -> Ptr (Sk_colorspace) -> Ptr (CFloat) -> CInt -> Sk_shader_tilemode -> Ptr (Sk_matrix) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_shader_t *sk_shader_new_radial_gradient(const sk_point_t *center, float radius, const sk_color_t colors[], const float colorPos[], int colorCount, sk_shader_tilemode_t tileMode, const sk_matrix_t *localMatrix)
@
-}
foreign import ccall "sk_shader_new_radial_gradient" sk_shader_new_radial_gradient ::
  Ptr (Sk_point) -- ^ C argument @"const sk_point_t * center"@
  -> CFloat -- ^ C argument @"float radius"@
  -> Ptr (Sk_color) -- ^ C argument @"const sk_color_t [] colors"@
  -> Ptr (CFloat) -- ^ C argument @"const float [] colorPos"@
  -> CInt -- ^ C argument @"int colorCount"@
  -> Sk_shader_tilemode -- ^ C argument @"sk_shader_tilemode_t tileMode"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * localMatrix"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_shader_new_radial_gradient'
foreign import ccall "&sk_shader_new_radial_gradient" p'sk_shader_new_radial_gradient ::
  FunPtr (Ptr (Sk_point) -> CFloat -> Ptr (Sk_color) -> Ptr (CFloat) -> CInt -> Sk_shader_tilemode -> Ptr (Sk_matrix) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_shader_t *sk_shader_new_radial_gradient_color4f(const sk_point_t *center, float radius, const sk_color4f_t *colors, const sk_colorspace_t *colorspace, const float colorPos[], int colorCount, sk_shader_tilemode_t tileMode, const sk_matrix_t *localMatrix)
@
-}
foreign import ccall "sk_shader_new_radial_gradient_color4f" sk_shader_new_radial_gradient_color4f ::
  Ptr (Sk_point) -- ^ C argument @"const sk_point_t * center"@
  -> CFloat -- ^ C argument @"float radius"@
  -> Ptr (Sk_color4f) -- ^ C argument @"const sk_color4f_t * colors"@
  -> Ptr (Sk_colorspace) -- ^ C argument @"const sk_colorspace_t * colorspace"@
  -> Ptr (CFloat) -- ^ C argument @"const float [] colorPos"@
  -> CInt -- ^ C argument @"int colorCount"@
  -> Sk_shader_tilemode -- ^ C argument @"sk_shader_tilemode_t tileMode"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * localMatrix"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_shader_new_radial_gradient_color4f'
foreign import ccall "&sk_shader_new_radial_gradient_color4f" p'sk_shader_new_radial_gradient_color4f ::
  FunPtr (Ptr (Sk_point) -> CFloat -> Ptr (Sk_color4f) -> Ptr (Sk_colorspace) -> Ptr (CFloat) -> CInt -> Sk_shader_tilemode -> Ptr (Sk_matrix) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_shader_t *sk_shader_new_sweep_gradient(const sk_point_t *center, const sk_color_t colors[], const float colorPos[], int colorCount, sk_shader_tilemode_t tileMode, float startAngle, float endAngle, const sk_matrix_t *localMatrix)
@
-}
foreign import ccall "sk_shader_new_sweep_gradient" sk_shader_new_sweep_gradient ::
  Ptr (Sk_point) -- ^ C argument @"const sk_point_t * center"@
  -> Ptr (Sk_color) -- ^ C argument @"const sk_color_t [] colors"@
  -> Ptr (CFloat) -- ^ C argument @"const float [] colorPos"@
  -> CInt -- ^ C argument @"int colorCount"@
  -> Sk_shader_tilemode -- ^ C argument @"sk_shader_tilemode_t tileMode"@
  -> CFloat -- ^ C argument @"float startAngle"@
  -> CFloat -- ^ C argument @"float endAngle"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * localMatrix"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_shader_new_sweep_gradient'
foreign import ccall "&sk_shader_new_sweep_gradient" p'sk_shader_new_sweep_gradient ::
  FunPtr (Ptr (Sk_point) -> Ptr (Sk_color) -> Ptr (CFloat) -> CInt -> Sk_shader_tilemode -> CFloat -> CFloat -> Ptr (Sk_matrix) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_shader_t *sk_shader_new_sweep_gradient_color4f(const sk_point_t *center, const sk_color4f_t *colors, const sk_colorspace_t *colorspace, const float colorPos[], int colorCount, sk_shader_tilemode_t tileMode, float startAngle, float endAngle, const sk_matrix_t *localMatrix)
@
-}
foreign import ccall "sk_shader_new_sweep_gradient_color4f" sk_shader_new_sweep_gradient_color4f ::
  Ptr (Sk_point) -- ^ C argument @"const sk_point_t * center"@
  -> Ptr (Sk_color4f) -- ^ C argument @"const sk_color4f_t * colors"@
  -> Ptr (Sk_colorspace) -- ^ C argument @"const sk_colorspace_t * colorspace"@
  -> Ptr (CFloat) -- ^ C argument @"const float [] colorPos"@
  -> CInt -- ^ C argument @"int colorCount"@
  -> Sk_shader_tilemode -- ^ C argument @"sk_shader_tilemode_t tileMode"@
  -> CFloat -- ^ C argument @"float startAngle"@
  -> CFloat -- ^ C argument @"float endAngle"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * localMatrix"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_shader_new_sweep_gradient_color4f'
foreign import ccall "&sk_shader_new_sweep_gradient_color4f" p'sk_shader_new_sweep_gradient_color4f ::
  FunPtr (Ptr (Sk_point) -> Ptr (Sk_color4f) -> Ptr (Sk_colorspace) -> Ptr (CFloat) -> CInt -> Sk_shader_tilemode -> CFloat -> CFloat -> Ptr (Sk_matrix) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_shader_t *sk_shader_new_two_point_conical_gradient(const sk_point_t *start, float startRadius, const sk_point_t *end, float endRadius, const sk_color_t colors[], const float colorPos[], int colorCount, sk_shader_tilemode_t tileMode, const sk_matrix_t *localMatrix)
@
-}
foreign import ccall "sk_shader_new_two_point_conical_gradient" sk_shader_new_two_point_conical_gradient ::
  Ptr (Sk_point) -- ^ C argument @"const sk_point_t * start"@
  -> CFloat -- ^ C argument @"float startRadius"@
  -> Ptr (Sk_point) -- ^ C argument @"const sk_point_t * end"@
  -> CFloat -- ^ C argument @"float endRadius"@
  -> Ptr (Sk_color) -- ^ C argument @"const sk_color_t [] colors"@
  -> Ptr (CFloat) -- ^ C argument @"const float [] colorPos"@
  -> CInt -- ^ C argument @"int colorCount"@
  -> Sk_shader_tilemode -- ^ C argument @"sk_shader_tilemode_t tileMode"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * localMatrix"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_shader_new_two_point_conical_gradient'
foreign import ccall "&sk_shader_new_two_point_conical_gradient" p'sk_shader_new_two_point_conical_gradient ::
  FunPtr (Ptr (Sk_point) -> CFloat -> Ptr (Sk_point) -> CFloat -> Ptr (Sk_color) -> Ptr (CFloat) -> CInt -> Sk_shader_tilemode -> Ptr (Sk_matrix) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_shader_t *sk_shader_new_two_point_conical_gradient_color4f(const sk_point_t *start, float startRadius, const sk_point_t *end, float endRadius, const sk_color4f_t *colors, const sk_colorspace_t *colorspace, const float colorPos[], int colorCount, sk_shader_tilemode_t tileMode, const sk_matrix_t *localMatrix)
@
-}
foreign import ccall "sk_shader_new_two_point_conical_gradient_color4f" sk_shader_new_two_point_conical_gradient_color4f ::
  Ptr (Sk_point) -- ^ C argument @"const sk_point_t * start"@
  -> CFloat -- ^ C argument @"float startRadius"@
  -> Ptr (Sk_point) -- ^ C argument @"const sk_point_t * end"@
  -> CFloat -- ^ C argument @"float endRadius"@
  -> Ptr (Sk_color4f) -- ^ C argument @"const sk_color4f_t * colors"@
  -> Ptr (Sk_colorspace) -- ^ C argument @"const sk_colorspace_t * colorspace"@
  -> Ptr (CFloat) -- ^ C argument @"const float [] colorPos"@
  -> CInt -- ^ C argument @"int colorCount"@
  -> Sk_shader_tilemode -- ^ C argument @"sk_shader_tilemode_t tileMode"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * localMatrix"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_shader_new_two_point_conical_gradient_color4f'
foreign import ccall "&sk_shader_new_two_point_conical_gradient_color4f" p'sk_shader_new_two_point_conical_gradient_color4f ::
  FunPtr (Ptr (Sk_point) -> CFloat -> Ptr (Sk_point) -> CFloat -> Ptr (Sk_color4f) -> Ptr (Sk_colorspace) -> Ptr (CFloat) -> CInt -> Sk_shader_tilemode -> Ptr (Sk_matrix) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_shader_t *sk_shader_new_perlin_noise_fractal_noise(float baseFrequencyX, float baseFrequencyY, int numOctaves, float seed, const sk_isize_t *tileSize)
@
-}
foreign import ccall "sk_shader_new_perlin_noise_fractal_noise" sk_shader_new_perlin_noise_fractal_noise ::
  CFloat -- ^ C argument @"float baseFrequencyX"@
  -> CFloat -- ^ C argument @"float baseFrequencyY"@
  -> CInt -- ^ C argument @"int numOctaves"@
  -> CFloat -- ^ C argument @"float seed"@
  -> Ptr (Sk_isize) -- ^ C argument @"const sk_isize_t * tileSize"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_shader_new_perlin_noise_fractal_noise'
foreign import ccall "&sk_shader_new_perlin_noise_fractal_noise" p'sk_shader_new_perlin_noise_fractal_noise ::
  FunPtr (CFloat -> CFloat -> CInt -> CFloat -> Ptr (Sk_isize) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_shader_t *sk_shader_new_perlin_noise_turbulence(float baseFrequencyX, float baseFrequencyY, int numOctaves, float seed, const sk_isize_t *tileSize)
@
-}
foreign import ccall "sk_shader_new_perlin_noise_turbulence" sk_shader_new_perlin_noise_turbulence ::
  CFloat -- ^ C argument @"float baseFrequencyX"@
  -> CFloat -- ^ C argument @"float baseFrequencyY"@
  -> CInt -- ^ C argument @"int numOctaves"@
  -> CFloat -- ^ C argument @"float seed"@
  -> Ptr (Sk_isize) -- ^ C argument @"const sk_isize_t * tileSize"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_shader_new_perlin_noise_turbulence'
foreign import ccall "&sk_shader_new_perlin_noise_turbulence" p'sk_shader_new_perlin_noise_turbulence ::
  FunPtr (CFloat -> CFloat -> CInt -> CFloat -> Ptr (Sk_isize) -> IO (Ptr (Sk_shader)))
