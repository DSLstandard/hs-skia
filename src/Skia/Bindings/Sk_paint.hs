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
module Skia.Bindings.Sk_paint where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
sk_paint_t *sk_paint_new(void)
@
-}
foreign import ccall "sk_paint_new" sk_paint_new ::
  IO (Ptr (Sk_paint)) -- ^ C return type: @"sk_paint_t *"@

-- | Function pointer to 'sk_paint_new'
foreign import ccall "&sk_paint_new" p'sk_paint_new ::
  FunPtr (IO (Ptr (Sk_paint)))

{- | C function signature:

@
sk_paint_t *sk_paint_clone(sk_paint_t *)
@
-}
foreign import ccall "sk_paint_clone" sk_paint_clone ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> IO (Ptr (Sk_paint)) -- ^ C return type: @"sk_paint_t *"@

-- | Function pointer to 'sk_paint_clone'
foreign import ccall "&sk_paint_clone" p'sk_paint_clone ::
  FunPtr (Ptr (Sk_paint) -> IO (Ptr (Sk_paint)))

{- | C function signature:

@
void sk_paint_delete(sk_paint_t *)
@
-}
foreign import ccall "sk_paint_delete" sk_paint_delete ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_delete'
foreign import ccall "&sk_paint_delete" p'sk_paint_delete ::
  FunPtr (Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_paint_reset(sk_paint_t *)
@
-}
foreign import ccall "sk_paint_reset" sk_paint_reset ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_reset'
foreign import ccall "&sk_paint_reset" p'sk_paint_reset ::
  FunPtr (Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
_Bool sk_paint_is_antialias(const sk_paint_t *)
@
-}
foreign import ccall "sk_paint_is_antialias" sk_paint_is_antialias ::
  Ptr (Sk_paint) -- ^ C argument type: @"const sk_paint_t *"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_paint_is_antialias'
foreign import ccall "&sk_paint_is_antialias" p'sk_paint_is_antialias ::
  FunPtr (Ptr (Sk_paint) -> IO (CBool))

{- | C function signature:

@
void sk_paint_set_antialias(sk_paint_t *, _Bool)
@
-}
foreign import ccall "sk_paint_set_antialias" sk_paint_set_antialias ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> CBool -- ^ C argument type: @"_Bool"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_set_antialias'
foreign import ccall "&sk_paint_set_antialias" p'sk_paint_set_antialias ::
  FunPtr (Ptr (Sk_paint) -> CBool -> IO (()))

{- | C function signature:

@
sk_color_t sk_paint_get_color(const sk_paint_t *)
@
-}
foreign import ccall "sk_paint_get_color" sk_paint_get_color ::
  Ptr (Sk_paint) -- ^ C argument type: @"const sk_paint_t *"@
  -> IO (Sk_color) -- ^ C return type: @"sk_color_t"@

-- | Function pointer to 'sk_paint_get_color'
foreign import ccall "&sk_paint_get_color" p'sk_paint_get_color ::
  FunPtr (Ptr (Sk_paint) -> IO (Sk_color))

{- | C function signature:

@
void sk_paint_get_color4f(const sk_paint_t *paint, sk_color4f_t *color)
@
-}
foreign import ccall "sk_paint_get_color4f" sk_paint_get_color4f ::
  Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * paint"@
  -> Ptr (Sk_color4f) -- ^ C argument @"sk_color4f_t * color"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_get_color4f'
foreign import ccall "&sk_paint_get_color4f" p'sk_paint_get_color4f ::
  FunPtr (Ptr (Sk_paint) -> Ptr (Sk_color4f) -> IO (()))

{- | C function signature:

@
void sk_paint_set_color(sk_paint_t *, sk_color_t)
@
-}
foreign import ccall "sk_paint_set_color" sk_paint_set_color ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> Sk_color -- ^ C argument type: @"sk_color_t"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_set_color'
foreign import ccall "&sk_paint_set_color" p'sk_paint_set_color ::
  FunPtr (Ptr (Sk_paint) -> Sk_color -> IO (()))

{- | C function signature:

@
void sk_paint_set_color4f(sk_paint_t *paint, sk_color4f_t *color, sk_colorspace_t *colorspace)
@
-}
foreign import ccall "sk_paint_set_color4f" sk_paint_set_color4f ::
  Ptr (Sk_paint) -- ^ C argument @"sk_paint_t * paint"@
  -> Ptr (Sk_color4f) -- ^ C argument @"sk_color4f_t * color"@
  -> Ptr (Sk_colorspace) -- ^ C argument @"sk_colorspace_t * colorspace"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_set_color4f'
foreign import ccall "&sk_paint_set_color4f" p'sk_paint_set_color4f ::
  FunPtr (Ptr (Sk_paint) -> Ptr (Sk_color4f) -> Ptr (Sk_colorspace) -> IO (()))

{- | C function signature:

@
sk_paint_style_t sk_paint_get_style(const sk_paint_t *)
@
-}
foreign import ccall "sk_paint_get_style" sk_paint_get_style ::
  Ptr (Sk_paint) -- ^ C argument type: @"const sk_paint_t *"@
  -> IO (Sk_paint_style) -- ^ C return type: @"sk_paint_style_t"@

-- | Function pointer to 'sk_paint_get_style'
foreign import ccall "&sk_paint_get_style" p'sk_paint_get_style ::
  FunPtr (Ptr (Sk_paint) -> IO (Sk_paint_style))

{- | C function signature:

@
void sk_paint_set_style(sk_paint_t *, sk_paint_style_t)
@
-}
foreign import ccall "sk_paint_set_style" sk_paint_set_style ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> Sk_paint_style -- ^ C argument type: @"sk_paint_style_t"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_set_style'
foreign import ccall "&sk_paint_set_style" p'sk_paint_set_style ::
  FunPtr (Ptr (Sk_paint) -> Sk_paint_style -> IO (()))

{- | C function signature:

@
float sk_paint_get_stroke_width(const sk_paint_t *)
@
-}
foreign import ccall "sk_paint_get_stroke_width" sk_paint_get_stroke_width ::
  Ptr (Sk_paint) -- ^ C argument type: @"const sk_paint_t *"@
  -> IO (CFloat) -- ^ C return type: @"float"@

-- | Function pointer to 'sk_paint_get_stroke_width'
foreign import ccall "&sk_paint_get_stroke_width" p'sk_paint_get_stroke_width ::
  FunPtr (Ptr (Sk_paint) -> IO (CFloat))

{- | C function signature:

@
void sk_paint_set_stroke_width(sk_paint_t *, float width)
@
-}
foreign import ccall "sk_paint_set_stroke_width" sk_paint_set_stroke_width ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> CFloat -- ^ C argument @"float width"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_set_stroke_width'
foreign import ccall "&sk_paint_set_stroke_width" p'sk_paint_set_stroke_width ::
  FunPtr (Ptr (Sk_paint) -> CFloat -> IO (()))

{- | C function signature:

@
float sk_paint_get_stroke_miter(const sk_paint_t *)
@
-}
foreign import ccall "sk_paint_get_stroke_miter" sk_paint_get_stroke_miter ::
  Ptr (Sk_paint) -- ^ C argument type: @"const sk_paint_t *"@
  -> IO (CFloat) -- ^ C return type: @"float"@

-- | Function pointer to 'sk_paint_get_stroke_miter'
foreign import ccall "&sk_paint_get_stroke_miter" p'sk_paint_get_stroke_miter ::
  FunPtr (Ptr (Sk_paint) -> IO (CFloat))

{- | C function signature:

@
void sk_paint_set_stroke_miter(sk_paint_t *, float miter)
@
-}
foreign import ccall "sk_paint_set_stroke_miter" sk_paint_set_stroke_miter ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> CFloat -- ^ C argument @"float miter"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_set_stroke_miter'
foreign import ccall "&sk_paint_set_stroke_miter" p'sk_paint_set_stroke_miter ::
  FunPtr (Ptr (Sk_paint) -> CFloat -> IO (()))

{- | C function signature:

@
sk_stroke_cap_t sk_paint_get_stroke_cap(const sk_paint_t *)
@
-}
foreign import ccall "sk_paint_get_stroke_cap" sk_paint_get_stroke_cap ::
  Ptr (Sk_paint) -- ^ C argument type: @"const sk_paint_t *"@
  -> IO (Sk_stroke_cap) -- ^ C return type: @"sk_stroke_cap_t"@

-- | Function pointer to 'sk_paint_get_stroke_cap'
foreign import ccall "&sk_paint_get_stroke_cap" p'sk_paint_get_stroke_cap ::
  FunPtr (Ptr (Sk_paint) -> IO (Sk_stroke_cap))

{- | C function signature:

@
void sk_paint_set_stroke_cap(sk_paint_t *, sk_stroke_cap_t)
@
-}
foreign import ccall "sk_paint_set_stroke_cap" sk_paint_set_stroke_cap ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> Sk_stroke_cap -- ^ C argument type: @"sk_stroke_cap_t"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_set_stroke_cap'
foreign import ccall "&sk_paint_set_stroke_cap" p'sk_paint_set_stroke_cap ::
  FunPtr (Ptr (Sk_paint) -> Sk_stroke_cap -> IO (()))

{- | C function signature:

@
sk_stroke_join_t sk_paint_get_stroke_join(const sk_paint_t *)
@
-}
foreign import ccall "sk_paint_get_stroke_join" sk_paint_get_stroke_join ::
  Ptr (Sk_paint) -- ^ C argument type: @"const sk_paint_t *"@
  -> IO (Sk_stroke_join) -- ^ C return type: @"sk_stroke_join_t"@

-- | Function pointer to 'sk_paint_get_stroke_join'
foreign import ccall "&sk_paint_get_stroke_join" p'sk_paint_get_stroke_join ::
  FunPtr (Ptr (Sk_paint) -> IO (Sk_stroke_join))

{- | C function signature:

@
void sk_paint_set_stroke_join(sk_paint_t *, sk_stroke_join_t)
@
-}
foreign import ccall "sk_paint_set_stroke_join" sk_paint_set_stroke_join ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> Sk_stroke_join -- ^ C argument type: @"sk_stroke_join_t"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_set_stroke_join'
foreign import ccall "&sk_paint_set_stroke_join" p'sk_paint_set_stroke_join ::
  FunPtr (Ptr (Sk_paint) -> Sk_stroke_join -> IO (()))

{- | C function signature:

@
void sk_paint_set_shader(sk_paint_t *, sk_shader_t *)
@
-}
foreign import ccall "sk_paint_set_shader" sk_paint_set_shader ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> Ptr (Sk_shader) -- ^ C argument type: @"sk_shader_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_set_shader'
foreign import ccall "&sk_paint_set_shader" p'sk_paint_set_shader ::
  FunPtr (Ptr (Sk_paint) -> Ptr (Sk_shader) -> IO (()))

{- | C function signature:

@
void sk_paint_set_maskfilter(sk_paint_t *, sk_maskfilter_t *)
@
-}
foreign import ccall "sk_paint_set_maskfilter" sk_paint_set_maskfilter ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> Ptr (Sk_maskfilter) -- ^ C argument type: @"sk_maskfilter_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_set_maskfilter'
foreign import ccall "&sk_paint_set_maskfilter" p'sk_paint_set_maskfilter ::
  FunPtr (Ptr (Sk_paint) -> Ptr (Sk_maskfilter) -> IO (()))

{- | C function signature:

@
void sk_paint_set_blendmode(sk_paint_t *, sk_blendmode_t)
@
-}
foreign import ccall "sk_paint_set_blendmode" sk_paint_set_blendmode ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> Sk_blendmode -- ^ C argument type: @"sk_blendmode_t"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_set_blendmode'
foreign import ccall "&sk_paint_set_blendmode" p'sk_paint_set_blendmode ::
  FunPtr (Ptr (Sk_paint) -> Sk_blendmode -> IO (()))

{- | C function signature:

@
void sk_paint_set_blender(sk_paint_t *paint, sk_blender_t *blender)
@
-}
foreign import ccall "sk_paint_set_blender" sk_paint_set_blender ::
  Ptr (Sk_paint) -- ^ C argument @"sk_paint_t * paint"@
  -> Ptr (Sk_blender) -- ^ C argument @"sk_blender_t * blender"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_set_blender'
foreign import ccall "&sk_paint_set_blender" p'sk_paint_set_blender ::
  FunPtr (Ptr (Sk_paint) -> Ptr (Sk_blender) -> IO (()))

{- | C function signature:

@
_Bool sk_paint_is_dither(const sk_paint_t *)
@
-}
foreign import ccall "sk_paint_is_dither" sk_paint_is_dither ::
  Ptr (Sk_paint) -- ^ C argument type: @"const sk_paint_t *"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_paint_is_dither'
foreign import ccall "&sk_paint_is_dither" p'sk_paint_is_dither ::
  FunPtr (Ptr (Sk_paint) -> IO (CBool))

{- | C function signature:

@
void sk_paint_set_dither(sk_paint_t *, _Bool)
@
-}
foreign import ccall "sk_paint_set_dither" sk_paint_set_dither ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> CBool -- ^ C argument type: @"_Bool"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_set_dither'
foreign import ccall "&sk_paint_set_dither" p'sk_paint_set_dither ::
  FunPtr (Ptr (Sk_paint) -> CBool -> IO (()))

{- | C function signature:

@
sk_shader_t *sk_paint_get_shader(sk_paint_t *)
@
-}
foreign import ccall "sk_paint_get_shader" sk_paint_get_shader ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_paint_get_shader'
foreign import ccall "&sk_paint_get_shader" p'sk_paint_get_shader ::
  FunPtr (Ptr (Sk_paint) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_maskfilter_t *sk_paint_get_maskfilter(sk_paint_t *)
@
-}
foreign import ccall "sk_paint_get_maskfilter" sk_paint_get_maskfilter ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> IO (Ptr (Sk_maskfilter)) -- ^ C return type: @"sk_maskfilter_t *"@

-- | Function pointer to 'sk_paint_get_maskfilter'
foreign import ccall "&sk_paint_get_maskfilter" p'sk_paint_get_maskfilter ::
  FunPtr (Ptr (Sk_paint) -> IO (Ptr (Sk_maskfilter)))

{- | C function signature:

@
void sk_paint_set_colorfilter(sk_paint_t *, sk_colorfilter_t *)
@
-}
foreign import ccall "sk_paint_set_colorfilter" sk_paint_set_colorfilter ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> Ptr (Sk_colorfilter) -- ^ C argument type: @"sk_colorfilter_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_set_colorfilter'
foreign import ccall "&sk_paint_set_colorfilter" p'sk_paint_set_colorfilter ::
  FunPtr (Ptr (Sk_paint) -> Ptr (Sk_colorfilter) -> IO (()))

{- | C function signature:

@
sk_colorfilter_t *sk_paint_get_colorfilter(sk_paint_t *)
@
-}
foreign import ccall "sk_paint_get_colorfilter" sk_paint_get_colorfilter ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> IO (Ptr (Sk_colorfilter)) -- ^ C return type: @"sk_colorfilter_t *"@

-- | Function pointer to 'sk_paint_get_colorfilter'
foreign import ccall "&sk_paint_get_colorfilter" p'sk_paint_get_colorfilter ::
  FunPtr (Ptr (Sk_paint) -> IO (Ptr (Sk_colorfilter)))

{- | C function signature:

@
void sk_paint_set_imagefilter(sk_paint_t *, sk_imagefilter_t *)
@
-}
foreign import ccall "sk_paint_set_imagefilter" sk_paint_set_imagefilter ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> Ptr (Sk_imagefilter) -- ^ C argument type: @"sk_imagefilter_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_set_imagefilter'
foreign import ccall "&sk_paint_set_imagefilter" p'sk_paint_set_imagefilter ::
  FunPtr (Ptr (Sk_paint) -> Ptr (Sk_imagefilter) -> IO (()))

{- | C function signature:

@
sk_imagefilter_t *sk_paint_get_imagefilter(sk_paint_t *)
@
-}
foreign import ccall "sk_paint_get_imagefilter" sk_paint_get_imagefilter ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> IO (Ptr (Sk_imagefilter)) -- ^ C return type: @"sk_imagefilter_t *"@

-- | Function pointer to 'sk_paint_get_imagefilter'
foreign import ccall "&sk_paint_get_imagefilter" p'sk_paint_get_imagefilter ::
  FunPtr (Ptr (Sk_paint) -> IO (Ptr (Sk_imagefilter)))

{- | C function signature:

@
sk_blendmode_t sk_paint_get_blendmode(sk_paint_t *)
@
-}
foreign import ccall "sk_paint_get_blendmode" sk_paint_get_blendmode ::
  Ptr (Sk_paint) -- ^ C argument type: @"sk_paint_t *"@
  -> IO (Sk_blendmode) -- ^ C return type: @"sk_blendmode_t"@

-- | Function pointer to 'sk_paint_get_blendmode'
foreign import ccall "&sk_paint_get_blendmode" p'sk_paint_get_blendmode ::
  FunPtr (Ptr (Sk_paint) -> IO (Sk_blendmode))

{- | C function signature:

@
sk_blender_t *sk_paint_get_blender(sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_paint_get_blender" sk_paint_get_blender ::
  Ptr (Sk_paint) -- ^ C argument @"sk_paint_t * cpaint"@
  -> IO (Ptr (Sk_blender)) -- ^ C return type: @"sk_blender_t *"@

-- | Function pointer to 'sk_paint_get_blender'
foreign import ccall "&sk_paint_get_blender" p'sk_paint_get_blender ::
  FunPtr (Ptr (Sk_paint) -> IO (Ptr (Sk_blender)))

{- | C function signature:

@
sk_path_effect_t *sk_paint_get_path_effect(sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_paint_get_path_effect" sk_paint_get_path_effect ::
  Ptr (Sk_paint) -- ^ C argument @"sk_paint_t * cpaint"@
  -> IO (Ptr (Sk_path_effect)) -- ^ C return type: @"sk_path_effect_t *"@

-- | Function pointer to 'sk_paint_get_path_effect'
foreign import ccall "&sk_paint_get_path_effect" p'sk_paint_get_path_effect ::
  FunPtr (Ptr (Sk_paint) -> IO (Ptr (Sk_path_effect)))

{- | C function signature:

@
void sk_paint_set_path_effect(sk_paint_t *cpaint, sk_path_effect_t *effect)
@
-}
foreign import ccall "sk_paint_set_path_effect" sk_paint_set_path_effect ::
  Ptr (Sk_paint) -- ^ C argument @"sk_paint_t * cpaint"@
  -> Ptr (Sk_path_effect) -- ^ C argument @"sk_path_effect_t * effect"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_paint_set_path_effect'
foreign import ccall "&sk_paint_set_path_effect" p'sk_paint_set_path_effect ::
  FunPtr (Ptr (Sk_paint) -> Ptr (Sk_path_effect) -> IO (()))

{- | C function signature:

@
_Bool sk_paint_get_fill_path(const sk_paint_t *cpaint, const sk_path_t *src, sk_path_t *dst, const sk_rect_t *cullRect, const sk_matrix_t *cmatrix)
@
-}
foreign import ccall "sk_paint_get_fill_path" sk_paint_get_fill_path ::
  Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * cpaint"@
  -> Ptr (Sk_path) -- ^ C argument @"const sk_path_t * src"@
  -> Ptr (Sk_path) -- ^ C argument @"sk_path_t * dst"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cullRect"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * cmatrix"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_paint_get_fill_path'
foreign import ccall "&sk_paint_get_fill_path" p'sk_paint_get_fill_path ::
  FunPtr (Ptr (Sk_paint) -> Ptr (Sk_path) -> Ptr (Sk_path) -> Ptr (Sk_rect) -> Ptr (Sk_matrix) -> IO (CBool))
