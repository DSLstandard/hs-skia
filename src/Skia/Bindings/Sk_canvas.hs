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
module Skia.Bindings.Sk_canvas where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void sk_canvas_destroy(sk_canvas_t *ccanvas)
@
-}
foreign import ccall "sk_canvas_destroy" sk_canvas_destroy ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_destroy'
foreign import ccall "&sk_canvas_destroy" p'sk_canvas_destroy ::
  FunPtr (Ptr (Sk_canvas) -> IO (()))

{- | C function signature:

@
void sk_canvas_clear(sk_canvas_t *ccanvas, sk_color_t color)
@
-}
foreign import ccall "sk_canvas_clear" sk_canvas_clear ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Sk_color -- ^ C argument @"sk_color_t color"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_clear'
foreign import ccall "&sk_canvas_clear" p'sk_canvas_clear ::
  FunPtr (Ptr (Sk_canvas) -> Sk_color -> IO (()))

{- | C function signature:

@
void sk_canvas_clear_color4f(sk_canvas_t *ccanvas, sk_color4f_t *color)
@
-}
foreign import ccall "sk_canvas_clear_color4f" sk_canvas_clear_color4f ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_color4f) -- ^ C argument @"sk_color4f_t * color"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_clear_color4f'
foreign import ccall "&sk_canvas_clear_color4f" p'sk_canvas_clear_color4f ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_color4f) -> IO (()))

{- | C function signature:

@
void sk_canvas_discard(sk_canvas_t *ccanvas)
@
-}
foreign import ccall "sk_canvas_discard" sk_canvas_discard ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_discard'
foreign import ccall "&sk_canvas_discard" p'sk_canvas_discard ::
  FunPtr (Ptr (Sk_canvas) -> IO (()))

{- | C function signature:

@
int sk_canvas_get_save_count(sk_canvas_t *ccanvas)
@
-}
foreign import ccall "sk_canvas_get_save_count" sk_canvas_get_save_count ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_canvas_get_save_count'
foreign import ccall "&sk_canvas_get_save_count" p'sk_canvas_get_save_count ::
  FunPtr (Ptr (Sk_canvas) -> IO (CInt))

{- | C function signature:

@
void sk_canvas_restore_to_count(sk_canvas_t *ccanvas, int saveCount)
@
-}
foreign import ccall "sk_canvas_restore_to_count" sk_canvas_restore_to_count ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> CInt -- ^ C argument @"int saveCount"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_restore_to_count'
foreign import ccall "&sk_canvas_restore_to_count" p'sk_canvas_restore_to_count ::
  FunPtr (Ptr (Sk_canvas) -> CInt -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_color(sk_canvas_t *ccanvas, sk_color_t color, sk_blendmode_t cmode)
@
-}
foreign import ccall "sk_canvas_draw_color" sk_canvas_draw_color ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Sk_color -- ^ C argument @"sk_color_t color"@
  -> Sk_blendmode -- ^ C argument @"sk_blendmode_t cmode"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_color'
foreign import ccall "&sk_canvas_draw_color" p'sk_canvas_draw_color ::
  FunPtr (Ptr (Sk_canvas) -> Sk_color -> Sk_blendmode -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_color4f(sk_canvas_t *ccanvas, sk_color4f_t *color, sk_blendmode_t cmode)
@
-}
foreign import ccall "sk_canvas_draw_color4f" sk_canvas_draw_color4f ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_color4f) -- ^ C argument @"sk_color4f_t * color"@
  -> Sk_blendmode -- ^ C argument @"sk_blendmode_t cmode"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_color4f'
foreign import ccall "&sk_canvas_draw_color4f" p'sk_canvas_draw_color4f ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_color4f) -> Sk_blendmode -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_points(sk_canvas_t *ccanvas, sk_point_mode_t pointMode, size_t count, const sk_point_t points[], const sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_canvas_draw_points" sk_canvas_draw_points ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Sk_point_mode -- ^ C argument @"sk_point_mode_t pointMode"@
  -> CSize -- ^ C argument @"size_t count"@
  -> Ptr (Sk_point) -- ^ C argument @"const sk_point_t [] points"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * cpaint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_points'
foreign import ccall "&sk_canvas_draw_points" p'sk_canvas_draw_points ::
  FunPtr (Ptr (Sk_canvas) -> Sk_point_mode -> CSize -> Ptr (Sk_point) -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_point(sk_canvas_t *ccanvas, float x, float y, const sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_canvas_draw_point" sk_canvas_draw_point ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> CFloat -- ^ C argument @"float x"@
  -> CFloat -- ^ C argument @"float y"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * cpaint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_point'
foreign import ccall "&sk_canvas_draw_point" p'sk_canvas_draw_point ::
  FunPtr (Ptr (Sk_canvas) -> CFloat -> CFloat -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_line(sk_canvas_t *ccanvas, float x0, float y0, float x1, float y1, sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_canvas_draw_line" sk_canvas_draw_line ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> CFloat -- ^ C argument @"float x0"@
  -> CFloat -- ^ C argument @"float y0"@
  -> CFloat -- ^ C argument @"float x1"@
  -> CFloat -- ^ C argument @"float y1"@
  -> Ptr (Sk_paint) -- ^ C argument @"sk_paint_t * cpaint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_line'
foreign import ccall "&sk_canvas_draw_line" p'sk_canvas_draw_line ::
  FunPtr (Ptr (Sk_canvas) -> CFloat -> CFloat -> CFloat -> CFloat -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_simple_text(sk_canvas_t *ccanvas, const void *text, size_t byte_length, sk_text_encoding_t encoding, float x, float y, const sk_font_t *cfont, const sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_canvas_draw_simple_text" sk_canvas_draw_simple_text ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (()) -- ^ C argument @"const void * text"@
  -> CSize -- ^ C argument @"size_t byte_length"@
  -> Sk_text_encoding -- ^ C argument @"sk_text_encoding_t encoding"@
  -> CFloat -- ^ C argument @"float x"@
  -> CFloat -- ^ C argument @"float y"@
  -> Ptr (Sk_font) -- ^ C argument @"const sk_font_t * cfont"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * cpaint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_simple_text'
foreign import ccall "&sk_canvas_draw_simple_text" p'sk_canvas_draw_simple_text ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (()) -> CSize -> Sk_text_encoding -> CFloat -> CFloat -> Ptr (Sk_font) -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_text_blob(sk_canvas_t *ccanvas, sk_textblob_t *text, float x, float y, const sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_canvas_draw_text_blob" sk_canvas_draw_text_blob ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_textblob) -- ^ C argument @"sk_textblob_t * text"@
  -> CFloat -- ^ C argument @"float x"@
  -> CFloat -- ^ C argument @"float y"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * cpaint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_text_blob'
foreign import ccall "&sk_canvas_draw_text_blob" p'sk_canvas_draw_text_blob ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_textblob) -> CFloat -> CFloat -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_reset_matrix(sk_canvas_t *ccanvas)
@
-}
foreign import ccall "sk_canvas_reset_matrix" sk_canvas_reset_matrix ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_reset_matrix'
foreign import ccall "&sk_canvas_reset_matrix" p'sk_canvas_reset_matrix ::
  FunPtr (Ptr (Sk_canvas) -> IO (()))

{- | C function signature:

@
void sk_canvas_set_matrix(sk_canvas_t *ccanvas, const sk_matrix44_t *cmatrix)
@
-}
foreign import ccall "sk_canvas_set_matrix" sk_canvas_set_matrix ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_matrix44) -- ^ C argument @"const sk_matrix44_t * cmatrix"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_set_matrix'
foreign import ccall "&sk_canvas_set_matrix" p'sk_canvas_set_matrix ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_matrix44) -> IO (()))

{- | C function signature:

@
void sk_canvas_get_matrix(sk_canvas_t *ccanvas, sk_matrix44_t *cmatrix)
@
-}
foreign import ccall "sk_canvas_get_matrix" sk_canvas_get_matrix ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_matrix44) -- ^ C argument @"sk_matrix44_t * cmatrix"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_get_matrix'
foreign import ccall "&sk_canvas_get_matrix" p'sk_canvas_get_matrix ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_matrix44) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_round_rect(sk_canvas_t *ccanvas, const sk_rect_t *crect, float rx, float ry, const sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_canvas_draw_round_rect" sk_canvas_draw_round_rect ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * crect"@
  -> CFloat -- ^ C argument @"float rx"@
  -> CFloat -- ^ C argument @"float ry"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * cpaint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_round_rect'
foreign import ccall "&sk_canvas_draw_round_rect" p'sk_canvas_draw_round_rect ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_rect) -> CFloat -> CFloat -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_clip_rect_with_operation(sk_canvas_t *ccanvas, const sk_rect_t *crect, sk_clipop_t op, _Bool doAA)
@
-}
foreign import ccall "sk_canvas_clip_rect_with_operation" sk_canvas_clip_rect_with_operation ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * crect"@
  -> Sk_clipop -- ^ C argument @"sk_clipop_t op"@
  -> CBool -- ^ C argument @"_Bool doAA"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_clip_rect_with_operation'
foreign import ccall "&sk_canvas_clip_rect_with_operation" p'sk_canvas_clip_rect_with_operation ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_rect) -> Sk_clipop -> CBool -> IO (()))

{- | C function signature:

@
void sk_canvas_clip_path_with_operation(sk_canvas_t *ccanvas, const sk_path_t *cpath, sk_clipop_t op, _Bool doAA)
@
-}
foreign import ccall "sk_canvas_clip_path_with_operation" sk_canvas_clip_path_with_operation ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_path) -- ^ C argument @"const sk_path_t * cpath"@
  -> Sk_clipop -- ^ C argument @"sk_clipop_t op"@
  -> CBool -- ^ C argument @"_Bool doAA"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_clip_path_with_operation'
foreign import ccall "&sk_canvas_clip_path_with_operation" p'sk_canvas_clip_path_with_operation ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_path) -> Sk_clipop -> CBool -> IO (()))

{- | C function signature:

@
void sk_canvas_clip_rrect_with_operation(sk_canvas_t *ccanvas, const sk_rrect_t *crect, sk_clipop_t op, _Bool doAA)
@
-}
foreign import ccall "sk_canvas_clip_rrect_with_operation" sk_canvas_clip_rrect_with_operation ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_rrect) -- ^ C argument @"const sk_rrect_t * crect"@
  -> Sk_clipop -- ^ C argument @"sk_clipop_t op"@
  -> CBool -- ^ C argument @"_Bool doAA"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_clip_rrect_with_operation'
foreign import ccall "&sk_canvas_clip_rrect_with_operation" p'sk_canvas_clip_rrect_with_operation ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_rrect) -> Sk_clipop -> CBool -> IO (()))

{- | C function signature:

@
_Bool sk_canvas_get_local_clip_bounds(sk_canvas_t *ccanvas, sk_rect_t *cbounds)
@
-}
foreign import ccall "sk_canvas_get_local_clip_bounds" sk_canvas_get_local_clip_bounds ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_rect) -- ^ C argument @"sk_rect_t * cbounds"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_canvas_get_local_clip_bounds'
foreign import ccall "&sk_canvas_get_local_clip_bounds" p'sk_canvas_get_local_clip_bounds ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_rect) -> IO (CBool))

{- | C function signature:

@
_Bool sk_canvas_get_device_clip_bounds(sk_canvas_t *ccanvas, sk_irect_t *cbounds)
@
-}
foreign import ccall "sk_canvas_get_device_clip_bounds" sk_canvas_get_device_clip_bounds ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_irect) -- ^ C argument @"sk_irect_t * cbounds"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_canvas_get_device_clip_bounds'
foreign import ccall "&sk_canvas_get_device_clip_bounds" p'sk_canvas_get_device_clip_bounds ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_irect) -> IO (CBool))

{- | C function signature:

@
int sk_canvas_save(sk_canvas_t *ccanvas)
@
-}
foreign import ccall "sk_canvas_save" sk_canvas_save ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_canvas_save'
foreign import ccall "&sk_canvas_save" p'sk_canvas_save ::
  FunPtr (Ptr (Sk_canvas) -> IO (CInt))

{- | C function signature:

@
int sk_canvas_save_layer(sk_canvas_t *ccanvas, const sk_rect_t *crect, const sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_canvas_save_layer" sk_canvas_save_layer ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * crect"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * cpaint"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_canvas_save_layer'
foreign import ccall "&sk_canvas_save_layer" p'sk_canvas_save_layer ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_rect) -> Ptr (Sk_paint) -> IO (CInt))

{- | C function signature:

@
int sk_canvas_save_layer_rec(sk_canvas_t *ccanvas, const sk_canvas_savelayerrec_t *crec)
@
-}
foreign import ccall "sk_canvas_save_layer_rec" sk_canvas_save_layer_rec ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_canvas_savelayerrec) -- ^ C argument @"const sk_canvas_savelayerrec_t * crec"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_canvas_save_layer_rec'
foreign import ccall "&sk_canvas_save_layer_rec" p'sk_canvas_save_layer_rec ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_canvas_savelayerrec) -> IO (CInt))

{- | C function signature:

@
void sk_canvas_restore(sk_canvas_t *ccanvas)
@
-}
foreign import ccall "sk_canvas_restore" sk_canvas_restore ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_restore'
foreign import ccall "&sk_canvas_restore" p'sk_canvas_restore ::
  FunPtr (Ptr (Sk_canvas) -> IO (()))

{- | C function signature:

@
void sk_canvas_translate(sk_canvas_t *ccanvas, float dx, float dy)
@
-}
foreign import ccall "sk_canvas_translate" sk_canvas_translate ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> CFloat -- ^ C argument @"float dx"@
  -> CFloat -- ^ C argument @"float dy"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_translate'
foreign import ccall "&sk_canvas_translate" p'sk_canvas_translate ::
  FunPtr (Ptr (Sk_canvas) -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_canvas_scale(sk_canvas_t *ccanvas, float sx, float sy)
@
-}
foreign import ccall "sk_canvas_scale" sk_canvas_scale ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> CFloat -- ^ C argument @"float sx"@
  -> CFloat -- ^ C argument @"float sy"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_scale'
foreign import ccall "&sk_canvas_scale" p'sk_canvas_scale ::
  FunPtr (Ptr (Sk_canvas) -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_canvas_rotate_degrees(sk_canvas_t *ccanvas, float degrees)
@
-}
foreign import ccall "sk_canvas_rotate_degrees" sk_canvas_rotate_degrees ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> CFloat -- ^ C argument @"float degrees"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_rotate_degrees'
foreign import ccall "&sk_canvas_rotate_degrees" p'sk_canvas_rotate_degrees ::
  FunPtr (Ptr (Sk_canvas) -> CFloat -> IO (()))

{- | C function signature:

@
void sk_canvas_rotate_radians(sk_canvas_t *ccanvas, float radians)
@
-}
foreign import ccall "sk_canvas_rotate_radians" sk_canvas_rotate_radians ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> CFloat -- ^ C argument @"float radians"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_rotate_radians'
foreign import ccall "&sk_canvas_rotate_radians" p'sk_canvas_rotate_radians ::
  FunPtr (Ptr (Sk_canvas) -> CFloat -> IO (()))

{- | C function signature:

@
void sk_canvas_skew(sk_canvas_t *ccanvas, float sx, float sy)
@
-}
foreign import ccall "sk_canvas_skew" sk_canvas_skew ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> CFloat -- ^ C argument @"float sx"@
  -> CFloat -- ^ C argument @"float sy"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_skew'
foreign import ccall "&sk_canvas_skew" p'sk_canvas_skew ::
  FunPtr (Ptr (Sk_canvas) -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_canvas_concat(sk_canvas_t *ccanvas, const sk_matrix44_t *cmatrix)
@
-}
foreign import ccall "sk_canvas_concat" sk_canvas_concat ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_matrix44) -- ^ C argument @"const sk_matrix44_t * cmatrix"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_concat'
foreign import ccall "&sk_canvas_concat" p'sk_canvas_concat ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_matrix44) -> IO (()))

{- | C function signature:

@
_Bool sk_canvas_quick_reject(sk_canvas_t *ccanvas, const sk_rect_t *crect)
@
-}
foreign import ccall "sk_canvas_quick_reject" sk_canvas_quick_reject ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * crect"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_canvas_quick_reject'
foreign import ccall "&sk_canvas_quick_reject" p'sk_canvas_quick_reject ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_rect) -> IO (CBool))

{- | C function signature:

@
void sk_canvas_clip_region(sk_canvas_t *ccanvas, const sk_region_t *region, sk_clipop_t op)
@
-}
foreign import ccall "sk_canvas_clip_region" sk_canvas_clip_region ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_region) -- ^ C argument @"const sk_region_t * region"@
  -> Sk_clipop -- ^ C argument @"sk_clipop_t op"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_clip_region'
foreign import ccall "&sk_canvas_clip_region" p'sk_canvas_clip_region ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_region) -> Sk_clipop -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_paint(sk_canvas_t *ccanvas, const sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_canvas_draw_paint" sk_canvas_draw_paint ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * cpaint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_paint'
foreign import ccall "&sk_canvas_draw_paint" p'sk_canvas_draw_paint ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_region(sk_canvas_t *ccanvas, const sk_region_t *cregion, const sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_canvas_draw_region" sk_canvas_draw_region ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_region) -- ^ C argument @"const sk_region_t * cregion"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * cpaint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_region'
foreign import ccall "&sk_canvas_draw_region" p'sk_canvas_draw_region ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_region) -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_rect(sk_canvas_t *ccanvas, const sk_rect_t *crect, const sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_canvas_draw_rect" sk_canvas_draw_rect ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * crect"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * cpaint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_rect'
foreign import ccall "&sk_canvas_draw_rect" p'sk_canvas_draw_rect ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_rect) -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_rrect(sk_canvas_t *ccanvas, const sk_rrect_t *crect, const sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_canvas_draw_rrect" sk_canvas_draw_rrect ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_rrect) -- ^ C argument @"const sk_rrect_t * crect"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * cpaint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_rrect'
foreign import ccall "&sk_canvas_draw_rrect" p'sk_canvas_draw_rrect ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_rrect) -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_circle(sk_canvas_t *ccanvas, float cx, float cy, float rad, const sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_canvas_draw_circle" sk_canvas_draw_circle ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> CFloat -- ^ C argument @"float cx"@
  -> CFloat -- ^ C argument @"float cy"@
  -> CFloat -- ^ C argument @"float rad"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * cpaint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_circle'
foreign import ccall "&sk_canvas_draw_circle" p'sk_canvas_draw_circle ::
  FunPtr (Ptr (Sk_canvas) -> CFloat -> CFloat -> CFloat -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_oval(sk_canvas_t *ccanvas, const sk_rect_t *crect, const sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_canvas_draw_oval" sk_canvas_draw_oval ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * crect"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * cpaint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_oval'
foreign import ccall "&sk_canvas_draw_oval" p'sk_canvas_draw_oval ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_rect) -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_path(sk_canvas_t *ccanvas, const sk_path_t *cpath, const sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_canvas_draw_path" sk_canvas_draw_path ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_path) -- ^ C argument @"const sk_path_t * cpath"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * cpaint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_path'
foreign import ccall "&sk_canvas_draw_path" p'sk_canvas_draw_path ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_path) -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_image(sk_canvas_t *ccanvas, const sk_image_t *cimage, float x, float y, const sk_sampling_options_t *sampling, const sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_canvas_draw_image" sk_canvas_draw_image ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_image) -- ^ C argument @"const sk_image_t * cimage"@
  -> CFloat -- ^ C argument @"float x"@
  -> CFloat -- ^ C argument @"float y"@
  -> Ptr (Sk_sampling_options) -- ^ C argument @"const sk_sampling_options_t * sampling"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * cpaint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_image'
foreign import ccall "&sk_canvas_draw_image" p'sk_canvas_draw_image ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_image) -> CFloat -> CFloat -> Ptr (Sk_sampling_options) -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_image_rect(sk_canvas_t *ccanvas, const sk_image_t *cimage, const sk_rect_t *csrcR, const sk_rect_t *cdstR, const sk_sampling_options_t *sampling, const sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_canvas_draw_image_rect" sk_canvas_draw_image_rect ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_image) -- ^ C argument @"const sk_image_t * cimage"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * csrcR"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cdstR"@
  -> Ptr (Sk_sampling_options) -- ^ C argument @"const sk_sampling_options_t * sampling"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * cpaint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_image_rect'
foreign import ccall "&sk_canvas_draw_image_rect" p'sk_canvas_draw_image_rect ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_image) -> Ptr (Sk_rect) -> Ptr (Sk_rect) -> Ptr (Sk_sampling_options) -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_picture(sk_canvas_t *ccanvas, const sk_picture_t *cpicture, const sk_matrix_t *cmatrix, const sk_paint_t *cpaint)
@
-}
foreign import ccall "sk_canvas_draw_picture" sk_canvas_draw_picture ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_picture) -- ^ C argument @"const sk_picture_t * cpicture"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * cmatrix"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * cpaint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_picture'
foreign import ccall "&sk_canvas_draw_picture" p'sk_canvas_draw_picture ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_picture) -> Ptr (Sk_matrix) -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_drawable(sk_canvas_t *ccanvas, sk_drawable_t *cdrawable, const sk_matrix_t *cmatrix)
@
-}
foreign import ccall "sk_canvas_draw_drawable" sk_canvas_draw_drawable ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_drawable) -- ^ C argument @"sk_drawable_t * cdrawable"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * cmatrix"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_drawable'
foreign import ccall "&sk_canvas_draw_drawable" p'sk_canvas_draw_drawable ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_drawable) -> Ptr (Sk_matrix) -> IO (()))

{- | C function signature:

@
sk_canvas_t *sk_canvas_new_from_bitmap(const sk_bitmap_t *bitmap)
@
-}
foreign import ccall "sk_canvas_new_from_bitmap" sk_canvas_new_from_bitmap ::
  Ptr (Sk_bitmap) -- ^ C argument @"const sk_bitmap_t * bitmap"@
  -> IO (Ptr (Sk_canvas)) -- ^ C return type: @"sk_canvas_t *"@

-- | Function pointer to 'sk_canvas_new_from_bitmap'
foreign import ccall "&sk_canvas_new_from_bitmap" p'sk_canvas_new_from_bitmap ::
  FunPtr (Ptr (Sk_bitmap) -> IO (Ptr (Sk_canvas)))

{- | C function signature:

@
sk_canvas_t *sk_canvas_new_from_raster(const sk_imageinfo_t *cinfo, void *pixels, size_t rowBytes, const sk_surfaceprops_t *props)
@
-}
foreign import ccall "sk_canvas_new_from_raster" sk_canvas_new_from_raster ::
  Ptr (Sk_imageinfo) -- ^ C argument @"const sk_imageinfo_t * cinfo"@
  -> Ptr (()) -- ^ C argument @"void * pixels"@
  -> CSize -- ^ C argument @"size_t rowBytes"@
  -> Ptr (Sk_surfaceprops) -- ^ C argument @"const sk_surfaceprops_t * props"@
  -> IO (Ptr (Sk_canvas)) -- ^ C return type: @"sk_canvas_t *"@

-- | Function pointer to 'sk_canvas_new_from_raster'
foreign import ccall "&sk_canvas_new_from_raster" p'sk_canvas_new_from_raster ::
  FunPtr (Ptr (Sk_imageinfo) -> Ptr (()) -> CSize -> Ptr (Sk_surfaceprops) -> IO (Ptr (Sk_canvas)))

{- | C function signature:

@
void sk_canvas_draw_annotation(sk_canvas_t *t, const sk_rect_t *rect, const char *key, sk_data_t *value)
@
-}
foreign import ccall "sk_canvas_draw_annotation" sk_canvas_draw_annotation ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * t"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * rect"@
  -> Ptr (CChar) -- ^ C argument @"const char * key"@
  -> Ptr (Sk_data) -- ^ C argument @"sk_data_t * value"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_annotation'
foreign import ccall "&sk_canvas_draw_annotation" p'sk_canvas_draw_annotation ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_rect) -> Ptr (CChar) -> Ptr (Sk_data) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_url_annotation(sk_canvas_t *t, const sk_rect_t *rect, sk_data_t *value)
@
-}
foreign import ccall "sk_canvas_draw_url_annotation" sk_canvas_draw_url_annotation ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * t"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * rect"@
  -> Ptr (Sk_data) -- ^ C argument @"sk_data_t * value"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_url_annotation'
foreign import ccall "&sk_canvas_draw_url_annotation" p'sk_canvas_draw_url_annotation ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_rect) -> Ptr (Sk_data) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_named_destination_annotation(sk_canvas_t *t, const sk_point_t *point, sk_data_t *value)
@
-}
foreign import ccall "sk_canvas_draw_named_destination_annotation" sk_canvas_draw_named_destination_annotation ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * t"@
  -> Ptr (Sk_point) -- ^ C argument @"const sk_point_t * point"@
  -> Ptr (Sk_data) -- ^ C argument @"sk_data_t * value"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_named_destination_annotation'
foreign import ccall "&sk_canvas_draw_named_destination_annotation" p'sk_canvas_draw_named_destination_annotation ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_point) -> Ptr (Sk_data) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_link_destination_annotation(sk_canvas_t *t, const sk_rect_t *rect, sk_data_t *value)
@
-}
foreign import ccall "sk_canvas_draw_link_destination_annotation" sk_canvas_draw_link_destination_annotation ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * t"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * rect"@
  -> Ptr (Sk_data) -- ^ C argument @"sk_data_t * value"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_link_destination_annotation'
foreign import ccall "&sk_canvas_draw_link_destination_annotation" p'sk_canvas_draw_link_destination_annotation ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_rect) -> Ptr (Sk_data) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_image_lattice(sk_canvas_t *ccanvas, const sk_image_t *image, const sk_lattice_t *lattice, const sk_rect_t *dst, sk_filter_mode_t mode, const sk_paint_t *paint)
@
-}
foreign import ccall "sk_canvas_draw_image_lattice" sk_canvas_draw_image_lattice ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_image) -- ^ C argument @"const sk_image_t * image"@
  -> Ptr (Sk_lattice) -- ^ C argument @"const sk_lattice_t * lattice"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * dst"@
  -> Sk_filter_mode -- ^ C argument @"sk_filter_mode_t mode"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * paint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_image_lattice'
foreign import ccall "&sk_canvas_draw_image_lattice" p'sk_canvas_draw_image_lattice ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_image) -> Ptr (Sk_lattice) -> Ptr (Sk_rect) -> Sk_filter_mode -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_image_nine(sk_canvas_t *ccanvas, const sk_image_t *image, const sk_irect_t *center, const sk_rect_t *dst, sk_filter_mode_t mode, const sk_paint_t *paint)
@
-}
foreign import ccall "sk_canvas_draw_image_nine" sk_canvas_draw_image_nine ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_image) -- ^ C argument @"const sk_image_t * image"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * center"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * dst"@
  -> Sk_filter_mode -- ^ C argument @"sk_filter_mode_t mode"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * paint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_image_nine'
foreign import ccall "&sk_canvas_draw_image_nine" p'sk_canvas_draw_image_nine ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_image) -> Ptr (Sk_irect) -> Ptr (Sk_rect) -> Sk_filter_mode -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_vertices(sk_canvas_t *ccanvas, const sk_vertices_t *vertices, sk_blendmode_t mode, const sk_paint_t *paint)
@
-}
foreign import ccall "sk_canvas_draw_vertices" sk_canvas_draw_vertices ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_vertices) -- ^ C argument @"const sk_vertices_t * vertices"@
  -> Sk_blendmode -- ^ C argument @"sk_blendmode_t mode"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * paint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_vertices'
foreign import ccall "&sk_canvas_draw_vertices" p'sk_canvas_draw_vertices ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_vertices) -> Sk_blendmode -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_arc(sk_canvas_t *ccanvas, const sk_rect_t *oval, float startAngle, float sweepAngle, _Bool useCenter, const sk_paint_t *paint)
@
-}
foreign import ccall "sk_canvas_draw_arc" sk_canvas_draw_arc ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * oval"@
  -> CFloat -- ^ C argument @"float startAngle"@
  -> CFloat -- ^ C argument @"float sweepAngle"@
  -> CBool -- ^ C argument @"_Bool useCenter"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * paint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_arc'
foreign import ccall "&sk_canvas_draw_arc" p'sk_canvas_draw_arc ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_rect) -> CFloat -> CFloat -> CBool -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_drrect(sk_canvas_t *ccanvas, const sk_rrect_t *outer, const sk_rrect_t *inner, const sk_paint_t *paint)
@
-}
foreign import ccall "sk_canvas_draw_drrect" sk_canvas_draw_drrect ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_rrect) -- ^ C argument @"const sk_rrect_t * outer"@
  -> Ptr (Sk_rrect) -- ^ C argument @"const sk_rrect_t * inner"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * paint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_drrect'
foreign import ccall "&sk_canvas_draw_drrect" p'sk_canvas_draw_drrect ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_rrect) -> Ptr (Sk_rrect) -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_atlas(sk_canvas_t *ccanvas, const sk_image_t *atlas, const sk_rsxform_t *xform, const sk_rect_t *tex, const sk_color_t *colors, int count, sk_blendmode_t mode, const sk_sampling_options_t *sampling, const sk_rect_t *cullRect, const sk_paint_t *paint)
@
-}
foreign import ccall "sk_canvas_draw_atlas" sk_canvas_draw_atlas ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_image) -- ^ C argument @"const sk_image_t * atlas"@
  -> Ptr (Sk_rsxform) -- ^ C argument @"const sk_rsxform_t * xform"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * tex"@
  -> Ptr (Sk_color) -- ^ C argument @"const sk_color_t * colors"@
  -> CInt -- ^ C argument @"int count"@
  -> Sk_blendmode -- ^ C argument @"sk_blendmode_t mode"@
  -> Ptr (Sk_sampling_options) -- ^ C argument @"const sk_sampling_options_t * sampling"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * cullRect"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * paint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_atlas'
foreign import ccall "&sk_canvas_draw_atlas" p'sk_canvas_draw_atlas ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_image) -> Ptr (Sk_rsxform) -> Ptr (Sk_rect) -> Ptr (Sk_color) -> CInt -> Sk_blendmode -> Ptr (Sk_sampling_options) -> Ptr (Sk_rect) -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_canvas_draw_patch(sk_canvas_t *ccanvas, const sk_point_t *cubics, const sk_color_t *colors, const sk_point_t *texCoords, sk_blendmode_t mode, const sk_paint_t *paint)
@
-}
foreign import ccall "sk_canvas_draw_patch" sk_canvas_draw_patch ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> Ptr (Sk_point) -- ^ C argument @"const sk_point_t * cubics"@
  -> Ptr (Sk_color) -- ^ C argument @"const sk_color_t * colors"@
  -> Ptr (Sk_point) -- ^ C argument @"const sk_point_t * texCoords"@
  -> Sk_blendmode -- ^ C argument @"sk_blendmode_t mode"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * paint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_canvas_draw_patch'
foreign import ccall "&sk_canvas_draw_patch" p'sk_canvas_draw_patch ::
  FunPtr (Ptr (Sk_canvas) -> Ptr (Sk_point) -> Ptr (Sk_color) -> Ptr (Sk_point) -> Sk_blendmode -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
_Bool sk_canvas_is_clip_empty(sk_canvas_t *ccanvas)
@
-}
foreign import ccall "sk_canvas_is_clip_empty" sk_canvas_is_clip_empty ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_canvas_is_clip_empty'
foreign import ccall "&sk_canvas_is_clip_empty" p'sk_canvas_is_clip_empty ::
  FunPtr (Ptr (Sk_canvas) -> IO (CBool))

{- | C function signature:

@
_Bool sk_canvas_is_clip_rect(sk_canvas_t *ccanvas)
@
-}
foreign import ccall "sk_canvas_is_clip_rect" sk_canvas_is_clip_rect ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * ccanvas"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_canvas_is_clip_rect'
foreign import ccall "&sk_canvas_is_clip_rect" p'sk_canvas_is_clip_rect ::
  FunPtr (Ptr (Sk_canvas) -> IO (CBool))

{- | C function signature:

@
sk_nodraw_canvas_t *sk_nodraw_canvas_new(int width, int height)
@
-}
foreign import ccall "sk_nodraw_canvas_new" sk_nodraw_canvas_new ::
  CInt -- ^ C argument @"int width"@
  -> CInt -- ^ C argument @"int height"@
  -> IO (Ptr (Sk_nodraw_canvas)) -- ^ C return type: @"sk_nodraw_canvas_t *"@

-- | Function pointer to 'sk_nodraw_canvas_new'
foreign import ccall "&sk_nodraw_canvas_new" p'sk_nodraw_canvas_new ::
  FunPtr (CInt -> CInt -> IO (Ptr (Sk_nodraw_canvas)))

{- | C function signature:

@
void sk_nodraw_canvas_destroy(sk_nodraw_canvas_t *t)
@
-}
foreign import ccall "sk_nodraw_canvas_destroy" sk_nodraw_canvas_destroy ::
  Ptr (Sk_nodraw_canvas) -- ^ C argument @"sk_nodraw_canvas_t * t"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_nodraw_canvas_destroy'
foreign import ccall "&sk_nodraw_canvas_destroy" p'sk_nodraw_canvas_destroy ::
  FunPtr (Ptr (Sk_nodraw_canvas) -> IO (()))

{- | C function signature:

@
sk_nway_canvas_t *sk_nway_canvas_new(int width, int height)
@
-}
foreign import ccall "sk_nway_canvas_new" sk_nway_canvas_new ::
  CInt -- ^ C argument @"int width"@
  -> CInt -- ^ C argument @"int height"@
  -> IO (Ptr (Sk_nway_canvas)) -- ^ C return type: @"sk_nway_canvas_t *"@

-- | Function pointer to 'sk_nway_canvas_new'
foreign import ccall "&sk_nway_canvas_new" p'sk_nway_canvas_new ::
  FunPtr (CInt -> CInt -> IO (Ptr (Sk_nway_canvas)))

{- | C function signature:

@
void sk_nway_canvas_destroy(sk_nway_canvas_t *t)
@
-}
foreign import ccall "sk_nway_canvas_destroy" sk_nway_canvas_destroy ::
  Ptr (Sk_nway_canvas) -- ^ C argument @"sk_nway_canvas_t * t"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_nway_canvas_destroy'
foreign import ccall "&sk_nway_canvas_destroy" p'sk_nway_canvas_destroy ::
  FunPtr (Ptr (Sk_nway_canvas) -> IO (()))

{- | C function signature:

@
void sk_nway_canvas_add_canvas(sk_nway_canvas_t *t, sk_canvas_t *canvas)
@
-}
foreign import ccall "sk_nway_canvas_add_canvas" sk_nway_canvas_add_canvas ::
  Ptr (Sk_nway_canvas) -- ^ C argument @"sk_nway_canvas_t * t"@
  -> Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * canvas"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_nway_canvas_add_canvas'
foreign import ccall "&sk_nway_canvas_add_canvas" p'sk_nway_canvas_add_canvas ::
  FunPtr (Ptr (Sk_nway_canvas) -> Ptr (Sk_canvas) -> IO (()))

{- | C function signature:

@
void sk_nway_canvas_remove_canvas(sk_nway_canvas_t *t, sk_canvas_t *canvas)
@
-}
foreign import ccall "sk_nway_canvas_remove_canvas" sk_nway_canvas_remove_canvas ::
  Ptr (Sk_nway_canvas) -- ^ C argument @"sk_nway_canvas_t * t"@
  -> Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * canvas"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_nway_canvas_remove_canvas'
foreign import ccall "&sk_nway_canvas_remove_canvas" p'sk_nway_canvas_remove_canvas ::
  FunPtr (Ptr (Sk_nway_canvas) -> Ptr (Sk_canvas) -> IO (()))

{- | C function signature:

@
void sk_nway_canvas_remove_all(sk_nway_canvas_t *t)
@
-}
foreign import ccall "sk_nway_canvas_remove_all" sk_nway_canvas_remove_all ::
  Ptr (Sk_nway_canvas) -- ^ C argument @"sk_nway_canvas_t * t"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_nway_canvas_remove_all'
foreign import ccall "&sk_nway_canvas_remove_all" p'sk_nway_canvas_remove_all ::
  FunPtr (Ptr (Sk_nway_canvas) -> IO (()))

{- | C function signature:

@
sk_overdraw_canvas_t *sk_overdraw_canvas_new(sk_canvas_t *canvas)
@
-}
foreign import ccall "sk_overdraw_canvas_new" sk_overdraw_canvas_new ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * canvas"@
  -> IO (Ptr (Sk_overdraw_canvas)) -- ^ C return type: @"sk_overdraw_canvas_t *"@

-- | Function pointer to 'sk_overdraw_canvas_new'
foreign import ccall "&sk_overdraw_canvas_new" p'sk_overdraw_canvas_new ::
  FunPtr (Ptr (Sk_canvas) -> IO (Ptr (Sk_overdraw_canvas)))

{- | C function signature:

@
void sk_overdraw_canvas_destroy(sk_overdraw_canvas_t *canvas)
@
-}
foreign import ccall "sk_overdraw_canvas_destroy" sk_overdraw_canvas_destroy ::
  Ptr (Sk_overdraw_canvas) -- ^ C argument @"sk_overdraw_canvas_t * canvas"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_overdraw_canvas_destroy'
foreign import ccall "&sk_overdraw_canvas_destroy" p'sk_overdraw_canvas_destroy ::
  FunPtr (Ptr (Sk_overdraw_canvas) -> IO (()))

{- | C function signature:

@
gr_recording_context_t *sk_get_recording_context(sk_canvas_t *canvas)
@
-}
foreign import ccall "sk_get_recording_context" sk_get_recording_context ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * canvas"@
  -> IO (Ptr (Gr_recording_context)) -- ^ C return type: @"gr_recording_context_t *"@

-- | Function pointer to 'sk_get_recording_context'
foreign import ccall "&sk_get_recording_context" p'sk_get_recording_context ::
  FunPtr (Ptr (Sk_canvas) -> IO (Ptr (Gr_recording_context)))

{- | C function signature:

@
sk_surface_t *sk_get_surface(sk_canvas_t *canvas)
@
-}
foreign import ccall "sk_get_surface" sk_get_surface ::
  Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * canvas"@
  -> IO (Ptr (Sk_surface)) -- ^ C return type: @"sk_surface_t *"@

-- | Function pointer to 'sk_get_surface'
foreign import ccall "&sk_get_surface" p'sk_get_surface ::
  FunPtr (Ptr (Sk_canvas) -> IO (Ptr (Sk_surface)))
