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
module Skia.Bindings.Sk_font where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
sk_font_t *sk_font_new(void)
@
-}
foreign import ccall "sk_font_new" sk_font_new ::
  IO (Ptr (Sk_font)) -- ^ C return type: @"sk_font_t *"@

-- | Function pointer to 'sk_font_new'
foreign import ccall "&sk_font_new" p'sk_font_new ::
  FunPtr (IO (Ptr (Sk_font)))

{- | C function signature:

@
sk_font_t *sk_font_new_with_values(sk_typeface_t *typeface, float size, float scaleX, float skewX)
@
-}
foreign import ccall "sk_font_new_with_values" sk_font_new_with_values ::
  Ptr (Sk_typeface) -- ^ C argument @"sk_typeface_t * typeface"@
  -> CFloat -- ^ C argument @"float size"@
  -> CFloat -- ^ C argument @"float scaleX"@
  -> CFloat -- ^ C argument @"float skewX"@
  -> IO (Ptr (Sk_font)) -- ^ C return type: @"sk_font_t *"@

-- | Function pointer to 'sk_font_new_with_values'
foreign import ccall "&sk_font_new_with_values" p'sk_font_new_with_values ::
  FunPtr (Ptr (Sk_typeface) -> CFloat -> CFloat -> CFloat -> IO (Ptr (Sk_font)))

{- | C function signature:

@
void sk_font_delete(sk_font_t *font)
@
-}
foreign import ccall "sk_font_delete" sk_font_delete ::
  Ptr (Sk_font) -- ^ C argument @"sk_font_t * font"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_delete'
foreign import ccall "&sk_font_delete" p'sk_font_delete ::
  FunPtr (Ptr (Sk_font) -> IO (()))

{- | C function signature:

@
_Bool sk_font_is_force_auto_hinting(const sk_font_t *font)
@
-}
foreign import ccall "sk_font_is_force_auto_hinting" sk_font_is_force_auto_hinting ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_font_is_force_auto_hinting'
foreign import ccall "&sk_font_is_force_auto_hinting" p'sk_font_is_force_auto_hinting ::
  FunPtr (Ptr (Sk_font) -> IO (CBool))

{- | C function signature:

@
void sk_font_set_force_auto_hinting(sk_font_t *font, _Bool value)
@
-}
foreign import ccall "sk_font_set_force_auto_hinting" sk_font_set_force_auto_hinting ::
  Ptr (Sk_font) -- ^ C argument @"sk_font_t * font"@
  -> CBool -- ^ C argument @"_Bool value"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_set_force_auto_hinting'
foreign import ccall "&sk_font_set_force_auto_hinting" p'sk_font_set_force_auto_hinting ::
  FunPtr (Ptr (Sk_font) -> CBool -> IO (()))

{- | C function signature:

@
_Bool sk_font_is_embedded_bitmaps(const sk_font_t *font)
@
-}
foreign import ccall "sk_font_is_embedded_bitmaps" sk_font_is_embedded_bitmaps ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_font_is_embedded_bitmaps'
foreign import ccall "&sk_font_is_embedded_bitmaps" p'sk_font_is_embedded_bitmaps ::
  FunPtr (Ptr (Sk_font) -> IO (CBool))

{- | C function signature:

@
void sk_font_set_embedded_bitmaps(sk_font_t *font, _Bool value)
@
-}
foreign import ccall "sk_font_set_embedded_bitmaps" sk_font_set_embedded_bitmaps ::
  Ptr (Sk_font) -- ^ C argument @"sk_font_t * font"@
  -> CBool -- ^ C argument @"_Bool value"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_set_embedded_bitmaps'
foreign import ccall "&sk_font_set_embedded_bitmaps" p'sk_font_set_embedded_bitmaps ::
  FunPtr (Ptr (Sk_font) -> CBool -> IO (()))

{- | C function signature:

@
_Bool sk_font_is_subpixel(const sk_font_t *font)
@
-}
foreign import ccall "sk_font_is_subpixel" sk_font_is_subpixel ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_font_is_subpixel'
foreign import ccall "&sk_font_is_subpixel" p'sk_font_is_subpixel ::
  FunPtr (Ptr (Sk_font) -> IO (CBool))

{- | C function signature:

@
void sk_font_set_subpixel(sk_font_t *font, _Bool value)
@
-}
foreign import ccall "sk_font_set_subpixel" sk_font_set_subpixel ::
  Ptr (Sk_font) -- ^ C argument @"sk_font_t * font"@
  -> CBool -- ^ C argument @"_Bool value"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_set_subpixel'
foreign import ccall "&sk_font_set_subpixel" p'sk_font_set_subpixel ::
  FunPtr (Ptr (Sk_font) -> CBool -> IO (()))

{- | C function signature:

@
_Bool sk_font_is_linear_metrics(const sk_font_t *font)
@
-}
foreign import ccall "sk_font_is_linear_metrics" sk_font_is_linear_metrics ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_font_is_linear_metrics'
foreign import ccall "&sk_font_is_linear_metrics" p'sk_font_is_linear_metrics ::
  FunPtr (Ptr (Sk_font) -> IO (CBool))

{- | C function signature:

@
void sk_font_set_linear_metrics(sk_font_t *font, _Bool value)
@
-}
foreign import ccall "sk_font_set_linear_metrics" sk_font_set_linear_metrics ::
  Ptr (Sk_font) -- ^ C argument @"sk_font_t * font"@
  -> CBool -- ^ C argument @"_Bool value"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_set_linear_metrics'
foreign import ccall "&sk_font_set_linear_metrics" p'sk_font_set_linear_metrics ::
  FunPtr (Ptr (Sk_font) -> CBool -> IO (()))

{- | C function signature:

@
_Bool sk_font_is_embolden(const sk_font_t *font)
@
-}
foreign import ccall "sk_font_is_embolden" sk_font_is_embolden ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_font_is_embolden'
foreign import ccall "&sk_font_is_embolden" p'sk_font_is_embolden ::
  FunPtr (Ptr (Sk_font) -> IO (CBool))

{- | C function signature:

@
void sk_font_set_embolden(sk_font_t *font, _Bool value)
@
-}
foreign import ccall "sk_font_set_embolden" sk_font_set_embolden ::
  Ptr (Sk_font) -- ^ C argument @"sk_font_t * font"@
  -> CBool -- ^ C argument @"_Bool value"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_set_embolden'
foreign import ccall "&sk_font_set_embolden" p'sk_font_set_embolden ::
  FunPtr (Ptr (Sk_font) -> CBool -> IO (()))

{- | C function signature:

@
_Bool sk_font_is_baseline_snap(const sk_font_t *font)
@
-}
foreign import ccall "sk_font_is_baseline_snap" sk_font_is_baseline_snap ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_font_is_baseline_snap'
foreign import ccall "&sk_font_is_baseline_snap" p'sk_font_is_baseline_snap ::
  FunPtr (Ptr (Sk_font) -> IO (CBool))

{- | C function signature:

@
void sk_font_set_baseline_snap(sk_font_t *font, _Bool value)
@
-}
foreign import ccall "sk_font_set_baseline_snap" sk_font_set_baseline_snap ::
  Ptr (Sk_font) -- ^ C argument @"sk_font_t * font"@
  -> CBool -- ^ C argument @"_Bool value"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_set_baseline_snap'
foreign import ccall "&sk_font_set_baseline_snap" p'sk_font_set_baseline_snap ::
  FunPtr (Ptr (Sk_font) -> CBool -> IO (()))

{- | C function signature:

@
sk_font_edging_t sk_font_get_edging(const sk_font_t *font)
@
-}
foreign import ccall "sk_font_get_edging" sk_font_get_edging ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> IO (Sk_font_edging) -- ^ C return type: @"sk_font_edging_t"@

-- | Function pointer to 'sk_font_get_edging'
foreign import ccall "&sk_font_get_edging" p'sk_font_get_edging ::
  FunPtr (Ptr (Sk_font) -> IO (Sk_font_edging))

{- | C function signature:

@
void sk_font_set_edging(sk_font_t *font, sk_font_edging_t value)
@
-}
foreign import ccall "sk_font_set_edging" sk_font_set_edging ::
  Ptr (Sk_font) -- ^ C argument @"sk_font_t * font"@
  -> Sk_font_edging -- ^ C argument @"sk_font_edging_t value"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_set_edging'
foreign import ccall "&sk_font_set_edging" p'sk_font_set_edging ::
  FunPtr (Ptr (Sk_font) -> Sk_font_edging -> IO (()))

{- | C function signature:

@
sk_font_hinting_t sk_font_get_hinting(const sk_font_t *font)
@
-}
foreign import ccall "sk_font_get_hinting" sk_font_get_hinting ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> IO (Sk_font_hinting) -- ^ C return type: @"sk_font_hinting_t"@

-- | Function pointer to 'sk_font_get_hinting'
foreign import ccall "&sk_font_get_hinting" p'sk_font_get_hinting ::
  FunPtr (Ptr (Sk_font) -> IO (Sk_font_hinting))

{- | C function signature:

@
void sk_font_set_hinting(sk_font_t *font, sk_font_hinting_t value)
@
-}
foreign import ccall "sk_font_set_hinting" sk_font_set_hinting ::
  Ptr (Sk_font) -- ^ C argument @"sk_font_t * font"@
  -> Sk_font_hinting -- ^ C argument @"sk_font_hinting_t value"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_set_hinting'
foreign import ccall "&sk_font_set_hinting" p'sk_font_set_hinting ::
  FunPtr (Ptr (Sk_font) -> Sk_font_hinting -> IO (()))

{- | C function signature:

@
sk_typeface_t *sk_font_get_typeface(const sk_font_t *font)
@
-}
foreign import ccall "sk_font_get_typeface" sk_font_get_typeface ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> IO (Ptr (Sk_typeface)) -- ^ C return type: @"sk_typeface_t *"@

-- | Function pointer to 'sk_font_get_typeface'
foreign import ccall "&sk_font_get_typeface" p'sk_font_get_typeface ::
  FunPtr (Ptr (Sk_font) -> IO (Ptr (Sk_typeface)))

{- | C function signature:

@
void sk_font_set_typeface(sk_font_t *font, sk_typeface_t *value)
@
-}
foreign import ccall "sk_font_set_typeface" sk_font_set_typeface ::
  Ptr (Sk_font) -- ^ C argument @"sk_font_t * font"@
  -> Ptr (Sk_typeface) -- ^ C argument @"sk_typeface_t * value"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_set_typeface'
foreign import ccall "&sk_font_set_typeface" p'sk_font_set_typeface ::
  FunPtr (Ptr (Sk_font) -> Ptr (Sk_typeface) -> IO (()))

{- | C function signature:

@
float sk_font_get_size(const sk_font_t *font)
@
-}
foreign import ccall "sk_font_get_size" sk_font_get_size ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> IO (CFloat) -- ^ C return type: @"float"@

-- | Function pointer to 'sk_font_get_size'
foreign import ccall "&sk_font_get_size" p'sk_font_get_size ::
  FunPtr (Ptr (Sk_font) -> IO (CFloat))

{- | C function signature:

@
void sk_font_set_size(sk_font_t *font, float value)
@
-}
foreign import ccall "sk_font_set_size" sk_font_set_size ::
  Ptr (Sk_font) -- ^ C argument @"sk_font_t * font"@
  -> CFloat -- ^ C argument @"float value"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_set_size'
foreign import ccall "&sk_font_set_size" p'sk_font_set_size ::
  FunPtr (Ptr (Sk_font) -> CFloat -> IO (()))

{- | C function signature:

@
float sk_font_get_scale_x(const sk_font_t *font)
@
-}
foreign import ccall "sk_font_get_scale_x" sk_font_get_scale_x ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> IO (CFloat) -- ^ C return type: @"float"@

-- | Function pointer to 'sk_font_get_scale_x'
foreign import ccall "&sk_font_get_scale_x" p'sk_font_get_scale_x ::
  FunPtr (Ptr (Sk_font) -> IO (CFloat))

{- | C function signature:

@
void sk_font_set_scale_x(sk_font_t *font, float value)
@
-}
foreign import ccall "sk_font_set_scale_x" sk_font_set_scale_x ::
  Ptr (Sk_font) -- ^ C argument @"sk_font_t * font"@
  -> CFloat -- ^ C argument @"float value"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_set_scale_x'
foreign import ccall "&sk_font_set_scale_x" p'sk_font_set_scale_x ::
  FunPtr (Ptr (Sk_font) -> CFloat -> IO (()))

{- | C function signature:

@
float sk_font_get_skew_x(const sk_font_t *font)
@
-}
foreign import ccall "sk_font_get_skew_x" sk_font_get_skew_x ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> IO (CFloat) -- ^ C return type: @"float"@

-- | Function pointer to 'sk_font_get_skew_x'
foreign import ccall "&sk_font_get_skew_x" p'sk_font_get_skew_x ::
  FunPtr (Ptr (Sk_font) -> IO (CFloat))

{- | C function signature:

@
void sk_font_set_skew_x(sk_font_t *font, float value)
@
-}
foreign import ccall "sk_font_set_skew_x" sk_font_set_skew_x ::
  Ptr (Sk_font) -- ^ C argument @"sk_font_t * font"@
  -> CFloat -- ^ C argument @"float value"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_set_skew_x'
foreign import ccall "&sk_font_set_skew_x" p'sk_font_set_skew_x ::
  FunPtr (Ptr (Sk_font) -> CFloat -> IO (()))

{- | C function signature:

@
int sk_font_text_to_glyphs(const sk_font_t *font, const void *text, size_t byteLength, sk_text_encoding_t encoding, uint16_t glyphs[], int maxGlyphCount)
@
-}
foreign import ccall "sk_font_text_to_glyphs" sk_font_text_to_glyphs ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> Ptr (()) -- ^ C argument @"const void * text"@
  -> CSize -- ^ C argument @"size_t byteLength"@
  -> Sk_text_encoding -- ^ C argument @"sk_text_encoding_t encoding"@
  -> Ptr (Word16) -- ^ C argument @"uint16_t [] glyphs"@
  -> CInt -- ^ C argument @"int maxGlyphCount"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_font_text_to_glyphs'
foreign import ccall "&sk_font_text_to_glyphs" p'sk_font_text_to_glyphs ::
  FunPtr (Ptr (Sk_font) -> Ptr (()) -> CSize -> Sk_text_encoding -> Ptr (Word16) -> CInt -> IO (CInt))

{- | C function signature:

@
uint16_t sk_font_unichar_to_glyph(const sk_font_t *font, int32_t uni)
@
-}
foreign import ccall "sk_font_unichar_to_glyph" sk_font_unichar_to_glyph ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> Int32 -- ^ C argument @"int32_t uni"@
  -> IO (Word16) -- ^ C return type: @"uint16_t"@

-- | Function pointer to 'sk_font_unichar_to_glyph'
foreign import ccall "&sk_font_unichar_to_glyph" p'sk_font_unichar_to_glyph ::
  FunPtr (Ptr (Sk_font) -> Int32 -> IO (Word16))

{- | C function signature:

@
void sk_font_unichars_to_glyphs(const sk_font_t *font, const int32_t uni[], int count, uint16_t glyphs[])
@
-}
foreign import ccall "sk_font_unichars_to_glyphs" sk_font_unichars_to_glyphs ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> Ptr (Int32) -- ^ C argument @"const int32_t [] uni"@
  -> CInt -- ^ C argument @"int count"@
  -> Ptr (Word16) -- ^ C argument @"uint16_t [] glyphs"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_unichars_to_glyphs'
foreign import ccall "&sk_font_unichars_to_glyphs" p'sk_font_unichars_to_glyphs ::
  FunPtr (Ptr (Sk_font) -> Ptr (Int32) -> CInt -> Ptr (Word16) -> IO (()))

{- | C function signature:

@
float sk_font_measure_text(const sk_font_t *font, const void *text, size_t byteLength, sk_text_encoding_t encoding, sk_rect_t *bounds, const sk_paint_t *paint)
@
-}
foreign import ccall "sk_font_measure_text" sk_font_measure_text ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> Ptr (()) -- ^ C argument @"const void * text"@
  -> CSize -- ^ C argument @"size_t byteLength"@
  -> Sk_text_encoding -- ^ C argument @"sk_text_encoding_t encoding"@
  -> Ptr (Sk_rect) -- ^ C argument @"sk_rect_t * bounds"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * paint"@
  -> IO (CFloat) -- ^ C return type: @"float"@

-- | Function pointer to 'sk_font_measure_text'
foreign import ccall "&sk_font_measure_text" p'sk_font_measure_text ::
  FunPtr (Ptr (Sk_font) -> Ptr (()) -> CSize -> Sk_text_encoding -> Ptr (Sk_rect) -> Ptr (Sk_paint) -> IO (CFloat))

{- | C function signature:

@
void sk_font_measure_text_no_return(const sk_font_t *font, const void *text, size_t byteLength, sk_text_encoding_t encoding, sk_rect_t *bounds, const sk_paint_t *paint, float *measuredWidth)
@
-}
foreign import ccall "sk_font_measure_text_no_return" sk_font_measure_text_no_return ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> Ptr (()) -- ^ C argument @"const void * text"@
  -> CSize -- ^ C argument @"size_t byteLength"@
  -> Sk_text_encoding -- ^ C argument @"sk_text_encoding_t encoding"@
  -> Ptr (Sk_rect) -- ^ C argument @"sk_rect_t * bounds"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * paint"@
  -> Ptr (CFloat) -- ^ C argument @"float * measuredWidth"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_measure_text_no_return'
foreign import ccall "&sk_font_measure_text_no_return" p'sk_font_measure_text_no_return ::
  FunPtr (Ptr (Sk_font) -> Ptr (()) -> CSize -> Sk_text_encoding -> Ptr (Sk_rect) -> Ptr (Sk_paint) -> Ptr (CFloat) -> IO (()))

{- | C function signature:

@
void sk_font_get_widths_bounds(const sk_font_t *font, const uint16_t glyphs[], int count, float widths[], sk_rect_t bounds[], const sk_paint_t *paint)
@
-}
foreign import ccall "sk_font_get_widths_bounds" sk_font_get_widths_bounds ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> Ptr (Word16) -- ^ C argument @"const uint16_t [] glyphs"@
  -> CInt -- ^ C argument @"int count"@
  -> Ptr (CFloat) -- ^ C argument @"float [] widths"@
  -> Ptr (Sk_rect) -- ^ C argument @"sk_rect_t [] bounds"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * paint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_get_widths_bounds'
foreign import ccall "&sk_font_get_widths_bounds" p'sk_font_get_widths_bounds ::
  FunPtr (Ptr (Sk_font) -> Ptr (Word16) -> CInt -> Ptr (CFloat) -> Ptr (Sk_rect) -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void sk_font_get_pos(const sk_font_t *font, const uint16_t glyphs[], int count, sk_point_t pos[], sk_point_t *origin)
@
-}
foreign import ccall "sk_font_get_pos" sk_font_get_pos ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> Ptr (Word16) -- ^ C argument @"const uint16_t [] glyphs"@
  -> CInt -- ^ C argument @"int count"@
  -> Ptr (Sk_point) -- ^ C argument @"sk_point_t [] pos"@
  -> Ptr (Sk_point) -- ^ C argument @"sk_point_t * origin"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_get_pos'
foreign import ccall "&sk_font_get_pos" p'sk_font_get_pos ::
  FunPtr (Ptr (Sk_font) -> Ptr (Word16) -> CInt -> Ptr (Sk_point) -> Ptr (Sk_point) -> IO (()))

{- | C function signature:

@
void sk_font_get_xpos(const sk_font_t *font, const uint16_t glyphs[], int count, float xpos[], float origin)
@
-}
foreign import ccall "sk_font_get_xpos" sk_font_get_xpos ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> Ptr (Word16) -- ^ C argument @"const uint16_t [] glyphs"@
  -> CInt -- ^ C argument @"int count"@
  -> Ptr (CFloat) -- ^ C argument @"float [] xpos"@
  -> CFloat -- ^ C argument @"float origin"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_get_xpos'
foreign import ccall "&sk_font_get_xpos" p'sk_font_get_xpos ::
  FunPtr (Ptr (Sk_font) -> Ptr (Word16) -> CInt -> Ptr (CFloat) -> CFloat -> IO (()))

{- | C function signature:

@
_Bool sk_font_get_path(const sk_font_t *font, uint16_t glyph, sk_path_t *path)
@
-}
foreign import ccall "sk_font_get_path" sk_font_get_path ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> Word16 -- ^ C argument @"uint16_t glyph"@
  -> Ptr (Sk_path) -- ^ C argument @"sk_path_t * path"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_font_get_path'
foreign import ccall "&sk_font_get_path" p'sk_font_get_path ::
  FunPtr (Ptr (Sk_font) -> Word16 -> Ptr (Sk_path) -> IO (CBool))

{- | C function signature:

@
void sk_font_get_paths(const sk_font_t *font, uint16_t glyphs[], int count, const sk_glyph_path_proc glyphPathProc, void *context)
@
-}
foreign import ccall "sk_font_get_paths" sk_font_get_paths ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> Ptr (Word16) -- ^ C argument @"uint16_t [] glyphs"@
  -> CInt -- ^ C argument @"int count"@
  -> FunPtr Sk_glyph_path_proc -- ^ C argument @"const sk_glyph_path_proc glyphPathProc"@
  -> Ptr (()) -- ^ C argument @"void * context"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_font_get_paths'
foreign import ccall "&sk_font_get_paths" p'sk_font_get_paths ::
  FunPtr (Ptr (Sk_font) -> Ptr (Word16) -> CInt -> FunPtr Sk_glyph_path_proc -> Ptr (()) -> IO (()))

{- | C function signature:

@
float sk_font_get_metrics(const sk_font_t *font, sk_fontmetrics_t *metrics)
@
-}
foreign import ccall "sk_font_get_metrics" sk_font_get_metrics ::
  Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> Ptr (Sk_fontmetrics) -- ^ C argument @"sk_fontmetrics_t * metrics"@
  -> IO (CFloat) -- ^ C return type: @"float"@

-- | Function pointer to 'sk_font_get_metrics'
foreign import ccall "&sk_font_get_metrics" p'sk_font_get_metrics ::
  FunPtr (Ptr (Sk_font) -> Ptr (Sk_fontmetrics) -> IO (CFloat))

{- | C function signature:

@
void sk_text_utils_get_path(const void *text, size_t length, sk_text_encoding_t encoding, float x, float y, const sk_font_t *font, sk_path_t *path)
@
-}
foreign import ccall "sk_text_utils_get_path" sk_text_utils_get_path ::
  Ptr (()) -- ^ C argument @"const void * text"@
  -> CSize -- ^ C argument @"size_t length"@
  -> Sk_text_encoding -- ^ C argument @"sk_text_encoding_t encoding"@
  -> CFloat -- ^ C argument @"float x"@
  -> CFloat -- ^ C argument @"float y"@
  -> Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> Ptr (Sk_path) -- ^ C argument @"sk_path_t * path"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_text_utils_get_path'
foreign import ccall "&sk_text_utils_get_path" p'sk_text_utils_get_path ::
  FunPtr (Ptr (()) -> CSize -> Sk_text_encoding -> CFloat -> CFloat -> Ptr (Sk_font) -> Ptr (Sk_path) -> IO (()))
