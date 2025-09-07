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
module Skia.Bindings.Skparagraph where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
skparagraph_text_style_t *skparagraph_text_style_new(void)
@
-}
foreign import ccall "skparagraph_text_style_new" skparagraph_text_style_new ::
  IO (Ptr (Skparagraph_text_style)) -- ^ C return type: @"skparagraph_text_style_t *"@

-- | Function pointer to 'skparagraph_text_style_new'
foreign import ccall "&skparagraph_text_style_new" p'skparagraph_text_style_new ::
  FunPtr (IO (Ptr (Skparagraph_text_style)))

{- | C function signature:

@
void skparagraph_text_style_delete(skparagraph_text_style_t *style)
@
-}
foreign import ccall "skparagraph_text_style_delete" skparagraph_text_style_delete ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_delete'
foreign import ccall "&skparagraph_text_style_delete" p'skparagraph_text_style_delete ::
  FunPtr (Ptr (Skparagraph_text_style) -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_color(skparagraph_text_style_t *style, sk_color_t color)
@
-}
foreign import ccall "skparagraph_text_style_set_color" skparagraph_text_style_set_color ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Sk_color -- ^ C argument @"sk_color_t color"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_color'
foreign import ccall "&skparagraph_text_style_set_color" p'skparagraph_text_style_set_color ::
  FunPtr (Ptr (Skparagraph_text_style) -> Sk_color -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_foreground_paint(skparagraph_text_style_t *style, sk_paint_t *paint)
@
-}
foreign import ccall "skparagraph_text_style_set_foreground_paint" skparagraph_text_style_set_foreground_paint ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Ptr (Sk_paint) -- ^ C argument @"sk_paint_t * paint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_foreground_paint'
foreign import ccall "&skparagraph_text_style_set_foreground_paint" p'skparagraph_text_style_set_foreground_paint ::
  FunPtr (Ptr (Skparagraph_text_style) -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_clear_foreground_paint(skparagraph_text_style_t *style)
@
-}
foreign import ccall "skparagraph_text_style_clear_foreground_paint" skparagraph_text_style_clear_foreground_paint ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_clear_foreground_paint'
foreign import ccall "&skparagraph_text_style_clear_foreground_paint" p'skparagraph_text_style_clear_foreground_paint ::
  FunPtr (Ptr (Skparagraph_text_style) -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_background_paint(skparagraph_text_style_t *style, sk_paint_t *paint)
@
-}
foreign import ccall "skparagraph_text_style_set_background_paint" skparagraph_text_style_set_background_paint ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Ptr (Sk_paint) -- ^ C argument @"sk_paint_t * paint"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_background_paint'
foreign import ccall "&skparagraph_text_style_set_background_paint" p'skparagraph_text_style_set_background_paint ::
  FunPtr (Ptr (Skparagraph_text_style) -> Ptr (Sk_paint) -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_clear_background_paint(skparagraph_text_style_t *style)
@
-}
foreign import ccall "skparagraph_text_style_clear_background_paint" skparagraph_text_style_clear_background_paint ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_clear_background_paint'
foreign import ccall "&skparagraph_text_style_clear_background_paint" p'skparagraph_text_style_clear_background_paint ::
  FunPtr (Ptr (Skparagraph_text_style) -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_decoration(skparagraph_text_style_t *style, skparagraph_text_decoration_flags_t decoration)
@
-}
foreign import ccall "skparagraph_text_style_set_decoration" skparagraph_text_style_set_decoration ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Skparagraph_text_decoration_flags -- ^ C argument @"skparagraph_text_decoration_flags_t decoration"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_decoration'
foreign import ccall "&skparagraph_text_style_set_decoration" p'skparagraph_text_style_set_decoration ::
  FunPtr (Ptr (Skparagraph_text_style) -> Skparagraph_text_decoration_flags -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_decoration_mode(skparagraph_text_style_t *style, skparagraph_text_decoration_mode_t mode)
@
-}
foreign import ccall "skparagraph_text_style_set_decoration_mode" skparagraph_text_style_set_decoration_mode ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Skparagraph_text_decoration_mode -- ^ C argument @"skparagraph_text_decoration_mode_t mode"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_decoration_mode'
foreign import ccall "&skparagraph_text_style_set_decoration_mode" p'skparagraph_text_style_set_decoration_mode ::
  FunPtr (Ptr (Skparagraph_text_style) -> Skparagraph_text_decoration_mode -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_decoration_style(skparagraph_text_style_t *style, skparagraph_text_decoration_style_t st)
@
-}
foreign import ccall "skparagraph_text_style_set_decoration_style" skparagraph_text_style_set_decoration_style ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Skparagraph_text_decoration_style -- ^ C argument @"skparagraph_text_decoration_style_t st"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_decoration_style'
foreign import ccall "&skparagraph_text_style_set_decoration_style" p'skparagraph_text_style_set_decoration_style ::
  FunPtr (Ptr (Skparagraph_text_style) -> Skparagraph_text_decoration_style -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_decoration_color(skparagraph_text_style_t *style, sk_color_t color)
@
-}
foreign import ccall "skparagraph_text_style_set_decoration_color" skparagraph_text_style_set_decoration_color ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Sk_color -- ^ C argument @"sk_color_t color"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_decoration_color'
foreign import ccall "&skparagraph_text_style_set_decoration_color" p'skparagraph_text_style_set_decoration_color ::
  FunPtr (Ptr (Skparagraph_text_style) -> Sk_color -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_decoration_thickness_multipler(skparagraph_text_style_t *style, sk_scalar_t m)
@
-}
foreign import ccall "skparagraph_text_style_set_decoration_thickness_multipler" skparagraph_text_style_set_decoration_thickness_multipler ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t m"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_decoration_thickness_multipler'
foreign import ccall "&skparagraph_text_style_set_decoration_thickness_multipler" p'skparagraph_text_style_set_decoration_thickness_multipler ::
  FunPtr (Ptr (Skparagraph_text_style) -> Sk_scalar -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_font_style(skparagraph_text_style_t *style, sk_fontstyle_t *fontStyle)
@
-}
foreign import ccall "skparagraph_text_style_set_font_style" skparagraph_text_style_set_font_style ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Ptr (Sk_fontstyle) -- ^ C argument @"sk_fontstyle_t * fontStyle"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_font_style'
foreign import ccall "&skparagraph_text_style_set_font_style" p'skparagraph_text_style_set_font_style ::
  FunPtr (Ptr (Skparagraph_text_style) -> Ptr (Sk_fontstyle) -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_add_shadow(skparagraph_text_style_t *style, skparagraph_text_shadow_t *shadow)
@
-}
foreign import ccall "skparagraph_text_style_add_shadow" skparagraph_text_style_add_shadow ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Ptr (Skparagraph_text_shadow) -- ^ C argument @"skparagraph_text_shadow_t * shadow"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_add_shadow'
foreign import ccall "&skparagraph_text_style_add_shadow" p'skparagraph_text_style_add_shadow ::
  FunPtr (Ptr (Skparagraph_text_style) -> Ptr (Skparagraph_text_shadow) -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_reset_shadow(skparagraph_text_style_t *style)
@
-}
foreign import ccall "skparagraph_text_style_reset_shadow" skparagraph_text_style_reset_shadow ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_reset_shadow'
foreign import ccall "&skparagraph_text_style_reset_shadow" p'skparagraph_text_style_reset_shadow ::
  FunPtr (Ptr (Skparagraph_text_style) -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_add_font_feature(skparagraph_text_style_t *style, sk_string_t *fontFeature, int value)
@
-}
foreign import ccall "skparagraph_text_style_add_font_feature" skparagraph_text_style_add_font_feature ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Ptr (Sk_string) -- ^ C argument @"sk_string_t * fontFeature"@
  -> CInt -- ^ C argument @"int value"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_add_font_feature'
foreign import ccall "&skparagraph_text_style_add_font_feature" p'skparagraph_text_style_add_font_feature ::
  FunPtr (Ptr (Skparagraph_text_style) -> Ptr (Sk_string) -> CInt -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_reset_font_features(skparagraph_text_style_t *style)
@
-}
foreign import ccall "skparagraph_text_style_reset_font_features" skparagraph_text_style_reset_font_features ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_reset_font_features'
foreign import ccall "&skparagraph_text_style_reset_font_features" p'skparagraph_text_style_reset_font_features ::
  FunPtr (Ptr (Skparagraph_text_style) -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_font_arguments(skparagraph_text_style_t *style, sk_fontarguments_t *args)
@
-}
foreign import ccall "skparagraph_text_style_set_font_arguments" skparagraph_text_style_set_font_arguments ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Ptr (Sk_fontarguments) -- ^ C argument @"sk_fontarguments_t * args"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_font_arguments'
foreign import ccall "&skparagraph_text_style_set_font_arguments" p'skparagraph_text_style_set_font_arguments ::
  FunPtr (Ptr (Skparagraph_text_style) -> Ptr (Sk_fontarguments) -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_font_size(skparagraph_text_style_t *style, sk_scalar_t size)
@
-}
foreign import ccall "skparagraph_text_style_set_font_size" skparagraph_text_style_set_font_size ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t size"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_font_size'
foreign import ccall "&skparagraph_text_style_set_font_size" p'skparagraph_text_style_set_font_size ::
  FunPtr (Ptr (Skparagraph_text_style) -> Sk_scalar -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_font_families(skparagraph_text_style_t *style, sk_string_t *families[], int familiesCount)
@
-}
foreign import ccall "skparagraph_text_style_set_font_families" skparagraph_text_style_set_font_families ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Ptr (Ptr (Sk_string)) -- ^ C argument @"sk_string_t *[] families"@
  -> CInt -- ^ C argument @"int familiesCount"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_font_families'
foreign import ccall "&skparagraph_text_style_set_font_families" p'skparagraph_text_style_set_font_families ::
  FunPtr (Ptr (Skparagraph_text_style) -> Ptr (Ptr (Sk_string)) -> CInt -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_baseline_shift(skparagraph_text_style_t *style, sk_scalar_t shift)
@
-}
foreign import ccall "skparagraph_text_style_set_baseline_shift" skparagraph_text_style_set_baseline_shift ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t shift"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_baseline_shift'
foreign import ccall "&skparagraph_text_style_set_baseline_shift" p'skparagraph_text_style_set_baseline_shift ::
  FunPtr (Ptr (Skparagraph_text_style) -> Sk_scalar -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_height(skparagraph_text_style_t *style, sk_scalar_t height)
@
-}
foreign import ccall "skparagraph_text_style_set_height" skparagraph_text_style_set_height ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t height"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_height'
foreign import ccall "&skparagraph_text_style_set_height" p'skparagraph_text_style_set_height ::
  FunPtr (Ptr (Skparagraph_text_style) -> Sk_scalar -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_height_override(skparagraph_text_style_t *style, sk_scalar_t height)
@
-}
foreign import ccall "skparagraph_text_style_set_height_override" skparagraph_text_style_set_height_override ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t height"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_height_override'
foreign import ccall "&skparagraph_text_style_set_height_override" p'skparagraph_text_style_set_height_override ::
  FunPtr (Ptr (Skparagraph_text_style) -> Sk_scalar -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_half_leading(skparagraph_text_style_t *style, sk_scalar_t height)
@
-}
foreign import ccall "skparagraph_text_style_set_half_leading" skparagraph_text_style_set_half_leading ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t height"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_half_leading'
foreign import ccall "&skparagraph_text_style_set_half_leading" p'skparagraph_text_style_set_half_leading ::
  FunPtr (Ptr (Skparagraph_text_style) -> Sk_scalar -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_letter_spacing(skparagraph_text_style_t *style, sk_scalar_t letterSpacing)
@
-}
foreign import ccall "skparagraph_text_style_set_letter_spacing" skparagraph_text_style_set_letter_spacing ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t letterSpacing"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_letter_spacing'
foreign import ccall "&skparagraph_text_style_set_letter_spacing" p'skparagraph_text_style_set_letter_spacing ::
  FunPtr (Ptr (Skparagraph_text_style) -> Sk_scalar -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_word_spacing(skparagraph_text_style_t *style, sk_scalar_t wordSpacing)
@
-}
foreign import ccall "skparagraph_text_style_set_word_spacing" skparagraph_text_style_set_word_spacing ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t wordSpacing"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_word_spacing'
foreign import ccall "&skparagraph_text_style_set_word_spacing" p'skparagraph_text_style_set_word_spacing ::
  FunPtr (Ptr (Skparagraph_text_style) -> Sk_scalar -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_typeface(skparagraph_text_style_t *style, sk_typeface_t *typeface)
@
-}
foreign import ccall "skparagraph_text_style_set_typeface" skparagraph_text_style_set_typeface ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Ptr (Sk_typeface) -- ^ C argument @"sk_typeface_t * typeface"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_typeface'
foreign import ccall "&skparagraph_text_style_set_typeface" p'skparagraph_text_style_set_typeface ::
  FunPtr (Ptr (Skparagraph_text_style) -> Ptr (Sk_typeface) -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_locale(skparagraph_text_style_t *style, sk_string_t *locale)
@
-}
foreign import ccall "skparagraph_text_style_set_locale" skparagraph_text_style_set_locale ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Ptr (Sk_string) -- ^ C argument @"sk_string_t * locale"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_locale'
foreign import ccall "&skparagraph_text_style_set_locale" p'skparagraph_text_style_set_locale ::
  FunPtr (Ptr (Skparagraph_text_style) -> Ptr (Sk_string) -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_text_baseline(skparagraph_text_style_t *style, skparagraph_text_baseline_t baseline)
@
-}
foreign import ccall "skparagraph_text_style_set_text_baseline" skparagraph_text_style_set_text_baseline ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> Skparagraph_text_baseline -- ^ C argument @"skparagraph_text_baseline_t baseline"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_text_baseline'
foreign import ccall "&skparagraph_text_style_set_text_baseline" p'skparagraph_text_style_set_text_baseline ::
  FunPtr (Ptr (Skparagraph_text_style) -> Skparagraph_text_baseline -> IO (()))

{- | C function signature:

@
void skparagraph_text_style_set_placeholder(skparagraph_text_style_t *style)
@
-}
foreign import ccall "skparagraph_text_style_set_placeholder" skparagraph_text_style_set_placeholder ::
  Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_text_style_set_placeholder'
foreign import ccall "&skparagraph_text_style_set_placeholder" p'skparagraph_text_style_set_placeholder ::
  FunPtr (Ptr (Skparagraph_text_style) -> IO (()))

{- | C function signature:

@
skparagraph_strut_style_t *skparagraph_strut_style_new(void)
@
-}
foreign import ccall "skparagraph_strut_style_new" skparagraph_strut_style_new ::
  IO (Ptr (Skparagraph_strut_style)) -- ^ C return type: @"skparagraph_strut_style_t *"@

-- | Function pointer to 'skparagraph_strut_style_new'
foreign import ccall "&skparagraph_strut_style_new" p'skparagraph_strut_style_new ::
  FunPtr (IO (Ptr (Skparagraph_strut_style)))

{- | C function signature:

@
void skparagraph_strut_style_delete(skparagraph_strut_style_t *strut)
@
-}
foreign import ccall "skparagraph_strut_style_delete" skparagraph_strut_style_delete ::
  Ptr (Skparagraph_strut_style) -- ^ C argument @"skparagraph_strut_style_t * strut"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_strut_style_delete'
foreign import ccall "&skparagraph_strut_style_delete" p'skparagraph_strut_style_delete ::
  FunPtr (Ptr (Skparagraph_strut_style) -> IO (()))

{- | C function signature:

@
void skparagraph_strut_style_set_font_families(skparagraph_strut_style_t *strut, sk_string_t *families[], int familiesCount)
@
-}
foreign import ccall "skparagraph_strut_style_set_font_families" skparagraph_strut_style_set_font_families ::
  Ptr (Skparagraph_strut_style) -- ^ C argument @"skparagraph_strut_style_t * strut"@
  -> Ptr (Ptr (Sk_string)) -- ^ C argument @"sk_string_t *[] families"@
  -> CInt -- ^ C argument @"int familiesCount"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_strut_style_set_font_families'
foreign import ccall "&skparagraph_strut_style_set_font_families" p'skparagraph_strut_style_set_font_families ::
  FunPtr (Ptr (Skparagraph_strut_style) -> Ptr (Ptr (Sk_string)) -> CInt -> IO (()))

{- | C function signature:

@
void skparagraph_strut_style_set_font_style(skparagraph_strut_style_t *strut, sk_fontstyle_t *style)
@
-}
foreign import ccall "skparagraph_strut_style_set_font_style" skparagraph_strut_style_set_font_style ::
  Ptr (Skparagraph_strut_style) -- ^ C argument @"skparagraph_strut_style_t * strut"@
  -> Ptr (Sk_fontstyle) -- ^ C argument @"sk_fontstyle_t * style"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_strut_style_set_font_style'
foreign import ccall "&skparagraph_strut_style_set_font_style" p'skparagraph_strut_style_set_font_style ::
  FunPtr (Ptr (Skparagraph_strut_style) -> Ptr (Sk_fontstyle) -> IO (()))

{- | C function signature:

@
void skparagraph_strut_style_set_font_size(skparagraph_strut_style_t *strut, sk_scalar_t size)
@
-}
foreign import ccall "skparagraph_strut_style_set_font_size" skparagraph_strut_style_set_font_size ::
  Ptr (Skparagraph_strut_style) -- ^ C argument @"skparagraph_strut_style_t * strut"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t size"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_strut_style_set_font_size'
foreign import ccall "&skparagraph_strut_style_set_font_size" p'skparagraph_strut_style_set_font_size ::
  FunPtr (Ptr (Skparagraph_strut_style) -> Sk_scalar -> IO (()))

{- | C function signature:

@
void skparagraph_strut_style_set_height(skparagraph_strut_style_t *strut, sk_scalar_t height)
@
-}
foreign import ccall "skparagraph_strut_style_set_height" skparagraph_strut_style_set_height ::
  Ptr (Skparagraph_strut_style) -- ^ C argument @"skparagraph_strut_style_t * strut"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t height"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_strut_style_set_height'
foreign import ccall "&skparagraph_strut_style_set_height" p'skparagraph_strut_style_set_height ::
  FunPtr (Ptr (Skparagraph_strut_style) -> Sk_scalar -> IO (()))

{- | C function signature:

@
void skparagraph_strut_style_set_leading(skparagraph_strut_style_t *strut, sk_scalar_t leading)
@
-}
foreign import ccall "skparagraph_strut_style_set_leading" skparagraph_strut_style_set_leading ::
  Ptr (Skparagraph_strut_style) -- ^ C argument @"skparagraph_strut_style_t * strut"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t leading"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_strut_style_set_leading'
foreign import ccall "&skparagraph_strut_style_set_leading" p'skparagraph_strut_style_set_leading ::
  FunPtr (Ptr (Skparagraph_strut_style) -> Sk_scalar -> IO (()))

{- | C function signature:

@
void skparagraph_strut_style_set_strut_enabled(skparagraph_strut_style_t *strut, _Bool v)
@
-}
foreign import ccall "skparagraph_strut_style_set_strut_enabled" skparagraph_strut_style_set_strut_enabled ::
  Ptr (Skparagraph_strut_style) -- ^ C argument @"skparagraph_strut_style_t * strut"@
  -> CBool -- ^ C argument @"_Bool v"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_strut_style_set_strut_enabled'
foreign import ccall "&skparagraph_strut_style_set_strut_enabled" p'skparagraph_strut_style_set_strut_enabled ::
  FunPtr (Ptr (Skparagraph_strut_style) -> CBool -> IO (()))

{- | C function signature:

@
void skparagraph_strut_style_set_force_strut_height(skparagraph_strut_style_t *strut, _Bool v)
@
-}
foreign import ccall "skparagraph_strut_style_set_force_strut_height" skparagraph_strut_style_set_force_strut_height ::
  Ptr (Skparagraph_strut_style) -- ^ C argument @"skparagraph_strut_style_t * strut"@
  -> CBool -- ^ C argument @"_Bool v"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_strut_style_set_force_strut_height'
foreign import ccall "&skparagraph_strut_style_set_force_strut_height" p'skparagraph_strut_style_set_force_strut_height ::
  FunPtr (Ptr (Skparagraph_strut_style) -> CBool -> IO (()))

{- | C function signature:

@
void skparagraph_strut_style_set_height_override(skparagraph_strut_style_t *strut, _Bool v)
@
-}
foreign import ccall "skparagraph_strut_style_set_height_override" skparagraph_strut_style_set_height_override ::
  Ptr (Skparagraph_strut_style) -- ^ C argument @"skparagraph_strut_style_t * strut"@
  -> CBool -- ^ C argument @"_Bool v"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_strut_style_set_height_override'
foreign import ccall "&skparagraph_strut_style_set_height_override" p'skparagraph_strut_style_set_height_override ::
  FunPtr (Ptr (Skparagraph_strut_style) -> CBool -> IO (()))

{- | C function signature:

@
void skparagraph_strut_style_set_half_leading(skparagraph_strut_style_t *strut, _Bool halfLeading)
@
-}
foreign import ccall "skparagraph_strut_style_set_half_leading" skparagraph_strut_style_set_half_leading ::
  Ptr (Skparagraph_strut_style) -- ^ C argument @"skparagraph_strut_style_t * strut"@
  -> CBool -- ^ C argument @"_Bool halfLeading"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_strut_style_set_half_leading'
foreign import ccall "&skparagraph_strut_style_set_half_leading" p'skparagraph_strut_style_set_half_leading ::
  FunPtr (Ptr (Skparagraph_strut_style) -> CBool -> IO (()))

{- | C function signature:

@
skparagraph_paragraph_style_t *skparagraph_paragraph_style_new(void)
@
-}
foreign import ccall "skparagraph_paragraph_style_new" skparagraph_paragraph_style_new ::
  IO (Ptr (Skparagraph_paragraph_style)) -- ^ C return type: @"skparagraph_paragraph_style_t *"@

-- | Function pointer to 'skparagraph_paragraph_style_new'
foreign import ccall "&skparagraph_paragraph_style_new" p'skparagraph_paragraph_style_new ::
  FunPtr (IO (Ptr (Skparagraph_paragraph_style)))

{- | C function signature:

@
void skparagraph_paragraph_style_delete(skparagraph_paragraph_style_t *style)
@
-}
foreign import ccall "skparagraph_paragraph_style_delete" skparagraph_paragraph_style_delete ::
  Ptr (Skparagraph_paragraph_style) -- ^ C argument @"skparagraph_paragraph_style_t * style"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_style_delete'
foreign import ccall "&skparagraph_paragraph_style_delete" p'skparagraph_paragraph_style_delete ::
  FunPtr (Ptr (Skparagraph_paragraph_style) -> IO (()))

{- | C function signature:

@
void skparagraph_paragraph_style_set_strut_style(skparagraph_paragraph_style_t *para, skparagraph_strut_style_t *strut)
@
-}
foreign import ccall "skparagraph_paragraph_style_set_strut_style" skparagraph_paragraph_style_set_strut_style ::
  Ptr (Skparagraph_paragraph_style) -- ^ C argument @"skparagraph_paragraph_style_t * para"@
  -> Ptr (Skparagraph_strut_style) -- ^ C argument @"skparagraph_strut_style_t * strut"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_style_set_strut_style'
foreign import ccall "&skparagraph_paragraph_style_set_strut_style" p'skparagraph_paragraph_style_set_strut_style ::
  FunPtr (Ptr (Skparagraph_paragraph_style) -> Ptr (Skparagraph_strut_style) -> IO (()))

{- | C function signature:

@
void skparagraph_paragraph_style_set_text_style(skparagraph_paragraph_style_t *para, skparagraph_text_style_t *text)
@
-}
foreign import ccall "skparagraph_paragraph_style_set_text_style" skparagraph_paragraph_style_set_text_style ::
  Ptr (Skparagraph_paragraph_style) -- ^ C argument @"skparagraph_paragraph_style_t * para"@
  -> Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * text"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_style_set_text_style'
foreign import ccall "&skparagraph_paragraph_style_set_text_style" p'skparagraph_paragraph_style_set_text_style ::
  FunPtr (Ptr (Skparagraph_paragraph_style) -> Ptr (Skparagraph_text_style) -> IO (()))

{- | C function signature:

@
void skparagraph_paragraph_style_set_text_direction(skparagraph_paragraph_style_t *para, skparagraph_text_direction_t dir)
@
-}
foreign import ccall "skparagraph_paragraph_style_set_text_direction" skparagraph_paragraph_style_set_text_direction ::
  Ptr (Skparagraph_paragraph_style) -- ^ C argument @"skparagraph_paragraph_style_t * para"@
  -> Skparagraph_text_direction -- ^ C argument @"skparagraph_text_direction_t dir"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_style_set_text_direction'
foreign import ccall "&skparagraph_paragraph_style_set_text_direction" p'skparagraph_paragraph_style_set_text_direction ::
  FunPtr (Ptr (Skparagraph_paragraph_style) -> Skparagraph_text_direction -> IO (()))

{- | C function signature:

@
void skparagraph_paragraph_style_set_text_align(skparagraph_paragraph_style_t *para, skparagraph_text_align_t align)
@
-}
foreign import ccall "skparagraph_paragraph_style_set_text_align" skparagraph_paragraph_style_set_text_align ::
  Ptr (Skparagraph_paragraph_style) -- ^ C argument @"skparagraph_paragraph_style_t * para"@
  -> Skparagraph_text_align -- ^ C argument @"skparagraph_text_align_t align"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_style_set_text_align'
foreign import ccall "&skparagraph_paragraph_style_set_text_align" p'skparagraph_paragraph_style_set_text_align ::
  FunPtr (Ptr (Skparagraph_paragraph_style) -> Skparagraph_text_align -> IO (()))

{- | C function signature:

@
void skparagraph_paragraph_style_set_max_lines(skparagraph_paragraph_style_t *para, size_t max_lines)
@
-}
foreign import ccall "skparagraph_paragraph_style_set_max_lines" skparagraph_paragraph_style_set_max_lines ::
  Ptr (Skparagraph_paragraph_style) -- ^ C argument @"skparagraph_paragraph_style_t * para"@
  -> CSize -- ^ C argument @"size_t max_lines"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_style_set_max_lines'
foreign import ccall "&skparagraph_paragraph_style_set_max_lines" p'skparagraph_paragraph_style_set_max_lines ::
  FunPtr (Ptr (Skparagraph_paragraph_style) -> CSize -> IO (()))

{- | C function signature:

@
void skparagraph_paragraph_style_set_ellipsis(skparagraph_paragraph_style_t *para, const sk_string_t *ellipsis)
@
-}
foreign import ccall "skparagraph_paragraph_style_set_ellipsis" skparagraph_paragraph_style_set_ellipsis ::
  Ptr (Skparagraph_paragraph_style) -- ^ C argument @"skparagraph_paragraph_style_t * para"@
  -> Ptr (Sk_string) -- ^ C argument @"const sk_string_t * ellipsis"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_style_set_ellipsis'
foreign import ccall "&skparagraph_paragraph_style_set_ellipsis" p'skparagraph_paragraph_style_set_ellipsis ::
  FunPtr (Ptr (Skparagraph_paragraph_style) -> Ptr (Sk_string) -> IO (()))

{- | C function signature:

@
void skparagraph_paragraph_style_set_height(skparagraph_paragraph_style_t *para, sk_scalar_t height)
@
-}
foreign import ccall "skparagraph_paragraph_style_set_height" skparagraph_paragraph_style_set_height ::
  Ptr (Skparagraph_paragraph_style) -- ^ C argument @"skparagraph_paragraph_style_t * para"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t height"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_style_set_height'
foreign import ccall "&skparagraph_paragraph_style_set_height" p'skparagraph_paragraph_style_set_height ::
  FunPtr (Ptr (Skparagraph_paragraph_style) -> Sk_scalar -> IO (()))

{- | C function signature:

@
void skparagraph_paragraph_style_set_text_height_behavior(skparagraph_paragraph_style_t *para, skparagraph_text_height_behavior_t behavior)
@
-}
foreign import ccall "skparagraph_paragraph_style_set_text_height_behavior" skparagraph_paragraph_style_set_text_height_behavior ::
  Ptr (Skparagraph_paragraph_style) -- ^ C argument @"skparagraph_paragraph_style_t * para"@
  -> Skparagraph_text_height_behavior -- ^ C argument @"skparagraph_text_height_behavior_t behavior"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_style_set_text_height_behavior'
foreign import ccall "&skparagraph_paragraph_style_set_text_height_behavior" p'skparagraph_paragraph_style_set_text_height_behavior ::
  FunPtr (Ptr (Skparagraph_paragraph_style) -> Skparagraph_text_height_behavior -> IO (()))

{- | C function signature:

@
void skparagraph_paragraph_style_set_replace_tab_characters(skparagraph_paragraph_style_t *para, _Bool v)
@
-}
foreign import ccall "skparagraph_paragraph_style_set_replace_tab_characters" skparagraph_paragraph_style_set_replace_tab_characters ::
  Ptr (Skparagraph_paragraph_style) -- ^ C argument @"skparagraph_paragraph_style_t * para"@
  -> CBool -- ^ C argument @"_Bool v"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_style_set_replace_tab_characters'
foreign import ccall "&skparagraph_paragraph_style_set_replace_tab_characters" p'skparagraph_paragraph_style_set_replace_tab_characters ::
  FunPtr (Ptr (Skparagraph_paragraph_style) -> CBool -> IO (()))

{- | C function signature:

@
void skparagraph_paragraph_style_set_apply_rounding_hack(skparagraph_paragraph_style_t *para, _Bool v)
@
-}
foreign import ccall "skparagraph_paragraph_style_set_apply_rounding_hack" skparagraph_paragraph_style_set_apply_rounding_hack ::
  Ptr (Skparagraph_paragraph_style) -- ^ C argument @"skparagraph_paragraph_style_t * para"@
  -> CBool -- ^ C argument @"_Bool v"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_style_set_apply_rounding_hack'
foreign import ccall "&skparagraph_paragraph_style_set_apply_rounding_hack" p'skparagraph_paragraph_style_set_apply_rounding_hack ::
  FunPtr (Ptr (Skparagraph_paragraph_style) -> CBool -> IO (()))

{- | C function signature:

@
void skparagraph_paragraph_delete(skparagraph_paragraph_t *para)
@
-}
foreign import ccall "skparagraph_paragraph_delete" skparagraph_paragraph_delete ::
  Ptr (Skparagraph_paragraph) -- ^ C argument @"skparagraph_paragraph_t * para"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_delete'
foreign import ccall "&skparagraph_paragraph_delete" p'skparagraph_paragraph_delete ::
  FunPtr (Ptr (Skparagraph_paragraph) -> IO (()))

{- | C function signature:

@
sk_scalar_t skparagraph_paragraph_get_max_width(skparagraph_paragraph_t *paragraph)
@
-}
foreign import ccall "skparagraph_paragraph_get_max_width" skparagraph_paragraph_get_max_width ::
  Ptr (Skparagraph_paragraph) -- ^ C argument @"skparagraph_paragraph_t * paragraph"@
  -> IO (Sk_scalar) -- ^ C return type: @"sk_scalar_t"@

-- | Function pointer to 'skparagraph_paragraph_get_max_width'
foreign import ccall "&skparagraph_paragraph_get_max_width" p'skparagraph_paragraph_get_max_width ::
  FunPtr (Ptr (Skparagraph_paragraph) -> IO (Sk_scalar))

{- | C function signature:

@
sk_scalar_t skparagraph_paragraph_get_height(skparagraph_paragraph_t *paragraph)
@
-}
foreign import ccall "skparagraph_paragraph_get_height" skparagraph_paragraph_get_height ::
  Ptr (Skparagraph_paragraph) -- ^ C argument @"skparagraph_paragraph_t * paragraph"@
  -> IO (Sk_scalar) -- ^ C return type: @"sk_scalar_t"@

-- | Function pointer to 'skparagraph_paragraph_get_height'
foreign import ccall "&skparagraph_paragraph_get_height" p'skparagraph_paragraph_get_height ::
  FunPtr (Ptr (Skparagraph_paragraph) -> IO (Sk_scalar))

{- | C function signature:

@
sk_scalar_t skparagraph_paragraph_get_min_intrinsic_width(skparagraph_paragraph_t *paragraph)
@
-}
foreign import ccall "skparagraph_paragraph_get_min_intrinsic_width" skparagraph_paragraph_get_min_intrinsic_width ::
  Ptr (Skparagraph_paragraph) -- ^ C argument @"skparagraph_paragraph_t * paragraph"@
  -> IO (Sk_scalar) -- ^ C return type: @"sk_scalar_t"@

-- | Function pointer to 'skparagraph_paragraph_get_min_intrinsic_width'
foreign import ccall "&skparagraph_paragraph_get_min_intrinsic_width" p'skparagraph_paragraph_get_min_intrinsic_width ::
  FunPtr (Ptr (Skparagraph_paragraph) -> IO (Sk_scalar))

{- | C function signature:

@
sk_scalar_t skparagraph_paragraph_get_max_intrinsic_width(skparagraph_paragraph_t *paragraph)
@
-}
foreign import ccall "skparagraph_paragraph_get_max_intrinsic_width" skparagraph_paragraph_get_max_intrinsic_width ::
  Ptr (Skparagraph_paragraph) -- ^ C argument @"skparagraph_paragraph_t * paragraph"@
  -> IO (Sk_scalar) -- ^ C return type: @"sk_scalar_t"@

-- | Function pointer to 'skparagraph_paragraph_get_max_intrinsic_width'
foreign import ccall "&skparagraph_paragraph_get_max_intrinsic_width" p'skparagraph_paragraph_get_max_intrinsic_width ::
  FunPtr (Ptr (Skparagraph_paragraph) -> IO (Sk_scalar))

{- | C function signature:

@
sk_scalar_t skparagraph_paragraph_get_alphabetic_baseline(skparagraph_paragraph_t *paragraph)
@
-}
foreign import ccall "skparagraph_paragraph_get_alphabetic_baseline" skparagraph_paragraph_get_alphabetic_baseline ::
  Ptr (Skparagraph_paragraph) -- ^ C argument @"skparagraph_paragraph_t * paragraph"@
  -> IO (Sk_scalar) -- ^ C return type: @"sk_scalar_t"@

-- | Function pointer to 'skparagraph_paragraph_get_alphabetic_baseline'
foreign import ccall "&skparagraph_paragraph_get_alphabetic_baseline" p'skparagraph_paragraph_get_alphabetic_baseline ::
  FunPtr (Ptr (Skparagraph_paragraph) -> IO (Sk_scalar))

{- | C function signature:

@
sk_scalar_t skparagraph_paragraph_get_ideographic_baseline(skparagraph_paragraph_t *paragraph)
@
-}
foreign import ccall "skparagraph_paragraph_get_ideographic_baseline" skparagraph_paragraph_get_ideographic_baseline ::
  Ptr (Skparagraph_paragraph) -- ^ C argument @"skparagraph_paragraph_t * paragraph"@
  -> IO (Sk_scalar) -- ^ C return type: @"sk_scalar_t"@

-- | Function pointer to 'skparagraph_paragraph_get_ideographic_baseline'
foreign import ccall "&skparagraph_paragraph_get_ideographic_baseline" p'skparagraph_paragraph_get_ideographic_baseline ::
  FunPtr (Ptr (Skparagraph_paragraph) -> IO (Sk_scalar))

{- | C function signature:

@
sk_scalar_t skparagraph_paragraph_get_longest_line(skparagraph_paragraph_t *paragraph)
@
-}
foreign import ccall "skparagraph_paragraph_get_longest_line" skparagraph_paragraph_get_longest_line ::
  Ptr (Skparagraph_paragraph) -- ^ C argument @"skparagraph_paragraph_t * paragraph"@
  -> IO (Sk_scalar) -- ^ C return type: @"sk_scalar_t"@

-- | Function pointer to 'skparagraph_paragraph_get_longest_line'
foreign import ccall "&skparagraph_paragraph_get_longest_line" p'skparagraph_paragraph_get_longest_line ::
  FunPtr (Ptr (Skparagraph_paragraph) -> IO (Sk_scalar))

{- | C function signature:

@
_Bool skparagraph_paragraph_did_exceed_max_lines(skparagraph_paragraph_t *paragraph)
@
-}
foreign import ccall "skparagraph_paragraph_did_exceed_max_lines" skparagraph_paragraph_did_exceed_max_lines ::
  Ptr (Skparagraph_paragraph) -- ^ C argument @"skparagraph_paragraph_t * paragraph"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'skparagraph_paragraph_did_exceed_max_lines'
foreign import ccall "&skparagraph_paragraph_did_exceed_max_lines" p'skparagraph_paragraph_did_exceed_max_lines ::
  FunPtr (Ptr (Skparagraph_paragraph) -> IO (CBool))

{- | C function signature:

@
void skparagraph_paragraph_layout(skparagraph_paragraph_t *para, sk_scalar_t width)
@
-}
foreign import ccall "skparagraph_paragraph_layout" skparagraph_paragraph_layout ::
  Ptr (Skparagraph_paragraph) -- ^ C argument @"skparagraph_paragraph_t * para"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t width"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_layout'
foreign import ccall "&skparagraph_paragraph_layout" p'skparagraph_paragraph_layout ::
  FunPtr (Ptr (Skparagraph_paragraph) -> Sk_scalar -> IO (()))

{- | C function signature:

@
void skparagraph_paragraph_paint_with_canvas(skparagraph_paragraph_t *para, sk_canvas_t *canvas, sk_scalar_t x, sk_scalar_t y)
@
-}
foreign import ccall "skparagraph_paragraph_paint_with_canvas" skparagraph_paragraph_paint_with_canvas ::
  Ptr (Skparagraph_paragraph) -- ^ C argument @"skparagraph_paragraph_t * para"@
  -> Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * canvas"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t x"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t y"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_paint_with_canvas'
foreign import ccall "&skparagraph_paragraph_paint_with_canvas" p'skparagraph_paragraph_paint_with_canvas ::
  FunPtr (Ptr (Skparagraph_paragraph) -> Ptr (Sk_canvas) -> Sk_scalar -> Sk_scalar -> IO (()))

{- | C function signature:

@
void skparagraph_paragraph_paint_with_painter(skparagraph_paragraph_t *para, skparagraph_paragraph_painter_t *painter, sk_scalar_t x, sk_scalar_t y)
@
-}
foreign import ccall "skparagraph_paragraph_paint_with_painter" skparagraph_paragraph_paint_with_painter ::
  Ptr (Skparagraph_paragraph) -- ^ C argument @"skparagraph_paragraph_t * para"@
  -> Ptr (Skparagraph_paragraph_painter) -- ^ C argument @"skparagraph_paragraph_painter_t * painter"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t x"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t y"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_paint_with_painter'
foreign import ccall "&skparagraph_paragraph_paint_with_painter" p'skparagraph_paragraph_paint_with_painter ::
  FunPtr (Ptr (Skparagraph_paragraph) -> Ptr (Skparagraph_paragraph_painter) -> Sk_scalar -> Sk_scalar -> IO (()))

{- | C function signature:

@
int skparagraph_paragraph_get_line_number_at(skparagraph_paragraph_t *para, size_t codeUnitIndex)
@
-}
foreign import ccall "skparagraph_paragraph_get_line_number_at" skparagraph_paragraph_get_line_number_at ::
  Ptr (Skparagraph_paragraph) -- ^ C argument @"skparagraph_paragraph_t * para"@
  -> CSize -- ^ C argument @"size_t codeUnitIndex"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'skparagraph_paragraph_get_line_number_at'
foreign import ccall "&skparagraph_paragraph_get_line_number_at" p'skparagraph_paragraph_get_line_number_at ::
  FunPtr (Ptr (Skparagraph_paragraph) -> CSize -> IO (CInt))

{- | C function signature:

@
_Bool skparagraph_paragraph_get_line_metrics_at(skparagraph_paragraph_t *para, int lineNumber, skparagraph_line_metrics_t *dstMetrics)
@
-}
foreign import ccall "skparagraph_paragraph_get_line_metrics_at" skparagraph_paragraph_get_line_metrics_at ::
  Ptr (Skparagraph_paragraph) -- ^ C argument @"skparagraph_paragraph_t * para"@
  -> CInt -- ^ C argument @"int lineNumber"@
  -> Ptr (Skparagraph_line_metrics) -- ^ C argument @"skparagraph_line_metrics_t * dstMetrics"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'skparagraph_paragraph_get_line_metrics_at'
foreign import ccall "&skparagraph_paragraph_get_line_metrics_at" p'skparagraph_paragraph_get_line_metrics_at ::
  FunPtr (Ptr (Skparagraph_paragraph) -> CInt -> Ptr (Skparagraph_line_metrics) -> IO (CBool))

{- | C function signature:

@
_Bool skparagraph_paragraph_get_closest_glyph_cluster_at(skparagraph_paragraph_t *para, sk_scalar_t x, sk_scalar_t y, skparagraph_glyph_cluster_info_t *dstGlyphInfo)
@
-}
foreign import ccall "skparagraph_paragraph_get_closest_glyph_cluster_at" skparagraph_paragraph_get_closest_glyph_cluster_at ::
  Ptr (Skparagraph_paragraph) -- ^ C argument @"skparagraph_paragraph_t * para"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t x"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t y"@
  -> Ptr (Skparagraph_glyph_cluster_info) -- ^ C argument @"skparagraph_glyph_cluster_info_t * dstGlyphInfo"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'skparagraph_paragraph_get_closest_glyph_cluster_at'
foreign import ccall "&skparagraph_paragraph_get_closest_glyph_cluster_at" p'skparagraph_paragraph_get_closest_glyph_cluster_at ::
  FunPtr (Ptr (Skparagraph_paragraph) -> Sk_scalar -> Sk_scalar -> Ptr (Skparagraph_glyph_cluster_info) -> IO (CBool))

{- | C function signature:

@
_Bool skparagraph_paragraph_get_glyph_cluster_at(skparagraph_paragraph_t *para, int codeUnitIndex, skparagraph_glyph_cluster_info_t *dstGlyphInfo)
@
-}
foreign import ccall "skparagraph_paragraph_get_glyph_cluster_at" skparagraph_paragraph_get_glyph_cluster_at ::
  Ptr (Skparagraph_paragraph) -- ^ C argument @"skparagraph_paragraph_t * para"@
  -> CInt -- ^ C argument @"int codeUnitIndex"@
  -> Ptr (Skparagraph_glyph_cluster_info) -- ^ C argument @"skparagraph_glyph_cluster_info_t * dstGlyphInfo"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'skparagraph_paragraph_get_glyph_cluster_at'
foreign import ccall "&skparagraph_paragraph_get_glyph_cluster_at" p'skparagraph_paragraph_get_glyph_cluster_at ::
  FunPtr (Ptr (Skparagraph_paragraph) -> CInt -> Ptr (Skparagraph_glyph_cluster_info) -> IO (CBool))

{- | C function signature:

@
void skparagraph_paragraph_get_glyph_position_at_coordinate(skparagraph_paragraph_t *para, sk_scalar_t x, sk_scalar_t y, int32_t *dstPosition, skparagraph_affinity_t *dstAffinity)
@
-}
foreign import ccall "skparagraph_paragraph_get_glyph_position_at_coordinate" skparagraph_paragraph_get_glyph_position_at_coordinate ::
  Ptr (Skparagraph_paragraph) -- ^ C argument @"skparagraph_paragraph_t * para"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t x"@
  -> Sk_scalar -- ^ C argument @"sk_scalar_t y"@
  -> Ptr (Int32) -- ^ C argument @"int32_t * dstPosition"@
  -> Ptr (Skparagraph_affinity) -- ^ C argument @"skparagraph_affinity_t * dstAffinity"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_get_glyph_position_at_coordinate'
foreign import ccall "&skparagraph_paragraph_get_glyph_position_at_coordinate" p'skparagraph_paragraph_get_glyph_position_at_coordinate ::
  FunPtr (Ptr (Skparagraph_paragraph) -> Sk_scalar -> Sk_scalar -> Ptr (Int32) -> Ptr (Skparagraph_affinity) -> IO (()))

{- | C function signature:

@
skparagraph_font_collection_t *skparagraph_font_collection_new(void)
@
-}
foreign import ccall "skparagraph_font_collection_new" skparagraph_font_collection_new ::
  IO (Ptr (Skparagraph_font_collection)) -- ^ C return type: @"skparagraph_font_collection_t *"@

-- | Function pointer to 'skparagraph_font_collection_new'
foreign import ccall "&skparagraph_font_collection_new" p'skparagraph_font_collection_new ::
  FunPtr (IO (Ptr (Skparagraph_font_collection)))

{- | C function signature:

@
void skparagraph_font_collection_delete(skparagraph_font_collection_t *style)
@
-}
foreign import ccall "skparagraph_font_collection_delete" skparagraph_font_collection_delete ::
  Ptr (Skparagraph_font_collection) -- ^ C argument @"skparagraph_font_collection_t * style"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_font_collection_delete'
foreign import ccall "&skparagraph_font_collection_delete" p'skparagraph_font_collection_delete ::
  FunPtr (Ptr (Skparagraph_font_collection) -> IO (()))

{- | C function signature:

@
void skparagraph_font_collection_set_asset_fontmgr(skparagraph_font_collection_t *col, sk_fontmgr_t *fontmgr)
@
-}
foreign import ccall "skparagraph_font_collection_set_asset_fontmgr" skparagraph_font_collection_set_asset_fontmgr ::
  Ptr (Skparagraph_font_collection) -- ^ C argument @"skparagraph_font_collection_t * col"@
  -> Ptr (Sk_fontmgr) -- ^ C argument @"sk_fontmgr_t * fontmgr"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_font_collection_set_asset_fontmgr'
foreign import ccall "&skparagraph_font_collection_set_asset_fontmgr" p'skparagraph_font_collection_set_asset_fontmgr ::
  FunPtr (Ptr (Skparagraph_font_collection) -> Ptr (Sk_fontmgr) -> IO (()))

{- | C function signature:

@
void skparagraph_font_collection_set_dynamic_fontmgr(skparagraph_font_collection_t *col, sk_fontmgr_t *fontmgr)
@
-}
foreign import ccall "skparagraph_font_collection_set_dynamic_fontmgr" skparagraph_font_collection_set_dynamic_fontmgr ::
  Ptr (Skparagraph_font_collection) -- ^ C argument @"skparagraph_font_collection_t * col"@
  -> Ptr (Sk_fontmgr) -- ^ C argument @"sk_fontmgr_t * fontmgr"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_font_collection_set_dynamic_fontmgr'
foreign import ccall "&skparagraph_font_collection_set_dynamic_fontmgr" p'skparagraph_font_collection_set_dynamic_fontmgr ::
  FunPtr (Ptr (Skparagraph_font_collection) -> Ptr (Sk_fontmgr) -> IO (()))

{- | C function signature:

@
void skparagraph_font_collection_set_test_fontmgr(skparagraph_font_collection_t *col, sk_fontmgr_t *fontmgr)
@
-}
foreign import ccall "skparagraph_font_collection_set_test_fontmgr" skparagraph_font_collection_set_test_fontmgr ::
  Ptr (Skparagraph_font_collection) -- ^ C argument @"skparagraph_font_collection_t * col"@
  -> Ptr (Sk_fontmgr) -- ^ C argument @"sk_fontmgr_t * fontmgr"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_font_collection_set_test_fontmgr'
foreign import ccall "&skparagraph_font_collection_set_test_fontmgr" p'skparagraph_font_collection_set_test_fontmgr ::
  FunPtr (Ptr (Skparagraph_font_collection) -> Ptr (Sk_fontmgr) -> IO (()))

{- | C function signature:

@
void skparagraph_font_collection_set_default_fontmgr(skparagraph_font_collection_t *col, sk_fontmgr_t *fontmgr)
@
-}
foreign import ccall "skparagraph_font_collection_set_default_fontmgr" skparagraph_font_collection_set_default_fontmgr ::
  Ptr (Skparagraph_font_collection) -- ^ C argument @"skparagraph_font_collection_t * col"@
  -> Ptr (Sk_fontmgr) -- ^ C argument @"sk_fontmgr_t * fontmgr"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_font_collection_set_default_fontmgr'
foreign import ccall "&skparagraph_font_collection_set_default_fontmgr" p'skparagraph_font_collection_set_default_fontmgr ::
  FunPtr (Ptr (Skparagraph_font_collection) -> Ptr (Sk_fontmgr) -> IO (()))

{- | C function signature:

@
void skparagraph_font_collection_set_default_fontmgr_with_def_family(skparagraph_font_collection_t *col, sk_fontmgr_t *fontmgr, const char *defaultFamilyName)
@
-}
foreign import ccall "skparagraph_font_collection_set_default_fontmgr_with_def_family" skparagraph_font_collection_set_default_fontmgr_with_def_family ::
  Ptr (Skparagraph_font_collection) -- ^ C argument @"skparagraph_font_collection_t * col"@
  -> Ptr (Sk_fontmgr) -- ^ C argument @"sk_fontmgr_t * fontmgr"@
  -> Ptr (CChar) -- ^ C argument @"const char * defaultFamilyName"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_font_collection_set_default_fontmgr_with_def_family'
foreign import ccall "&skparagraph_font_collection_set_default_fontmgr_with_def_family" p'skparagraph_font_collection_set_default_fontmgr_with_def_family ::
  FunPtr (Ptr (Skparagraph_font_collection) -> Ptr (Sk_fontmgr) -> Ptr (CChar) -> IO (()))

{- | C function signature:

@
void skparagraph_font_collection_set_default_fontmgr_with_def_families(skparagraph_font_collection_t *col, sk_fontmgr_t *fontmgr, sk_string_t *defaultFamilyNames[], int defaultFamilyNamesCount)
@
-}
foreign import ccall "skparagraph_font_collection_set_default_fontmgr_with_def_families" skparagraph_font_collection_set_default_fontmgr_with_def_families ::
  Ptr (Skparagraph_font_collection) -- ^ C argument @"skparagraph_font_collection_t * col"@
  -> Ptr (Sk_fontmgr) -- ^ C argument @"sk_fontmgr_t * fontmgr"@
  -> Ptr (Ptr (Sk_string)) -- ^ C argument @"sk_string_t *[] defaultFamilyNames"@
  -> CInt -- ^ C argument @"int defaultFamilyNamesCount"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_font_collection_set_default_fontmgr_with_def_families'
foreign import ccall "&skparagraph_font_collection_set_default_fontmgr_with_def_families" p'skparagraph_font_collection_set_default_fontmgr_with_def_families ::
  FunPtr (Ptr (Skparagraph_font_collection) -> Ptr (Sk_fontmgr) -> Ptr (Ptr (Sk_string)) -> CInt -> IO (()))

{- | C function signature:

@
void skparagraph_font_collection_enable_font_fallback(skparagraph_font_collection_t *col)
@
-}
foreign import ccall "skparagraph_font_collection_enable_font_fallback" skparagraph_font_collection_enable_font_fallback ::
  Ptr (Skparagraph_font_collection) -- ^ C argument @"skparagraph_font_collection_t * col"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_font_collection_enable_font_fallback'
foreign import ccall "&skparagraph_font_collection_enable_font_fallback" p'skparagraph_font_collection_enable_font_fallback ::
  FunPtr (Ptr (Skparagraph_font_collection) -> IO (()))

{- | C function signature:

@
void skparagraph_font_collection_disable_font_fallback(skparagraph_font_collection_t *col)
@
-}
foreign import ccall "skparagraph_font_collection_disable_font_fallback" skparagraph_font_collection_disable_font_fallback ::
  Ptr (Skparagraph_font_collection) -- ^ C argument @"skparagraph_font_collection_t * col"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_font_collection_disable_font_fallback'
foreign import ccall "&skparagraph_font_collection_disable_font_fallback" p'skparagraph_font_collection_disable_font_fallback ::
  FunPtr (Ptr (Skparagraph_font_collection) -> IO (()))

{- | C function signature:

@
_Bool skparagraph_font_collection_font_fallback_enabled(skparagraph_font_collection_t *col)
@
-}
foreign import ccall "skparagraph_font_collection_font_fallback_enabled" skparagraph_font_collection_font_fallback_enabled ::
  Ptr (Skparagraph_font_collection) -- ^ C argument @"skparagraph_font_collection_t * col"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'skparagraph_font_collection_font_fallback_enabled'
foreign import ccall "&skparagraph_font_collection_font_fallback_enabled" p'skparagraph_font_collection_font_fallback_enabled ::
  FunPtr (Ptr (Skparagraph_font_collection) -> IO (CBool))

{- | C function signature:

@
skparagraph_paragraph_builder_impl_t *skparagraph_paragraph_builder_impl_new(skparagraph_paragraph_style_t *style, skparagraph_font_collection_t *fontCollection, skunicode_skunicode_t *unicode)
@
-}
foreign import ccall "skparagraph_paragraph_builder_impl_new" skparagraph_paragraph_builder_impl_new ::
  Ptr (Skparagraph_paragraph_style) -- ^ C argument @"skparagraph_paragraph_style_t * style"@
  -> Ptr (Skparagraph_font_collection) -- ^ C argument @"skparagraph_font_collection_t * fontCollection"@
  -> Ptr (Skunicode_skunicode) -- ^ C argument @"skunicode_skunicode_t * unicode"@
  -> IO (Ptr (Skparagraph_paragraph_builder_impl)) -- ^ C return type: @"skparagraph_paragraph_builder_impl_t *"@

-- | Function pointer to 'skparagraph_paragraph_builder_impl_new'
foreign import ccall "&skparagraph_paragraph_builder_impl_new" p'skparagraph_paragraph_builder_impl_new ::
  FunPtr (Ptr (Skparagraph_paragraph_style) -> Ptr (Skparagraph_font_collection) -> Ptr (Skunicode_skunicode) -> IO (Ptr (Skparagraph_paragraph_builder_impl)))

{- | C function signature:

@
void skparagraph_paragraph_builder_impl_delete(skparagraph_paragraph_builder_impl_t *builder)
@
-}
foreign import ccall "skparagraph_paragraph_builder_impl_delete" skparagraph_paragraph_builder_impl_delete ::
  Ptr (Skparagraph_paragraph_builder_impl) -- ^ C argument @"skparagraph_paragraph_builder_impl_t * builder"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_builder_impl_delete'
foreign import ccall "&skparagraph_paragraph_builder_impl_delete" p'skparagraph_paragraph_builder_impl_delete ::
  FunPtr (Ptr (Skparagraph_paragraph_builder_impl) -> IO (()))

{- | C function signature:

@
void skparagraph_paragraph_builder_add_text_utf8(skparagraph_paragraph_builder_t *builder, const char *text)
@
-}
foreign import ccall "skparagraph_paragraph_builder_add_text_utf8" skparagraph_paragraph_builder_add_text_utf8 ::
  Ptr (Skparagraph_paragraph_builder) -- ^ C argument @"skparagraph_paragraph_builder_t * builder"@
  -> Ptr (CChar) -- ^ C argument @"const char * text"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_builder_add_text_utf8'
foreign import ccall "&skparagraph_paragraph_builder_add_text_utf8" p'skparagraph_paragraph_builder_add_text_utf8 ::
  FunPtr (Ptr (Skparagraph_paragraph_builder) -> Ptr (CChar) -> IO (()))

{- | C function signature:

@
void skparagraph_paragraph_builder_add_text_utf8_len(skparagraph_paragraph_builder_t *builder, const char *text, size_t len)
@
-}
foreign import ccall "skparagraph_paragraph_builder_add_text_utf8_len" skparagraph_paragraph_builder_add_text_utf8_len ::
  Ptr (Skparagraph_paragraph_builder) -- ^ C argument @"skparagraph_paragraph_builder_t * builder"@
  -> Ptr (CChar) -- ^ C argument @"const char * text"@
  -> CSize -- ^ C argument @"size_t len"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_builder_add_text_utf8_len'
foreign import ccall "&skparagraph_paragraph_builder_add_text_utf8_len" p'skparagraph_paragraph_builder_add_text_utf8_len ::
  FunPtr (Ptr (Skparagraph_paragraph_builder) -> Ptr (CChar) -> CSize -> IO (()))

{- | C function signature:

@
void skparagraph_paragraph_builder_add_placeholder(skparagraph_paragraph_builder_t *builder, skparagraph_placeholder_style_t *instyle)
@
-}
foreign import ccall "skparagraph_paragraph_builder_add_placeholder" skparagraph_paragraph_builder_add_placeholder ::
  Ptr (Skparagraph_paragraph_builder) -- ^ C argument @"skparagraph_paragraph_builder_t * builder"@
  -> Ptr (Skparagraph_placeholder_style) -- ^ C argument @"skparagraph_placeholder_style_t * instyle"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_builder_add_placeholder'
foreign import ccall "&skparagraph_paragraph_builder_add_placeholder" p'skparagraph_paragraph_builder_add_placeholder ::
  FunPtr (Ptr (Skparagraph_paragraph_builder) -> Ptr (Skparagraph_placeholder_style) -> IO (()))

{- | C function signature:

@
skparagraph_paragraph_t *skparagraph_paragraph_builder_build(skparagraph_paragraph_builder_t *builder)
@
-}
foreign import ccall "skparagraph_paragraph_builder_build" skparagraph_paragraph_builder_build ::
  Ptr (Skparagraph_paragraph_builder) -- ^ C argument @"skparagraph_paragraph_builder_t * builder"@
  -> IO (Ptr (Skparagraph_paragraph)) -- ^ C return type: @"skparagraph_paragraph_t *"@

-- | Function pointer to 'skparagraph_paragraph_builder_build'
foreign import ccall "&skparagraph_paragraph_builder_build" p'skparagraph_paragraph_builder_build ::
  FunPtr (Ptr (Skparagraph_paragraph_builder) -> IO (Ptr (Skparagraph_paragraph)))

{- | C function signature:

@
void skparagraph_paragraph_builder_push_style(skparagraph_paragraph_builder_t *builder, skparagraph_text_style_t *style)
@
-}
foreign import ccall "skparagraph_paragraph_builder_push_style" skparagraph_paragraph_builder_push_style ::
  Ptr (Skparagraph_paragraph_builder) -- ^ C argument @"skparagraph_paragraph_builder_t * builder"@
  -> Ptr (Skparagraph_text_style) -- ^ C argument @"skparagraph_text_style_t * style"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_builder_push_style'
foreign import ccall "&skparagraph_paragraph_builder_push_style" p'skparagraph_paragraph_builder_push_style ::
  FunPtr (Ptr (Skparagraph_paragraph_builder) -> Ptr (Skparagraph_text_style) -> IO (()))

{- | C function signature:

@
void skparagraph_paragraph_builder_pop(skparagraph_paragraph_builder_t *builder)
@
-}
foreign import ccall "skparagraph_paragraph_builder_pop" skparagraph_paragraph_builder_pop ::
  Ptr (Skparagraph_paragraph_builder) -- ^ C argument @"skparagraph_paragraph_builder_t * builder"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_builder_pop'
foreign import ccall "&skparagraph_paragraph_builder_pop" p'skparagraph_paragraph_builder_pop ::
  FunPtr (Ptr (Skparagraph_paragraph_builder) -> IO (()))

{- | C function signature:

@
void skparagraph_paragraph_builder_reset(skparagraph_paragraph_builder_t *builderP)
@
-}
foreign import ccall "skparagraph_paragraph_builder_reset" skparagraph_paragraph_builder_reset ::
  Ptr (Skparagraph_paragraph_builder) -- ^ C argument @"skparagraph_paragraph_builder_t * builderP"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skparagraph_paragraph_builder_reset'
foreign import ccall "&skparagraph_paragraph_builder_reset" p'skparagraph_paragraph_builder_reset ::
  FunPtr (Ptr (Skparagraph_paragraph_builder) -> IO (()))
