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
module Skia.Bindings.Sk_typeface where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void sk_typeface_unref(sk_typeface_t *typeface)
@
-}
foreign import ccall "sk_typeface_unref" sk_typeface_unref ::
  Ptr (Sk_typeface) -- ^ C argument @"sk_typeface_t * typeface"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_typeface_unref'
foreign import ccall "&sk_typeface_unref" p'sk_typeface_unref ::
  FunPtr (Ptr (Sk_typeface) -> IO (()))

{- | C function signature:

@
sk_fontstyle_t *sk_typeface_get_fontstyle(const sk_typeface_t *typeface)
@
-}
foreign import ccall "sk_typeface_get_fontstyle" sk_typeface_get_fontstyle ::
  Ptr (Sk_typeface) -- ^ C argument @"const sk_typeface_t * typeface"@
  -> IO (Ptr (Sk_fontstyle)) -- ^ C return type: @"sk_fontstyle_t *"@

-- | Function pointer to 'sk_typeface_get_fontstyle'
foreign import ccall "&sk_typeface_get_fontstyle" p'sk_typeface_get_fontstyle ::
  FunPtr (Ptr (Sk_typeface) -> IO (Ptr (Sk_fontstyle)))

{- | C function signature:

@
int sk_typeface_get_font_weight(const sk_typeface_t *typeface)
@
-}
foreign import ccall "sk_typeface_get_font_weight" sk_typeface_get_font_weight ::
  Ptr (Sk_typeface) -- ^ C argument @"const sk_typeface_t * typeface"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_typeface_get_font_weight'
foreign import ccall "&sk_typeface_get_font_weight" p'sk_typeface_get_font_weight ::
  FunPtr (Ptr (Sk_typeface) -> IO (CInt))

{- | C function signature:

@
int sk_typeface_get_font_width(const sk_typeface_t *typeface)
@
-}
foreign import ccall "sk_typeface_get_font_width" sk_typeface_get_font_width ::
  Ptr (Sk_typeface) -- ^ C argument @"const sk_typeface_t * typeface"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_typeface_get_font_width'
foreign import ccall "&sk_typeface_get_font_width" p'sk_typeface_get_font_width ::
  FunPtr (Ptr (Sk_typeface) -> IO (CInt))

{- | C function signature:

@
sk_font_style_slant_t sk_typeface_get_font_slant(const sk_typeface_t *typeface)
@
-}
foreign import ccall "sk_typeface_get_font_slant" sk_typeface_get_font_slant ::
  Ptr (Sk_typeface) -- ^ C argument @"const sk_typeface_t * typeface"@
  -> IO (Sk_font_style_slant) -- ^ C return type: @"sk_font_style_slant_t"@

-- | Function pointer to 'sk_typeface_get_font_slant'
foreign import ccall "&sk_typeface_get_font_slant" p'sk_typeface_get_font_slant ::
  FunPtr (Ptr (Sk_typeface) -> IO (Sk_font_style_slant))

{- | C function signature:

@
_Bool sk_typeface_is_fixed_pitch(const sk_typeface_t *typeface)
@
-}
foreign import ccall "sk_typeface_is_fixed_pitch" sk_typeface_is_fixed_pitch ::
  Ptr (Sk_typeface) -- ^ C argument @"const sk_typeface_t * typeface"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_typeface_is_fixed_pitch'
foreign import ccall "&sk_typeface_is_fixed_pitch" p'sk_typeface_is_fixed_pitch ::
  FunPtr (Ptr (Sk_typeface) -> IO (CBool))

{- | C function signature:

@
void sk_typeface_unichars_to_glyphs(const sk_typeface_t *typeface, const int32_t unichars[], int count, uint16_t glyphs[])
@
-}
foreign import ccall "sk_typeface_unichars_to_glyphs" sk_typeface_unichars_to_glyphs ::
  Ptr (Sk_typeface) -- ^ C argument @"const sk_typeface_t * typeface"@
  -> Ptr (Int32) -- ^ C argument @"const int32_t [] unichars"@
  -> CInt -- ^ C argument @"int count"@
  -> Ptr (Word16) -- ^ C argument @"uint16_t [] glyphs"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_typeface_unichars_to_glyphs'
foreign import ccall "&sk_typeface_unichars_to_glyphs" p'sk_typeface_unichars_to_glyphs ::
  FunPtr (Ptr (Sk_typeface) -> Ptr (Int32) -> CInt -> Ptr (Word16) -> IO (()))

{- | C function signature:

@
uint16_t sk_typeface_unichar_to_glyph(const sk_typeface_t *typeface, const int32_t unichar)
@
-}
foreign import ccall "sk_typeface_unichar_to_glyph" sk_typeface_unichar_to_glyph ::
  Ptr (Sk_typeface) -- ^ C argument @"const sk_typeface_t * typeface"@
  -> Int32 -- ^ C argument @"const int32_t unichar"@
  -> IO (Word16) -- ^ C return type: @"uint16_t"@

-- | Function pointer to 'sk_typeface_unichar_to_glyph'
foreign import ccall "&sk_typeface_unichar_to_glyph" p'sk_typeface_unichar_to_glyph ::
  FunPtr (Ptr (Sk_typeface) -> Int32 -> IO (Word16))

{- | C function signature:

@
int sk_typeface_count_glyphs(const sk_typeface_t *typeface)
@
-}
foreign import ccall "sk_typeface_count_glyphs" sk_typeface_count_glyphs ::
  Ptr (Sk_typeface) -- ^ C argument @"const sk_typeface_t * typeface"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_typeface_count_glyphs'
foreign import ccall "&sk_typeface_count_glyphs" p'sk_typeface_count_glyphs ::
  FunPtr (Ptr (Sk_typeface) -> IO (CInt))

{- | C function signature:

@
int sk_typeface_count_tables(const sk_typeface_t *typeface)
@
-}
foreign import ccall "sk_typeface_count_tables" sk_typeface_count_tables ::
  Ptr (Sk_typeface) -- ^ C argument @"const sk_typeface_t * typeface"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_typeface_count_tables'
foreign import ccall "&sk_typeface_count_tables" p'sk_typeface_count_tables ::
  FunPtr (Ptr (Sk_typeface) -> IO (CInt))

{- | C function signature:

@
int sk_typeface_get_table_tags(const sk_typeface_t *typeface, sk_font_table_tag_t tags[])
@
-}
foreign import ccall "sk_typeface_get_table_tags" sk_typeface_get_table_tags ::
  Ptr (Sk_typeface) -- ^ C argument @"const sk_typeface_t * typeface"@
  -> Ptr (Sk_font_table_tag) -- ^ C argument @"sk_font_table_tag_t [] tags"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_typeface_get_table_tags'
foreign import ccall "&sk_typeface_get_table_tags" p'sk_typeface_get_table_tags ::
  FunPtr (Ptr (Sk_typeface) -> Ptr (Sk_font_table_tag) -> IO (CInt))

{- | C function signature:

@
size_t sk_typeface_get_table_size(const sk_typeface_t *typeface, sk_font_table_tag_t tag)
@
-}
foreign import ccall "sk_typeface_get_table_size" sk_typeface_get_table_size ::
  Ptr (Sk_typeface) -- ^ C argument @"const sk_typeface_t * typeface"@
  -> Sk_font_table_tag -- ^ C argument @"sk_font_table_tag_t tag"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_typeface_get_table_size'
foreign import ccall "&sk_typeface_get_table_size" p'sk_typeface_get_table_size ::
  FunPtr (Ptr (Sk_typeface) -> Sk_font_table_tag -> IO (CSize))

{- | C function signature:

@
size_t sk_typeface_get_table_data(const sk_typeface_t *typeface, sk_font_table_tag_t tag, size_t offset, size_t length, void *data)
@
-}
foreign import ccall "sk_typeface_get_table_data" sk_typeface_get_table_data ::
  Ptr (Sk_typeface) -- ^ C argument @"const sk_typeface_t * typeface"@
  -> Sk_font_table_tag -- ^ C argument @"sk_font_table_tag_t tag"@
  -> CSize -- ^ C argument @"size_t offset"@
  -> CSize -- ^ C argument @"size_t length"@
  -> Ptr (()) -- ^ C argument @"void * data"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_typeface_get_table_data'
foreign import ccall "&sk_typeface_get_table_data" p'sk_typeface_get_table_data ::
  FunPtr (Ptr (Sk_typeface) -> Sk_font_table_tag -> CSize -> CSize -> Ptr (()) -> IO (CSize))

{- | C function signature:

@
sk_data_t *sk_typeface_copy_table_data(const sk_typeface_t *typeface, sk_font_table_tag_t tag)
@
-}
foreign import ccall "sk_typeface_copy_table_data" sk_typeface_copy_table_data ::
  Ptr (Sk_typeface) -- ^ C argument @"const sk_typeface_t * typeface"@
  -> Sk_font_table_tag -- ^ C argument @"sk_font_table_tag_t tag"@
  -> IO (Ptr (Sk_data)) -- ^ C return type: @"sk_data_t *"@

-- | Function pointer to 'sk_typeface_copy_table_data'
foreign import ccall "&sk_typeface_copy_table_data" p'sk_typeface_copy_table_data ::
  FunPtr (Ptr (Sk_typeface) -> Sk_font_table_tag -> IO (Ptr (Sk_data)))

{- | C function signature:

@
int sk_typeface_get_units_per_em(const sk_typeface_t *typeface)
@
-}
foreign import ccall "sk_typeface_get_units_per_em" sk_typeface_get_units_per_em ::
  Ptr (Sk_typeface) -- ^ C argument @"const sk_typeface_t * typeface"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_typeface_get_units_per_em'
foreign import ccall "&sk_typeface_get_units_per_em" p'sk_typeface_get_units_per_em ::
  FunPtr (Ptr (Sk_typeface) -> IO (CInt))

{- | C function signature:

@
_Bool sk_typeface_get_kerning_pair_adjustments(const sk_typeface_t *typeface, const uint16_t glyphs[], int count, int32_t adjustments[])
@
-}
foreign import ccall "sk_typeface_get_kerning_pair_adjustments" sk_typeface_get_kerning_pair_adjustments ::
  Ptr (Sk_typeface) -- ^ C argument @"const sk_typeface_t * typeface"@
  -> Ptr (Word16) -- ^ C argument @"const uint16_t [] glyphs"@
  -> CInt -- ^ C argument @"int count"@
  -> Ptr (Int32) -- ^ C argument @"int32_t [] adjustments"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_typeface_get_kerning_pair_adjustments'
foreign import ccall "&sk_typeface_get_kerning_pair_adjustments" p'sk_typeface_get_kerning_pair_adjustments ::
  FunPtr (Ptr (Sk_typeface) -> Ptr (Word16) -> CInt -> Ptr (Int32) -> IO (CBool))

{- | C function signature:

@
sk_string_t *sk_typeface_get_family_name(const sk_typeface_t *typeface)
@
-}
foreign import ccall "sk_typeface_get_family_name" sk_typeface_get_family_name ::
  Ptr (Sk_typeface) -- ^ C argument @"const sk_typeface_t * typeface"@
  -> IO (Ptr (Sk_string)) -- ^ C return type: @"sk_string_t *"@

-- | Function pointer to 'sk_typeface_get_family_name'
foreign import ccall "&sk_typeface_get_family_name" p'sk_typeface_get_family_name ::
  FunPtr (Ptr (Sk_typeface) -> IO (Ptr (Sk_string)))

{- | C function signature:

@
sk_stream_asset_t *sk_typeface_open_stream(const sk_typeface_t *typeface, int *ttcIndex)
@
-}
foreign import ccall "sk_typeface_open_stream" sk_typeface_open_stream ::
  Ptr (Sk_typeface) -- ^ C argument @"const sk_typeface_t * typeface"@
  -> Ptr (CInt) -- ^ C argument @"int * ttcIndex"@
  -> IO (Ptr (Sk_stream_asset)) -- ^ C return type: @"sk_stream_asset_t *"@

-- | Function pointer to 'sk_typeface_open_stream'
foreign import ccall "&sk_typeface_open_stream" p'sk_typeface_open_stream ::
  FunPtr (Ptr (Sk_typeface) -> Ptr (CInt) -> IO (Ptr (Sk_stream_asset)))

{- | C function signature:

@
void sk_fontmgr_unref(sk_fontmgr_t *)
@
-}
foreign import ccall "sk_fontmgr_unref" sk_fontmgr_unref ::
  Ptr (Sk_fontmgr) -- ^ C argument type: @"sk_fontmgr_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_fontmgr_unref'
foreign import ccall "&sk_fontmgr_unref" p'sk_fontmgr_unref ::
  FunPtr (Ptr (Sk_fontmgr) -> IO (()))

{- | C function signature:

@
int sk_fontmgr_count_families(sk_fontmgr_t *)
@
-}
foreign import ccall "sk_fontmgr_count_families" sk_fontmgr_count_families ::
  Ptr (Sk_fontmgr) -- ^ C argument type: @"sk_fontmgr_t *"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_fontmgr_count_families'
foreign import ccall "&sk_fontmgr_count_families" p'sk_fontmgr_count_families ::
  FunPtr (Ptr (Sk_fontmgr) -> IO (CInt))

{- | C function signature:

@
void sk_fontmgr_get_family_name(sk_fontmgr_t *, int index, sk_string_t *familyName)
@
-}
foreign import ccall "sk_fontmgr_get_family_name" sk_fontmgr_get_family_name ::
  Ptr (Sk_fontmgr) -- ^ C argument type: @"sk_fontmgr_t *"@
  -> CInt -- ^ C argument @"int index"@
  -> Ptr (Sk_string) -- ^ C argument @"sk_string_t * familyName"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_fontmgr_get_family_name'
foreign import ccall "&sk_fontmgr_get_family_name" p'sk_fontmgr_get_family_name ::
  FunPtr (Ptr (Sk_fontmgr) -> CInt -> Ptr (Sk_string) -> IO (()))

{- | C function signature:

@
sk_fontstyleset_t *sk_fontmgr_create_styleset(sk_fontmgr_t *, int index)
@
-}
foreign import ccall "sk_fontmgr_create_styleset" sk_fontmgr_create_styleset ::
  Ptr (Sk_fontmgr) -- ^ C argument type: @"sk_fontmgr_t *"@
  -> CInt -- ^ C argument @"int index"@
  -> IO (Ptr (Sk_fontstyleset)) -- ^ C return type: @"sk_fontstyleset_t *"@

-- | Function pointer to 'sk_fontmgr_create_styleset'
foreign import ccall "&sk_fontmgr_create_styleset" p'sk_fontmgr_create_styleset ::
  FunPtr (Ptr (Sk_fontmgr) -> CInt -> IO (Ptr (Sk_fontstyleset)))

{- | C function signature:

@
sk_fontstyleset_t *sk_fontmgr_match_family(sk_fontmgr_t *, const char *familyName)
@
-}
foreign import ccall "sk_fontmgr_match_family" sk_fontmgr_match_family ::
  Ptr (Sk_fontmgr) -- ^ C argument type: @"sk_fontmgr_t *"@
  -> Ptr (CChar) -- ^ C argument @"const char * familyName"@
  -> IO (Ptr (Sk_fontstyleset)) -- ^ C return type: @"sk_fontstyleset_t *"@

-- | Function pointer to 'sk_fontmgr_match_family'
foreign import ccall "&sk_fontmgr_match_family" p'sk_fontmgr_match_family ::
  FunPtr (Ptr (Sk_fontmgr) -> Ptr (CChar) -> IO (Ptr (Sk_fontstyleset)))

{- | C function signature:

@
sk_typeface_t *sk_fontmgr_match_family_style(sk_fontmgr_t *, const char *familyName, sk_fontstyle_t *style)
@
-}
foreign import ccall "sk_fontmgr_match_family_style" sk_fontmgr_match_family_style ::
  Ptr (Sk_fontmgr) -- ^ C argument type: @"sk_fontmgr_t *"@
  -> Ptr (CChar) -- ^ C argument @"const char * familyName"@
  -> Ptr (Sk_fontstyle) -- ^ C argument @"sk_fontstyle_t * style"@
  -> IO (Ptr (Sk_typeface)) -- ^ C return type: @"sk_typeface_t *"@

-- | Function pointer to 'sk_fontmgr_match_family_style'
foreign import ccall "&sk_fontmgr_match_family_style" p'sk_fontmgr_match_family_style ::
  FunPtr (Ptr (Sk_fontmgr) -> Ptr (CChar) -> Ptr (Sk_fontstyle) -> IO (Ptr (Sk_typeface)))

{- | C function signature:

@
sk_typeface_t *sk_fontmgr_match_family_style_character(sk_fontmgr_t *, const char *familyName, sk_fontstyle_t *style, const char **bcp47, int bcp47Count, int32_t character)
@
-}
foreign import ccall "sk_fontmgr_match_family_style_character" sk_fontmgr_match_family_style_character ::
  Ptr (Sk_fontmgr) -- ^ C argument type: @"sk_fontmgr_t *"@
  -> Ptr (CChar) -- ^ C argument @"const char * familyName"@
  -> Ptr (Sk_fontstyle) -- ^ C argument @"sk_fontstyle_t * style"@
  -> Ptr (Ptr (CChar)) -- ^ C argument @"const char ** bcp47"@
  -> CInt -- ^ C argument @"int bcp47Count"@
  -> Int32 -- ^ C argument @"int32_t character"@
  -> IO (Ptr (Sk_typeface)) -- ^ C return type: @"sk_typeface_t *"@

-- | Function pointer to 'sk_fontmgr_match_family_style_character'
foreign import ccall "&sk_fontmgr_match_family_style_character" p'sk_fontmgr_match_family_style_character ::
  FunPtr (Ptr (Sk_fontmgr) -> Ptr (CChar) -> Ptr (Sk_fontstyle) -> Ptr (Ptr (CChar)) -> CInt -> Int32 -> IO (Ptr (Sk_typeface)))

{- | C function signature:

@
sk_typeface_t *sk_fontmgr_create_from_data(sk_fontmgr_t *, sk_data_t *data, int index)
@
-}
foreign import ccall "sk_fontmgr_create_from_data" sk_fontmgr_create_from_data ::
  Ptr (Sk_fontmgr) -- ^ C argument type: @"sk_fontmgr_t *"@
  -> Ptr (Sk_data) -- ^ C argument @"sk_data_t * data"@
  -> CInt -- ^ C argument @"int index"@
  -> IO (Ptr (Sk_typeface)) -- ^ C return type: @"sk_typeface_t *"@

-- | Function pointer to 'sk_fontmgr_create_from_data'
foreign import ccall "&sk_fontmgr_create_from_data" p'sk_fontmgr_create_from_data ::
  FunPtr (Ptr (Sk_fontmgr) -> Ptr (Sk_data) -> CInt -> IO (Ptr (Sk_typeface)))

{- | C function signature:

@
sk_typeface_t *sk_fontmgr_create_from_stream(sk_fontmgr_t *, sk_stream_asset_t *stream, int index)
@
-}
foreign import ccall "sk_fontmgr_create_from_stream" sk_fontmgr_create_from_stream ::
  Ptr (Sk_fontmgr) -- ^ C argument type: @"sk_fontmgr_t *"@
  -> Ptr (Sk_stream_asset) -- ^ C argument @"sk_stream_asset_t * stream"@
  -> CInt -- ^ C argument @"int index"@
  -> IO (Ptr (Sk_typeface)) -- ^ C return type: @"sk_typeface_t *"@

-- | Function pointer to 'sk_fontmgr_create_from_stream'
foreign import ccall "&sk_fontmgr_create_from_stream" p'sk_fontmgr_create_from_stream ::
  FunPtr (Ptr (Sk_fontmgr) -> Ptr (Sk_stream_asset) -> CInt -> IO (Ptr (Sk_typeface)))

{- | C function signature:

@
sk_typeface_t *sk_fontmgr_create_from_file(sk_fontmgr_t *, const char *path, int index)
@
-}
foreign import ccall "sk_fontmgr_create_from_file" sk_fontmgr_create_from_file ::
  Ptr (Sk_fontmgr) -- ^ C argument type: @"sk_fontmgr_t *"@
  -> Ptr (CChar) -- ^ C argument @"const char * path"@
  -> CInt -- ^ C argument @"int index"@
  -> IO (Ptr (Sk_typeface)) -- ^ C return type: @"sk_typeface_t *"@

-- | Function pointer to 'sk_fontmgr_create_from_file'
foreign import ccall "&sk_fontmgr_create_from_file" p'sk_fontmgr_create_from_file ::
  FunPtr (Ptr (Sk_fontmgr) -> Ptr (CChar) -> CInt -> IO (Ptr (Sk_typeface)))

{- | C function signature:

@
sk_fontstyle_t *sk_fontstyle_new(int weight, int width, sk_font_style_slant_t slant)
@
-}
foreign import ccall "sk_fontstyle_new" sk_fontstyle_new ::
  CInt -- ^ C argument @"int weight"@
  -> CInt -- ^ C argument @"int width"@
  -> Sk_font_style_slant -- ^ C argument @"sk_font_style_slant_t slant"@
  -> IO (Ptr (Sk_fontstyle)) -- ^ C return type: @"sk_fontstyle_t *"@

-- | Function pointer to 'sk_fontstyle_new'
foreign import ccall "&sk_fontstyle_new" p'sk_fontstyle_new ::
  FunPtr (CInt -> CInt -> Sk_font_style_slant -> IO (Ptr (Sk_fontstyle)))

{- | C function signature:

@
void sk_fontstyle_delete(sk_fontstyle_t *fs)
@
-}
foreign import ccall "sk_fontstyle_delete" sk_fontstyle_delete ::
  Ptr (Sk_fontstyle) -- ^ C argument @"sk_fontstyle_t * fs"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_fontstyle_delete'
foreign import ccall "&sk_fontstyle_delete" p'sk_fontstyle_delete ::
  FunPtr (Ptr (Sk_fontstyle) -> IO (()))

{- | C function signature:

@
int sk_fontstyle_get_weight(const sk_fontstyle_t *fs)
@
-}
foreign import ccall "sk_fontstyle_get_weight" sk_fontstyle_get_weight ::
  Ptr (Sk_fontstyle) -- ^ C argument @"const sk_fontstyle_t * fs"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_fontstyle_get_weight'
foreign import ccall "&sk_fontstyle_get_weight" p'sk_fontstyle_get_weight ::
  FunPtr (Ptr (Sk_fontstyle) -> IO (CInt))

{- | C function signature:

@
int sk_fontstyle_get_width(const sk_fontstyle_t *fs)
@
-}
foreign import ccall "sk_fontstyle_get_width" sk_fontstyle_get_width ::
  Ptr (Sk_fontstyle) -- ^ C argument @"const sk_fontstyle_t * fs"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_fontstyle_get_width'
foreign import ccall "&sk_fontstyle_get_width" p'sk_fontstyle_get_width ::
  FunPtr (Ptr (Sk_fontstyle) -> IO (CInt))

{- | C function signature:

@
sk_font_style_slant_t sk_fontstyle_get_slant(const sk_fontstyle_t *fs)
@
-}
foreign import ccall "sk_fontstyle_get_slant" sk_fontstyle_get_slant ::
  Ptr (Sk_fontstyle) -- ^ C argument @"const sk_fontstyle_t * fs"@
  -> IO (Sk_font_style_slant) -- ^ C return type: @"sk_font_style_slant_t"@

-- | Function pointer to 'sk_fontstyle_get_slant'
foreign import ccall "&sk_fontstyle_get_slant" p'sk_fontstyle_get_slant ::
  FunPtr (Ptr (Sk_fontstyle) -> IO (Sk_font_style_slant))

{- | C function signature:

@
sk_fontstyleset_t *sk_fontstyleset_create_empty(void)
@
-}
foreign import ccall "sk_fontstyleset_create_empty" sk_fontstyleset_create_empty ::
  IO (Ptr (Sk_fontstyleset)) -- ^ C return type: @"sk_fontstyleset_t *"@

-- | Function pointer to 'sk_fontstyleset_create_empty'
foreign import ccall "&sk_fontstyleset_create_empty" p'sk_fontstyleset_create_empty ::
  FunPtr (IO (Ptr (Sk_fontstyleset)))

{- | C function signature:

@
void sk_fontstyleset_unref(sk_fontstyleset_t *fss)
@
-}
foreign import ccall "sk_fontstyleset_unref" sk_fontstyleset_unref ::
  Ptr (Sk_fontstyleset) -- ^ C argument @"sk_fontstyleset_t * fss"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_fontstyleset_unref'
foreign import ccall "&sk_fontstyleset_unref" p'sk_fontstyleset_unref ::
  FunPtr (Ptr (Sk_fontstyleset) -> IO (()))

{- | C function signature:

@
int sk_fontstyleset_get_count(sk_fontstyleset_t *fss)
@
-}
foreign import ccall "sk_fontstyleset_get_count" sk_fontstyleset_get_count ::
  Ptr (Sk_fontstyleset) -- ^ C argument @"sk_fontstyleset_t * fss"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_fontstyleset_get_count'
foreign import ccall "&sk_fontstyleset_get_count" p'sk_fontstyleset_get_count ::
  FunPtr (Ptr (Sk_fontstyleset) -> IO (CInt))

{- | C function signature:

@
void sk_fontstyleset_get_style(sk_fontstyleset_t *fss, int index, sk_fontstyle_t *fs, sk_string_t *style)
@
-}
foreign import ccall "sk_fontstyleset_get_style" sk_fontstyleset_get_style ::
  Ptr (Sk_fontstyleset) -- ^ C argument @"sk_fontstyleset_t * fss"@
  -> CInt -- ^ C argument @"int index"@
  -> Ptr (Sk_fontstyle) -- ^ C argument @"sk_fontstyle_t * fs"@
  -> Ptr (Sk_string) -- ^ C argument @"sk_string_t * style"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_fontstyleset_get_style'
foreign import ccall "&sk_fontstyleset_get_style" p'sk_fontstyleset_get_style ::
  FunPtr (Ptr (Sk_fontstyleset) -> CInt -> Ptr (Sk_fontstyle) -> Ptr (Sk_string) -> IO (()))

{- | C function signature:

@
sk_typeface_t *sk_fontstyleset_create_typeface(sk_fontstyleset_t *fss, int index)
@
-}
foreign import ccall "sk_fontstyleset_create_typeface" sk_fontstyleset_create_typeface ::
  Ptr (Sk_fontstyleset) -- ^ C argument @"sk_fontstyleset_t * fss"@
  -> CInt -- ^ C argument @"int index"@
  -> IO (Ptr (Sk_typeface)) -- ^ C return type: @"sk_typeface_t *"@

-- | Function pointer to 'sk_fontstyleset_create_typeface'
foreign import ccall "&sk_fontstyleset_create_typeface" p'sk_fontstyleset_create_typeface ::
  FunPtr (Ptr (Sk_fontstyleset) -> CInt -> IO (Ptr (Sk_typeface)))

{- | C function signature:

@
sk_typeface_t *sk_fontstyleset_match_style(sk_fontstyleset_t *fss, sk_fontstyle_t *style)
@
-}
foreign import ccall "sk_fontstyleset_match_style" sk_fontstyleset_match_style ::
  Ptr (Sk_fontstyleset) -- ^ C argument @"sk_fontstyleset_t * fss"@
  -> Ptr (Sk_fontstyle) -- ^ C argument @"sk_fontstyle_t * style"@
  -> IO (Ptr (Sk_typeface)) -- ^ C return type: @"sk_typeface_t *"@

-- | Function pointer to 'sk_fontstyleset_match_style'
foreign import ccall "&sk_fontstyleset_match_style" p'sk_fontstyleset_match_style ::
  FunPtr (Ptr (Sk_fontstyleset) -> Ptr (Sk_fontstyle) -> IO (Ptr (Sk_typeface)))
