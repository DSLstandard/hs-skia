#pragma once

#include "sk_types.h"

SK_C_PLUS_PLUS_BEGIN_GUARD

// TextStyle

SK_C_API skparagraph_text_style_t *skparagraph_text_style_new(void);

SK_C_API void skparagraph_text_style_delete(skparagraph_text_style_t *style);

SK_C_API void skparagraph_text_style_set_color(
    skparagraph_text_style_t *style, sk_color_t color
);

SK_C_API void skparagraph_text_style_set_foreground_paint(
    skparagraph_text_style_t *style, sk_paint_t *paint
);

SK_C_API void skparagraph_text_style_clear_foreground_paint(
    skparagraph_text_style_t *style
);

SK_C_API void skparagraph_text_style_set_background_paint(
    skparagraph_text_style_t *style, sk_paint_t *paint
);

SK_C_API void skparagraph_text_style_clear_background_paint(
    skparagraph_text_style_t *style
);

SK_C_API void skparagraph_text_style_set_decoration(
    skparagraph_text_style_t *style,
    skparagraph_text_decoration_flags_t decoration
);

SK_C_API void skparagraph_text_style_set_decoration_mode(
    skparagraph_text_style_t *style, skparagraph_text_decoration_mode_t mode
);

SK_C_API void skparagraph_text_style_set_decoration_style(
    skparagraph_text_style_t *style, skparagraph_text_decoration_style_t st
);

SK_C_API void skparagraph_text_style_set_decoration_color(
    skparagraph_text_style_t *style, sk_color_t color
);

SK_C_API void skparagraph_text_style_set_decoration_thickness_multipler(
    skparagraph_text_style_t *style, sk_scalar_t m
);

SK_C_API void skparagraph_text_style_set_font_style(
    skparagraph_text_style_t *style, sk_fontstyle_t *fontStyle
);

SK_C_API void skparagraph_text_style_add_shadow(
    skparagraph_text_style_t *style, skparagraph_text_shadow_t *shadow
);

SK_C_API void skparagraph_text_style_reset_shadow(
    skparagraph_text_style_t *style
);

SK_C_API void skparagraph_text_style_add_font_feature(
    skparagraph_text_style_t *style, sk_string_t *fontFeature, int value
);

SK_C_API void skparagraph_text_style_reset_font_features(
    skparagraph_text_style_t *style
);

SK_C_API void skparagraph_text_style_set_font_arguments(
    skparagraph_text_style_t *style, sk_fontarguments_t *args
);

SK_C_API void skparagraph_text_style_set_font_size(
    skparagraph_text_style_t *style, sk_scalar_t size
);

SK_C_API void skparagraph_text_style_set_font_families(
    skparagraph_text_style_t *style, sk_string_t *families[], int familiesCount
);

SK_C_API void skparagraph_text_style_set_baseline_shift(
    skparagraph_text_style_t *style, sk_scalar_t shift
);

SK_C_API void skparagraph_text_style_set_height(
    skparagraph_text_style_t *style, sk_scalar_t height
);

SK_C_API void skparagraph_text_style_set_height_override(
    skparagraph_text_style_t *style, sk_scalar_t height
);

SK_C_API void skparagraph_text_style_set_half_leading(
    skparagraph_text_style_t *style, sk_scalar_t height
);

SK_C_API void skparagraph_text_style_set_letter_spacing(
    skparagraph_text_style_t *style, sk_scalar_t letterSpacing
);

SK_C_API void skparagraph_text_style_set_word_spacing(
    skparagraph_text_style_t *style, sk_scalar_t wordSpacing
);

SK_C_API void skparagraph_text_style_set_typeface(
    skparagraph_text_style_t *style, sk_typeface_t *typeface
);

SK_C_API void skparagraph_text_style_set_locale(
    skparagraph_text_style_t *style, sk_string_t *locale
);

SK_C_API void skparagraph_text_style_set_text_baseline(
    skparagraph_text_style_t *style, skparagraph_text_baseline_t baseline
);

SK_C_API void skparagraph_text_style_set_placeholder(
    skparagraph_text_style_t *style
);

// StrutStyle

SK_C_API skparagraph_strut_style_t *skparagraph_strut_style_new(void);
SK_C_API void skparagraph_strut_style_delete(skparagraph_strut_style_t *strut);
SK_C_API void skparagraph_strut_style_set_font_families(
    skparagraph_strut_style_t *strut, sk_string_t *families[], int familiesCount
);
SK_C_API void skparagraph_strut_style_set_font_style(
    skparagraph_strut_style_t *strut, sk_fontstyle_t *style
);
SK_C_API void skparagraph_strut_style_set_font_size(
    skparagraph_strut_style_t *strut, sk_scalar_t size
);
SK_C_API void skparagraph_strut_style_set_height(
    skparagraph_strut_style_t *strut, sk_scalar_t height
);
SK_C_API void skparagraph_strut_style_set_leading(
    skparagraph_strut_style_t *strut, sk_scalar_t leading
);
SK_C_API void skparagraph_strut_style_set_strut_enabled(
    skparagraph_strut_style_t *strut, bool v
);
SK_C_API void skparagraph_strut_style_set_force_strut_height(
    skparagraph_strut_style_t *strut, bool v
);
SK_C_API void skparagraph_strut_style_set_height_override(
    skparagraph_strut_style_t *strut, bool v
);
SK_C_API void skparagraph_strut_style_set_half_leading(
    skparagraph_strut_style_t *strut, bool halfLeading
);

// ParagraphStyle

SK_C_API skparagraph_paragraph_style_t *skparagraph_paragraph_style_new(void);
SK_C_API void skparagraph_paragraph_style_delete(
    skparagraph_paragraph_style_t *style
);
SK_C_API void skparagraph_paragraph_style_set_strut_style(
    skparagraph_paragraph_style_t *para, skparagraph_strut_style_t *strut
);
SK_C_API void skparagraph_paragraph_style_set_text_style(
    skparagraph_paragraph_style_t *para, skparagraph_text_style_t *text
);
SK_C_API void skparagraph_paragraph_style_set_text_direction(
    skparagraph_paragraph_style_t *para, skparagraph_text_direction_t dir
);
SK_C_API void skparagraph_paragraph_style_set_text_align(
    skparagraph_paragraph_style_t *para, skparagraph_text_align_t align
);
SK_C_API void skparagraph_paragraph_style_set_max_lines(
    skparagraph_paragraph_style_t *para, size_t max_lines
);
SK_C_API void skparagraph_paragraph_style_set_ellipsis(
    skparagraph_paragraph_style_t *para, const sk_string_t *ellipsis
);
SK_C_API void skparagraph_paragraph_style_set_height(
    skparagraph_paragraph_style_t *para, sk_scalar_t height
);
SK_C_API void skparagraph_paragraph_style_set_text_height_behavior(
    skparagraph_paragraph_style_t *para,
    skparagraph_text_height_behavior_t behavior
);
SK_C_API void skparagraph_paragraph_style_set_replace_tab_characters(
    skparagraph_paragraph_style_t *para, bool v
);
SK_C_API void skparagraph_paragraph_style_set_apply_rounding_hack(
    skparagraph_paragraph_style_t *para, bool v
);

// Paragraph

SK_C_API void skparagraph_paragraph_delete(skparagraph_paragraph_t *para);
SK_C_API sk_scalar_t
skparagraph_paragraph_get_max_width(skparagraph_paragraph_t *paragraph);
SK_C_API sk_scalar_t
skparagraph_paragraph_get_height(skparagraph_paragraph_t *paragraph);
SK_C_API sk_scalar_t skparagraph_paragraph_get_min_intrinsic_width(
    skparagraph_paragraph_t *paragraph
);
SK_C_API sk_scalar_t skparagraph_paragraph_get_max_intrinsic_width(
    skparagraph_paragraph_t *paragraph
);
SK_C_API sk_scalar_t skparagraph_paragraph_get_alphabetic_baseline(
    skparagraph_paragraph_t *paragraph
);
SK_C_API sk_scalar_t skparagraph_paragraph_get_ideographic_baseline(
    skparagraph_paragraph_t *paragraph
);
SK_C_API sk_scalar_t
skparagraph_paragraph_get_longest_line(skparagraph_paragraph_t *paragraph);
SK_C_API bool skparagraph_paragraph_did_exceed_max_lines(
    skparagraph_paragraph_t *paragraph
);

SK_C_API void skparagraph_paragraph_layout(
    skparagraph_paragraph_t *para, sk_scalar_t width
);
SK_C_API void skparagraph_paragraph_paint_with_canvas(
    skparagraph_paragraph_t *para, sk_canvas_t *canvas, sk_scalar_t x,
    sk_scalar_t y
);
SK_C_API void skparagraph_paragraph_paint_with_painter(
    skparagraph_paragraph_t *para, skparagraph_paragraph_painter_t *painter,
    sk_scalar_t x, sk_scalar_t y
);

// SKParagraph's so-called "Editing API"

SK_C_API int skparagraph_paragraph_get_line_number_at(
    skparagraph_paragraph_t *para, size_t codeUnitIndex
);

SK_C_API bool skparagraph_paragraph_get_line_metrics_at(
    skparagraph_paragraph_t *para, int lineNumber,
    skparagraph_line_metrics_t *dstMetrics
);

SK_C_API bool skparagraph_paragraph_get_closest_glyph_cluster_at(
    skparagraph_paragraph_t *para, sk_scalar_t x, sk_scalar_t y,
    skparagraph_glyph_cluster_info_t *dstGlyphInfo
);

// Returns true if glyph cluster was found; false if not (which usually means
// the paragraph is empty)
SK_C_API bool skparagraph_paragraph_get_glyph_cluster_at(
    skparagraph_paragraph_t *para, int codeUnitIndex,
    skparagraph_glyph_cluster_info_t *dstGlyphInfo
);

// Returns the index of the glyph that corresponds to the provided coordinate,
// with the top left corner as the origin, and +y direction as down
SK_C_API void skparagraph_paragraph_get_glyph_position_at_coordinate(
    skparagraph_paragraph_t *para, sk_scalar_t x, sk_scalar_t y,
    int32_t *dstPosition, skparagraph_affinity_t *dstAffinity
);

// FontCollection

SK_C_API skparagraph_font_collection_t *skparagraph_font_collection_new(void);
SK_C_API void skparagraph_font_collection_delete(
    skparagraph_font_collection_t *style
);
SK_C_API void skparagraph_font_collection_set_asset_fontmgr(
    skparagraph_font_collection_t *col, sk_fontmgr_t *fontmgr
);

SK_C_API void skparagraph_font_collection_set_dynamic_fontmgr(
    skparagraph_font_collection_t *col, sk_fontmgr_t *fontmgr
);

SK_C_API void skparagraph_font_collection_set_test_fontmgr(
    skparagraph_font_collection_t *col, sk_fontmgr_t *fontmgr
);

SK_C_API void skparagraph_font_collection_set_default_fontmgr(
    skparagraph_font_collection_t *col, sk_fontmgr_t *fontmgr
);

SK_C_API void skparagraph_font_collection_set_default_fontmgr_with_def_family(
    skparagraph_font_collection_t *col, sk_fontmgr_t *fontmgr,
    const char *defaultFamilyName
);

SK_C_API void skparagraph_font_collection_set_default_fontmgr_with_def_families(
    skparagraph_font_collection_t *col, sk_fontmgr_t *fontmgr,
    sk_string_t *defaultFamilyNames[], int defaultFamilyNamesCount
);

SK_C_API void skparagraph_font_collection_enable_font_fallback(
    skparagraph_font_collection_t *col
);

SK_C_API void skparagraph_font_collection_disable_font_fallback(
    skparagraph_font_collection_t *col
);

SK_C_API bool skparagraph_font_collection_font_fallback_enabled(
    skparagraph_font_collection_t *col
);

// Builder

SK_C_API skparagraph_paragraph_builder_impl_t *
skparagraph_paragraph_builder_impl_new(
    skparagraph_paragraph_style_t *style,
    skparagraph_font_collection_t *fontCollection,
    skunicode_skunicode_t *unicode
);
SK_C_API void skparagraph_paragraph_builder_impl_delete(
    skparagraph_paragraph_builder_impl_t *builder
);
SK_C_API void skparagraph_paragraph_builder_add_text_utf8(
    skparagraph_paragraph_builder_t *builder, const char *text
);
SK_C_API void skparagraph_paragraph_builder_add_text_utf8_len(
    skparagraph_paragraph_builder_t *builder, const char *text, size_t len
);
SK_C_API void skparagraph_paragraph_builder_add_placeholder(
    skparagraph_paragraph_builder_t *builder,
    skparagraph_placeholder_style_t *instyle
);
SK_C_API skparagraph_paragraph_t *skparagraph_paragraph_builder_build(
    skparagraph_paragraph_builder_t *builder
);
SK_C_API void skparagraph_paragraph_builder_push_style(
    skparagraph_paragraph_builder_t *builder, skparagraph_text_style_t *style
);
SK_C_API void skparagraph_paragraph_builder_pop(
    skparagraph_paragraph_builder_t *builder
);
SK_C_API void skparagraph_paragraph_builder_reset(skparagraph_paragraph_builder_t *builderP);

SK_C_PLUS_PLUS_END_GUARD