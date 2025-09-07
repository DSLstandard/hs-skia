#include "skia_capi/skparagraph.h"

#include <optional>
#include <vector>

#include "common_include_pch.hpp"

// TextStyle

skparagraph_text_style_t *skparagraph_text_style_new(void) {
    return ToSkParagraphTextStyle(new skia::textlayout::TextStyle());
}

void skparagraph_text_style_delete(skparagraph_text_style_t *style) {
    delete AsSkParagraphTextStyle(style);
}

void skparagraph_text_style_set_color(
    skparagraph_text_style_t *style, sk_color_t color
) {
    AsSkParagraphTextStyle(style)->setColor(color);
}

void skparagraph_text_style_set_foreground_paint(
    skparagraph_text_style_t *style, sk_paint_t *paint
) {
    AsSkParagraphTextStyle(style)->setForegroundPaint(*AsPaint(paint));
}

void skparagraph_text_style_clear_foreground_paint(
    skparagraph_text_style_t *style
) {
    AsSkParagraphTextStyle(style)->clearForegroundColor();
}

void skparagraph_text_style_set_background_paint(
    skparagraph_text_style_t *style, sk_paint_t *paint
) {
    AsSkParagraphTextStyle(style)->setBackgroundPaint(*AsPaint(paint));
}

void skparagraph_text_style_clear_background_paint(
    skparagraph_text_style_t *style
) {
    AsSkParagraphTextStyle(style)->clearBackgroundColor();
}

void skparagraph_text_style_set_decoration(
    skparagraph_text_style_t *style,
    skparagraph_text_decoration_flags_t decoration
) {
    AsSkParagraphTextStyle(style)->setDecoration(
        (skia::textlayout::TextDecoration)decoration
    );
}

void skparagraph_text_style_set_decoration_mode(
    skparagraph_text_style_t *style, skparagraph_text_decoration_mode_t mode
) {
    AsSkParagraphTextStyle(style)->setDecorationMode(
        (skia::textlayout::TextDecorationMode)mode
    );
}

void skparagraph_text_style_set_decoration_style(
    skparagraph_text_style_t *style, skparagraph_text_decoration_style_t st
) {
    AsSkParagraphTextStyle(style)->setDecorationStyle(
        (skia::textlayout::TextDecorationStyle)st
    );
}

void skparagraph_text_style_set_decoration_color(
    skparagraph_text_style_t *style, sk_color_t color
) {
    AsSkParagraphTextStyle(style)->setDecorationColor(color);
}

void skparagraph_text_style_set_decoration_thickness_multipler(
    skparagraph_text_style_t *style, sk_scalar_t m
) {
    AsSkParagraphTextStyle(style)->setDecorationThicknessMultiplier(m);
}

void skparagraph_text_style_set_font_style(
    skparagraph_text_style_t *style, sk_fontstyle_t *fontStyle
) {
    AsSkParagraphTextStyle(style)->setFontStyle(*AsFontStyle(fontStyle));
}

void skparagraph_text_style_add_shadow(
    skparagraph_text_style_t *style, skparagraph_text_shadow_t *shadow
) {
    AsSkParagraphTextStyle(style)->addShadow(
        {shadow->fColor, AsPoint(shadow->fOffset), shadow->fBlurSigma}
    );
}

void skparagraph_text_style_reset_shadow(skparagraph_text_style_t *style) {
    AsSkParagraphTextStyle(style)->resetShadows();
}

void skparagraph_text_style_add_font_feature(
    skparagraph_text_style_t *style, sk_string_t *fontFeature, int value
) {
    AsSkParagraphTextStyle(style)->addFontFeature(
        *AsString(fontFeature), value
    );
}

void skparagraph_text_style_reset_font_features(skparagraph_text_style_t *style
) {
    AsSkParagraphTextStyle(style)->resetFontFeatures();
}

// NOTE: args can be nullptr to specify std::nullopt.
void skparagraph_text_style_set_font_arguments(
    skparagraph_text_style_t *style, sk_fontarguments_t *args
) {
    std::optional<SkFontArguments> inArgs =
        args ? std::optional<SkFontArguments>{*AsFontArguments(args)}
             : std::nullopt;
    AsSkParagraphTextStyle(style)->setFontArguments(inArgs);
}

void skparagraph_text_style_set_font_size(
    skparagraph_text_style_t *style, sk_scalar_t size
) {
    AsSkParagraphTextStyle(style)->setFontSize(size);
}

void skparagraph_text_style_set_font_families(
    skparagraph_text_style_t *style, sk_string_t *families[], int familiesCount
) {
    std::vector<SkString> vec(familiesCount);

    for (int i = 0; i < familiesCount; i++) {
        vec[i] = *AsString(families[i]);
    }

    AsSkParagraphTextStyle(style)->setFontFamilies(vec);
}

void skparagraph_text_style_set_baseline_shift(
    skparagraph_text_style_t *style, sk_scalar_t shift
) {
    AsSkParagraphTextStyle(style)->setBaselineShift(shift);
}

void skparagraph_text_style_set_height(
    skparagraph_text_style_t *style, sk_scalar_t height
) {
    AsSkParagraphTextStyle(style)->setHeight(height);
}

void skparagraph_text_style_set_height_override(
    skparagraph_text_style_t *style, sk_scalar_t height
) {
    AsSkParagraphTextStyle(style)->setHeightOverride(height);
}

void skparagraph_text_style_set_half_leading(
    skparagraph_text_style_t *style, sk_scalar_t height
) {
    AsSkParagraphTextStyle(style)->setHalfLeading(height);
}

void skparagraph_text_style_set_letter_spacing(
    skparagraph_text_style_t *style, sk_scalar_t letterSpacing
) {
    AsSkParagraphTextStyle(style)->setLetterSpacing(letterSpacing);
}

void skparagraph_text_style_set_word_spacing(
    skparagraph_text_style_t *style, sk_scalar_t wordSpacing
) {
    AsSkParagraphTextStyle(style)->setWordSpacing(wordSpacing);
}

void skparagraph_text_style_set_typeface(
    skparagraph_text_style_t *style, sk_typeface_t *typeface
) {
    AsSkParagraphTextStyle(style)->setTypeface(sk_ref_sp(AsTypeface(typeface)));
}

void skparagraph_text_style_set_locale(
    skparagraph_text_style_t *style, sk_string_t *locale
) {
    AsSkParagraphTextStyle(style)->setLocale(*AsString(locale));
}

void skparagraph_text_style_set_text_baseline(
    skparagraph_text_style_t *style, skparagraph_text_baseline_t baseline
) {
    AsSkParagraphTextStyle(style)->setTextBaseline(
        (skia::textlayout::TextBaseline)baseline
    );
}

void skparagraph_text_style_set_placeholder(skparagraph_text_style_t *style) {
    AsSkParagraphTextStyle(style)->setPlaceholder();
}

// StrutStyle

skparagraph_strut_style_t *skparagraph_strut_style_new() {
    return ToSkParagraphStrutStyle(new skia::textlayout::StrutStyle());
}

void skparagraph_strut_style_delete(skparagraph_strut_style_t *strut) {
    delete AsSkParagraphStrutStyle(strut);
}

void skparagraph_strut_style_set_font_families(
    skparagraph_strut_style_t *strut, sk_string_t *families[], int familiesCount
) {
    std::vector<SkString> vec(familiesCount);

    for (int i = 0; i < familiesCount; i++) {
        vec[i] = *AsString(families[i]);
    }

    AsSkParagraphStrutStyle(strut)->setFontFamilies(vec);
}

void skparagraph_strut_style_set_font_style(
    skparagraph_strut_style_t *strut, sk_fontstyle_t *style
) {
    AsSkParagraphStrutStyle(strut)->setFontStyle(*AsFontStyle(style));
}

void skparagraph_strut_style_set_font_size(
    skparagraph_strut_style_t *strut, sk_scalar_t size
) {
    AsSkParagraphStrutStyle(strut)->setFontSize(size);
}

void skparagraph_strut_style_set_height(
    skparagraph_strut_style_t *strut, sk_scalar_t height
) {
    AsSkParagraphStrutStyle(strut)->setHeight(height);
}

void skparagraph_strut_style_set_leading(
    skparagraph_strut_style_t *strut, sk_scalar_t leading
) {
    AsSkParagraphStrutStyle(strut)->setLeading(leading);
}

void skparagraph_strut_style_set_strut_enabled(
    skparagraph_strut_style_t *strut, bool v
) {
    AsSkParagraphStrutStyle(strut)->setStrutEnabled(v);
}

void skparagraph_strut_style_set_force_strut_height(
    skparagraph_strut_style_t *strut, bool v
) {
    AsSkParagraphStrutStyle(strut)->setForceStrutHeight(v);
}

void skparagraph_strut_style_set_height_override(
    skparagraph_strut_style_t *strut, bool v
) {
    AsSkParagraphStrutStyle(strut)->setHeightOverride(v);
}

void skparagraph_strut_style_set_half_leading(
    skparagraph_strut_style_t *strut, bool halfLeading
) {
    AsSkParagraphStrutStyle(strut)->setHalfLeading(halfLeading);
}

// ParagraphStyle

skparagraph_paragraph_style_t *skparagraph_paragraph_style_new(void) {
    return ToSkParagraphStyle(new skia::textlayout::ParagraphStyle());
}

void skparagraph_paragraph_style_delete(skparagraph_paragraph_style_t *style) {
    delete AsSkParagraphStyle(style);
}

void skparagraph_paragraph_style_set_strut_style(
    skparagraph_paragraph_style_t *para, skparagraph_strut_style_t *strut
) {
    AsSkParagraphStyle(para)->setStrutStyle(*AsSkParagraphStrutStyle(strut));
}

void skparagraph_paragraph_style_set_text_style(
    skparagraph_paragraph_style_t *para, skparagraph_text_style_t *text
) {
    AsSkParagraphStyle(para)->setTextStyle(*AsSkParagraphTextStyle(text));
}

void skparagraph_paragraph_style_set_text_direction(
    skparagraph_paragraph_style_t *para, skparagraph_text_direction_t dir
) {
    AsSkParagraphStyle(para)->setTextDirection((skia::textlayout::TextDirection
    )dir);
}

void skparagraph_paragraph_style_set_text_align(
    skparagraph_paragraph_style_t *para, skparagraph_text_align_t align
) {
    AsSkParagraphStyle(para)->setTextAlign((skia::textlayout::TextAlign)align);
}

void skparagraph_paragraph_style_set_max_lines(
    skparagraph_paragraph_style_t *para, size_t max_lines
) {
    // TIP: fLinesLimit == std::numeric_limits<size_t>::max(); is how Google
    // Skia makes it limitless
    AsSkParagraphStyle(para)->setMaxLines(max_lines);
}

void skparagraph_paragraph_style_set_ellipsis(
    skparagraph_paragraph_style_t *para, const sk_string_t *ellipsis
) {
    AsSkParagraphStyle(para)->setEllipsis(*AsString(ellipsis));
}

void skparagraph_paragraph_style_set_height(
    skparagraph_paragraph_style_t *para, sk_scalar_t height
) {
    AsSkParagraphStyle(para)->setHeight(height);
}

void skparagraph_paragraph_style_set_text_height_behavior(
    skparagraph_paragraph_style_t *para,
    skparagraph_text_height_behavior_t behavior
) {
    AsSkParagraphStyle(para)->setTextHeightBehavior(
        (skia::textlayout::TextHeightBehavior)behavior
    );
}

void skparagraph_paragraph_style_set_replace_tab_characters(
    skparagraph_paragraph_style_t *para, bool v
) {
    AsSkParagraphStyle(para)->setReplaceTabCharacters(v);
}

void skparagraph_paragraph_style_set_apply_rounding_hack(
    skparagraph_paragraph_style_t *para, bool v
) {
    AsSkParagraphStyle(para)->setApplyRoundingHack(v);
}

// Paragraph

void skparagraph_paragraph_delete(skparagraph_paragraph_t *para) {
    delete AsSkParagraph(para);
}

void skparagraph_paragraph_layout(
    skparagraph_paragraph_t *para, sk_scalar_t width
) {
    AsSkParagraph(para)->layout(width);
}

void skparagraph_paragraph_paint_with_canvas(
    skparagraph_paragraph_t *para, sk_canvas_t *canvas, sk_scalar_t x,
    sk_scalar_t y
) {
    AsSkParagraph(para)->paint(AsCanvas(canvas), x, y);
}

void skparagraph_paragraph_paint_with_painter(
    skparagraph_paragraph_t *para, skparagraph_paragraph_painter_t *painter,
    sk_scalar_t x, sk_scalar_t y
) {
    AsSkParagraph(para)->paint(AsSkParagraphPainter(painter), x, y);
}

sk_scalar_t skparagraph_paragraph_get_max_width(
    skparagraph_paragraph_t *paragraph
) {
    return AsSkParagraph(paragraph)->getMaxWidth();
}

sk_scalar_t skparagraph_paragraph_get_height(skparagraph_paragraph_t *paragraph
) {
    return AsSkParagraph(paragraph)->getHeight();
}

sk_scalar_t skparagraph_paragraph_get_min_intrinsic_width(
    skparagraph_paragraph_t *paragraph
) {
    return AsSkParagraph(paragraph)->getMinIntrinsicWidth();
}

sk_scalar_t skparagraph_paragraph_get_max_intrinsic_width(
    skparagraph_paragraph_t *paragraph
) {
    return AsSkParagraph(paragraph)->getMaxIntrinsicWidth();
}

sk_scalar_t skparagraph_paragraph_get_alphabetic_baseline(
    skparagraph_paragraph_t *paragraph
) {
    return AsSkParagraph(paragraph)->getAlphabeticBaseline();
}

sk_scalar_t skparagraph_paragraph_get_ideographic_baseline(
    skparagraph_paragraph_t *paragraph
) {
    return AsSkParagraph(paragraph)->getIdeographicBaseline();
}

sk_scalar_t skparagraph_paragraph_get_longest_line(
    skparagraph_paragraph_t *paragraph
) {
    return AsSkParagraph(paragraph)->getLongestLine();
}

bool skparagraph_paragraph_did_exceed_max_lines(
    skparagraph_paragraph_t *paragraph
) {
    return AsSkParagraph(paragraph)->didExceedMaxLines();
}

// SKParagraph's so-called "Editing API"

int skparagraph_paragraph_get_line_number_at(
    skparagraph_paragraph_t *para, size_t codeUnitIndex
) {
    return AsSkParagraph(para)->getLineNumberAt(codeUnitIndex);
}

// NOTES:
//
// - Caller must pre-allocate dstMetrics.
bool skparagraph_paragraph_get_line_metrics_at(
    skparagraph_paragraph_t *para, int lineNumber,
    skparagraph_line_metrics_t *dstMetrics
) {
    skia::textlayout::LineMetrics metrics;
    bool ok = AsSkParagraph(para)->getLineMetricsAt(lineNumber, &metrics);
    if (ok) {
        dstMetrics->fStartIndex = metrics.fStartIndex;
        dstMetrics->fEndIndex = metrics.fEndIndex;
        dstMetrics->fEndExcludingWhitespaces = metrics.fEndExcludingWhitespaces;
        dstMetrics->fEndIncludingNewline = metrics.fEndIncludingNewline;
        dstMetrics->fHardBreak = metrics.fHardBreak;
        dstMetrics->fAscent = metrics.fAscent;
        dstMetrics->fDescent = metrics.fDescent;
        dstMetrics->fUnscaledAscent = metrics.fUnscaledAscent;
        dstMetrics->fHeight = metrics.fHeight;
        dstMetrics->fWidth = metrics.fWidth;
        dstMetrics->fLeft = metrics.fLeft;
        dstMetrics->fBaseline = metrics.fBaseline;
        dstMetrics->fLineNumber = metrics.fLineNumber;
    }

    return ok;
}

static void ToGlyphClusterInfo(
    skparagraph_glyph_cluster_info_t &dstInfo,
    const skia::textlayout::Paragraph::GlyphClusterInfo &srcInfo
) {
    dstInfo.fBounds = ToRect(srcInfo.fBounds);

    dstInfo.fClusterTextRange_start = srcInfo.fClusterTextRange.start;
    dstInfo.fClusterTextRange_end = srcInfo.fClusterTextRange.end;

    dstInfo.fGlyphClusterPosition =
        (skparagraph_text_direction_t)srcInfo.fGlyphClusterPosition;
}

// Returns true if glyph cluster was found; false if not (which usually means
// the paragraph is empty)
bool skparagraph_paragraph_get_closest_glyph_cluster_at(
    skparagraph_paragraph_t *para, sk_scalar_t x, sk_scalar_t y,
    skparagraph_glyph_cluster_info_t *dstGlyphInfo
) {
    skia::textlayout::Paragraph::GlyphClusterInfo info;
    bool found = AsSkParagraph(para)->getClosestGlyphClusterAt(x, y, &info);
    if (found) {
        ToGlyphClusterInfo(*dstGlyphInfo, info);
    }
    return found;
}

// Returns true if glyph cluster was found; false if not (which usually means
// the paragraph is empty)
bool skparagraph_paragraph_get_glyph_cluster_at(
    skparagraph_paragraph_t *para, int codeUnitIndex,
    skparagraph_glyph_cluster_info_t *dstGlyphInfo
) {
    skia::textlayout::Paragraph::GlyphClusterInfo info;
    bool found = AsSkParagraph(para)->getGlyphClusterAt(codeUnitIndex, &info);
    if (found) {
        ToGlyphClusterInfo(*dstGlyphInfo, info);
    }
    return found;
}

// Returns the index of the glyph that corresponds to the provided coordinate,
// with the top left corner as the origin, and +y direction as down
void skparagraph_paragraph_get_glyph_position_at_coordinate(
    skparagraph_paragraph_t *para, sk_scalar_t x, sk_scalar_t y,
    int32_t *dstPosition, skparagraph_affinity_t *dstAffinity
) {
    auto p = AsSkParagraph(para)->getGlyphPositionAtCoordinate(x, y);
    *dstPosition = p.position;
    *dstAffinity = (skparagraph_affinity_t)p.affinity;
}

// FontCollection

skparagraph_font_collection_t *skparagraph_font_collection_new(void) {
    return ToSkParagraphFontCollection(new skia::textlayout::FontCollection());
}

void skparagraph_font_collection_delete(skparagraph_font_collection_t *col) {
    delete AsSkParagraphFontCollection(col);
}

void skparagraph_font_collection_set_asset_fontmgr(
    skparagraph_font_collection_t *col, sk_fontmgr_t *fontmgr
) {
    AsSkParagraphFontCollection(col)->setAssetFontManager(
        sk_ref_sp(AsFontMgr(fontmgr))
    );
}

void skparagraph_font_collection_set_dynamic_fontmgr(
    skparagraph_font_collection_t *col, sk_fontmgr_t *fontmgr
) {
    AsSkParagraphFontCollection(col)->setDynamicFontManager(
        sk_ref_sp(AsFontMgr(fontmgr))
    );
}

void skparagraph_font_collection_set_test_fontmgr(
    skparagraph_font_collection_t *col, sk_fontmgr_t *fontmgr
) {
    AsSkParagraphFontCollection(col)->setTestFontManager(
        sk_ref_sp(AsFontMgr(fontmgr))
    );
}

void skparagraph_font_collection_set_default_fontmgr(
    skparagraph_font_collection_t *col, sk_fontmgr_t *fontmgr
) {
    AsSkParagraphFontCollection(col)->setDynamicFontManager(
        sk_ref_sp(AsFontMgr(fontmgr))
    );
}

void skparagraph_font_collection_set_default_fontmgr_with_def_family(
    skparagraph_font_collection_t *col, sk_fontmgr_t *fontmgr,
    const char *defaultFamilyName
) {
    AsSkParagraphFontCollection(col)->setDefaultFontManager(
        sk_ref_sp(AsFontMgr(fontmgr)), defaultFamilyName
    );
}

void skparagraph_font_collection_set_default_fontmgr_with_def_families(
    skparagraph_font_collection_t *col, sk_fontmgr_t *fontmgr,
    sk_string_t *defaultFamilyNames[], int defaultFamilyNamesCount
) {
    std::vector<SkString> vec(defaultFamilyNamesCount);

    for (int i = 0; i < defaultFamilyNamesCount; i++) {
        vec[i] = *AsString(defaultFamilyNames[i]);
    }

    AsSkParagraphFontCollection(col)->setDefaultFontManager(
        sk_ref_sp(AsFontMgr(fontmgr)), vec
    );
}

void skparagraph_font_collection_enable_font_fallback(
    skparagraph_font_collection_t *col
) {
    AsSkParagraphFontCollection(col)->enableFontFallback();
}

void skparagraph_font_collection_disable_font_fallback(
    skparagraph_font_collection_t *col
) {
    AsSkParagraphFontCollection(col)->disableFontFallback();
}

bool skparagraph_font_collection_font_fallback_enabled(
    skparagraph_font_collection_t *col
) {
    return AsSkParagraphFontCollection(col)->fontFallbackEnabled();
}

// Builder

skparagraph_paragraph_builder_impl_t *skparagraph_paragraph_builder_impl_new(
    skparagraph_paragraph_style_t *style,
    skparagraph_font_collection_t *fontCollection,
    skunicode_skunicode_t *unicode
) {
    return ToSkParagraphBuilderImpl(new skia::textlayout::ParagraphBuilderImpl(
        *AsSkParagraphStyle(style),
        sk_ref_sp(AsSkParagraphFontCollection(fontCollection)),
        sk_ref_sp(AsSkUnicode(unicode))
    ));
}

void skparagraph_paragraph_builder_impl_delete(
    skparagraph_paragraph_builder_impl_t *builder
) {
    delete AsSkParagraphBuilderImpl(builder);
}

void skparagraph_paragraph_builder_add_text_utf8(
    skparagraph_paragraph_builder_t *builder, const char *text
) {
    AsSkParagraphBuilder(builder)->addText(text);
}

void skparagraph_paragraph_builder_add_text_utf8_len(
    skparagraph_paragraph_builder_t *builder, const char *text, size_t len
) {
    AsSkParagraphBuilder(builder)->addText(text, len);
}

void skparagraph_paragraph_builder_add_placeholder(
    skparagraph_paragraph_builder_t *builder,
    skparagraph_placeholder_style_t *instyle
) {
    skia::textlayout::PlaceholderStyle style(
        instyle->fWidth, instyle->fHeight,
        (skia::textlayout::PlaceholderAlignment)instyle->fAlignment,
        (skia::textlayout::TextBaseline)instyle->fBaseline,
        instyle->fBaselineOffset
    );
    AsSkParagraphBuilder(builder)->addPlaceholder(style);
}

skparagraph_paragraph_t *skparagraph_paragraph_builder_build(
    skparagraph_paragraph_builder_t *builder
) {
    return ToSkParagraph(AsSkParagraphBuilder(builder)->Build().release());
}

void skparagraph_paragraph_builder_push_style(
    skparagraph_paragraph_builder_t *builder, skparagraph_text_style_t *style
) {
    AsSkParagraphBuilder(builder)->pushStyle(*AsSkParagraphTextStyle(style));
}

void skparagraph_paragraph_builder_pop(skparagraph_paragraph_builder_t *builder
) {
    AsSkParagraphBuilder(builder)->pop();
}

void skparagraph_paragraph_builder_reset(skparagraph_paragraph_builder_t *builder
) {
    AsSkParagraphBuilder(builder)->Reset();
}