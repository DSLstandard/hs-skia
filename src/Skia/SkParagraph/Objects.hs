module Skia.SkParagraph.Objects where

import Foreign
import Language.C.Inline.Cpp qualified as C
import Skia.Core
import Skia.Internal.THUtils

$(qGenerateSkObject "skia::textlayout::FontCollection" "FontCollection" [] "")
$(qGenerateSkObject "skia::textlayout::LineMetrics" "LineMetricsRaw" [] "")
$(qGenerateSkObject "skia::textlayout::Paragraph" "Paragraph" [] "")
$(qGenerateSkObject "skia::textlayout::Paragraph::GlyphClusterInfo" "GlyphClusterInfoRaw" [] "")
$(qGenerateSkObject "skia::textlayout::ParagraphBuilder" "ParagraphBuilder" [] "")
$(qGenerateSkObject "skia::textlayout::ParagraphPainter" "ParagraphPainter" [] "")
$(qGenerateSkObject "skia::textlayout::ParagraphStyle" "ParagraphStyle" [] "")
$(qGenerateSkObject "skia::textlayout::PlaceholderStyle" "PlaceholderStyleRaw" [] "")
$(qGenerateSkObject "skia::textlayout::StrutStyle" "StrutStyle" [] "")
$(qGenerateSkObject "skia::textlayout::StyleMetrics" "StyleMetricsRaw" [] "")
$(qGenerateSkObject "skia::textlayout::TextDecoration" "TextDecorationRaw" [] "")
$(qGenerateSkObject "skia::textlayout::TextShadow" "TextShadowRaw" [] "")
$(qGenerateSkObject "skia::textlayout::TextStyle" "TextStyle" [] "")

$(qGenerateSkObject "skia::textlayout::CanvasParagraphPainter" "CanvasParagraphPainter" [''ParagraphPainter] "")
$(qGenerateSkObject "skia::textlayout::ParagraphBuilderImpl" "ParagraphBuilderImpl" [''ParagraphBuilder] "")

cppSkParagraphObjectTypes :: C.Context
cppSkParagraphObjectTypes = formInlineCppTypePairs $(qGatherInlineSkObjectEntries)
