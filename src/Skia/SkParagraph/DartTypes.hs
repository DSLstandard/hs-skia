module Skia.SkParagraph.DartTypes where

import Language.C.Inline.Cpp qualified as C
import Skia.SkParagraph.Internal.Prelude
import Skia.Internal.THUtils

C.context $ C.cppCtx <> cppSkiaObjectTypes <> cppSkParagraphObjectTypes

C.include "modules/skparagraph/include/DartTypes.h"

$( qGenerateSkEnum
  "TextBaseline"
  "Text baseline, specifying the baseline to use for aligning text"
  [ ("Alphabetic", "skia::textlayout::TextBaseline::kAlphabetic", "The normal alphabetic baseline")
  , ("Ideographic", "skia::textlayout::TextBaseline::kIdeographic", "The ideographic baseline")
  ]
 )

$( qGenerateSkEnum
  "TextDirection"
  "Text direction, specifying the direction of the text flow"
  [ ("RTL", "skia::textlayout::TextDirection::kRtl", "Right to Left")
  , ("LTR", "skia::textlayout::TextDirection::kLtr", "Left to Right")
  ]
 )

$( qGenerateSkEnum
  "TextHeightBehavior"
  "Text height behavior, specifying how to handle first ascent and last descent"
  [ ("All", "skia::textlayout::TextHeightBehavior::kAll", "Default behavior")
  , ("DisableFirstAscent", "skia::textlayout::TextHeightBehavior::kDisableFirstAscent", "Disable first ascent")
  , ("DisableLastDescent", "skia::textlayout::TextHeightBehavior::kDisableLastDescent", "Disable last descent")
  , ("DisableAll", "skia::textlayout::TextHeightBehavior::kDisableAll", "Disable both first ascent and last descent")
  ]
 )

$( qGenerateSkEnum
  "LineMetricStyle"
  "Line metric style, specifying how to calculate line metrics"
  [ ("Typographic", "skia::textlayout::LineMetricStyle::Typographic", "Use ascent, descent, etc from a fixed baseline")
  , ("CSS", "skia::textlayout::LineMetricStyle::CSS", "Use ascent, descent, etc like css with the leading split and with height adjustments")
  ]
 )

$( qGenerateSkEnum
  "Affinity"
  "Affinity of a text position, specifying the preferred direction for cursor movement"
  [ ("Upstream", "skia::textlayout::Affinity::kUpstream", "Upstream affinity")
  , ("Downstream", "skia::textlayout::Affinity::kDownstream", "Downstream affinity")
  ]
 )

$( qGenerateSkEnum
  "RectHeightStyle"
  "Style for the height of text rects"
  [ ("Tight", "skia::textlayout::RectHeightStyle::kTight", "Tight bounding boxes that fit heights per run")
  , ("Max", "skia::textlayout::RectHeightStyle::kMax", "Maximum height of all runs in the line")
  , ("IncludeLineSpacingMiddle", "skia::textlayout::RectHeightStyle::kIncludeLineSpacingMiddle", "Cover half of space above and half of space below")
  , ("IncludeLineSpacingTop", "skia::textlayout::RectHeightStyle::kIncludeLineSpacingTop", "Add line spacing to the top of the rect")
  , ("IncludeLineSpacingBottom", "skia::textlayout::RectHeightStyle::kIncludeLineSpacingBottom", "Add line spacing to the bottom of the rect")
  , ("Strut", "skia::textlayout::RectHeightStyle::kStrut", "Use strut metrics")
  ]
 )

$( qGenerateSkEnum
  "RectWidthStyle"
  "Style for the width of text rects"
  [ ("Tight", "skia::textlayout::RectWidthStyle::kTight", "Tight bounding boxes per run")
  , ("Max", "skia::textlayout::RectWidthStyle::kMax", "Extend width to match the widest rect")
  ]
 )

$( qGenerateSkEnum
  "TextAlign"
  "Text alignment, specifying how to align text horizontally"
  [ ("Left", "skia::textlayout::TextAlign::kLeft", "Align to the left")
  , ("Right", "skia::textlayout::TextAlign::kRight", "Align to the right")
  , ("Center", "skia::textlayout::TextAlign::kCenter", "Align to the center")
  , ("Justify", "skia::textlayout::TextAlign::kJustify", "Justify text")
  , ("Start", "skia::textlayout::TextAlign::kStart", "Align to the start (depends on text direction)")
  , ("End", "skia::textlayout::TextAlign::kEnd", "Align to the end (depends on text direction)")
  ]
 )
