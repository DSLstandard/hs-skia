module Skia.SKParagraph.Enums where

import NeatInterpolation
import Skia.Bindings.Types
import Skia.Internal.THUtils

$( qGenerateSKEnum
    "Affinity"
    ''Skparagraph_affinity
    ""
    [ ("Upstream", 'UPSTREAM_SKPARAGRAPH_AFFINITY, "")
    , ("Downstream", 'DOWNSTREAM_SKPARAGRAPH_AFFINITY, "")
    ]
 )

$( qGenerateSKEnum
    "RectHeightStyle"
    ''Skparagraph_rect_height_style
    ""
    [
      ( "Tight"
      , 'TIGHT_SKPARAGRAPH_RECT_HEIGHT_STYLE
      , "Provide tight bounding boxes that fit heights per run."
      )
    ,
      ( "Max"
      , 'MAX_SKPARAGRAPH_RECT_HEIGHT_STYLE
      , [text|
          The height of the boxes will be the maximum height of all runs in the line.
          All rects in the same line will be the same height."
        |]
      )
    ,
      ( "IncludeLineSpacingMiddle"
      , 'INCLUDE_LINE_SPACING_MIDDLE_SKPARAGRAPH_RECT_HEIGHT_STYLE
      , [text|
          Extends the top and/or bottom edge of the bounds to fully cover any line
          spacing. The top edge of each line should be the same as the bottom edge
          of the line above. There should be no gaps in vertical coverage given any
          ParagraphStyle line_height.

          The top and bottom of each rect will cover half of the
          space above and half of the space below the line.
        |]
      )
    ,
      ( "IncludeLineSpacingTop"
      , 'INCLUDE_LINE_SPACING_TOP_SKPARAGRAPH_RECT_HEIGHT_STYLE
      , "The line spacing will be added to the top of the rect."
      )
    ,
      ( "IncludeLineSpacingBottom"
      , 'INCLUDE_LINE_SPACING_BOTTOM_SKPARAGRAPH_RECT_HEIGHT_STYLE
      , "The line spacing will be added to the bottom of the rect."
      )
    , ("Strut", 'STRUT_SKPARAGRAPH_RECT_HEIGHT_STYLE, "")
    ]
 )

$( qGenerateSKEnum
    "RectWidthStyle"
    ''Skparagraph_rect_width_style
    ""
    [
      ( "Tight"
      , 'TIGHT_SKPARAGRAPH_RECT_WIDTH_STYLE
      , "Provide tight bounding boxes that fit widths to the runs of each line independently."
      )
    ,
      ( "Max"
      , 'MAX_SKPARAGRAPH_RECT_WIDTH_STYLE
      , "Extends the width of the last rect of each line to match the position of the widest rect over all the lines."
      )
    ]
 )

$( qGenerateSKEnum
    "TextAlign"
    ''Skparagraph_text_align
    ""
    [ ("Left", 'LEFT_SKPARAGRAPH_TEXT_ALIGN, "")
    , ("Right", 'RIGHT_SKPARAGRAPH_TEXT_ALIGN, "")
    , ("Center", 'CENTER_SKPARAGRAPH_TEXT_ALIGN, "")
    , ("Justify", 'JUSTIFY_SKPARAGRAPH_TEXT_ALIGN, "")
    , ("Start", 'START_SKPARAGRAPH_TEXT_ALIGN, "")
    , ("End", 'END_SKPARAGRAPH_TEXT_ALIGN, "")
    ]
 )

$( qGenerateSKEnum
    "TextDirection"
    ''Skparagraph_text_direction
    ""
    [ ("RTL", 'RTL_SKPARAGRAPH_TEXT_DIRECTION, "")
    , ("LTR", 'LTR_SKPARAGRAPH_TEXT_DIRECTION, "")
    ]
 )

$( qGenerateSKEnum
    "TextBaseline"
    ''Skparagraph_text_baseline
    ""
    [ ("Alphabetic", 'ALPHABETIC_SKPARAGRAPH_TEXT_BASELINE, "")
    , ("Ideographic", 'IDEOGRAPHIC_SKPARAGRAPH_TEXT_BASELINE, "")
    ]
 )

$( qGenerateSKEnum
    "TextHeightBehavior"
    ''Skparagraph_text_height_behavior
    ""
    [ ("All", 'ALL_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR, "")
    , ("DisableFirstAscent", 'DISABLE_FIRST_ASCENT_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR, "")
    , ("DisableLastDescent", 'DISABLE_LAST_DESCENT_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR, "")
    , ("DisableAll", 'DISABLE_ALL_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR, "")
    ]
 )

$( qGenerateSKEnum
    "LineMetricStyle"
    ''Skparagraph_line_metric_style
    ""
    [
      ( "Typographic"
      , 'TYPOGRAPHIC_SKPARAGRAPH_LINE_METRIC_STYLE
      , "Use ascent, descent, etc from a fixed baseline."
      )
    ,
      ( "CSS"
      , 'CSS_SKPARAGRAPH_LINE_METRIC_STYLE
      , "Use ascent, descent, etc like css with the leading split and with height adjustments"
      )
    ]
 )

$( qGenerateSKEnum
    "TextDecorationMode"
    ''Skparagraph_text_decoration_mode
    ""
    [ ("Gaps", 'GAPS_SKPARAGRAPH_TEXT_DECORATION_MODE, "")
    , ("Through", 'THROUGH_SKPARAGRAPH_TEXT_DECORATION_MODE, "")
    ]
 )

$( qGenerateSKEnum
    "TextDecorationStyle"
    ''Skparagraph_text_decoration_style
    ""
    [ ("Solid", 'SOLID_SKPARAGRAPH_TEXT_DECORATION_STYLE, "")
    , ("Double", 'DOUBLE_SKPARAGRAPH_TEXT_DECORATION_STYLE, "")
    , ("Dotted", 'DOTTED_SKPARAGRAPH_TEXT_DECORATION_STYLE, "")
    , ("Dashed", 'DASHED_SKPARAGRAPH_TEXT_DECORATION_STYLE, "")
    , ("Wavy", 'WAVY_SKPARAGRAPH_TEXT_DECORATION_STYLE, "")
    ]
 )

$( qGenerateSKEnum
    "PlaceholderAlignment"
    ''Skparagraph_placeholder_alignment
    "Where to vertically align the placeholder relative to the surrounding text."
    [ ("Baseline", 'BASELINE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT, "Match the baseline of the placeholder with the baseline.")
    , ("AboveBaseline", 'ABOVE_BASELINE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT, "Align the bottom edge of the placeholder with the baseline such that the placeholder sits on top of the baseline.")
    , ("BelowBaseline", 'BELOW_BASELINE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT, "Align the top edge of the placeholder with the baseline specified in such that the placeholder hangs below the baseline.")
    , ("Top", 'TOP_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT, "Align the top edge of the placeholder with the top edge of the font. When the placeholder is very tall, the extra space will hang from the top and extend through the bottom of the line.")
    , ("Bottom", 'BOTTOM_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT, "Align the bottom edge of the placeholder with the top edge of the font. When the placeholder is very tall, the extra space will rise from the bottom and extend through the top of the line.")
    , ("Middle", 'MIDDLE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT, "Align the middle of the placeholder with the middle of the text. When the placeholder is very tall, the extra space will grow equally from the top and bottom of the line.")
    ]
 )
