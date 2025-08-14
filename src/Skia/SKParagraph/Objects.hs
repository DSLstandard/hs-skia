module Skia.SKParagraph.Objects where

import Foreign
import Skia.Bindings
import Skia.Bindings.Types
import Skia.Core
import Skia.Internal.THUtils

-- * Google Skia's `modules/skparagraph`

$(qGenerateSKObject "FontCollection" ''Skparagraph_font_collection [] "")

$(qGenerateSKObject "StrutStyle" ''Skparagraph_strut_style [] "")
$(qGenerateSKObject "TextStyle" ''Skparagraph_text_style [] "")
$(qGenerateSKObject "ParagraphStyle" ''Skparagraph_paragraph_style [] "")

$(qGenerateSKObject "Paragraph" ''Skparagraph_paragraph [] "")

$(qGenerateSKObject "ParagraphBuilder" ''Skparagraph_paragraph_builder [] "")
$(qGenerateSKObject "ParagraphBuilderImpl" ''Skparagraph_paragraph_builder_impl [''ParagraphBuilder] "")

$(qGenerateSKObject "ParagraphPainter" ''Skparagraph_paragraph_painter [] "")
$(qGenerateSKObject "CanvasParagraphPainter" ''Skparagraph_paragraph_painter [''ParagraphPainter] "")
