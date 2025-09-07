module Skia.SkFontStyle where

import Language.C.Inline.Cpp qualified as C
import Skia.Internal.Prelude
import Skia.Internal.THUtils

C.context $ mconcat
  [ C.cppCtx
  , cppSkiaObjectTypes
  ]

C.include "core/SkFontStyle.h"

-- * Font weight

-- See include/core/SkFontStyle.h
type Weight = Int

weight'Invisible :: Weight
weight'Invisible = 0

weight'Thin :: Weight
weight'Thin = 100

weight'ExtraLight :: Weight
weight'ExtraLight = 200

weight'Light :: Weight
weight'Light = 300

weight'Normal :: Weight
weight'Normal = 400

weight'Medium :: Weight
weight'Medium = 500

weight'SemiBold :: Weight
weight'SemiBold = 600

weight'Bold :: Weight
weight'Bold = 700

weight'ExtraBold :: Weight
weight'ExtraBold = 800

weight'Black :: Weight
weight'Black = 900

weight'ExtraBlack :: Weight
weight'ExtraBlack = 1000

-- * Font width

-- See include/core/SkFontStyle.h
type Width = Int

width'UltraCondensed :: Width
width'UltraCondensed = 1

width'ExtraCondensed :: Width
width'ExtraCondensed = 2

width'Condensed :: Width
width'Condensed = 3

width'SemiCondensed :: Width
width'SemiCondensed = 4

width'Normal :: Width
width'Normal = 5

width'SemiExpanded :: Width
width'SemiExpanded = 6

width'Expanded :: Width
width'Expanded = 7

width'ExtraExpanded :: Width
width'ExtraExpanded = 8

width'UltraExpanded :: Width
width'UltraExpanded = 9

-- * Slant

$( qGenerateSkEnum
  "Slant"
  ""
  [ ("Upright", "SkFontStyle::Slant::kUpright_Slant", "")
  , ("Italic", "SkFontStyle::Slant::kItalic_Slant", "")
  , ("Oblique", "SkFontStyle::Slant::kOblique_Slant", "")
  ]
 )

-- * FontStyle

{-
Google Skia's FontStyle is a very cheap structure - a single int32_t encoding
weight, width, and slant.

To see the definition of the data fields, see Google Skia's
include/core/SkFontStyle.h

This datatype only holds what is decoded values.

We use 'peekFontStyle', 'useFontStyle', and 'managed allocaFontStyle' to do FFI
for Skia functions involving 'Sk_fontstyle'.
-}
data FontStyle = FontStyle
  { weight :: Weight
  , width :: Width
  , slant :: Slant
  }
  deriving (Show, Eq, Ord)

style'Normal :: FontStyle
style'Normal =
  FontStyle
    { weight = weight'Normal
    , width = width'Normal
    , slant = Slant'Upright
    }

style'Bold :: FontStyle
style'Bold =
  FontStyle
    { weight = weight'Bold
    , width = width'Normal
    , slant = Slant'Upright
    }

style'Italic :: FontStyle
style'Italic =
  FontStyle
    { weight = weight'Normal
    , width = width'Normal
    , slant = Slant'Italic
    }

style'BoldItalic :: FontStyle
style'BoldItalic =
  FontStyle
    { weight = weight'Bold
    , width = width'Normal
    , slant = Slant'Italic
    }

-- * Marshal utils

peekSkFontStyle :: (MonadIO m) => Ptr C'SkFontStyle -> m FontStyle
peekSkFontStyle p = liftIO do
  weight <- fromIntegral <$> [C.exp| int { $(SkFontStyle* p)->weight() } |]
  width <- fromIntegral <$> [C.exp| int { $(SkFontStyle* p)->width() } |]
  slant <- unmarshalSkEnumOrDie =<< [C.exp| int { (int) $(SkFontStyle* p)->slant() } |]
  pure FontStyle{weight, width, slant}

allocaSkFontStyle :: Managed (Ptr C'SkFontStyle)
allocaSkFontStyle =
  managed $
    Control.Exception.bracket
      ([C.block|SkFontStyle* { return new SkFontStyle(); }|])
      (\p -> [C.block|void { delete $(SkFontStyle* p); }|])

marshalSkFontStyle :: FontStyle -> Managed (Ptr C'SkFontStyle)
marshalSkFontStyle style =
  managed $
    Control.Exception.bracket
      ( do
          let cwidth :: CInt = fromIntegral style.width
          let cweight :: CInt = fromIntegral style.weight
          let cslant :: CInt = marshalSkEnum style.slant
          [C.exp| SkFontStyle* {
            new SkFontStyle(
              $(int cweight),
              $(int cwidth),
              (SkFontStyle::Slant) $(int cslant)
            )
          }|]
      )
      (\p -> [C.block|void { delete $(SkFontStyle* p); }|])