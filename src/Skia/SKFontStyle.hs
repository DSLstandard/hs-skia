module Skia.SKFontStyle where

import Skia.Bindings.Sk_typeface
import Skia.Bindings.Types
import Skia.Internal.Prelude

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

-- * SKFontStyle

{-
Google Skia's SKFontStyle is a very cheap structure - a single int32_t encoding
weight, width, and slant.

To see the definition of the data fields, see Google Skia's
include/core/SkFontStyle.h

This datatype only holds what is decoded values.

We use 'peekSKFontStyle', 'useSKFontStyle', and 'managed allocaSKFontStyle' to do FFI
for Skia functions involving 'Sk_fontstyle'.
-}
data SKFontStyle = SKFontStyle
  { weight :: Weight
  , width :: Width
  , slant :: SKFontStyleSlant
  }
  deriving (Show, Eq, Ord)

style'Normal :: SKFontStyle
style'Normal =
  SKFontStyle
    { weight = weight'Normal
    , width = width'Normal
    , slant = SKFontStyleSlant'Upright
    }

style'Bold :: SKFontStyle
style'Bold =
  SKFontStyle
    { weight = weight'Bold
    , width = width'Normal
    , slant = SKFontStyleSlant'Upright
    }

style'Italic :: SKFontStyle
style'Italic =
  SKFontStyle
    { weight = weight'Normal
    , width = width'Normal
    , slant = SKFontStyleSlant'Italic
    }

style'BoldItalic :: SKFontStyle
style'BoldItalic =
  SKFontStyle
    { weight = weight'Bold
    , width = width'Normal
    , slant = SKFontStyleSlant'Italic
    }

-- * Marshal utils

peekSKFontStyle :: (MonadIO m) => Ptr Sk_fontstyle -> m SKFontStyle
peekSKFontStyle ptr = liftIO do
  weight <- fmap fromIntegral $ sk_fontstyle_get_weight ptr
  width <- fmap fromIntegral $ sk_fontstyle_get_width ptr
  slant <- unmarshalSKEnumOrDie =<< sk_fontstyle_get_slant ptr
  pure SKFontStyle{..}

marshalSKFontStyle :: SKFontStyle -> Managed (Ptr Sk_fontstyle)
marshalSKFontStyle style =
  managed $
    Control.Exception.bracket
      ( sk_fontstyle_new
          (fromIntegral style.weight)
          (fromIntegral style.width)
          (marshalSKEnum style.slant)
      )
      sk_fontstyle_delete
