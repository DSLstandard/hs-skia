module Skia.SKParagraph.TextShadow where

import Skia.Bindings.Types
import Skia.Color
import Skia.SKParagraph.Internal.Prelude

data TextShadow = TextShadow
  { color :: SKColor
  , offset :: V2 Float
  , blurSigma :: Double
  }
  deriving (Show, Eq, Ord)

-- * Marshal utils

marshalTextShadow :: TextShadow -> Managed (Ptr Skparagraph_text_shadow)
marshalTextShadow shadow =
  storable $
    Skparagraph_text_shadow
      { fColor = unSKColor shadow.color
      , fOffset = toSKPoint shadow.offset
      , fBlurSigma = coerce shadow.blurSigma
      }
