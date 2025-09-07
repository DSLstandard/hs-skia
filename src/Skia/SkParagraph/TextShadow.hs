module Skia.SkParagraph.TextShadow where

import Skia.SkColor
import Skia.SkParagraph.Internal.Prelude
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes <> cppSkParagraphObjectTypes

C.include "modules/skparagraph/include/TextShadow.h"

data TextShadow = TextShadow
  { color :: SkColor
  , offset :: V2 Float
  , blurSigma :: Double
  }
  deriving (Show, Eq, Ord)

-- * Marshal utils
marshalTextShadow :: TextShadow -> Managed (Ptr C'TextShadowRaw)
marshalTextShadow shadow = do
  managed $ bracket
    ( do
        let SkColor fColor = shadow.color
        let (coerce -> V2 fOffsetX fOffsetY) = shadow.offset
        let fBlurSigma :: CDouble = coerce shadow.blurSigma

        [C.exp|skia::textlayout::TextShadow* {
          new skia::textlayout::TextShadow(
            $(uint32_t fColor),
            SkPoint::Make($(float fOffsetX), $(float fOffsetY)),
            $(double fBlurSigma)
          )
        }|]
    )
    (\p -> [C.block| void { delete $(skia::textlayout::TextShadow* p); } |])