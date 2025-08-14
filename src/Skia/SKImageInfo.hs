module Skia.SKImageInfo (
  module Skia.SKImageInfo,
  SKImageInfo (..), -- from Skia.Types.Extra,
)
where

import Skia.Bindings.Types
import Skia.Internal.Prelude

{- | Describes pixel dimensions and encoding. 'SKBitmap', 'SKImage', 'SKPixmap',
and 'SKSurface' can be created from 'SKImageInfo'.

'SKImageInfo' can be retrieved from 'SKBitmap' and 'SKPixmap', but not from
'SKImage' and 'SKSurface'. For example, 'SKImage' and 'SKSurface'
implementations may defer pixel depth, so may not completely specify
'SKImageInfo'.

'SKImageInfo' contains dimensions, the pixel integral width and height. It
encodes how pixel bits describe alpha, transparency; color components red, blue,
and green; and 'SKColorSpace', the range and linearity of colors.
-}
data SKImageInfo = SKImageInfo
  { colorspace :: SKColorSpace -- TODO: Use 'Maybe' to allow 'nullPtr'?
  , width :: Int
  , height :: Int
  , colorType :: SKColorType
  , alphaType :: SKAlphaType
  }
  deriving (Show)

-- * Marshal utils

unmarshalSKImageInfo :: (MonadIO m) => Sk_imageinfo -> m SKImageInfo
unmarshalSKImageInfo iminfo = do
  -- NOTE: This function is IO just for 'unmarshalSKEnumOrDie"
  colorType <- unmarshalSKEnumOrDie iminfo.colorType
  alphaType <- unmarshalSKEnumOrDie iminfo.alphaType
  pure
    SKImageInfo
      { colorspace = fromPtr iminfo.colorspace
      , width = fromIntegral iminfo.width
      , height = fromIntegral iminfo.height
      , colorType
      , alphaType
      }

marshalSKImageInfo :: SKImageInfo -> Sk_imageinfo
marshalSKImageInfo iminfo =
  Sk_imageinfo
    { colorspace = ptr iminfo.colorspace
    , width = fromIntegral iminfo.width
    , height = fromIntegral iminfo.height
    , colorType = marshalSKEnum iminfo.colorType
    , alphaType = marshalSKEnum iminfo.alphaType
    }
