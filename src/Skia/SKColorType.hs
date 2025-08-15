module Skia.SKColorType where

import Skia.Bindings.Sk_color
import Skia.Internal.Prelude

{- | Selects the native 32-bit ARGB format for the current configuration. This
can lead to inconsistent results across platforms, so use with caution.
-}
getNativeArgb32Format :: (MonadIO m) => m SKColorType
getNativeArgb32Format = liftIO do
  ty <- liftIO $ sk_colortype_get_default_8888
  unmarshalSKEnumOrDie ty

alphaType :: SKColorType -> SKAlphaType -> SKAlphaType
alphaType colorType alphaType =
  -- See SkiaSharp's binding/SkiaSharp/Definitions.cs
  case colorType of
    SKColorType'Unknown -> SKAlphaType'Unknown
    -- SkiaSharp: "Opaque or premul"
    SKColorType'Alpha'8 -> onOpaqueOrPremul
    SKColorType'Read'A16'Unorm -> onOpaqueOrPremul
    SKColorType'Read'A16'Float -> onOpaqueOrPremul
    -- SkiaSharp: "Any"
    SKColorType'ARGB'4444 -> onAny
    SKColorType'RGBA'8888 -> onAny
    SKColorType'BGRA'8888 -> onAny
    SKColorType'SRGBA'8888 -> onAny
    SKColorType'RGBA'1010102 -> onAny
    SKColorType'BGRA'1010102 -> onAny
    SKColorType'RGBA'F16'Norm -> onAny
    SKColorType'RGBA'F16 -> onAny
    SKColorType'RGBA'F32 -> onAny
    SKColorType'Read'R16G16B16A16'Unorm -> onAny
    -- SkiaSharp: "Opaque"
    SKColorType'Gray'8 -> onOpaque
    SKColorType'Read'R8G8'Unorm -> onOpaque
    SKColorType'Read'R16G16'Unorm -> onOpaque
    SKColorType'Read'R16G16'Float -> onOpaque
    SKColorType'RGB'565 -> onOpaque
    SKColorType'RGB'888x -> onOpaque
    SKColorType'RGB'101010x -> onOpaque
    SKColorType'BGR'101010x -> onOpaque
    SKColorType'BGR'101010x'XR -> onOpaque
    SKColorType'R8'Unorm -> onOpaque
 where
  onOpaqueOrPremul = case alphaType of
    SKAlphaType'Unpremul -> SKAlphaType'Premul
    _ -> alphaType

  onAny = alphaType

  onOpaque = SKAlphaType'Opaque
