module Skia.SkColorType where

import Skia.Internal.Prelude
import Skia.Internal.THUtils
import NeatInterpolation
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkColorType.h"

$( qGenerateSkEnum
  "SkColorType"
  [trimming|
    Describes how pixel bits encode color. A pixel may be an alpha mask, a grayscale, RGB, or ARGB.

    'Skia.SkColorType.getNative32' selects the native 32-bit ARGB format for the current configuration.
  |]
  [ ("Unknown", "SkColorType::kUnknown_SkColorType", "Uninitialized")
  , ("Alpha'8", "SkColorType::kAlpha_8_SkColorType", "Pixel with alpha in 8-bit byte")
  , ("RGB'565", "SkColorType::kRGB_565_SkColorType", "Pixel with 5 bits red, 6 bits green, 5 bits blue, in 16-bit word")
  , ("ARGB'4444", "SkColorType::kARGB_4444_SkColorType", "Pixel with 4 bits for alpha, red, green, blue; in 16-bit word")
  , ("RGBA'8888", "SkColorType::kRGBA_8888_SkColorType", "Pixel with 8 bits for red, green, blue, alpha; in 32-bit word")
  , ("RGB'888x", "SkColorType::kRGB_888x_SkColorType", "Pixel with 8 bits each for red, green, blue; in 32-bit word")
  , ("BGRA'8888", "SkColorType::kBGRA_8888_SkColorType", "Pixel with 8 bits for blue, green, red, alpha; in 32-bit word")
  , ("RGBA'1010102", "SkColorType::kRGBA_1010102_SkColorType", "10 bits for red, green, blue; 2 bits for alpha; in 32-bit word")
  , ("BGRA'1010102", "SkColorType::kBGRA_1010102_SkColorType", "10 bits for blue, green, red; 2 bits for alpha; in 32-bit word")
  , ("RGB'101010x", "SkColorType::kRGB_101010x_SkColorType", "Pixel with 10 bits each for red, green, blue; in 32-bit word")
  , ("BGR'101010x", "SkColorType::kBGR_101010x_SkColorType", "Pixel with 10 bits each for blue, green, red; in 32-bit word")
  , ("BGR'101010x'XR", "SkColorType::kBGR_101010x_XR_SkColorType", "Pixel with 10 bits each for blue, green, red; in 32-bit word, extended range")
  , ("BGRA'10101010'XR", "SkColorType::kBGRA_10101010_XR_SkColorType", "Pixel with 10 bits each for blue, green, red, alpha; in 64-bit word, extended range")
  , ("RGBA'10x6", "SkColorType::kRGBA_10x6_SkColorType", "Pixel with 10 used bits (most significant) followed by 6 unused bits for red, green, blue, alpha; in 64-bit word")
  , ("Gray'8", "SkColorType::kGray_8_SkColorType", "Pixel with grayscale level in 8-bit byte")
  , ("RGBA'F16Norm", "SkColorType::kRGBA_F16Norm_SkColorType", "Pixel with half floats in [0,1] for red, green, blue, alpha; in 64-bit word")
  , ("RGBA'F16", "SkColorType::kRGBA_F16_SkColorType", "Pixel with half floats or red, green, blue, alpha; in 64-bit word")
  , ("RGB'F16F16F16x", "SkColorType::kRGB_F16F16F16x_SkColorType", "Pixel with half floats for red, green, blue; in 64-bit word")
  , ("RGBA'F32", "SkColorType::kRGBA_F32_SkColorType", "Pixel using C float for red, green, blue, alpha; in 128-bit word")
  , ("R8G8'Unorm", "SkColorType::kR8G8_unorm_SkColorType", "Pixel with a @uint8_t@ for red and green. This color type is only for reading from - not for rendering to")
  , ("A16'Float", "SkColorType::kA16_float_SkColorType", "Pixel with a half float for alpha. This color type is only for reading from - not for rendering to")
  , ("R16G16'Float", "SkColorType::kR16G16_float_SkColorType", "Pixel with a half float for red and green. This color type is only for reading from - not for rendering to")
  , ("A16'Unorm", "SkColorType::kA16_unorm_SkColorType", "Pixel with a little endian @uint16_t@ for alpha. This color type is only for reading from - not for rendering to")
  , ("R16G16'Unorm", "SkColorType::kR16G16_unorm_SkColorType", "Pixel with a little endian @uint16_t@ for red and green. This color type is only for reading from - not for rendering to")
  , ("R16G16B16A16'Unorm", "SkColorType::kR16G16B16A16_unorm_SkColorType", "Pixel with a little endian @uint16_t@ for red, green, blue, and alpha. This color type is only for reading from - not for rendering to")
  , ("SRGBA'8888", "SkColorType::kSRGBA_8888_SkColorType", "Pixel with 8 bits for red, green, blue, alpha in sRGB colorspace; in 32-bit word")
  , ("R8'Unorm", "SkColorType::kR8_unorm_SkColorType", "Pixel with 8 bits for red; in 8-bit byte")
  ]
 )
