module Skia.SkBlendMode where

import Skia.Internal.Prelude
import Skia.Internal.THUtils
import NeatInterpolation
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkBlendMode.h"

$( qGenerateSkEnum
  "SkBlendMode"
  [trimming|
    Blends are operators that take in two colors (source, destination) and return a new color.
    Many of these operate the same on all 4 components: red, green, blue, alpha. For these,
    we just document what happens to one component, rather than naming each one separately.

    Different SkColorTypes have different representations for color components:

    @
        8-bit: 0..255
        6-bit: 0..63
        5-bit: 0..31
        4-bit: 0..15
        floats: 0...1
    @

    The documentation is expressed as if the component values are always 0..1 (floats).
    
    For brevity, the documentation uses the following abbreviations

    @
    s  : source
    d  : destination
    sa : source alpha
    da : destination alpha
    @

    Results are abbreviated

    @
    r  : if all 4 components are computed in the same manner
    ra : result alpha component
    rc : result "color": red, green, blue components
    @
  |]
  [ ("Clear", "SkBlendMode::kClear", "@r = 0@")
  , ("Src", "SkBlendMode::kSrc", "@r = s@")
  , ("Dst", "SkBlendMode::kDst", "@r = d@")
  , ("SrcOver", "SkBlendMode::kSrcOver", "@r = s + (1-sa)*d@")
  , ("DstOver", "SkBlendMode::kDstOver", "@r = d + (1-da)*s@")
  , ("SrcIn", "SkBlendMode::kSrcIn", "@r = s * da@")
  , ("DstIn", "SkBlendMode::kDstIn", "@r = d * sa@")
  , ("SrcOut", "SkBlendMode::kSrcOut", "@r = s * (1-da)@")
  , ("DstOut", "SkBlendMode::kDstOut", "@r = d * (1-sa)@")
  , ("SrcATop", "SkBlendMode::kSrcATop", "@r = s*da + d*(1-sa)@")
  , ("DstATop", "SkBlendMode::kDstATop", "@r = d*sa + s*(1-da)@")
  , ("Xor", "SkBlendMode::kXor", "@r = s*(1-da) + d*(1-sa)@")
  , ("Plus", "SkBlendMode::kPlus", "@r = min(s + d, 1)@")
  , ("Modulate", "SkBlendMode::kModulate", "@r = s*d@")
  , ("Screen", "SkBlendMode::kScreen", "@r = s + d - s*d@")
  , ("Overlay", "SkBlendMode::kOverlay", "Multiply or screen, depending on destination")
  , ("Darken", "SkBlendMode::kDarken", "@rc = s + d - max(s*da, d*sa), ra = 'SkBlendMode::kSrcOver'@")
  , ("Lighten", "SkBlendMode::kLighten", "@rc = s + d - min(s*da, d*sa), ra = 'SkBlendMode::kSrcOver'@")
  , ("ColorDodge", "SkBlendMode::kColorDodge", "Brighten destination to reflect source")
  , ("ColorBurn", "SkBlendMode::kColorBurn", "Darken destination to reflect source")
  , ("HardLight", "SkBlendMode::kHardLight", "Multiply or screen, depending on source")
  , ("SoftLight", "SkBlendMode::kSoftLight", "Lighten or darken, depending on source")
  , ("Difference", "SkBlendMode::kDifference", "@rc = s + d - 2*(min(s*da, d*sa)), ra = 'SkBlendMode::kSrcOver'@")
  , ("Exclusion", "SkBlendMode::kExclusion", "@rc = s + d - two(s*d), ra = 'SkBlendMode::kSrcOver'@")
  , ("Multiply", "SkBlendMode::kMultiply", "@r = s*(1-da) + d*(1-sa) + s*d@")
  , ("Hue", "SkBlendMode::kHue", "Hue of source with saturation and luminosity of destination")
  , ("Saturation", "SkBlendMode::kSaturation", "Saturation of source with hue and luminosity of destination")
  , ("Color", "SkBlendMode::kColor", "Hue and saturation of source with luminosity of destination")
  , ("Luminosity", "SkBlendMode::kLuminosity", "Luminosity of source with hue and saturation of destination")
  ]
 )