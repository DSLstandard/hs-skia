module Skia.SkAlphaType where

import Skia.Internal.Prelude
import Skia.Internal.THUtils
import NeatInterpolation
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkAlphaType.h"

$( qGenerateSkEnum
  "SkAlphaType"
  [trimming|
      Describes how to interpret the alpha component of a pixel. A pixel may
      be opaque, or alpha, describing multiple levels of transparency.

      In simple blending, alpha weights the draw color and the destination
      color to create a new color. If alpha describes a weight from zero to one:

      @
      new color = draw color * alpha + destination color * (1 - alpha)
      @

      In practice alpha is encoded in two or more bits, where 1.0 equals all bits set.

      RGB may have alpha included in each component value; the stored
      value is the original RGB multiplied by alpha. Premultiplied color
      components improve performance.
  |]
  [ ("Unknown", "SkAlphaType::kUnknown_SkAlphaType", "Uninitialized")
  , ("Opaque", "SkAlphaType::kOpaque_SkAlphaType", "Pixel is opaque")
  , ("Premul", "SkAlphaType::kPremul_SkAlphaType", "Pixel components are premultiplied by alpha")
  , ("Unpremul", "SkAlphaType::kUnpremul_SkAlphaType", "Pixel components are independent of alpha")
  ]
 )