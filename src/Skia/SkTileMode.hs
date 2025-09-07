module Skia.SkTileMode where

import Skia.Internal.Prelude
import Skia.Internal.THUtils
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkTileMode.h"

$( qGenerateSkEnum
  "SkTileMode"
  "The logical operations that specify the tiling mode."
  [ ("Clamp", "SkTileMode::kClamp", "Replicate the edge color if the shader draws outside of its original bounds.")
  , ("Repeat", "SkTileMode::kRepeat", "Repeat the shader's image horizontally and vertically.")
  , ("Mirror", "SkTileMode::kMirror", "Repeat the shader's image horizontally and vertically, alternating mirror images so that adjacent images always seam.")
  , ("Decal", "SkTileMode::kDecal", "Only draw within the original domain, return transparent-black everywhere else.")
  ]
 )
