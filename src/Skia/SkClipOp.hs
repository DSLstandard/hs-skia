module Skia.SkClipOp where

import Skia.Internal.Prelude
import Skia.Internal.THUtils
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkClipOp.h"

$( qGenerateSkEnum
  "SkClipOp"
  "The logical operations that can be performed when combining two clips."
  [ ("Difference", "SkClipOp::kDifference", "")
  , ("Intersect", "SkClipOp::kIntersect", "")
  ]
 )
