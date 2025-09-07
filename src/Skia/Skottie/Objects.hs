module Skia.Skottie.Objects where

import Skia.Objects
import Skia.Internal.Prelude
import Skia.Internal.THUtils
import Language.C.Inline.Cpp qualified as C
import NeatInterpolation

$( qGenerateSkObject
    "skottie::Animation"
    "Animation"
    [''SkNVRefCnt] -- class SK_API Animation : public SkNVRefCnt<Animation> {
    [trimming|
      skottie::Animation represents a complete Lottie animation object.
    |]
 )

$( qGenerateSkObject
    "skottie::Animation::Builder"
    "AnimationBuilder"
    [] -- class SK_API Builder final {
    [trimming|
      Builder for skottie::Animation objects.
    |]
 )

cppSkottieObjectTypes :: C.Context
cppSkottieObjectTypes = formInlineCppTypePairs $(qGatherInlineSkObjectEntries)
