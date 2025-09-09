module Skia.SkPathTypes where

import Language.C.Inline.Cpp qualified as C
import Skia.Internal.Prelude
import Skia.Internal.THUtils
import NeatInterpolation

C.context $
  mconcat
    [ C.cppCtx
    , cppSkiaObjectTypes
    ]

C.include "core/SkPathTypes.h"

$( qGenerateSkEnum "Direction"
  [trimming|
    Represents direction for adding closed contours to paths.
  |]
  [("CW", "SkPathDirection::kCW", "clockwise direction for adding closed contours")
  ,("CCW", "SkPathDirection::kCCW", "counter-clockwise direction for adding closed contours")
  ]
 )

$( qGenerateSkEnum "SegmentMask"
  [trimming|
    Mask values for different path segment types.
  |]
  [("Line", "SkPathSegmentMask::kLine_SkPathSegmentMask", "line segment mask (1 << 0)")
  ,("Quad", "SkPathSegmentMask::kQuad_SkPathSegmentMask", "quadratic segment mask (1 << 1)")
  ,("Conic", "SkPathSegmentMask::kConic_SkPathSegmentMask", "conic segment mask (1 << 2)")
  ,("Cubic", "SkPathSegmentMask::kCubic_SkPathSegmentMask", "cubic segment mask (1 << 3)")
  ]
 )

$( qGenerateSkEnum "Verb"
  [trimming|
    Verb types that can appear in a path.
    Each verb indicates what follows it in the point array.
  |]
  [("Move", "SkPathVerb::kMove", "move, returns 1 point")
  ,("Line", "SkPathVerb::kLine", "line, returns 2 points")
  ,("Quad", "SkPathVerb::kQuad", "quadratic, returns 3 points")
  ,("Conic", "SkPathVerb::kConic", "conic, returns 3 points + 1 weight")
  ,("Cubic", "SkPathVerb::kCubic", "cubic, returns 4 points")
  ,("Close", "SkPathVerb::kClose", "close contour, returns 0 points")
  ]
 )


$( qGenerateSkEnum "FillType"
  [trimming|
    Specifies how to compute the interior of a path.
  |]
  [("Winding", "SkPathFillType::kWinding", "computes \"inside\" by a non-zero sum of signed edge crossings")
  ,("EvenOdd", "SkPathFillType::kEvenOdd", "computes \"inside\" by an odd number of edge crossings")
  ,("InverseWinding", "SkPathFillType::kInverseWinding", "same as Winding, but draws outside of the path, rather than inside")
  ,("InverseEvenOdd", "SkPathFillType::kInverseEvenOdd", "same as EvenOdd, but draws outside of the path, rather than inside")
  ]
 )
