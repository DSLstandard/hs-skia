module Skia.SkPathEffect where

import Language.C.Inline.Cpp qualified as C
import Linear
import Skia.Internal.Prelude
import Skia.Internal.THUtils
import Skia.Linear
import Skia.SkRefCnt qualified as SkRefCnt

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkPathEffect.h"
C.include "effects/Sk1DPathEffect.h"
C.include "effects/Sk2DPathEffect.h"
C.include "effects/SkCornerPathEffect.h"
C.include "effects/SkDashPathEffect.h"
C.include "effects/SkDiscretePathEffect.h"
C.include "effects/SkTrimPathEffect.h"

-- TODO: Figure out why SkiaSharp's binding/SkiaSharp/SkPathEffect.cs does not
-- use @sk_path_effect_unref@.

{- | Returns a patheffect that apples each effect (first and second) to the
original path, and returns a path with the sum of these.

@
result = first(path) + second(path)
@
-}
createSum ::
  (MonadResource m) =>
  -- | First
  SkPathEffect ->
  -- | Second
  SkPathEffect ->
  m (ReleaseKey, SkPathEffect)
createSum (ptr -> first) (ptr -> second) =
  allocateSkObjectNeverNull'
    [C.exp| SkPathEffect* {
      SkPathEffect::MakeSum(
        sk_ref_sp($(SkPathEffect* first)),
        sk_ref_sp($(SkPathEffect* second))
      ).release()
    }|]
    SkRefCnt.decrement

{- | Returns a patheffect that applies the inner effect to the path, and then
applies the outer effect to the result of the inner's.

@
result = outer(inner(path))
@
-}
createCompose ::
  (MonadResource m) =>
  -- | Outer
  SkPathEffect ->
  -- | Inner
  SkPathEffect ->
  m (ReleaseKey, SkPathEffect)
createCompose (ptr -> outer) (ptr -> inner) =
  allocateSkObjectNeverNull'
    [C.exp| SkPathEffect* {
      SkPathEffect::MakeCompose(
        sk_ref_sp($(SkPathEffect* outer)),
        sk_ref_sp($(SkPathEffect* inner))
      ).release()
    }|]
    SkRefCnt.decrement

{- | Breaks the path into segments of segLength length, and randomly move the
endpoints away from the original path by a maximum of deviation. Note: works on
filled or framed paths
-}
createDiscrete ::
  (MonadResource m) =>
  -- | Segment length
  Float ->
  -- | Deviation
  Float ->
  -- | seedAssist; This is a caller-supplied seedAssist that modifies the seed
  -- value that is used to randomize the path segments' endpoints. If not
  -- supplied it defaults to 0, in which case filtering a path multiple times
  -- will result in the same set of segments (this is useful for testing). If a
  -- caller does not want this behaviour they can pass in a different seedAssist
  -- to get a different set of path segments.
  Word32 ->
  m (ReleaseKey, SkPathEffect)
createDiscrete (coerce -> segLength) (coerce -> deviation) seedAssist =
  allocateSkObjectNeverNull'
    [C.exp| SkPathEffect* {
      SkDiscretePathEffect::Make(
        $(float segLength),
        $(float deviation),
        $(uint32_t seedAssist)
      ).release()
    }|]
    SkRefCnt.decrement

{- | A PathEffect that can turn sharp corners into various treatments (e.g.
rounded corners)
-}
createCorner ::
  (MonadResource m) =>
  -- | radius; radius must be > 0 to have an effect. It specifies the distance
  -- from each corner that should be "rounded".
  Float ->
  m (ReleaseKey, SkPathEffect)
createCorner (coerce -> radius) =
  allocateSkObjectNeverNull'
    [C.exp| SkPathEffect* {
      SkCornerPathEffect::Make($(float radius)).release()
    }|]
    SkRefCnt.decrement

$( qGenerateSkEnum
  "SkPathEffect1DStyle"
  "Option for 'create1DPath'"
  [ ("Translate", "SkPath1DPathEffect::Style::kTranslate_Style", "translate the shape to each position")
  , ("Rotate", "SkPath1DPathEffect::Style::kRotate_Style", "rotate the shape about its center")
  , ("Morph", "SkPath1DPathEffect::Style::kMorph_Style", "transform each point, and turn lines into curves")
  ]
 )

-- | Creates a dash by replicating the specified path.
create1DPath ::
  (MonadResource m) =>
  -- | path; The path to replicate (dash)
  SkPath ->
  -- | advance; The space between instances of path
  Float ->
  -- | phase; distance (mod advance) along path for its initial position
  Float ->
  -- | style; how to transform path at each point (based on the current position and tangent)
  SkPathEffect1DStyle ->
  m (ReleaseKey, SkPathEffect)
create1DPath (ptr -> path) (coerce -> advance) (coerce -> phase) (marshalSkEnum -> style) =
  allocateSkObjectNeverNull'
    [C.exp| SkPathEffect* {
      SkPath1DPathEffect::Make(
        *$(SkPath* path),
        $(float advance),
        $(float phase),
        (SkPath1DPathEffect::Style) ($(int style))
      ).release()
    }|]
    SkRefCnt.decrement

create2DLine ::
  (MonadResource m) =>
  -- | Width
  Float ->
  -- | Matrix
  M33 Float ->
  m (ReleaseKey, SkPathEffect)
create2DLine (coerce -> width) matrix = do
  allocateSkObjectNeverNull'
    ( evalManaged do
        matrix' <- marshalSkMatrix matrix
        liftIO [C.exp| SkPathEffect* {
          SkLine2DPathEffect::Make($(float width), *$(SkMatrix* matrix')).release()
        }|]
    )
    SkRefCnt.decrement

create2DPath ::
  (MonadResource m) =>
  M33 Float ->
  SkPath ->
  m (ReleaseKey, SkPathEffect)
create2DPath matrix (ptr -> path) =
  allocateSkObjectNeverNull'
    ( evalManaged do
        matrix' <- marshalSkMatrix matrix
        liftIO [C.exp| SkPathEffect* {
          SkPath2DPathEffect::Make(*$(SkMatrix* matrix'), *$(SkPath* path)).release()
        }|]
    )
    SkRefCnt.decrement

{- | Creates a dash path effect.

For example: if intervals[] = {10, 20}, count = 2, and phase = 25, this will set
up a dashed path like so:

  *  5 pixels off

  *  10 pixels on

  *  20 pixels off

  *  10 pixels on

  *  20 pixels off

  * ...

A phase of -5, 25, 55, 85, etc. would all result in the same path, because the
  sum of all the intervals is 30.

Note: only affects stroked paths.
-}
createDash ::
  (MonadResource m) =>
  -- | intervals; array containing an even number of entries (>=2), with the
  -- even indices specifying the length of "on" intervals, and the odd indices
  -- specifying the length of "off" intervals. This array will be copied in
  -- Make, and can be disposed of freely after.
  [Float] ->
  -- | phase; offset into the intervals array (mod the sum of all of the
  -- intervals).
  Float ->
  m (ReleaseKey, SkPathEffect)
createDash intervals (coerce -> phase) =
  allocateSkObjectNeverNull'
    ( evalManaged do
        (coercePtr -> intervals', fromIntegral -> intervalsLen) <- managed $ withArrayLen' intervals
        liftIO [C.exp| SkPathEffect* {
          SkDashPathEffect::Make($(float* intervals'), $(int intervalsLen), $(float phase)).release()
        }|]
    )
    SkRefCnt.decrement


$(qGenerateSkEnum
  "SkTrimPathMode"
  "Mode for 'createTrim'"
  [ ("Normal", "SkTrimPathEffect::Mode::kNormal", "return the subset path [start,stop]")
  , ("Inverted", "SkTrimPathEffect::Mode::kInverted", "return the complement/subset paths [0,start] + [stop,1]")
  ]
  )

{- | Take start and stop "t" values (values between 0...1), and return a path
that is that subset of the original path.

e.g.
@
Make(0.5, 1.0) --> return the 2nd half of the path
Make(0.33333, 0.66667) --> return the middle third of the path
@

The trim values apply to the entire path, so if it contains several contours,
all of them are including in the calculation.

startT and stopT must be 0..1 inclusive. If they are outside of that interval,
they will be pinned to the nearest legal value. If either is NaN, null will be
returned.

Note: for Mode::kNormal, this will return one (logical) segment (even if it is
spread across multiple contours). For Mode::kInverted, this will return 2
logical segments: stopT..1 and 0...startT, in this order.
-}
createTrim ::
  (MonadResource m) =>
  -- | startT
  Float ->
  -- | stopT
  Float ->
  SkTrimPathMode ->
  m (ReleaseKey, SkPathEffect)
createTrim (coerce -> start) (coerce -> stop) (marshalSkEnum -> mode) =
  allocateSkObjectNeverNull'
    ( do
        [C.exp| SkPathEffect* {
          SkTrimPathEffect::Make(
            $(float start), $(float stop),
            (SkTrimPathEffect::Mode) $(int mode)
          ).release()
        }|]
    )
    SkRefCnt.decrement
