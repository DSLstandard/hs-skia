module Skia.SKPathEffect where

import Linear
import Skia.Bindings.Sk_patheffect
import Skia.Internal.Prelude

-- TODO: Figure out why SkiaSharp's binding/SkiaSharp/SKPathEffect.cs does not
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
  SKPathEffect ->
  -- | Second
  SKPathEffect ->
  m (ReleaseKey, SKPathEffect)
createSum first second =
  allocateSKObjectNeverNull
    (sk_path_effect_create_sum (ptr first) (ptr second))
    sk_path_effect_unref

{- | Returns a patheffect that applies the inner effect to the path, and then
applies the outer effect to the result of the inner's.

@
result = outer(inner(path))
@
-}
createCompose ::
  (MonadResource m) =>
  -- | Outer
  SKPathEffect ->
  -- | Inner
  SKPathEffect ->
  m (ReleaseKey, SKPathEffect)
createCompose outer inner =
  allocateSKObjectNeverNull
    (sk_path_effect_create_compose (ptr outer) (ptr inner))
    sk_path_effect_unref

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
  m (ReleaseKey, SKPathEffect)
createDiscrete segLength deviation seedAssist =
  allocateSKObjectNeverNull
    (sk_path_effect_create_discrete (coerce segLength) (coerce deviation) seedAssist)
    sk_path_effect_unref

{- | A PathEffect that can turn sharp corners into various treatments (e.g.
rounded corners)
-}
createCorner ::
  (MonadResource m) =>
  -- | radius; radius must be > 0 to have an effect. It specifies the distance
  -- from each corner that should be "rounded".
  Float ->
  m (ReleaseKey, SKPathEffect)
createCorner radius =
  allocateSKObjectNeverNull
    (sk_path_effect_create_corner (coerce radius))
    sk_path_effect_unref

-- | Creates a dash by replicating the specified path.
create1DPath ::
  (MonadResource m) =>
  -- | path; The path to replicate (dash)
  SKPath ->
  -- | advance; The space between instances of path
  Float ->
  -- | phase; distance (mod advance) along path for its initial position
  Float ->
  -- | style; how to transform path at each point (based on the current position and tangent)
  SKPathEffect1DStyle ->
  m (ReleaseKey, SKPathEffect)
create1DPath path advance phase style =
  allocateSKObjectNeverNull
    (sk_path_effect_create_1d_path (ptr path) (coerce advance) (coerce phase) (marshalSKEnum style))
    sk_path_effect_unref

create2DLine ::
  (MonadResource m) =>
  -- | Width
  Float ->
  -- | Matrix
  M33 Float ->
  m (ReleaseKey, SKPathEffect)
create2DLine width matrix = do
  allocateSKObjectNeverNull
    ( evalManaged do
        matrix' <- storable $ toSKMatrix matrix
        liftIO $ sk_path_effect_create_2d_line (coerce width) matrix'
    )
    sk_path_effect_unref

create2DPath ::
  (MonadResource m) =>
  M33 Float ->
  SKPath ->
  m (ReleaseKey, SKPathEffect)
create2DPath matrix path =
  allocateSKObjectNeverNull
    ( evalManaged do
        matrix' <- storable $ toSKMatrix matrix
        liftIO $ sk_path_effect_create_2d_path matrix' (ptr path)
    )
    sk_path_effect_unref

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
  m (ReleaseKey, SKPathEffect)
createDash intervals phase =
  allocateSKObjectNeverNull
    ( evalManaged do
        (intervals', intervalsLen) <- managed $ withArrayLen' intervals
        liftIO $ sk_path_effect_create_dash (castPtr intervals') (fromIntegral intervalsLen) (coerce phase)
    )
    sk_path_effect_unref

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
  SKPathEffectTrimMode ->
  m (ReleaseKey, SKPathEffect)
createTrim start stop mode =
  allocateSKObjectNeverNull
    (sk_path_effect_create_trim (coerce start) (coerce stop) (marshalSKEnum mode))
    sk_path_effect_unref
