module Skia.SKRRect where

import Data.Traversable
import Linear
import Skia.Bindings.Sk_rrect
import Skia.Internal.Prelude
import Skia.Rect

data Radii a = Radii
  { upperLeft :: a
  , upperRight :: a
  , lowerRight :: a
  , lowerLeft :: a
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

{- | A Radii with each field set to the corresponding 'SKRoundRectCorner' enum
value.

This is used for doing traversal tricks.
-}
radiiCorners :: Radii SKRoundRectCorner
radiiCorners =
  Radii
    { upperLeft = SKRoundRectCorner'UpperLeft
    , upperRight = SKRoundRectCorner'UpperRight
    , lowerRight = SKRoundRectCorner'LowerRight
    , lowerLeft = SKRoundRectCorner'LowerLeft
    }

{- | Initializes bounds at (0, 0), the origin, with zero width and height.

Initializes corner radii to (0, 0), and sets type of kEmpty_Type.
-}
createEmpty :: (MonadResource m) => m (ReleaseKey, SKRRect)
createEmpty =
  allocateSKObjectNeverNull
    sk_rrect_new
    sk_rrect_delete

{- | Initializes to copy of rrect bounds and corner radii.

The returned 'SKRRect' is freed when 'Acquire' releases.
-}
clone :: (MonadResource m) => SKRRect -> m (ReleaseKey, SKRRect)
clone rrect =
  allocateSKObjectNeverNull
    (sk_rrect_new_copy (ptr rrect))
    sk_rrect_delete

getType :: (MonadIO m) => SKRRect -> m SKRoundRectType
getType rrect = evalManaged do
  t <- liftIO $ sk_rrect_get_type (ptr rrect)
  unmarshalSKEnumOrDie t

getRect :: (MonadIO m) => SKRRect -> m (Rect Float)
getRect rrect = evalManaged do
  rect' <- managed alloca
  liftIO $ sk_rrect_get_rect (ptr rrect) rect'
  liftIO $ peekWith fromSKRect rect'

getRadii :: (MonadIO m) => SKRRect -> SKRoundRectCorner -> m (V2 Float)
getRadii rrect corner = evalManaged do
  radii' <- managed alloca
  liftIO $ sk_rrect_get_radii (ptr rrect) (marshalSKEnum corner) radii'
  liftIO $ peekWith fromSKPoint radii'

getAllRadii :: (MonadIO m) => SKRRect -> m (Radii (V2 Float))
getAllRadii rrect = evalManaged do
  for radiiCorners \corner -> do
    getRadii rrect corner

getWidth :: (MonadIO m) => SKRRect -> m Float
getWidth rrect = evalManaged do
  liftIO $ coerce <$> sk_rrect_get_width (ptr rrect)

getHeight :: (MonadIO m) => SKRRect -> m Float
getHeight rrect = evalManaged do
  liftIO $ coerce <$> sk_rrect_get_height (ptr rrect)

setEmpty :: (MonadIO m) => SKRRect -> m ()
setEmpty rrect = evalManaged do
  liftIO $ sk_rrect_set_empty (ptr rrect)

setRect :: (MonadIO m) => SKRRect -> Rect Float -> m ()
setRect rrect rect = evalManaged do
  rect' <- storable $ toSKRect rect
  liftIO $ sk_rrect_set_rect (ptr rrect) rect'

setOval :: (MonadIO m) => SKRRect -> Rect Float -> m ()
setOval rrect oval = evalManaged do
  oval' <- storable $ toSKRect oval
  liftIO $ sk_rrect_set_oval (ptr rrect) oval'

setRectXY ::
  (MonadIO m) =>
  SKRRect ->
  Rect Float ->
  -- | X and Y radius
  V2 Float ->
  m ()
setRectXY rrect rect (V2 rx ry) = evalManaged do
  rect' <- storable $ toSKRect rect
  liftIO $ sk_rrect_set_rect_xy (ptr rrect) rect' (coerce rx) (coerce ry)

setRectRadii ::
  (MonadIO m) =>
  SKRRect ->
  Rect Float ->
  Radii (V2 Float) ->
  m ()
setRectRadii rrect rect radii = evalManaged do
  rect' <- storable $ toSKRect rect

  let array = toSKPoint <$> [radii.upperLeft, radii.upperRight, radii.lowerRight, radii.lowerLeft]
  radii' <- managed $ withArray array
  liftIO $ sk_rrect_set_rect_radii (ptr rrect) rect' radii'

{- | Sets bounds to rect. Sets radii to (leftRad, topRad), (rightRad, topRad),
(rightRad, bottomRad), (leftRad, bottomRad).

If rect is empty, sets to kEmpty_Type. Otherwise, if leftRad and rightRad are
zero, sets to kRect_Type. Otherwise, if topRad and bottomRad are zero, sets
to kRect_Type. Otherwise, if leftRad and rightRad are equal and at least half
rect.width(), and topRad and bottomRad are equal at least half rect.height(),
sets to kOval_Type. Otherwise, if leftRad and rightRad are equal, and topRad
and bottomRad are equal, sets to kSimple_Type. Otherwise, sets to
kNinePatch_Type.

Nine patch refers to the nine parts defined by the radii: one center
rectangle, four edge patches, and four corner patches.
-}
setNinePatch ::
  (MonadIO m) =>
  SKRRect ->
  -- | bounds of rounded rectangle
  Rect Float ->
  -- | left-top and left-bottom x-axis radius
  Float ->
  -- | left-top and right-top y-axis radius
  Float ->
  -- | right-top and right-bottom x-axis radius
  Float ->
  -- | left-bottom and right-bottom y-axis radius
  Float ->
  m ()
setNinePatch rrect rect leftRad topRad rightRad bottomRad = evalManaged do
  rect' <- storable $ toSKRect rect
  liftIO $ sk_rrect_set_nine_patch (ptr rrect) rect' (coerce leftRad) (coerce topRad) (coerce rightRad) (coerce bottomRad)

{- | Insets bounds by dx and dy, and adjusts radii by dx and dy. dx and dy may
be positive, negative, or zero.

If either corner radius is zero, the corner has no curvature and is unchanged.
Otherwise, if adjusted radius becomes negative, pins radius to zero. If dx
exceeds half bounds width, bounds left and right are set to bounds x-axis
center. If dy exceeds half bounds height, bounds top and bottom are set to
bounds y-axis center.

If dx or dy cause the bounds to become infinite, bounds is zeroed.
-}
inset :: (MonadIO m) => SKRRect -> V2 Float -> m ()
inset rrect (V2 dx dy) = evalManaged do
  liftIO $ sk_rrect_inset (ptr rrect) (coerce dx) (coerce dy)

{- | Outsets bounds by dx and dy, and adjusts radii by dx and dy. dx and dy may
be positive, negative, or zero.

If either corner radius is zero, the corner has no curvature and is unchanged.
Otherwise, if adjusted radius becomes negative, pins radius to zero. If dx
exceeds half bounds width, bounds left and right are set to bounds x-axis
center. If dy exceeds half bounds height, bounds top and bottom are set to
bounds y-axis center.

If dx or dy cause the bounds to become infinite, bounds is zeroed.
-}
outset :: (MonadIO m) => SKRRect -> V2 Float -> m ()
outset rrect (V2 dx dy) = evalManaged do
  liftIO $ sk_rrect_outset (ptr rrect) (coerce dx) (coerce dy)

-- | Translates SkRRect by (dx, dy).
offset :: (MonadIO m) => SKRRect -> V2 Float -> m ()
offset rrect (V2 dx dy) = evalManaged do
  liftIO $ sk_rrect_offset (ptr rrect) (coerce dx) (coerce dy)

{- | Returns true if rect is inside the bounds and corner radii, and if SkRRect
and rect are not empty.
-}
containsRect :: (MonadIO m) => SKRRect -> Rect Float -> m Bool
containsRect rrect rect = evalManaged do
  rect' <- storable $ toSKRect rect
  liftIO $ toBool <$> sk_rrect_contains (ptr rrect) rect'

{- | Returns true if bounds and radii values are finite and describe a SkRRect
SkRRect::Type that matches getType(). All SkRRect methods construct valid
types, even if the input values are not valid. Invalid SkRRect data can only
be generated by corrupting memory.
-}
isValid :: (MonadIO m) => SKRRect -> m Bool
isValid rrect = evalManaged do
  liftIO $ toBool <$> sk_rrect_is_valid (ptr rrect)

-- | Returns true when successful. Returns false when failed.
transformToDest ::
  (MonadIO m) =>
  SKRRect ->
  M33 Float ->
  -- | Destination round rect
  SKRRect ->
  m Bool
transformToDest rrect matrix dstRRect = evalManaged do
  matrix' <- storable $ toSKMatrix matrix

  liftIO $ toBool <$> sk_rrect_transform (ptr rrect) matrix' (ptr dstRRect)
