module Skia.SkRRect where

import Data.Traversable
import Language.C.Inline.Cpp qualified as C
import Linear
import NeatInterpolation
import Skia.Internal.Prelude
import Skia.Internal.THUtils
import Skia.Linear
import Skia.SkRect

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkRRect.h"

$( qGenerateSkEnum
  "Corner"
  "The radii are stored: top-left, top-right, bottom-right, bottom-left."
  [ ("UpperLeft", "SkRRect::Corner::kUpperLeft_Corner", "Index of top-left corner radii")
  , ("UpperRight", "SkRRect::Corner::kUpperRight_Corner", "Index of top-right corner radii")
  , ("LowerRight", "SkRRect::Corner::kLowerRight_Corner", "Index of bottom-right corner radii")
  , ("LowerLeft", "SkRRect::Corner::kLowerLeft_Corner", "Index of bottom-left corner radii")
  ]
 )

$( qGenerateSkEnum
  "RectType"
  [trimming|
    Type describes possible specializations of SkRRect.
    Each Type is exclusive; a SkRRect may only have one type.

    Type members become progressively less restrictive; larger values of
    Type have more degrees of freedom than smaller values.
  |]
  [ ("Empty", "SkRRect::Type::kEmpty_Type", "zero width or height")
  , ("Rect", "SkRRect::Type::kRect_Type", "non-zero width and height, and zeroed radii")
  , ("Oval", "SkRRect::Type::kOval_Type", "non-zero width and height filled with radii")
  , ("Simple", "SkRRect::Type::kSimple_Type", "non-zero width and height with equal radii")
  , ("NinePatch", "SkRRect::Type::kNinePatch_Type", "non-zero width and height with axis-aligned radii")
  , ("Complex", "SkRRect::Type::kComplex_Type", "non-zero width and height with arbitrary radii")
  ]
 )

data Radii a = Radii
  { upperLeft :: a
  , upperRight :: a
  , lowerRight :: a
  , lowerLeft :: a
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

{- | A Radii with each field set to the corresponding 'SkRoundRectCorner' enum
value.

This is used for doing traversal tricks.
-}
radiiCorners :: Radii Corner
radiiCorners =
  Radii
    { upperLeft = Corner'UpperLeft
    , upperRight = Corner'UpperRight
    , lowerRight = Corner'LowerRight
    , lowerLeft = Corner'LowerLeft
    }

{- | Initializes bounds at (0, 0), the origin, with zero width and height.

Initializes corner radii to (0, 0), and sets type of kEmpty_Type.
-}
createEmpty :: (MonadResource m) => m (ReleaseKey, SkRRect)
createEmpty =
  allocateSkObjectNeverNull
    [C.exp| SkRRect* { new SkRRect() } |]
    (\p -> [C.block| void { delete $(SkRRect* p); } |])

{- | Initializes to copy of rrect bounds and corner radii.

The returned 'SkRRect' is freed when 'Acquire' releases.
-}
createCopy :: (MonadResource m) => SkRRect -> m (ReleaseKey, SkRRect)
createCopy (ptr -> rrect) =
  allocateSkObjectNeverNull
    [C.exp| SkRRect* { new SkRRect(*$(SkRRect* rrect)) } |]
    (\p -> [C.block| void { delete $(SkRRect* p); } |])

-- | Returns the 'RectType' of the 'SkRRect'.
getType :: (MonadIO m) => SkRRect -> m RectType
getType (ptr -> rrect) = liftIO do
  unmarshalSkEnumOrDie =<< [C.block|int {
    return $(SkRRect* rrect)->getType();
  }|]

getRect :: (MonadIO m) => SkRRect -> m (Rect Float)
getRect (ptr -> rrect) = evalManaged do
  rect' <- allocaSkRect
  liftIO do
    [C.block| void {
      *$(SkRect* rect') = $(SkRRect* rrect)->rect();
    }|]
  peekSkRect rect'

getRadii :: (MonadIO m) => SkRRect -> Corner -> m (V2 Float)
getRadii (ptr -> rrect) (marshalSkEnum -> corner) = evalManaged do
  fX <- managed $ alloca @CFloat
  fY <- managed $ alloca @CFloat
  liftIO do
    [C.block| void {
      auto r = $(SkRRect* rrect)->radii((SkRRect::Corner) $(int corner));
      *$(float* fX) = r.fX;
      *$(float* fY) = r.fY;
    }|]
  fX <- liftIO $ peek fX
  fY <- liftIO $ peek fY
  pure $ coerce $ V2 fX fY

getAllRadii :: (MonadIO m) => SkRRect -> m (Radii (V2 Float))
getAllRadii rrect = evalManaged do
  for radiiCorners \corner -> do
    getRadii rrect corner

getWidth :: (MonadIO m) => SkRRect -> m Float
getWidth (ptr -> rrect) = liftIO $ coerce <$>
  [C.exp| float { $(SkRRect* rrect)->width() }|]

getHeight :: (MonadIO m) => SkRRect -> m Float
getHeight (ptr -> rrect) = liftIO $ coerce <$>
  [C.exp| float { $(SkRRect* rrect)->height() }|]

-- | Sets bounds to zero width and height at (0, 0), the origin. Sets corner
-- radii to zero and sets type to kEmpty_Type.
setEmpty :: (MonadIO m) => SkRRect -> m ()
setEmpty (ptr -> rrect) = liftIO do
  [C.block| void { $(SkRRect* rrect)->setEmpty(); }|]

-- | Sets bounds to sorted rect, and sets corner radii to zero. If set bounds
-- has width and height, and sets type to kRect_Type; otherwise, sets type to
-- kEmpty_Type.
setRect :: (MonadIO m) => SkRRect -> Rect Float -> m ()
setRect (ptr -> rrect) rect = evalManaged do
  rect' <- marshalSkRect rect
  liftIO $ [C.block| void { $(SkRRect* rrect)->setRect(*$(SkRect* rect')); }|]

-- | Sets bounds to oval, x-axis radii to half oval.width(), and all y-axis
-- radii to half oval.height(). If oval bounds is empty, sets to kEmpty_Type.
-- Otherwise, sets to kOval_Type.
setOval :: (MonadIO m) => SkRRect -> Rect Float -> m ()
setOval (ptr -> rrect) rect = evalManaged do
  rect' <- marshalSkRect rect
  liftIO $ [C.block| void { $(SkRRect* rrect)->setOval(*$(SkRect* rect')); }|]

-- | Sets to rounded rectangle with the same radii for all four corners.
--
-- If rect is empty, sets to kEmpty_Type.
--
-- Otherwise, if xRad or yRad is zero, sets to kRect_Type.
--
-- Otherwise, if xRad is at least half rect.width() and yRad is at least half
-- rect.height(), sets to kOval_Type.
--
-- Otherwise, sets to kSimple_Type.
setRectXY ::
  (MonadIO m) =>
  SkRRect ->
  Rect Float ->
  -- | X and Y radius
  V2 Float ->
  m ()
setRectXY (ptr -> rrect) rect (coerce -> V2 rx ry) = evalManaged do
  rect' <- marshalSkRect rect
  liftIO $ [C.block| void { 
    $(SkRRect* rrect)->setRectXY(*$(SkRect* rect'), $(float rx), $(float ry)); 
  }|]

setRectRadii ::
  (MonadIO m) =>
  SkRRect ->
  Rect Float ->
  Radii (V2 Float) ->
  m ()
setRectRadii (ptr -> rrect) rect radii = evalManaged do
  rect' <- marshalSkRect rect

  upperLeft' <- marshalSkPoint radii.upperLeft
  upperRight' <- marshalSkPoint radii.upperRight
  lowerRight' <- marshalSkPoint radii.lowerRight
  lowerLeft' <- marshalSkPoint radii.lowerLeft

  liftIO [C.block| void {
    SkVector radii[4] = {
      *$(SkPoint* upperLeft'),
      *$(SkPoint* upperRight'),
      *$(SkPoint* lowerRight'),
      *$(SkPoint* lowerLeft')
    };
    $(SkRRect* rrect)->setRectRadii(
      *$(const SkRect* rect'),
      radii
    );
  }|]

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
  SkRRect ->
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
setNinePatch (ptr -> rrect) rect (coerce -> leftRad) (coerce -> topRad) (coerce -> rightRad) (coerce -> bottomRad) = evalManaged do
  rect' <- marshalSkRect rect
  liftIO [C.block| void {
    $(SkRRect* rrect)->setNinePatch(
      *$(const SkRect* rect'),
      $(float leftRad),
      $(float topRad),
      $(float rightRad),
      $(float bottomRad)
    );
  }|]

{- | Insets bounds by dx and dy, and adjusts radii by dx and dy. dx and dy may
be positive, negative, or zero.

If either corner radius is zero, the corner has no curvature and is unchanged.
Otherwise, if adjusted radius becomes negative, pins radius to zero. If dx
exceeds half bounds width, bounds left and right are set to bounds x-axis
center. If dy exceeds half bounds height, bounds top and bottom are set to
bounds y-axis center.

If dx or dy cause the bounds to become infinite, bounds is zeroed.
-}
inset :: (MonadIO m) => SkRRect -> V2 Float -> m ()
inset (ptr -> rrect) (coerce -> V2 dx dy) = liftIO do
  [C.block| void {
    $(SkRRect* rrect)->inset($(float dx), $(float dy));
  }|]

{- | Outsets bounds by dx and dy, and adjusts radii by dx and dy. dx and dy may
be positive, negative, or zero.

If either corner radius is zero, the corner has no curvature and is unchanged.
Otherwise, if adjusted radius becomes negative, pins radius to zero. If dx
exceeds half bounds width, bounds left and right are set to bounds x-axis
center. If dy exceeds half bounds height, bounds top and bottom are set to
bounds y-axis center.

If dx or dy cause the bounds to become infinite, bounds is zeroed.
-}
outset :: (MonadIO m) => SkRRect -> V2 Float -> m ()
outset (ptr -> rrect) (coerce -> V2 dx dy) = liftIO do
  [C.block| void {
    $(SkRRect* rrect)->outset($(float dx), $(float dy));
  }|]

-- | Translates SkRRect by (dx, dy).
offset :: (MonadIO m) => SkRRect -> V2 Float -> m ()
offset (ptr -> rrect) (coerce -> V2 dx dy) = liftIO do
  [C.block| void {
    $(SkRRect* rrect)->offset($(float dx), $(float dy));
  }|]

{- | Returns true if rect is inside the bounds and corner radii, and if SkRRect
and rect are not empty.
-}
containsRect :: (MonadIO m) => SkRRect -> Rect Float -> m Bool
containsRect (ptr -> rrect) rect = evalManaged do
  rect' <- marshalSkRect rect
  toBool <$> liftIO [C.block| bool {
    return $(SkRRect* rrect)->contains(*$(const SkRect* rect'));
  }|]

{- | Returns true if bounds and radii values are finite and describe a SkRRect
SkRRect::Type that matches getType(). All SkRRect methods construct valid
types, even if the input values are not valid. Invalid SkRRect data can only
be generated by corrupting memory.
-}
isValid :: (MonadIO m) => SkRRect -> m Bool
isValid (ptr -> rrect) = liftIO $ toBool <$>
  [C.block| bool {
    return $(SkRRect* rrect)->isValid();
  }|]

-- | Returns true when successful. Returns false when failed.
transformToDest ::
  (MonadIO m) =>
  SkRRect ->
  M33 Float ->
  -- | Destination round rect
  SkRRect ->
  m Bool
transformToDest (ptr -> rrect) matrix (ptr -> dst) = evalManaged do
  matrix' <- marshalSkMatrix matrix
  toBool <$> liftIO [C.block| bool {
    return $(SkRRect* rrect)->transform(
      *$(SkMatrix* matrix'),
      $(SkRRect* dst)
    );
  }|]