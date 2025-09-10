module Skia.SkPath where

import Data.Bits
import Data.Traversable
import Language.C.Inline.Cpp qualified as C
import Linear
import NeatInterpolation
import Skia.Internal.Prelude
import Skia.Internal.THUtils
import Skia.Linear
import Skia.SkPathTypes as SkPathTypes
import Skia.SkRect as SkRect

C.context $
  mconcat
    [ C.cppCtx
    , cppSkiaObjectTypes
    ]

C.include "core/SkPath.h"
C.include "pathops/SkPathOps.h"

{- | Constructs an empty SkPath. By default, SkPath has no verbs, no SkPoint,
and no weights. FillType is set to kWinding.
-}
createEmpty :: (MonadResource m) => m (ReleaseKey, SkPath)
createEmpty =
  allocateSkObjectNeverNull
    [C.exp| SkPath* { new SkPath() } |]
    (\p -> [C.block| void { delete $(SkPath* p); } |])

{- | Constructs a copy of an existing path. This makes two paths identical by
value. Internally, path and the returned result share pointer values. The
underlying verb array, SkPoint array and weights are copied when modified.

Creating a SkPath copy is very efficient and never allocates memory. SkPath are
always copied by value from the interface; the underlying shared pointers are
not exposed.

You must free the returned 'SkPath' with 'destroy' when done with it.
-}
createCopy :: (MonadResource m) => SkPath -> m (ReleaseKey, SkPath)
createCopy (ptr -> path) =
  allocateSkObjectNeverNull
    [C.exp| SkPath* { new SkPath(*$(SkPath* path)) } |]
    (\p -> [C.block| void { delete $(SkPath* p); } |])

-- | Adds beginning of contour at SkPoint (x, y).
moveTo :: (MonadIO m) => SkPath -> V2 Float -> m ()
moveTo (ptr -> path) (coerce -> V2 x y) = liftIO do
  [C.block| void {
    $(SkPath* path)->moveTo($(float x), $(float y));
  }|]

{- | Adds beginning of contour relative to last point. If SkPath is empty,
starts contour at (dx, dy). Otherwise, start contour at last point offset by
(dx, dy). Function name stands for "relative move to".
-}
rMoveTo :: (MonadIO m) => SkPath -> V2 Float -> m ()
rMoveTo (ptr -> path) (coerce -> V2 dx dy) = liftIO do
  [C.block| void {
    $(SkPath* path)->rMoveTo($(float dx), $(float dy));
  }|]

{- | Adds line from last point to (x, y). If SkPath is empty, or last
SkPath::Verb is kClose_Verb, last point is set to (0, 0) before adding line.

lineTo() appends kMove_Verb to verb array and (0, 0) to SkPoint array, if
needed.

lineTo() then appends kLine_Verb to verb array and (x, y) to SkPoint array.
-}
lineTo :: (MonadIO m) => SkPath -> V2 Float -> m ()
lineTo (ptr -> path) (coerce -> V2 x y) = liftIO do
  [C.block| void {
    $(SkPath* path)->lineTo($(float x), $(float y));
  }|]

rLineTo :: (MonadIO m) => SkPath -> V2 Float -> m ()
rLineTo (ptr -> path) (coerce -> V2 dx dy) = liftIO do
  [C.block| void {
    $(SkPath* path)->rLineTo($(float dx), $(float dy));
  }|]

quadTo ::
  (MonadIO m) =>
  SkPath ->
  -- | Control point 1
  V2 Float ->
  -- | End point
  V2 Float ->
  m ()
quadTo (ptr -> path) (coerce -> V2 x1 y1) (coerce -> V2 x2 y2) = liftIO do
  [C.block| void {
    $(SkPath* path)->quadTo(
      $(float x1), $(float y1),
      $(float x2), $(float y2)
    );
  }|]

rQuadTo ::
  (MonadIO m) =>
  SkPath ->
  -- | Control point 1
  V2 Float ->
  -- | End point
  V2 Float ->
  m ()
rQuadTo (ptr -> path) (coerce -> V2 dx1 dy1) (coerce -> V2 dx2 dy2) = liftIO do
  [C.block| void {
    $(SkPath* path)->rQuadTo(
      $(float dx1), $(float dy1),
      $(float dx2), $(float dy2)
    );
  }|]

cubicTo ::
  (MonadIO m) =>
  SkPath ->
  -- | Control point 1
  V2 Float ->
  -- | Control point 2
  V2 Float ->
  -- | End point
  V2 Float ->
  m ()
cubicTo (ptr -> path) (coerce -> V2 x1 y1) (coerce -> V2 x2 y2) (coerce -> V2 x3 y3) = liftIO do
  [C.block| void {
    $(SkPath* path)->cubicTo(
      $(float x1), $(float y1),
      $(float x2), $(float y2),
      $(float x3), $(float y3)
    );
  }|]

rCubicTo ::
  (MonadIO m) =>
  SkPath ->
  -- | Control point 1
  V2 Float ->
  -- | Control point 2
  V2 Float ->
  -- | End point
  V2 Float ->
  m ()
rCubicTo (ptr -> path) (coerce -> V2 dx1 dy1) (coerce -> V2 dx2 dy2) (coerce -> V2 dx3 dy3) = liftIO do
  [C.block| void {
    $(SkPath* path)->rCubicTo(
      $(float dx1), $(float dy1),
      $(float dx2), $(float dy2),
      $(float dx3), $(float dy3)
    );
  }|]

conicTo ::
  (MonadIO m) =>
  SkPath ->
  -- | Control point 1
  V2 Float ->
  -- | End point
  V2 Float ->
  -- | Weight of the added conic
  Float ->
  m ()
conicTo (ptr -> path) (coerce -> V2 x1 y1) (coerce -> V2 x2 y2) (coerce -> weight) = liftIO do
  [C.block| void {
    $(SkPath* path)->conicTo(
      $(float x1), $(float y1),
      $(float x2), $(float y2),
      $(float weight)
    );
  }|]

rConicTo ::
  (MonadIO m) =>
  SkPath ->
  -- | Control point 1
  V2 Float ->
  -- | End point
  V2 Float ->
  -- | Weight of the added conic
  Float ->
  m ()
rConicTo (ptr -> path) (coerce -> V2 dx1 dy1) (coerce -> V2 dx2 dy2) (coerce -> weight) = liftIO do
  [C.block| void {
    $(SkPath* path)->rConicTo(
      $(float dx1), $(float dy1),
      $(float dx2), $(float dy2),
      $(float weight)
    );
  }|]

$( qGenerateSkEnum
    "ArcSize"
    [trimming|
    Represents arc size for path operations.
    When drawing an arc, there are generally two ways to draw the arc between two points:
    a smaller arc or a larger arc that goes around the other way.
  |]
    [ ("Small", "SkPath::ArcSize::kSmall_ArcSize", "smaller of arc pair")
    , ("Large", "SkPath::ArcSize::kLarge_ArcSize", "larger of arc pair")
    ]
 )

arcTo ::
  (MonadIO m) =>
  SkPath ->
  -- | Radii on axes before x-axis rotation
  V2 Float ->
  -- | X-axis rotation in degrees
  Float ->
  -- | Chooses smaller or larger arc
  ArcSize ->
  -- | Chooses closewise or counterclockwise
  SkPathDirection ->
  -- | End of arc
  V2 Float ->
  m ()
arcTo (ptr -> path) (coerce -> V2 rx ry) (coerce -> xAxisRotate) (marshalSkEnum -> largeArc) (marshalSkEnum -> sweep) (coerce -> V2 x y) = liftIO do
  [C.block| void {
    $(SkPath* path)->arcTo(
      $(float rx), $(float ry),
      $(float xAxisRotate),
      (SkPath::ArcSize) ((int)$(int largeArcInt)),
      (SkPathDirection) ((int) $(int sweepInt)),
      $(float x), $(float y)
    );
  }|]
 where
  largeArcInt = fromIntegral $ fromEnum largeArc
  sweepInt = fromIntegral $ fromEnum sweep

rArcTo ::
  (MonadIO m) =>
  SkPath ->
  -- | Radii on axes before x-axis rotation
  V2 Float ->
  -- | X-axis rotation in degrees
  Float ->
  -- | Chooses smaller or larger arc
  ArcSize ->
  -- | Chooses clockwise or counterclockwise
  SkPathDirection ->
  -- | End of arc (relative)
  V2 Float ->
  m ()
rArcTo (ptr -> path) (coerce -> V2 rx ry) (coerce -> xAxisRotate) (marshalSkEnum -> largeArc) (marshalSkEnum -> sweep) (coerce -> V2 dx dy) = liftIO do
  [C.block| void {
    $(SkPath* path)->rArcTo(
      $(float rx), $(float ry),
      $(float xAxisRotate),
      (SkPath::ArcSize) ((int) $(int largeArc)),
      (SkPathDirection) ((int) $(int sweep)),
      $(float dx), $(float dy)
    );
  }|]

arcToWithPoints ::
  (MonadIO m) =>
  SkPath ->
  -- | Common point to pair of tangents
  V2 Float ->
  -- | End of second tangent
  V2 Float ->
  -- | Radius: Distance from arc to circle center
  Float ->
  m ()
arcToWithPoints (ptr -> path) (coerce -> V2 x1 y1) (coerce -> V2 x2 y2) (coerce -> radius) = liftIO do
  [C.block| void {
    $(SkPath* path)->arcTo(
      $(float x1), $(float y1),
      $(float x2), $(float y2),
      $(float radius)
    );
  }|]

close :: (MonadIO m) => SkPath -> m ()
close (ptr -> path) = liftIO do
  [C.block| void {
    $(SkPath* path)->close();
  }|]

addRect :: (MonadIO m) => (MonadIO m) => SkPath -> Rect Float -> SkPathDirection -> m ()
addRect (ptr -> path) rect (marshalSkEnum -> dir) = evalManaged do
  rect' <- marshalSkRect rect
  liftIO
    [C.block| void {
    $(SkPath* path)->addRect(*$(SkRect* rect'), (SkPathDirection) $(int dir));
  }|]

addRectWithStart :: (MonadIO m) => SkPath -> Rect Float -> SkPathDirection -> Int -> m ()
addRectWithStart (ptr -> path) rect (marshalSkEnum -> dir) (fromIntegral -> startIndex) = evalManaged do
  rect' <- marshalSkRect rect
  liftIO
    [C.block| void {
    $(SkPath* path)->addRect(*$(SkRect* rect'), (SkPathDirection) $(int dir), $(int startIndex));
  }|]

addRRect :: (MonadIO m) => SkPath -> SkRRect -> SkPathDirection -> m ()
addRRect (ptr -> path) (ptr -> rrect) (marshalSkEnum -> dir) = liftIO do
  [C.block| void {
    $(SkPath* path)->addRRect(*$(SkRRect* rrect), (SkPathDirection) $(int dir));
  }|]

addRRectWithStart :: (MonadIO m) => SkPath -> SkRRect -> SkPathDirection -> Int -> m ()
addRRectWithStart (ptr -> path) (ptr -> rrect) (marshalSkEnum -> dir) (fromIntegral -> startIndex) = liftIO do
  [C.block| void {
    $(SkPath* path)->addRRect(*$(SkRRect* rrect), (SkPathDirection) $(int dir), $(int startIndex));
  }|]

addRoundRect ::
  (MonadIO m) =>
  SkPath ->
  Rect Float ->
  -- | x and y radii
  V2 Float ->
  SkPathDirection ->
  m ()
addRoundRect (ptr -> path) rect (coerce -> V2 rx ry) (marshalSkEnum -> dir) = evalManaged do
  rect' <- marshalSkRect rect
  liftIO
    [C.block| void {
    $(SkPath* path)->addRoundRect(*$(SkRect* rect'), $(float rx), $(float ry), (SkPathDirection) $(int dir));
  }|]

addOval :: (MonadIO m) => SkPath -> Rect Float -> SkPathDirection -> m ()
addOval (ptr -> path) rect (marshalSkEnum -> dir) = evalManaged do
  rect' <- marshalSkRect rect
  liftIO
    [C.block| void {
    $(SkPath* path)->addOval(*$(SkRect* rect'), (SkPathDirection) $(int dir));
  }|]

addCircle ::
  (MonadIO m) =>
  SkPath ->
  -- | Circle center
  V2 Float ->
  -- | Radius
  Float ->
  SkPathDirection ->
  m ()
addCircle (ptr -> path) (coerce -> V2 cx cy) (coerce -> radius) (marshalSkEnum -> dir) = liftIO do
  [C.block| void {
    $(SkPath* path)->addCircle($(float cx), $(float cy), $(float radius), (SkPathDirection) $(int dir));
  }|]

addArc ::
  (MonadIO m) =>
  SkPath ->
  Rect Float ->
  -- | Start angle in degrees
  Float ->
  -- | Sweep angle in degrees
  Float ->
  m ()
addArc (ptr -> path) rect (coerce -> startAngle) (coerce -> sweepAngle) = evalManaged do
  rect' <- marshalSkRect rect
  liftIO
    [C.block| void {
    $(SkPath* path)->addArc(*$(SkRect* rect'), $(float startAngle), $(float sweepAngle));
  }|]

getFillType :: (MonadIO m) => SkPath -> m SkPathFillType
getFillType (ptr -> path) = liftIO do
  unmarshalSkEnumOrDie
    =<< [C.exp| int {
    (int) $(SkPath* path)->getFillType()
  }|]

setFillType :: (MonadIO m) => SkPath -> SkPathFillType -> m ()
setFillType (ptr -> path) (marshalSkEnum -> filltype) = liftIO do
  [C.block| void {
    $(SkPath* path)->setFillType((SkPathFillType)$(int filltype));
  }|]

transform :: (MonadIO m) => SkPath -> M33 Float -> m ()
transform (ptr -> path) matrix = evalManaged do
  matrix' <- marshalSkMatrix matrix
  liftIO
    [C.block| void {
    $(SkPath* path)->transform(*$(SkMatrix* matrix'));
  }|]

transformToDest ::
  (MonadIO m) =>
  SkPath ->
  M33 Float ->
  -- | Destination path.
  SkPath ->
  m ()
transformToDest (ptr -> path) matrix (ptr -> dstPath) = evalManaged do
  matrix' <- marshalSkMatrix matrix
  liftIO
    [C.block| void {
    $(SkPath* path)->transform(*$(SkMatrix* matrix'), $(SkPath* dstPath));
  }|]

$( qGenerateSkEnum
    "AddPathMode"
    [trimming|
    AddPathMode chooses how addPath() appends. Adding one SkPath to another can extend
    the last contour or start a new contour.
  |]
    [ ("Append", "SkPath::AddPathMode::kAppend_AddPathMode", "Contours are appended to the destination path as new contours.")
    , ("Extend", "SkPath::AddPathMode::kExtend_AddPathMode", "Extends the last contour of the destination path with the first contour of the source path, connecting them with a line.")
    ]
 )

addPath ::
  (MonadIO m) =>
  SkPath ->
  -- | Other
  SkPath ->
  -- | Offset
  V2 Float ->
  AddPathMode ->
  m ()
addPath (ptr -> path) (ptr -> other) (coerce -> V2 dx dy) (marshalSkEnum -> addMode) = liftIO do
  [C.block| void {
    $(SkPath* path)->addPath(
      *$(SkPath* other),
      $(float dx),
      $(float dy),
      (SkPath::AddPathMode)$(int addMode)
    );
  }|]

addPathReversed ::
  (MonadIO m) =>
  SkPath ->
  -- | Other
  SkPath ->
  m ()
addPathReversed (ptr -> path) (ptr -> other) = liftIO do
  [C.block| void {
    $(SkPath* path)->addPath(*$(SkPath* other), SkPath::kAppend_AddPathMode, true);
  }|]

addPathWithTransform ::
  (MonadIO m) =>
  SkPath ->
  -- | Other
  SkPath ->
  -- | Transform
  M33 Float ->
  AddPathMode ->
  m ()
addPathWithTransform (ptr -> path) (ptr -> other) matrix (marshalSkEnum -> addMode) = evalManaged do
  matrix' <- marshalSkMatrix matrix
  liftIO
    [C.block| void {
    $(SkPath* path)->addPath(
      *$(SkPath* other),
      *$(SkMatrix* matrix'),
      (SkPath::AddPathMode) $(int addMode)
    );
  }|]

reset :: (MonadIO m) => SkPath -> m ()
reset (ptr -> path) = liftIO do
  [C.block| void {
    $(SkPath* path)->reset();
  }|]

rewind :: (MonadIO m) => SkPath -> m ()
rewind (ptr -> path) = liftIO do
  [C.block| void {
    $(SkPath* path)->rewind();
  }|]

{- | Returns true if the point (x, y) is contained by SkPath, taking into
account FillType.
-}
containsPoint :: (MonadIO m) => SkPath -> V2 Float -> m Bool
containsPoint (ptr -> path) (coerce -> V2 x y) = liftIO do
  toBool
    <$> [C.block| bool {
    return $(SkPath* path)->contains($(float x), $(float y));
  }|]

-- | Returns the last point on the path. Returns 'Nothing' if the path is empty.
getLastPoint :: (MonadIO m) => SkPath -> m (Maybe (V2 Float))
getLastPoint (ptr -> path) = evalManaged do
  point' <- allocaSkPoint
  hasPoint <-
    toBool
      <$> liftIO
        [C.exp| bool {
    bool hasPoint = $(SkPath* path)->countPoints() > 0;
    if (hasPoint) {
      *$(SkPoint* point') = $(SkPath* path)->getLastPoint();
    }
    return hasPoint;
  }|]

  if hasPoint
    then Just <$> peekSkPoint point'
    else pure Nothing

{- | Returns true if the path is convex. If necessary, it will first compute the
convexity.
-}
isConvex :: (MonadIO m) => SkPath -> m Bool
isConvex (ptr -> path) =
  liftIO $
    toBool
      <$> [C.exp| bool { $(SkPath* path)->isConvex() } |]

{- | Returns 'Just' with a tuple of info if the SkPath is equivalent to a
rectangle when filled. Returns 'Nothing' if the path is not a rectangle.

When a path is a rectangle, the returned rect may be smaller than the SkPath's
full bounds. This can happen because the path bounds might include moveTo points
that don't affect the filled area of the rectangle.
-}
isRect ::
  (MonadIO m) =>
  SkPath ->
  -- | Returns (rect bounds, 'true' if the 'SkPath' is closed, rect path
  -- direction). Returns 'Nothing' if 'SkPath' is not a rectangle.
  m (Maybe (Rect Float, Bool, SkPathDirection))
isRect (ptr -> path) = evalManaged do
  bounds' <- allocaSkRect
  isClosed' <- managed alloca
  direction' <- managed alloca

  isrect <-
    liftIO
      [C.block| bool {
    return $(SkPath* path)->isRect(
      $(SkRect* bounds'),
      $(bool* isClosed'),
      (SkPathDirection*) $(int* direction')
    );
  }|]

  if toBool isrect
    then do
      bounds <- peekSkRect bounds'
      isClosed <- peekWith toBool isClosed'
      direction <- unmarshalSkEnumOrDie =<< peekWith id direction'
      pure $ Just (bounds, isClosed, direction)
    else do
      pure Nothing

{- | Returns 'Just' if this path is recognized as an oval or circle, along with
the bounding rectangle of the oval. Returns 'Nothing' otherwise.
-}
isOval :: (MonadIO m) => SkPath -> m (Maybe (Rect Float))
isOval (ptr -> path) = evalManaged do
  bounds' <- allocaSkRect
  isoval <-
    liftIO
      [C.block| bool {
    return $(SkPath* path)->isOval($(SkRect* bounds'));
  }|]

  if toBool isoval
    then do
      bounds <- peekSkRect bounds'
      pure (Just bounds)
    else do
      pure Nothing

{- | Returns true if path is representable as SkRRect. Returns false if path is
representable as oval, circle, or SkRect.

rrect receives bounds of SkRRect.

rrect is unmodified if SkRRect is not found.
-}
isRRect ::
  (MonadIO m) =>
  SkPath ->
  -- | rrect; Destination storage
  SkRRect ->
  m Bool
isRRect (ptr -> path) (ptr -> rrect) =
  liftIO $
    toBool
      <$> [C.exp| bool {
    $(SkPath* path)->isRRect($(SkRRect* rrect))
  }|]

{- | Like 'isLine', but returns 'Nothing' if false; returns the (start, end)
points of the line if true.
-}
isLine :: (MonadIO m) => SkPath -> m (Maybe (V2 Float, V2 Float))
isLine (ptr -> path) = evalManaged do
  -- NOTE: 'sk_path_is_line' takes 'sk_point_t line[2]' instead of two
  -- separate 'sk_point_t's, which it makes the code here slightly ugly.

  start' <- allocaSkPoint
  end' <- allocaSkPoint

  isline <-
    liftIO
      [C.exp| bool {
    SkPoint line[2];
    bool isline = $(SkPath* path)->isLine()
    if (isline) {
      *$(SkPoint* start') = line[0];
      *$(SkPoint* end') = line[1];
    }
    return isline;
  }|]

  if toBool isline
    then do
      start <- peekSkPoint start'
      end <- peekSkPoint end'
      pure (Just (start, end))
    else do
      pure Nothing

-- | Returns the number of points in the path.
countPoints :: (MonadIO m) => SkPath -> m Int
countPoints (ptr -> path) = liftIO do
  fromIntegral <$> [C.exp| int { $(SkPath* path)->countPoints() } |]

{- | Returns SkPoint at index in SkPoint array. Valid range for index is 0 to
countPoints() - 1. Returns (0, 0) if index is out of range.
-}
getPoint ::
  (MonadIO m) =>
  SkPath ->
  -- | Index
  Int ->
  m (V2 Float)
getPoint (ptr -> path) (fromIntegral -> index) = evalManaged do
  point' <- allocaSkPoint
  liftIO
    [C.exp| void {
    *$(SkPoint* point') = $(SkPath* path)->getPoint($(int index));
  }|]
  peekSkPoint point'

{- | Returns the number of verbs: kMove_Verb, kLine_Verb, kQuad_Verb,
kConic_Verb, kCubic_Verb, and kClose_Verb; added to SkPath.
-}
countVerbs :: (MonadIO m) => SkPath -> m Int
countVerbs (ptr -> path) = liftIO do
  fromIntegral
    <$> [C.block| int {
    return $(SkPath* path)->countVerbs();
  }|]

getVerbs :: (MonadIO m) => SkPath -> m [SkPathVerb]
getVerbs path@(ptr -> path') = evalManaged do
  n@(fromIntegral -> cn) <- countVerbs path
  cverbs' <- managed $ allocaBytes n

  cactualN <-
    liftIO
      [C.block|int {
    return $(SkPath* path')->getVerbs($(uint8_t* cverbs'), $(int cn));
  }|]
  let actualN = fromIntegral cactualN

  when isHsSkiaAssertionsEnabled do
    unless (n == actualN) do
      liftIO $ throwIO $ InternalError $ "getVerbs: expected " <> show n <> " verbs, but got " <> show actualN

  cverbs <- liftIO $ peekArray n cverbs'
  for cverbs \cverb -> do
    unmarshalSkEnumOrDie (fromIntegral cverb)

-- * Path Bounds

{- | Returns minimum and maximum axes values of SkPoint array. Returns 'Nothing'
if the path contains no points.

Returned bounds width and height may be larger or smaller than area affected
when SkPath is drawn.
-}
getBounds :: (MonadIO m) => SkPath -> m (Maybe (Rect Float))
getBounds (ptr -> path) = evalManaged do
  bounds' <- allocaSkRect
  liftIO
    [C.block| void {
    *$(SkRect* bounds') = $(SkPath* path)->getBounds();
  }|]

  bounds <- peekSkRect bounds'
  if SkRect.isEmpty bounds
    then pure Nothing
    else pure (Just bounds)

{- | Updates Skia's internal bounds cache so that subsequent calls to
'getBounds' are instantaneous. Unaltered copies of the path may also access this
cached bounds through 'getBounds'.

Currently, this is effectively the same as calling 'getBounds' and ignoring the
result.

Call this to prepare a path that will be subsequently drawn from multiple
threads, to avoid a race condition where each draw separately computes the
bounds.
-}
updateBoundsCache :: (MonadIO m) => SkPath -> m ()
updateBoundsCache (ptr -> path) = liftIO do
  [C.block| void {
    $(SkPath* path)->updateBoundsCache();
  }|]

{- | Returns minimum and maximum axes values of the lines and curves in SkPath.
Returns 'Nothing' if SkPath contains no points. Returned bounds width and height
may be larger or smaller than area affected when SkPath is drawn.

Includes points associated with 'MoveTo' verbs that define empty contours.

Behaves identically to 'getBounds' when SkPath contains only lines. If SkPath
contains curves, computed bounds includes the maximum extent of the quad, conic,
or cubic; is slower than 'getBounds'; and unlike 'getBounds', does not cache the
result.
-}
computeTightBounds :: (MonadIO m) => SkPath -> m (Maybe (Rect Float))
computeTightBounds (ptr -> path) = evalManaged do
  bounds' <- allocaSkRect
  liftIO
    [C.block| void {
    *$(SkRect* bounds') = $(SkPath* path)->computeTightBounds();
  }|]

  bounds <- peekSkRect bounds'
  if SkRect.isEmpty bounds
    then pure Nothing
    else pure (Just bounds)

-- * Segment mask

{- | Indicates which types of segments are present in the path.

Returned by 'getSegmentMasks'.
-}
data SegmentMasks = SegmentMasks
  { hasLine :: Bool
  , hasQuad :: Bool
  , hasConic :: Bool
  , hasCubic :: Bool
  }

$( qGenerateSkEnum
    "SkPathSegmentMask"
    [trimming|
    Mask values for different path segment types.
  |]
    [ ("Line", "SkPathSegmentMask::kLine_SkPathSegmentMask", "line segment mask (1 << 0)")
    , ("Quad", "SkPathSegmentMask::kQuad_SkPathSegmentMask", "quadratic segment mask (1 << 1)")
    , ("Conic", "SkPathSegmentMask::kConic_SkPathSegmentMask", "conic segment mask (1 << 2)")
    , ("Cubic", "SkPathSegmentMask::kCubic_SkPathSegmentMask", "cubic segment mask (1 << 3)")
    ]
 )

{-| Returns a 'SegmentMasks' value that indicates which segment types are
present in the 'SkPath'. Each field in the result corresponds to whether the
path contains one or more segments of that type. Returns a value with all fields
False if 'SkPath' contains no lines or curves: quads, conics, or cubics.

'getSegmentMasks' returns a cached result; it is very fast.
-}
getSegmentMasks :: (MonadIO m) => SkPath -> m SegmentMasks
getSegmentMasks (ptr -> path) = liftIO do
  mask <- [C.exp| uint32_t { $(SkPath* path)->getSegmentMasks() } |]
  let testMask c = 0 /= (mask .&. c)
  pure
    SegmentMasks
      { hasLine = testMask [C.pure| uint32_t { SkPath::SegmentMask::kLine_SegmentMask }|]
      , hasQuad = testMask [C.pure| uint32_t { SkPath::SegmentMask::kQuad_SegmentMask }|]
      , hasConic = testMask [C.pure| uint32_t { SkPath::SegmentMask::kConic_SegmentMask }|]
      , hasCubic = testMask [C.pure| uint32_t { SkPath::SegmentMask::kCubic_SegmentMask }|]
      }

-- * SkPathOps utils

$( qGenerateSkEnum
  "Op"
  [trimming|
    The logical operations that can be performed when combining two paths.
  |]
  [ ("Difference", "SkPathOp::kDifference_SkPathOp", "subtract the op path from the first path")
  , ("Intersect", "SkPathOp::kIntersect_SkPathOp", "intersect the two paths")
  , ("Union", "SkPathOp::kUnion_SkPathOp", "union (inclusive-or) the two paths")
  , ("XOR", "SkPathOp::kXOR_SkPathOp", "exclusive-or the two paths")
  , ("ReverseDifference", "SkPathOp::kReverseDifference_SkPathOp", "subtract the first path from the op path")
  ]
 )

{- | Returns true if operation was able to produce a result; otherwise, result
is unmodified.
-}
opToDest ::
  (MonadIO m) =>
  -- | Path 1
  SkPath ->
  -- | Path 2
  SkPath ->
  -- | Operation mode
  Op ->
  -- | Destination path
  SkPath ->
  m Bool
opToDest (ptr -> path1) (ptr -> path2) (marshalSkEnum -> opMode) (ptr -> dstPath) = liftIO $ toBool <$>
  [C.block| bool {
    return Op(*$(SkPath* path1), *$(SkPath* path2), (SkPathOp) $(int opMode), *$(SkPath* dstPath));
  }|]

{- | Set this path to a set of non-overlapping contours that describe the same
area as the original path. The curve order is reduced where possible so that
cubics may be turned into quadratics, and quadratics maybe turned into lines.

Returns true if operation was able to produce a result; otherwise, result is
unmodified.
-}
simplifyToDest ::
  (MonadIO m) =>
  SkPath ->
  -- | Destination path
  SkPath ->
  m Bool
simplifyToDest (ptr -> path) (ptr -> dstPath) = liftIO $ toBool <$>
  [C.block| bool {
    return Simplify(*$(SkPath* path), $(SkPath* dstPath));
  }|]

{- | Set the result with fill type winding to area equivalent to path. Returns
true if successful. Does not detect if path contains contours which contain
self-crossings or cross other contours; in these cases, may return true even
though result does not fill same area as path.

Returns true if operation was able to produce a result; otherwise, result is
unmodified. The result may be the input.
-}
toWindingToDest ::
  (MonadIO m) =>
  SkPath ->
  -- | Destination path
  SkPath ->
  m Bool
toWindingToDest (ptr -> path) (ptr -> dstPath) = liftIO $ toBool <$>
  [C.block| bool {
    return AsWinding(*$(SkPath* path), $(SkPath* dstPath));
  }|]