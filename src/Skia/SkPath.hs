module Skia.SkPath where

import Data.ByteString qualified as BS
import Data.Function
import Data.Int
import Language.C.Inline.Cpp qualified as C
import Linear
import NeatInterpolation
import Skia.Internal.Prelude
import Skia.Internal.THUtils
import Skia.Linear
import Skia.SkPathTypes as SkPathTypes
import Skia.SkRRect qualified as SkRRect
import Skia.SkRect as SkRect
import Skia.SkString qualified as SkString

C.context $
  mconcat
    [ C.cppCtx
    , cppSkiaObjectTypes
    ]

C.include "core/SkPath.h"

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

$( qGenerateSkEnum "ArcSize"
  [trimming|
    Represents arc size for path operations.
    When drawing an arc, there are generally two ways to draw the arc between two points:
    a smaller arc or a larger arc that goes around the other way.
  |]
  [("Small", "SkPath::ArcSize::kSmall_ArcSize", "smaller of arc pair")
  ,("Large", "SkPath::ArcSize::kLarge_ArcSize", "larger of arc pair")
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
  SkPathTypes.Direction ->
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
  SkPathTypes.Direction ->
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

addRect :: (MonadIO m) => (MonadIO m) => SkPath -> Rect Float -> SkPathTypes.Direction -> m ()
addRect (ptr -> path) rect (marshalSkEnum -> dir) = evalManaged do
  rect' <- marshalSkRect rect
  liftIO [C.block| void {
    $(SkPath* path)->addRect(*$(SkRect* rect'), (SkPathDirection) $(int dir));
  }|]

addRectWithStart :: (MonadIO m) => SkPath -> Rect Float -> SkPathTypes.Direction -> Int -> m ()
addRectWithStart (ptr -> path) rect (marshalSkEnum -> dir) (fromIntegral -> startIndex) = evalManaged do
  rect' <- marshalSkRect rect
  liftIO [C.block| void {
    $(SkPath* path)->addRect(*$(SkRect* rect'), (SkPathDirection) $(int dir), $(int startIndex));
  }|]

addRRect :: (MonadIO m) => SkPath -> SkRRect -> SkPathTypes.Direction -> m ()
addRRect (ptr -> path) (ptr -> rrect) (marshalSkEnum -> dir) = liftIO do
  [C.block| void {
    $(SkPath* path)->addRRect(*$(SkRRect* rrect), (SkPathDirection) $(int dir));
  }|]

addRRectWithStart :: (MonadIO m) => SkPath -> SkRRect -> SkPathTypes.Direction -> Int -> m ()
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
  SkPathTypes.Direction ->
  m ()
addRoundRect (ptr -> path) rect (coerce -> V2 rx ry) (marshalSkEnum -> dir) = evalManaged do
  rect' <- marshalSkRect rect
  liftIO [C.block| void {
    $(SkPath* path)->addRoundRect(*$(SkRect* rect'), $(float rx), $(float ry), (SkPathDirection) $(int dir));
  }|]

addOval :: (MonadIO m) => SkPath -> Rect Float -> SkPathTypes.Direction -> m ()
addOval (ptr -> path) rect (marshalSkEnum -> dir) = evalManaged do
  rect' <- marshalSkRect rect
  liftIO [C.block| void {
    $(SkPath* path)->addOval(*$(SkRect* rect'), (SkPathDirection) $(int dir));
  }|]

addCircle ::
  (MonadIO m) =>
  SkPath ->
  -- | Circle center
  V2 Float ->
  -- | Radius
  Float ->
  SkPathTypes.Direction ->
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
  liftIO [C.block| void {
    $(SkPath* path)->addArc(*$(SkRect* rect'), $(float startAngle), $(float sweepAngle));
  }|]

{- | Returns minimum and maximum axes values of SkPoint array. Returns 'Nothing'
if the path contains no points.

Returned bounds width and height may be larger or smaller than area affected
when SkPath is drawn.
-}
getBounds :: (MonadIO m) => SkPath -> m (Maybe (Rect Float))
getBounds (ptr -> path) = evalManaged do
  bounds' <- allocaSkRect
  liftIO [C.block| void {
    *$(SkRect* bounds') = $(SkPath* path)->getBounds();
  }|]
  
  bounds <- peekSkRect bounds'
  if SkRect.isEmpty bounds
    then pure Nothing
    else pure (Just bounds)

{- | Returns minimum and maximum axes values of the lines and curves in SkPath.
Returns (0, 0, 0, 0) if SkPath contains no points. Returned bounds width and
height may be larger or smaller than area affected when SkPath is drawn.

Includes SkPoint associated with kMove_Verb that define empty contours.

Behaves identically to getBounds() when SkPath contains only lines. If SkPath
contains curves, computed bounds includes the maximum extent of the quad, conic,
or cubic; is slower than getBounds(); and unlike getBounds(), does not cache the
result.
-}
computeTightBounds :: (MonadIO m) => SkPath -> m (Maybe (Rect Float))
computeTightBounds (ptr -> path) = evalManaged do
  bounds' <- allocaSkRect
  liftIO [C.block| void {
    *$(SkRect* bounds') = $(SkPath* path)->computeTightBounds();
  }|]
  
  bounds <- peekSkRect bounds'
  if SkRect.isEmpty bounds
    then pure Nothing
    else pure (Just bounds)

getFillType :: (MonadIO m) => SkPath -> m SkPathTypes.FillType
getFillType (ptr -> path) = liftIO do
  unmarshalSkEnumOrDie =<< [C.exp| int {
    (int) $(SkPath* path)->getFillType()
  }|]

setFillType :: (MonadIO m) => SkPath -> SkPathTypes.FillType -> m ()
setFillType (ptr -> path) (marshalSkEnum -> filltype) = liftIO do
  [C.block| void {
    $(SkPath* path)->setFillType((SkPathFillType)$(int filltype));
  }|]

transform :: (MonadIO m) => SkPath -> M33 Float -> m ()
transform (ptr -> path) matrix = evalManaged do
  matrix' <- marshalSkMatrix matrix
  liftIO [C.block| void {
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
  liftIO [C.block| void {
    $(SkPath* path)->transform(*$(SkMatrix* matrix'), $(SkPath* dstPath));
  }|]

$( qGenerateSkEnum "AddPathMode"
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
  liftIO [C.block| void {
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

{- | Returns the number of points in the path.
-}
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
  liftIO [C.exp| void {
    *$(SkPoint* point') = $(SkPath* path)->getPoint($(int index));
  }|]
  peekSkPoint point'

countVerbs :: (MonadIO m) => SkPath -> m Int
countVerbs (ptr -> path) = liftIO do
  fromIntegral <$> [C.block| int {
    return $(SkPath* path)->countVerbs();
  }|]

{- | Returns true if the point (x, y) is contained by SkPath, taking into
account FillType.
-}
containsPoint :: (MonadIO m) => SkPath -> V2 Float -> m Bool
containsPoint (ptr -> path) (coerce -> V2 x y) = liftIO do
  toBool <$> [C.block| bool {
    return $(SkPath* path)->contains($(float x), $(float y));
  }|]

-- -- | Returns the last point on the path. Returns 'Nothing' if the path is empty.
-- getLastPoint :: (MonadIO m) => SkPath -> m (Maybe (V2 Float))
-- getLastPoint path = evalManaged do
--   point' <- managed alloca
-- 
--   exists <- liftIO $ toBool <$> sk_path_get_last_point (ptr path) point'
--   if exists
--     then do
--       point <- liftIO $ fromSKPoint <$> peek point'
--       pure (Just point)
--     else do
--       pure Nothing
-- 
-- isConvex :: (MonadIO m) => SkPath -> m Bool
-- isConvex path = evalManaged do
--   liftIO $ toBool <$> sk_path_is_convex (ptr path)
-- 
-- asRect ::
--   (MonadIO m) =>
--   SkPath ->
--   -- | Returns (rect bounds, 'true' if the 'SkPath' is closed, rect path
--   -- direction). Returns 'Nothing' if 'SkPath' is not a rectangle.
--   m (Maybe (Rect Float, Bool, SkPathTypes.Direction))
-- asRect path = evalManaged do
--   bounds' <- managed alloca
--   isClosed' <- managed alloca
--   direction' <- managed alloca
-- 
--   isrect <- liftIO $ fmap toBool $ sk_path_is_rect (ptr path) bounds' isClosed' direction'
-- 
--   if isrect
--     then do
--       bounds <- peekWith fromSkRect bounds'
--       isClosed <- peekWith toBool isClosed'
--       direction <- unmarshalSkEnumOrDie =<< peekWith id direction'
--       pure $ Just (bounds, isClosed, direction)
--     else do
--       pure Nothing
-- 
-- asOval :: (MonadIO m) => SkPath -> m (Maybe (Rect Float))
-- asOval path = evalManaged do
--   bounds' <- managed alloca
--   exists <- liftIO $ fmap toBool $ sk_path_is_oval (ptr path) bounds'
--   if exists
--     then do
--       bounds <- peekWith fromSkRect bounds'
--       pure (Just bounds)
--     else do
--       pure Nothing
-- 
-- asRRect :: (MonadResource m) => SkPath -> m (Maybe (ReleaseKey, SkRRect))
-- asRRect path = do
--   (key, rrect) <- SkRRect.createEmpty
--   exists <- liftIO $ toBool <$> sk_path_is_rrect (ptr path) (ptr rrect)
--   if exists
--     then do
--       pure $ Just (key, rrect)
--     else do
--       release key
--       pure Nothing
-- 
-- {- | Like 'isLine', but returns 'Nothing' if false; returns the (start, end)
-- points of the line if true.
-- -}
-- asLine :: (MonadIO m) => SkPath -> m (Maybe (V2 Float, V2 Float))
-- asLine path = evalManaged do
--   -- NOTE: 'sk_path_is_line' takes 'sk_point_t line[2]' instead of two
--   -- separate 'sk_point_t's, which it makes the code here slightly ugly.
-- 
--   points' <- managed $ allocaArray 2
-- 
--   exists <- liftIO $ toBool <$> sk_path_is_line (ptr path) points'
--   if exists
--     then do
--       start <- liftIO $ fromSKPoint <$> peekElemOff points' 0
--       end <- liftIO $ fromSKPoint <$> peekElemOff points' 1
--       pure (Just (start, end))
--     else do
--       pure Nothing
-- 
-- getSegmentMasks :: (MonadIO m) => SkPath -> m Word32
-- getSegmentMasks path = evalManaged do
--   liftIO $ sk_path_get_segment_masks (ptr path)
-- 
-- {- | Returns true if operation was able to produce a result; otherwise, result
-- is unmodified.
-- -}
-- opToDest ::
--   (MonadIO m) =>
--   -- | Path 1
--   SkPath ->
--   -- | Path 2
--   SkPath ->
--   -- | Operation mode
--   SkPathOp ->
--   -- | Destination path
--   SkPath ->
--   m Bool
-- opToDest path1 path2 opMode dstPath = evalManaged do
--   liftIO $ toBool <$> sk_pathop_op (ptr path1) (ptr path2) (marshalSkEnum opMode) (ptr dstPath)
-- 
-- {- | Set this path to a set of non-overlapping contours that describe the same
-- area as the original path. The curve order is reduced where possible so that
-- cubics may be turned into quadratics, and quadratics maybe turned into lines.
-- 
-- Returns true if operation was able to produce a result; otherwise, result is
-- unmodified.
-- -}
-- simplifyToDest ::
--   (MonadIO m) =>
--   SkPath ->
--   -- | Destination path
--   SkPath ->
--   m Bool
-- simplifyToDest path dstPath = evalManaged do
--   liftIO $ toBool <$> sk_pathop_simplify (ptr path) (ptr dstPath)
-- 
-- {- | Computes the rectangle of the tight bounds of the path. Returns 'Nothing' if
-- the bounds could not be computed.
-- -}
-- tightBounds :: (MonadIO m) => SkPath -> m (Maybe (Rect Float))
-- tightBounds path = evalManaged do
--   bounds' <- managed alloca
--   success <- liftIO $ toBool <$> sk_pathop_tight_bounds (ptr path) bounds'
--   if success
--     then do
--       bounds <- liftIO $ fromSkRect <$> peek bounds'
--       pure $ Just bounds
--     else do
--       pure Nothing
-- 
-- -- TODO: I don't understand what 'toWindingToDest' does.
-- 
-- {- | Set the result with fill type winding to area equivalent to path. Returns
--  true if successful. Does not detect if path contains contours which contain
--  self-crossings or cross other contours; in these cases, may return true even
--  though result does not fill same area as path.
-- 
--  Returns true if operation was able to produce a result; otherwise, result is
--  unmodified. The result may be the input.
-- -}
-- toWindingToDest :: (MonadIO m) => SkPath -> SkPath -> m Bool
-- toWindingToDest path result = evalManaged do
--   liftIO $ toBool <$> sk_pathop_as_winding (ptr path) (ptr result)
-- 