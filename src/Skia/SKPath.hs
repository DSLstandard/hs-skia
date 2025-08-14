module Skia.SKPath where

import Data.ByteString qualified as BS
import Data.Function
import Data.Int
import Linear
import Skia.Bindings.Sk_path
import Skia.Bindings.Types
import Skia.Internal.Prelude
import Skia.Rect as Rect
import Skia.SKRRect qualified as SKRRect
import Skia.SKString qualified as SKString

{- | Constructs an empty SkPath. By default, SkPath has no verbs, no SkPoint,
and no weights. FillType is set to kWinding.
-}
createEmpty :: (MonadResource m) => m (ReleaseKey, SKPath)
createEmpty =
  allocateSKObjectNeverNull
    sk_path_new
    sk_path_delete

{- | Constructs a copy of an existing path. This makes two paths identical by
value. Internally, path and the returned result share pointer values. The
underlying verb array, SkPoint array and weights are copied when modified.

Creating a SkPath copy is very efficient and never allocates memory. SkPath are
always copied by value from the interface; the underlying shared pointers are
not exposed.

You must free the returned 'SKPath' with 'destroy' when done with it.
-}
clone :: (MonadResource m) => SKPath -> m (ReleaseKey, SKPath)
clone path =
  allocateSKObjectNeverNull
    (sk_path_clone (ptr path))
    sk_path_delete

-- | Adds beginning of contour at SkPoint (x, y).
moveTo :: (MonadIO m) => SKPath -> V2 Float -> m ()
moveTo path p =
  liftIO $ sk_path_move_to (ptr path) & applyV2 (coerce p)

{- | Adds beginning of contour relative to last point. If SkPath is empty,
starts contour at (dx, dy). Otherwise, start contour at last point offset by
(dx, dy). Function name stands for "relative move to".
-}
rMoveTo :: (MonadIO m) => SKPath -> V2 Float -> m ()
rMoveTo path dp =
  liftIO $ sk_path_rmove_to (ptr path) & applyV2 (coerce dp)

{- | Adds line from last point to (x, y). If SkPath is empty, or last
SkPath::Verb is kClose_Verb, last point is set to (0, 0) before adding line.

lineTo() appends kMove_Verb to verb array and (0, 0) to SkPoint array, if
needed.

lineTo() then appends kLine_Verb to verb array and (x, y) to SkPoint array.
-}
lineTo :: (MonadIO m) => SKPath -> V2 Float -> m ()
lineTo path p =
  liftIO $ sk_path_line_to (ptr path) & applyV2 (coerce p)

rLineTo :: (MonadIO m) => SKPath -> V2 Float -> m ()
rLineTo path dp =
  liftIO $ sk_path_rline_to (ptr path) & applyV2 (coerce dp)

quadTo ::
  (MonadIO m) =>
  SKPath ->
  -- | Control point 1
  V2 Float ->
  -- | End point
  V2 Float ->
  m ()
quadTo path c1 c2 =
  liftIO $ sk_path_quad_to (ptr path) & applyV2 (coerce c1) & applyV2 (coerce c2)

rQuadTo ::
  (MonadIO m) =>
  SKPath ->
  -- | Control point 1
  V2 Float ->
  -- | End point
  V2 Float ->
  m ()
rQuadTo path c1 c2 =
  liftIO $ sk_path_rquad_to (ptr path) & applyV2 (coerce c1) & applyV2 (coerce c2)

cubicTo ::
  (MonadIO m) =>
  SKPath ->
  -- | Control point 1
  V2 Float ->
  -- | Control point 2
  V2 Float ->
  -- | End point
  V2 Float ->
  m ()
cubicTo path c1 c2 c3 =
  liftIO $ sk_path_cubic_to (ptr path) & applyV2 (coerce c1) & applyV2 (coerce c2) & applyV2 (coerce c3)

rCubicTo ::
  (MonadIO m) =>
  SKPath ->
  -- | Control point 1
  V2 Float ->
  -- | Control point 2
  V2 Float ->
  -- | End point
  V2 Float ->
  m ()
rCubicTo path c1 c2 c3 =
  liftIO $ sk_path_rcubic_to (ptr path) & applyV2 (coerce c1) & applyV2 (coerce c2) & applyV2 (coerce c3)

conicTo ::
  (MonadIO m) =>
  SKPath ->
  -- | Control point 1
  V2 Float ->
  -- | End point
  V2 Float ->
  -- | Weight of the added conic
  Float ->
  m ()
conicTo path c1 c2 weight =
  liftIO $ sk_path_conic_to (ptr path) & applyV2 (coerce c1) & applyV2 (coerce c2) & apply (coerce weight)

rConicTo ::
  (MonadIO m) =>
  SKPath ->
  -- | Control point 1
  V2 Float ->
  -- | End point
  V2 Float ->
  -- | Weight of the added conic
  Float ->
  m ()
rConicTo path c1 c2 weight =
  liftIO $ sk_path_rconic_to (ptr path) & applyV2 (coerce c1) & applyV2 (coerce c2) & apply (coerce weight)

arcTo ::
  (MonadIO m) =>
  SKPath ->
  -- | Radii on axes before x-axis rotation
  V2 Float ->
  -- | X-axis rotation in degrees
  Float ->
  -- | Chooses smaller or larger arc
  SKPathArcSize ->
  -- | Chooses closewise or counterclockwise
  SKPathDirection ->
  -- | End of arc
  V2 Float ->
  m ()
arcTo path radii xAxisRotate largeArc sweep xy =
  liftIO $
    sk_path_arc_to (ptr path)
      & applyV2 (coerce radii)
      & apply (coerce xAxisRotate)
      & apply (marshalSKEnum largeArc)
      & apply (marshalSKEnum sweep)
      & applyV2 (coerce xy)

rArcTo ::
  (MonadIO m) =>
  SKPath ->
  -- | Radii on axes before x-axis rotation
  V2 Float ->
  -- | X-axis rotation in degrees
  Float ->
  -- | Chooses smaller or larger arc
  SKPathArcSize ->
  -- | Chooses closewise or counterclockwise
  SKPathDirection ->
  -- | End of arc
  V2 Float ->
  m ()
rArcTo path radii xAxisRotate largeArc sweep dxdy =
  liftIO $
    sk_path_rarc_to (ptr path)
      & applyV2 (coerce radii)
      & apply (coerce xAxisRotate)
      & apply (marshalSKEnum largeArc)
      & apply (marshalSKEnum sweep)
      & applyV2 (coerce dxdy)

arcToWithOval ::
  (MonadIO m) =>
  SKPath ->
  -- | Oval
  Rect Float ->
  -- | Start angle
  Float ->
  -- | Sweep angle
  Float ->
  -- | Force move to?
  Bool ->
  m ()
arcToWithOval path oval startAngle sweepAngle forceMoveTo = evalManaged do
  oval' <- storable $ toSKRect oval
  liftIO $ sk_path_arc_to_with_oval (ptr path) oval' (coerce startAngle) (coerce sweepAngle) (fromBool forceMoveTo)

arcToWithPoints ::
  (MonadIO m) =>
  SKPath ->
  -- | Common point to pair of tangents
  V2 Float ->
  -- | End of second tangent
  V2 Float ->
  -- | Radius: Distance from arc to circle center
  Float ->
  m ()
arcToWithPoints path c1 c2 radius = evalManaged do
  liftIO $
    sk_path_arc_to_with_points (ptr path)
      & applyV2 (coerce c1)
      & applyV2 (coerce c2)
      & apply (coerce radius)

close :: (MonadIO m) => SKPath -> m ()
close path = evalManaged do
  liftIO $ sk_path_close (ptr path)

data RectPoint
  = RectPoint'TopLeft
  | RectPoint'RightTop
  | RectPoint'BottomRight
  | RectPoint'BottomLeft
  deriving (Show, Eq, Ord, Enum, Bounded)

{- | PRIVATE FUNCTION. Maps 'RectPoint' to their corresponding \"startIndex\".
Used by functions like 'sk_path_add_rrect_start'.

See src/core/SkPathMakers.h's SkPath_RectPointIterator.
-}
privRectPointToStartIndex :: RectPoint -> Word32
privRectPointToStartIndex = \case
  RectPoint'TopLeft -> 0
  RectPoint'RightTop -> 1
  RectPoint'BottomRight -> 2
  RectPoint'BottomLeft -> 3

addRect :: (MonadIO m) => (MonadIO m) => SKPath -> Rect Float -> SKPathDirection -> m ()
addRect path rect dir = evalManaged do
  rect' <- storable $ toSKRect rect
  liftIO $ sk_path_add_rect (ptr path) rect' (marshalSKEnum dir)

addRectWithStart :: (MonadIO m) => (MonadIO m) => SKPath -> Rect Float -> SKPathDirection -> RectPoint -> m ()
addRectWithStart path rect dir startPoint = evalManaged do
  rect' <- storable $ toSKRect rect
  liftIO $ sk_path_add_rect_start (ptr path) rect' (marshalSKEnum dir) (privRectPointToStartIndex startPoint)

addRRect :: (MonadIO m) => (MonadIO m) => SKPath -> SKRRect -> SKPathDirection -> m ()
addRRect path rrect dir = evalManaged do
  liftIO $ sk_path_add_rrect (ptr path) (ptr rrect) (marshalSKEnum dir)

addRRectWithStart :: (MonadIO m) => (MonadIO m) => SKPath -> SKRRect -> SKPathDirection -> RectPoint -> m ()
addRRectWithStart path rrect dir startPoint = evalManaged do
  liftIO $ sk_path_add_rrect_start (ptr path) (ptr rrect) (marshalSKEnum dir) (privRectPointToStartIndex startPoint)

addRoundedRect ::
  (MonadIO m) =>
  SKPath ->
  Rect Float ->
  -- | Radii
  V2 Float ->
  SKPathDirection ->
  m ()
addRoundedRect path rect radii dir = evalManaged do
  rect' <- storable $ toSKRect rect
  liftIO $
    sk_path_add_rounded_rect (ptr path)
      & apply rect'
      & applyV2 (coerce radii)
      & apply (marshalSKEnum dir)

addOval :: (MonadIO m) => SKPath -> Rect Float -> SKPathDirection -> m ()
addOval path rect dir = evalManaged do
  rect' <- storable $ toSKRect rect
  liftIO $ sk_path_add_oval (ptr path) rect' (marshalSKEnum dir)

addCircle ::
  (MonadIO m) =>
  SKPath ->
  -- | Circle center
  V2 Float ->
  -- | Radius
  Float ->
  SKPathDirection ->
  m ()
addCircle path center radius dir = evalManaged do
  liftIO $
    sk_path_add_circle (ptr path)
      & applyV2 (coerce center)
      & apply (coerce radius)
      & apply (marshalSKEnum dir)

addArc ::
  (MonadIO m) =>
  SKPath ->
  Rect Float ->
  -- | Start angle in degrees
  Float ->
  -- | Sweep angle in degrees
  Float ->
  m ()
addArc path oval startAngle sweepAngle = evalManaged do
  oval' <- storable $ toSKRect oval
  liftIO $ sk_path_add_arc (ptr path) oval' (coerce startAngle) (coerce sweepAngle)

addPolyRaw ::
  (MonadIO m) =>
  SKPath ->
  -- | Points array
  Ptr Sk_point ->
  -- | Number of elements in points array
  Int ->
  -- | If true, add line connecting contour end and start.
  Bool ->
  m ()
addPolyRaw path pts count close = evalManaged do
  liftIO $ sk_path_add_poly (ptr path) pts (fromIntegral count) (fromBool close)

-- | Like 'addPolyRaw' but takes in a list of (V2 Float) as points.
addPolyByList ::
  (MonadIO m) =>
  SKPath ->
  [V2 Float] ->
  -- | If true, add line connecting contour end and start.
  Bool ->
  m ()
addPolyByList path pts close = evalManaged do
  -- FIXME: Optimize? fmapping is bad.
  (pts', numPts) <- managed $ withArrayLen' $ fmap toSKPoint $ pts
  addPolyRaw path pts' numPts close

{- | Returns minimum and maximum axes values of SkPoint array. Returns 'Nothing'
if the path contains no points.

Returned bounds width and height may be larger or smaller than area affected
when SkPath is drawn.
-}
getBounds :: (MonadIO m) => SKPath -> m (Maybe (Rect Float))
getBounds path = evalManaged do
  bounds' <- managed alloca

  liftIO $ sk_path_get_bounds (ptr path) bounds'

  bounds <- liftIO $ fromSKRect <$> peek bounds'
  if Rect.isEmpty bounds
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
computeTightBounds :: (MonadIO m) => SKPath -> m (Maybe (Rect Float))
computeTightBounds path = evalManaged do
  bounds' <- managed alloca

  liftIO $ sk_path_compute_tight_bounds (ptr path) bounds'

  bounds <- liftIO $ fromSKRect <$> peek bounds'
  if Rect.isEmpty bounds
    then pure Nothing
    else pure (Just bounds)

getFillType :: (MonadIO m) => SKPath -> m SKPathFillType
getFillType path = evalManaged do
  filltype <- liftIO $ sk_path_get_filltype (ptr path)
  unmarshalSKEnumOrDie filltype

setFillType :: (MonadIO m) => SKPath -> SKPathFillType -> m ()
setFillType path filltype = evalManaged do
  liftIO $ sk_path_set_filltype (ptr path) (marshalSKEnum filltype)

transform :: (MonadIO m) => SKPath -> M33 Float -> m ()
transform path matrix = evalManaged do
  matrix' <- storable $ toSKMatrix matrix
  liftIO $ sk_path_transform (ptr path) matrix'

transformToDest ::
  (MonadIO m) =>
  SKPath ->
  M33 Float ->
  -- | Destination path.
  SKPath ->
  m ()
transformToDest path matrix dstPath = evalManaged do
  matrix' <- storable $ toSKMatrix matrix
  liftIO $ sk_path_transform_to_dest (ptr path) matrix' (ptr dstPath)

addPath ::
  (MonadIO m) =>
  SKPath ->
  -- | Other
  SKPath ->
  SKPathAddMode ->
  m ()
addPath path other addMode = evalManaged do
  liftIO $ sk_path_add_path (ptr path) (ptr other) (marshalSKEnum addMode)

addPathReversed ::
  (MonadIO m) =>
  SKPath ->
  -- | Other
  SKPath ->
  m ()
addPathReversed path other = evalManaged do
  liftIO $ sk_path_add_path_reverse (ptr path) (ptr other)

addPathWithOffset ::
  (MonadIO m) =>
  SKPath ->
  -- | Other
  SKPath ->
  -- | Offset
  V2 Float ->
  SKPathAddMode ->
  m ()
addPathWithOffset path other offset addMode = evalManaged do
  liftIO $
    sk_path_add_path_offset (ptr path) (ptr other)
      & applyV2 (coerce offset)
      & apply (marshalSKEnum addMode)

addPathWithTransform ::
  (MonadIO m) =>
  SKPath ->
  -- | Other
  SKPath ->
  -- | Transform
  M33 Float ->
  SKPathAddMode ->
  m ()
addPathWithTransform path other matrix addMode = evalManaged do
  matrix' <- storable $ toSKMatrix matrix
  liftIO $ sk_path_add_path_matrix (ptr path) (ptr other) matrix' (marshalSKEnum addMode)

reset :: (MonadIO m) => SKPath -> m ()
reset path = evalManaged do
  liftIO $ sk_path_reset (ptr path)

rewind :: (MonadIO m) => SKPath -> m ()
rewind path = evalManaged do
  liftIO $ sk_path_rewind (ptr path)

countPoints :: (MonadIO m) => SKPath -> m Int
countPoints path = evalManaged do
  liftIO $ fromIntegral <$> sk_path_count_points (ptr path)

{- | Returns SkPoint at index in SkPoint array. Valid range for index is 0 to
countPoints() - 1. Returns (0, 0) if index is out of range.
-}
getPoint ::
  (MonadIO m) =>
  SKPath ->
  -- | Index
  Int32 ->
  m (V2 Float)
getPoint path index = evalManaged do
  point' <- managed alloca
  liftIO $ sk_path_get_point (ptr path) (coerce index) point'
  liftIO $ fromSKPoint <$> peek point'

{- | Returns number of points in SkPath. Up to max points are copied.  points
may be nullptr; then, max must be zero.  If max is greater than number of
points, excess points storage is unaltered.

Returns SkPoint array length
-}
getPointsToDest ::
  (MonadIO m) =>
  SKPath ->
  -- | Destination array.
  Ptr Sk_point ->
  -- | Maximum count to copy. Must be greater than or equal to zero.
  Int32 ->
  m Int32
getPointsToDest path dstPoints maxCount = evalManaged do
  liftIO $ coerce <$> sk_path_get_points (ptr path) dstPoints (coerce maxCount)

countVerbs :: (MonadIO m) => SKPath -> m Int
countVerbs path = evalManaged do
  liftIO $ fromIntegral <$> sk_path_count_verbs (ptr path)

{- | Returns true if the point (x, y) is contained by SkPath, taking into
account FillType.
-}
containsPoint :: (MonadIO m) => SKPath -> V2 Float -> m Bool
containsPoint path p = evalManaged do
  liftIO $ toBool <$> (sk_path_contains (ptr path) & applyV2 (coerce p))

parseSVGToDestRaw ::
  (MonadIO m) =>
  -- | Destination path
  SKPath ->
  -- | Null-terminated C string
  CString ->
  -- | Returns false if failed.
  m Bool
parseSVGToDestRaw dstPath str' = evalManaged do
  liftIO $ toBool <$> sk_path_parse_svg_string (ptr dstPath) str'

{- | Like 'parseSVGToDestRaw' but takes in a ByteString. However, it does an
O(n) copy of the ByteString to add a null-terminator at the end.
-}
parseSVGToDest ::
  (MonadIO m) =>
  -- | Destination path
  SKPath ->
  BS.ByteString ->
  m Bool
parseSVGToDest dstPath str = liftIO do
  BS.useAsCString str \str' -> do
    parseSVGToDestRaw dstPath str'

{- | Renders the input 'SKPath' as an SVG string and returns a ByteString of it
in UTF-8 encoding.
-}
renderSvgString :: (MonadIO m) => SKPath -> m BS.ByteString
renderSvgString path = liftIO $ runResourceT do
  (_, str) <- SKString.createEmpty
  liftIO $ sk_path_to_svg_string (ptr path) (ptr str)
  SKString.getAsByteStringUtf8 str

-- | Returns the last point on the path. Returns 'Nothing' if the path is empty.
getLastPoint :: (MonadIO m) => SKPath -> m (Maybe (V2 Float))
getLastPoint path = evalManaged do
  point' <- managed alloca

  exists <- liftIO $ toBool <$> sk_path_get_last_point (ptr path) point'
  if exists
    then do
      point <- liftIO $ fromSKPoint <$> peek point'
      pure (Just point)
    else do
      pure Nothing

isConvex :: (MonadIO m) => SKPath -> m Bool
isConvex path = evalManaged do
  liftIO $ toBool <$> sk_path_is_convex (ptr path)

asRect ::
  (MonadIO m) =>
  SKPath ->
  -- | Returns (rect bounds, 'true' if the 'SKPath' is closed, rect path
  -- direction). Returns 'Nothing' if 'SKPath' is not a rectangle.
  m (Maybe (Rect Float, Bool, SKPathDirection))
asRect path = evalManaged do
  bounds' <- managed alloca
  isClosed' <- managed alloca
  direction' <- managed alloca

  isrect <- liftIO $ fmap toBool $ sk_path_is_rect (ptr path) bounds' isClosed' direction'

  if isrect
    then do
      bounds <- peekWith fromSKRect bounds'
      isClosed <- peekWith toBool isClosed'
      direction <- unmarshalSKEnumOrDie =<< peekWith id direction'
      pure $ Just (bounds, isClosed, direction)
    else do
      pure Nothing

asOval :: (MonadIO m) => SKPath -> m (Maybe (Rect Float))
asOval path = evalManaged do
  bounds' <- managed alloca
  exists <- liftIO $ fmap toBool $ sk_path_is_oval (ptr path) bounds'
  if exists
    then do
      bounds <- peekWith fromSKRect bounds'
      pure (Just bounds)
    else do
      pure Nothing

asRRect :: (MonadResource m) => SKPath -> m (Maybe (ReleaseKey, SKRRect))
asRRect path = do
  (key, rrect) <- SKRRect.createEmpty
  exists <- liftIO $ toBool <$> sk_path_is_rrect (ptr path) (ptr rrect)
  if exists
    then do
      pure $ Just (key, rrect)
    else do
      release key
      pure Nothing

{- | Like 'isLine', but returns 'Nothing' if false; returns the (start, end)
points of the line if true.
-}
asLine :: (MonadIO m) => SKPath -> m (Maybe (V2 Float, V2 Float))
asLine path = evalManaged do
  -- NOTE: 'sk_path_is_line' takes 'sk_point_t line[2]' instead of two
  -- separate 'sk_point_t's, which it makes the code here slightly ugly.

  points' <- managed $ allocaArray 2

  exists <- liftIO $ toBool <$> sk_path_is_line (ptr path) points'
  if exists
    then do
      start <- liftIO $ fromSKPoint <$> peekElemOff points' 0
      end <- liftIO $ fromSKPoint <$> peekElemOff points' 1
      pure (Just (start, end))
    else do
      pure Nothing

getSegmentMasks :: (MonadIO m) => SKPath -> m Word32
getSegmentMasks path = evalManaged do
  liftIO $ sk_path_get_segment_masks (ptr path)

{- | Returns true if operation was able to produce a result; otherwise, result
is unmodified.
-}
opToDest ::
  (MonadIO m) =>
  -- | Path 1
  SKPath ->
  -- | Path 2
  SKPath ->
  -- | Operation mode
  SKPathOp ->
  -- | Destination path
  SKPath ->
  m Bool
opToDest path1 path2 opMode dstPath = evalManaged do
  liftIO $ toBool <$> sk_pathop_op (ptr path1) (ptr path2) (marshalSKEnum opMode) (ptr dstPath)

{- | Set this path to a set of non-overlapping contours that describe the same
area as the original path. The curve order is reduced where possible so that
cubics may be turned into quadratics, and quadratics maybe turned into lines.

Returns true if operation was able to produce a result; otherwise, result is
unmodified.
-}
simplifyToDest ::
  (MonadIO m) =>
  SKPath ->
  -- | Destination path
  SKPath ->
  m Bool
simplifyToDest path dstPath = evalManaged do
  liftIO $ toBool <$> sk_pathop_simplify (ptr path) (ptr dstPath)

{- | Computes the rectangle of the tight bounds of the path. Returns 'Nothing' if
the bounds could not be computed.
-}
tightBounds :: (MonadIO m) => SKPath -> m (Maybe (Rect Float))
tightBounds path = evalManaged do
  bounds' <- managed alloca
  success <- liftIO $ toBool <$> sk_pathop_tight_bounds (ptr path) bounds'
  if success
    then do
      bounds <- liftIO $ fromSKRect <$> peek bounds'
      pure $ Just bounds
    else do
      pure Nothing

-- TODO: I don't understand what 'toWindingToDest' does.

{- | Set the result with fill type winding to area equivalent to path. Returns
 true if successful. Does not detect if path contains contours which contain
 self-crossings or cross other contours; in these cases, may return true even
 though result does not fill same area as path.

 Returns true if operation was able to produce a result; otherwise, result is
 unmodified. The result may be the input.
-}
toWindingToDest :: (MonadIO m) => SKPath -> SKPath -> m Bool
toWindingToDest path result = evalManaged do
  liftIO $ toBool <$> sk_pathop_as_winding (ptr path) (ptr result)
