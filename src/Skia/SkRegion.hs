module Skia.SkRegion where

import Language.C.Inline.Cpp qualified as C
import Linear
import Skia.Internal.Prelude
import Skia.Internal.THUtils
import Skia.SkRect as SkRect

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkRegion.h"

{- | Constructs an empty SkRegion. SkRegion is set to empty bounds at (0, 0)
with zero width and height.
-}
createEmpty :: (MonadResource m) => m (ReleaseKey, SkRegion)
createEmpty =
  allocateSkObjectNeverNull
    [C.exp| SkRegion* { new SkRegion } |]
    (\p -> [C.block| void { delete $(SkRegion* p); } |])

isEmpty :: (MonadIO m) => SkRegion -> m Bool
isEmpty (ptr -> region) = liftIO do
  toBool <$> [C.block| bool {
    return $(SkRegion* region)->isEmpty();
  }|]

isRect :: (MonadIO m) => SkRegion -> m Bool
isRect (ptr -> region) = liftIO do
  toBool <$> [C.block| bool {
    return $(SkRegion* region)->isRect();
  }|]

isComplex :: (MonadIO m) => SkRegion -> m Bool
isComplex (ptr -> region) = liftIO do
  toBool <$> [C.block| bool {
    return $(SkRegion* region)->isComplex();
  }|]

{- | Constructs an empty SkRegion. SkRegion is set to empty bounds at (0, 0)
with zero width and height.
-}
setEmpty :: (MonadIO m) => SkRegion -> m ()
setEmpty (ptr -> region) = liftIO do
  [C.block| void {
    $(SkRegion* region)->setEmpty();
  }|]

{- | Constructs a rectangular SkRegion matching the bounds of rect. If rect is
empty, constructs empty and returns false.
-}
setRect :: (MonadIO m) => SkRegion -> Rect Int -> m Bool
setRect (ptr -> region) rect = evalManaged do
  rect' <- marshalSkIRect rect
  toBool <$> liftIO [C.block| bool {
    return $(SkRegion* region)->setRect(*$(SkIRect* rect'));
  }|]

{- | Constructs SkRegion to match outline of path within clip. Returns false if
constructed SkRegion is empty.

Constructed SkRegion draws the same pixels as path through clip when
anti-aliasing is disabled.
-}
setRegion :: (MonadIO m) => SkRegion -> SkRegion -> m Bool
setRegion (ptr -> region) (ptr -> other) = liftIO do
  toBool <$> [C.block| bool {
    return $(SkRegion* region)->setRegion(*$(SkRegion* other));
  }|]

{- | Constructs SkRegion to match outline of path within clip. Returns false if
constructed SkRegion is empty.

Constructed SkRegion draws the same pixels as path through clip when
anti-aliasing is disabled.
-}
setPath ::
  (MonadIO m) =>
  SkRegion ->
  SkPath ->
  -- | Clip
  SkRegion ->
  m Bool
setPath (ptr -> region) (ptr -> path) (ptr -> clip) = liftIO do
  toBool <$> [C.block| bool {
    return $(SkRegion* region)->setPath(*$(SkPath* path), *$(SkRegion* clip));
  }|]

{- | Gets the bounds of this region.

If the region is empty, returns 'Nothing'.
-}
getBounds :: (MonadIO m) => SkRegion -> m (Maybe (Rect Int))
getBounds (ptr -> region) = evalManaged do
  bounds' <- allocaSkIRect
  liftIO [C.block|void {
    *$(SkIRect* bounds') = $(SkRegion* region)->getBounds();
  }|]

  bounds <- peekSkIRect bounds'
  pure $ if SkRect.isEmpty bounds then Nothing else Just bounds

{- | Returns true if the SkRegion contains the specified point.
-}
containsPoint :: (MonadIO m) => SkRegion -> V2 Int -> m Bool
containsPoint (ptr -> region) (V2 (fromIntegral -> x) (fromIntegral -> y)) = liftIO do
  toBool <$> [C.block| bool {
    return $(SkRegion* region)->contains($(int x), $(int y));
  }|]

{- | Returns true if the SkRegion contains all points in the rect.
-}
containsRect :: (MonadIO m) => SkRegion -> Rect Int -> m Bool
containsRect (ptr -> region) rect = evalManaged do
  rect' <- marshalSkIRect rect
  toBool <$> liftIO [C.block| bool {
    return $(SkRegion* region)->contains(*$(SkIRect* rect'));
  }|]

{- | Returns true if the SkRegion contains all points in the specified region.
-}
containsRegion :: (MonadIO m) => SkRegion -> SkRegion -> m Bool
containsRegion (ptr -> region) (ptr -> other) = liftIO do
  toBool <$> [C.block| bool {
    return $(SkRegion* region)->contains(*$(SkRegion* other));
  }|]

{- | Returns true if SkRegion is a single rectangle and contains r. May return
false even though SkRegion contains r.
-}
quickContainsRect :: (MonadIO m) => SkRegion -> Rect Int -> m Bool
quickContainsRect (ptr -> region) rect = evalManaged do
  rect' <- marshalSkIRect rect
  toBool <$> liftIO [C.block| bool {
    return $(SkRegion* region)->quickContains(*$(SkIRect* rect'));
  }|]

{- | Returns true if SkRegion does not intersect rect. Returns true if rect is
empty or SkRegion is empty. May return false even though SkRegion does not
intersect rect.
-}
quickRejectRect :: (MonadIO m) => SkRegion -> Rect Int -> m Bool
quickRejectRect (ptr -> region) rect = evalManaged do
  rect' <- marshalSkIRect rect
  toBool <$> liftIO [C.block| bool {
    return $(SkRegion* region)->quickReject(*$(SkIRect* rect'));
  }|]

{- | Returns true if SkRegion does not intersect rgn. Returns true if rgn is
empty or SkRegion is empty. May return false even though SkRegion does not
intersect rgn.
-}
quickRejectRegion :: (MonadIO m) => SkRegion -> SkRegion -> m Bool
quickRejectRegion (ptr -> region) (ptr -> other) = liftIO do
  toBool <$> [C.block| bool {
    return $(SkRegion* region)->quickReject(*$(SkRegion* other));
  }|]

-- | Translates the SkRegion by (dx, dy).
translate :: (MonadIO m) => SkRegion -> V2 Int -> m ()
translate (ptr -> region) (V2 (fromIntegral -> dx) (fromIntegral -> dy)) = liftIO do
  [C.block| void {
    $(SkRegion* region)->translate($(int dx), $(int dy));
  }|]

$( qGenerateSkEnum
  "Op"
  "The logical operations that can be performed when combining two SkRegion."
  [ ("Difference", "SkRegion::Op::kDifference_Op", "target minus operand")
  , ("Intersect", "SkRegion::Op::kIntersect_Op", "target intersected with operand")
  , ("Union", "SkRegion::Op::kUnion_Op", "target unioned with operand")
  , ("Xor", "SkRegion::Op::kXOR_Op", "target exclusive or with operand")
  , ("ReverseDifference", "SkRegion::Op::kReverseDifference_Op", "operand minus target")
  , ("Replace", "SkRegion::Op::kReplace_Op", "replace target with operand")
  ]
 )

{- | Set this region to the result of applying the operation to this region and
the specified rectangle.

Returns true if the resulting region is non-empty.
-}
opRect :: (MonadIO m) => SkRegion -> Rect Int -> Op -> m Bool
opRect (ptr -> region) rect (marshalSkEnum -> operation) = evalManaged do
  rect' <- marshalSkIRect rect
  toBool <$> liftIO [C.block| bool {
    return $(SkRegion* region)->op(*$(SkIRect* rect'), (SkRegion::Op) $(int operation));
  }|]

{- | Set this region to the result of applying the operation to this region and
the specified region.

Returns true if the resulting region is non-empty.
-}
opRegion :: (MonadIO m) => SkRegion -> SkRegion -> Op -> m Bool
opRegion (ptr -> region) (ptr -> other) (marshalSkEnum -> operation) = liftIO do
  toBool <$> [C.block| bool {
    return $(SkRegion* region)->op(*$(SkRegion* other), (SkRegion::Op) $(int operation));
  }|]

{- | Returns true if SkRegion intersects rect. Returns false if either rect or
SkRegion is empty, or do not intersect.
-}
intersectsRect :: (MonadIO m) => SkRegion -> Rect Int -> m Bool
intersectsRect (ptr -> region) rect = evalManaged do
  rect' <- marshalSkIRect rect
  toBool <$> liftIO [C.block| bool {
    return $(SkRegion* region)->intersects(*$(SkIRect* rect'));
  }|]

{- | Returns true if SkRegion intersects other.
Returns false if either other or SkRegion is empty, or do not intersect.
-}
intersectsRegion :: (MonadIO m) => SkRegion -> SkRegion -> m Bool
intersectsRegion (ptr -> region) (ptr -> other) = liftIO do
  toBool <$> [C.block| bool {
    return $(SkRegion* region)->intersects(*$(SkRegion* other));
  }|]

{- | Appends outline of SkRegion to path. Returns true if SkRegion is not empty;
otherwise, returns false, and leaves path unmodified.
-}
getBoundaryPathToDest ::
  (MonadIO m) =>
  SkRegion ->
  -- | Destination path
  SkPath ->
  m Bool
getBoundaryPathToDest (ptr -> region) (ptr -> dstpath) = liftIO do
  toBool <$> [C.block| bool {
    return $(SkRegion* region)->getBoundaryPath($(SkPath* dstpath));
  }|]