module Skia.SKRegion where

import Linear
import Skia.Bindings.Sk_region
import Skia.Bindings.Types
import Skia.Internal.Prelude
import Skia.Rect as Rect

{- | Constructs an empty SkRegion. SkRegion is set to empty bounds at (0, 0)
with zero width and height.
-}
createEmpty :: (MonadResource m) => m (ReleaseKey, SKRegion)
createEmpty =
  allocateSKObjectNeverNull
    sk_region_new
    sk_region_delete

isEmpty :: (MonadIO m) => SKRegion -> m Bool
isEmpty region = evalManaged do
  r <- liftIO $ sk_region_is_empty (ptr region)
  pure $ toBool r

isRect :: (MonadIO m) => SKRegion -> m Bool
isRect region = evalManaged do
  r <- liftIO $ sk_region_is_rect (ptr region)
  pure $ toBool r

isComplex :: (MonadIO m) => SKRegion -> m Bool
isComplex region = evalManaged do
  r <- liftIO $ sk_region_is_complex (ptr region)
  pure $ toBool r

{- | Constructs an empty SkRegion. SkRegion is set to empty bounds at (0, 0)
with zero width and height.
-}
setEmpty :: (MonadIO m) => SKRegion -> m ()
setEmpty region = evalManaged do
  -- Google's commnet says:
  --
  -- Constructs an empty SkRegion. SkRegion is set to empty bounds at (0, 0)
  -- with zero width and height. ***Always returns false.***
  void $ liftIO $ sk_region_set_empty (ptr region)

{- | Constructs a rectangular SkRegion matching the bounds of rect. If rect is
empty, constructs empty and returns false.
-}
setRect :: (MonadIO m) => SKRegion -> Rect Int -> m Bool
setRect region rect = evalManaged do
  rect' <- storable $ toSKIRect rect

  r <- liftIO $ sk_region_set_rect (ptr region) rect'
  pure $ toBool r

{- | Constructs SkRegion as the union of SkIRect in rects array. If count is
zero, constructs empty SkRegion. Returns false if constructed SkRegion is empty.

May be faster than repeated calls to op().
-}
setRectsRaw ::
  (MonadIO m) =>
  SKRegion ->
  -- | Pointer to an array of rectangles.
  Ptr Sk_irect ->
  -- | Number of rectangles.
  Int ->
  m Bool
setRectsRaw region rects' count = evalManaged do
  r <- liftIO $ sk_region_set_rects (ptr region) rects' (fromIntegral count)
  pure $ toBool r

{- | Constructs SkRegion to match outline of path within clip. Returns false if
constructed SkRegion is empty.

Constructed SkRegion draws the same pixels as path through clip when
anti-aliasing is disabled.
-}
setRegion :: (MonadIO m) => SKRegion -> SKRegion -> m Bool
setRegion region other = evalManaged do
  r <- liftIO $ sk_region_set_region (ptr region) (ptr other)
  pure $ toBool r

{- | Constructs SkRegion to match outline of path within clip. Returns false if
constructed SkRegion is empty.

Constructed SkRegion draws the same pixels as path through clip when
anti-aliasing is disabled.
-}
setPath ::
  (MonadIO m) =>
  SKRegion ->
  SKPath ->
  -- | Clip
  SKRegion ->
  m Bool
setPath region path clip = evalManaged do
  r <- liftIO $ sk_region_set_path (ptr region) (ptr path) (ptr clip)
  pure $ toBool r

{- | Gets the bounds of this region.

If the region is empty, returns an empty rectangle.
-}
getBounds :: (MonadIO m) => SKRegion -> m (Maybe (Rect Int))
getBounds region = evalManaged do
  bounds' <- managed alloca
  liftIO $ sk_region_get_bounds (ptr region) bounds'

  bounds <- liftIO $ fromSKIRect <$> peek bounds'
  pure $ if Rect.isEmpty bounds then Nothing else Just bounds

containsPoint :: (MonadIO m) => SKRegion -> V2 Int -> m Bool
containsPoint region (V2 x y) = evalManaged do
  r <- liftIO $ sk_region_contains_point (ptr region) (fromIntegral x) (fromIntegral y)
  pure $ toBool r

containsRect :: (MonadIO m) => SKRegion -> Rect Int -> m Bool
containsRect region rect = evalManaged do
  rect' <- storable $ toSKIRect rect
  r <- liftIO $ sk_region_contains_rect (ptr region) rect'
  pure $ toBool r

containsRegion :: (MonadIO m) => SKRegion -> SKRegion -> m Bool
containsRegion region other = evalManaged do
  r <- liftIO $ sk_region_contains (ptr region) (ptr other)
  pure $ toBool r

{- | Returns true if SkRegion is a single rectangle and contains r. May return
false even though SkRegion contains r.
-}
quickContainsRect :: (MonadIO m) => SKRegion -> Rect Int -> m Bool
quickContainsRect region rect = evalManaged do
  rect' <- storable $ toSKIRect rect
  r <- liftIO $ sk_region_quick_contains (ptr region) rect'
  pure $ toBool r

{- | Returns true if SkRegion does not intersect rect. Returns true if rect is
empty or SkRegion is empty. May return false even though SkRegion does not
intersect rect.
-}
quickRejectRect :: (MonadIO m) => SKRegion -> Rect Int -> m Bool
quickRejectRect region rect = evalManaged do
  rect' <- storable $ toSKIRect rect
  r <- liftIO $ sk_region_quick_reject_rect (ptr region) rect'
  pure $ toBool r

{- | Returns true if SkRegion does not intersect rgn. Returns true if rgn is
empty or SkRegion is empty. May return false even though SkRegion does not
intersect rgn.
-}
quickRejectRegion :: (MonadIO m) => SKRegion -> SKRegion -> m Bool
quickRejectRegion region other = evalManaged do
  r <- liftIO $ sk_region_quick_reject (ptr region) (ptr other)
  pure $ toBool r

translate :: (MonadIO m) => SKRegion -> V2 Int -> m ()
translate region (V2 dx dy) = evalManaged do
  liftIO $ sk_region_translate (ptr region) (fromIntegral dx) (fromIntegral dy)

{- | Set this region to the result of applying the operation to this region and
the specified rectangle.

Returns true if the resulting region is non-empty.
-}
opRect :: (MonadIO m) => SKRegion -> Rect Int -> SKRegionOp -> m Bool
opRect region rect operation = evalManaged do
  rect' <- storable $ toSKIRect rect

  r <- liftIO $ sk_region_op_rect (ptr region) rect' (marshalSKEnum operation)
  pure $ toBool r

{- | Set this region to the result of applying the operation to this region and
the specified region.

Returns true if the resulting region is non-empty.
-}
opRegion :: (MonadIO m) => SKRegion -> SKRegion -> SKRegionOp -> m Bool
opRegion region other operation = evalManaged do
  r <- liftIO $ sk_region_op (ptr region) (ptr other) (marshalSKEnum operation)
  pure $ toBool r

{- | Returns true if SkRegion intersects rect. Returns false if either rect or
SkRegion is empty, or do not intersect.
-}
intersectsRect :: (MonadIO m) => SKRegion -> Rect Int -> m Bool
intersectsRect region rect = evalManaged do
  rect' <- storable $ toSKIRect rect
  r <- liftIO $ sk_region_intersects_rect (ptr region) rect'
  pure $ toBool r

{- | Returns true if SkRegion intersects other.
Returns false if either other or SkRegion is empty, or do not intersect.
-}
intersectsRegion :: (MonadIO m) => SKRegion -> SKRegion -> m Bool
intersectsRegion region other = evalManaged do
  r <- liftIO $ sk_region_intersects (ptr region) (ptr other)
  pure $ toBool r

{- | Appends outline of SkRegion to path. Returns true if SkRegion is not empty;
otherwise, returns false, and leaves path unmodified.
-}
getBoundaryPathToDest ::
  (MonadIO m) =>
  SKRegion ->
  -- | Destination path
  SKPath ->
  m Bool
getBoundaryPathToDest region path = evalManaged do
  r <- liftIO $ sk_region_get_boundary_path (ptr region) (ptr path)
  pure $ toBool r
