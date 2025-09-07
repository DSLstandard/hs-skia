{- | You may want to these links to learn more about Lottie and Skottie
animations:

* <https://lottiefiles.com/what-is-lottie>

* <https://skia.org/docs/user/modules/skottie/>
-}
module Skia.Skottie.Animation where

import Data.Text qualified as T
import Language.C.Inline.Cpp qualified as C
import Linear
import Skia.Objects
import Skia.SkRect
import Skia.SkString qualified as SkString
import Skia.Skottie.Internal.Prelude
import Skia.SkSG.Objects as SkSG

C.context $ mconcat
  [ C.cppCtx
  , cppSkiaObjectTypes
  , cppSkottieObjectTypes
  , cppSkSGObjectTypes
  ]

C.include "modules/skottie/include/Skottie.h"

-- | Returns the animation frame rate (frames / second).
getFPS :: (MonadIO m) => Animation -> m Double
getFPS (ptr -> anim) = liftIO $ coerce <$>
  [C.block| double {
    return $(skottie::Animation* anim)->fps();
  }|]

-- | Returns the dimensions of the animation.
getSize :: (MonadIO m) => Animation -> m (V2 Float)
getSize (ptr -> anim) = liftIO do
  fWidth <- [C.exp| int32_t { $(skottie::Animation* anim)->size().fWidth } |]
  fHeight <- [C.exp| int32_t { $(skottie::Animation* anim)->size().fHeight } |]
  pure $ V2 (fromIntegral fWidth) (fromIntegral fHeight)

-- | Returns the animation duration in seconds.
getDuration :: (MonadIO m) => Animation -> m Double
getDuration (ptr -> anim) = liftIO $ coerce <$>
  [C.exp| double { $(skottie::Animation* anim)->duration() }|]

-- | Animation in point, in frame index units.
getInPoint :: (MonadIO m) => Animation -> m Double
getInPoint (ptr -> anim) = liftIO $ coerce <$>
  [C.exp| double { $(skottie::Animation* anim)->inPoint() }|]

-- | Animation out point, in frame index units.
getOutPoint :: (MonadIO m) => Animation -> m Double
getOutPoint (ptr -> anim) = liftIO $ coerce <$>
  [C.exp| double { $(skottie::Animation* anim)->outPoint() }|]

-- | Returns the version of the Lottie JSON spec of the animation
getVersion :: (MonadIO m) => Animation -> m T.Text
getVersion (ptr -> anim) = liftIO $ runResourceT do
  (_, version@(ptr -> version')) <- SkString.createEmpty
  liftIO do
    [C.block| void {
      *$(SkString* version') = $(skottie::Animation* anim)->version();
    }|]
  SkString.getAsText version

data RenderFlags = RenderFlags
  { skipTopLevelIsolation :: Bool
  -- ^ When rendering into a known transparent buffer, clients can pass
  -- this flag to avoid some unnecessary compositing overhead for
  -- animations using layer blend modes.

  , disableTopLevelClipping :: Bool
  -- ^ By default, content is clipped to the intrinsic animation
  -- bounds (as determined by its size).  If this flag is set,
  -- then the animation can draw outside of the bounds.
  }
  deriving (Show, Eq, Ord)

-- | Default render flags with all options disabled.
defaultRenderFlags :: RenderFlags
defaultRenderFlags =
  RenderFlags
    { skipTopLevelIsolation = False
    , disableTopLevelClipping = False
    }

{- | Draws the current animation frame.

It is undefined behavior to call 'render'' on a newly created 'Animation'
before specifying an initial frame via 'seek' or its variants.
-}
render ::
  (MonadIO m, IsSkCanvas canvas) =>
  Animation ->
  -- | Destination canvas
  canvas ->
  -- | Optional draw destination rect
  Maybe (Rect Float) ->
  -- | Render flags. Consider using 'defaultRenderFlags'.
  RenderFlags ->
  m ()
render (ptr -> anim) (ptr . toA SkCanvas -> dstCanvas) dstRect flags = evalManaged do
  dstRect' <- maybe (pure nullPtr) marshalSkRect dstRect
  let
    renderFlags' = makeBitFlags
      [ (flags.skipTopLevelIsolation, [C.pure|uint32_t { skottie::Animation::RenderFlag::kSkipTopLevelIsolation }|])
      , (flags.disableTopLevelClipping, [C.pure|uint32_t { skottie::Animation::RenderFlag::kDisableTopLevelClipping }|])
      ]
  liftIO do
    [C.block| void {
      $(skottie::Animation* anim)->render(
        $(SkCanvas* dstCanvas),
        $(SkRect* dstRect'),
        $(uint32_t renderFlags')
      );
    }|]

{-# DEPRECATED seek "Google Skia recommends \"us[ing] one of the other versions\" of 'seek'." #-}

-- | Updates the animation state for @t@.
seek ::
  (MonadIO m) =>
  Animation ->
  -- | @t@: normalized [0..1] frame selector (0 -> first frame, 1 -> final frame)
  Float ->
  -- | Optional invalidation controller (dirty region tracking)
  Maybe SkSG.InvalidationController ->
  m ()
seek (ptr -> anim) (coerce -> t) (maybe nullPtr ptr -> ic) = liftIO do
  [C.block| void {
    $(skottie::Animation* anim)->seek(
      $(float t),
      $(sksg::InvalidationController* ic)
    );
  }|]

{- |
Update the animation state to match @t@, specified as a frame index i.e.
relative to @'getDuration' * 'getFPS'@.

Fractional values are allowed and meaningful - e.g.

  * 0.0 -> first frame

  * 1.0 -> second frame

  * 0.5 -> halfway between first and second frame
-}
seekFrame ::
  (MonadIO m) =>
  Animation ->
  -- | Frame index. Should be between [0..'getDuration' * 'getFPS')
  Float ->
  -- | Optional invalidation controller (dirty region tracking)
  Maybe SkSG.InvalidationController ->
  m ()
seekFrame (ptr -> anim) (coerce -> frame) (maybe nullPtr ptr -> ic) = liftIO do
  [C.block| void {
    $(skottie::Animation* anim)->seekFrame(
      $(float frame),
      $(sksg::InvalidationController* ic)
    );
  }|]

{- | Update the animation state to match @t@, specified in frame time i.e.
relative to 'getDuration'.
-}
seekFrameTime ::
  (MonadIO m) =>
  Animation ->
  -- | Frame time. Should be between [0..'getDuration' * 'getFPS')
  Float ->
  -- | Optional invalidation controller (dirty region tracking)
  Maybe SkSG.InvalidationController ->
  m ()
seekFrameTime (ptr -> anim) (coerce -> t) (maybe nullPtr ptr -> ic) = liftIO do
  [C.block| void {
    $(skottie::Animation* anim)->seekFrameTime(
      $(float t),
      $(sksg::InvalidationController* ic)
    );
  }|]
