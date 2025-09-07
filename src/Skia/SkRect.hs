module Skia.SkRect where

import Language.C.Inline.Cpp qualified as C
import Skia.Internal.Prelude
import Data.Int

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkRect.h"

data Rect a = Rect
  { left :: a
  , top :: a
  , right :: a
  , bottom :: a
  }
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

makeWH :: (Num a) => V2 a -> Rect a
makeWH (V2 w h) = Rect{left = 0, top = 0, right = w, bottom = h}

-- | Returns the width of the 'Rect'.
width :: (Num a) => Rect a -> a
width Rect{..} = right - left

-- | Returns the height of the 'Rect'.
height :: (Num a) => Rect a -> a
height Rect{..} = bottom - top

-- | Returns the size (width, height) of the 'Rect'.
size :: (Num a) => Rect a -> V2 a
size rect = V2 (ThisModule.width rect) (ThisModule.height rect)

-- NOTE: It looks strange, but this definition is from SkiaSharp C#.

{- | Returns a 'Rect' that is considered \"empty\".

This is defined as @Rect 0 0 0 0@ in Google Skia.
-}
empty :: (Num a) => Rect a
empty = Rect 0 0 0 0

-- NOTE: It looks strange, but this definition is from SkiaSharp C#.

{- | Returns true if the input 'Rect' is considered \"empty\".

'empty' is the only 'Rect' that returns true here.
-}
isEmpty :: (Num a, Eq a) => Rect a -> Bool
isEmpty rect = rect == empty

-- * Marshal utils

allocaSkRect :: Managed (Ptr C'SkRect)
allocaSkRect =
  managed $ bracket
    [C.block| SkRect* {
      return new SkRect;
    }|]
    ( \p -> do
      [C.block|void {
        delete $(SkRect* p);
      }|]
    )

marshalSkRect :: Rect Float -> Managed (Ptr C'SkRect)
marshalSkRect (coerce -> Rect{left, top, right, bottom}) = do
  crect <- allocaSkRect
  liftIO [C.block| void {
    $(SkRect* crect)->fLeft = $(float left);
    $(SkRect* crect)->fTop = $(float top);
    $(SkRect* crect)->fRight = $(float right);
    $(SkRect* crect)->fBottom = $(float bottom);
  }|]
  pure crect

peekSkRect :: MonadIO m => Ptr C'SkRect -> m (Rect Float)
peekSkRect crect = liftIO do
  left <- [C.exp| float { $(SkRect* crect)->fLeft } |]
  top <- [C.exp| float { $(SkRect* crect)->fTop } |]
  right <- [C.exp| float { $(SkRect* crect)->fRight } |]
  bottom <- [C.exp| float { $(SkRect* crect)->fBottom } |]
  pure $ coerce Rect{left, top, right, bottom}

allocaSkIRect :: Managed (Ptr C'SkIRect)
allocaSkIRect =
  managed $ bracket
    [C.block| SkIRect* {
      return new SkIRect;
    }|]
    ( \p -> do
      [C.block|void {
        delete $(SkIRect* p);
      }|]
    )

marshalSkIRect :: Rect Int -> Managed (Ptr C'SkIRect)
marshalSkIRect (fmap (fromIntegral @Int @Int32) -> Rect{left, top, right, bottom}) = do
  crect <- allocaSkIRect
  liftIO [C.block| void {
    $(SkIRect* crect)->fLeft = $(int32_t left);
    $(SkIRect* crect)->fTop = $(int32_t top);
    $(SkIRect* crect)->fRight = $(int32_t right);
    $(SkIRect* crect)->fBottom = $(int32_t bottom);
  }|]
  pure crect

peekSkIRect :: MonadIO m => Ptr C'SkIRect -> m (Rect Int)
peekSkIRect crect = liftIO do
  left <- [C.exp| int32_t { $(SkIRect* crect)->fLeft } |]
  top <- [C.exp| int32_t { $(SkIRect* crect)->fTop } |]
  right <- [C.exp| int32_t { $(SkIRect* crect)->fRight } |]
  bottom <- [C.exp| int32_t { $(SkIRect* crect)->fBottom } |]
  pure $ fmap (fromIntegral @Int32 @Int) Rect{left, top, right, bottom}