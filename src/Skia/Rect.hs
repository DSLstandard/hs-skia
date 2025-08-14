module Skia.Rect where

import Skia.Bindings.Types
import Skia.Internal.Prelude

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

toSKIRect :: Rect Int -> Sk_irect
toSKIRect (fmap fromIntegral -> Rect{..}) = Sk_irect{..}

fromSKIRect :: Sk_irect -> Rect Int
fromSKIRect Sk_irect{..} = fmap fromIntegral Rect{..}

toSKRect :: Rect Float -> Sk_rect
toSKRect (coerce -> Rect{..}) = Sk_rect{..}

fromSKRect :: Sk_rect -> Rect Float
fromSKRect Sk_rect{..} = coerce Rect{..}
