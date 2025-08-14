module Skia.SKSamplingOptions where

import Data.Maybe
import Skia.Bindings.Types
import Skia.Internal.Prelude

-- DEVNOTE: These two structs are placed in Google Skia's
-- include/core/SkSamplingOptions.h. I don't know a good place to put these
-- datatypes, so here is 'SKSamplingOptions.hs'.

{-
Specify B and C (each between 0...1) to create a shader that applies the corresponding
cubic reconstruction filter to the image.

Example values:

@
    B = 1/3, C = 1/3   "Mitchell" filter
    B = 0,   C = 1/2   "Catmull-Rom" filter
@

See "Reconstruction Filters in Computer Graphics" (Don P. Mitchell & Arun N. Netravali, 1988)
https://www.cs.utexas.edu/~fussell/courses/cs384g-fall2013/lectures/mitchell/Mitchell.pdf

Desmos worksheet https://www.desmos.com/calculator/aghdpicrvr

Nice overview https://entropymine.com/imageworsener/bicubic/
-}
data SKCubicResampler = SKCubicResampler
  { b :: Float
  , c :: Float
  }
  deriving (Show, Eq, Ord)

data SKSamplingOptions = SKSamplingOptions
  { maxAniso :: Int
  , cubicResampler :: Maybe SKCubicResampler
  , filterMode :: SKFilterMode
  , mipmapMode :: SKMipmapMode
  }
  deriving (Show, Eq, Ord)

-- * Marshal utils

marshalSKCubicResampler :: SKCubicResampler -> Sk_cubic_resampler
marshalSKCubicResampler i =
  Sk_cubic_resampler
    { fB = coerce i.b
    , fC = coerce i.c
    }

marshalSKSamplingOptions :: SKSamplingOptions -> Sk_sampling_options
marshalSKSamplingOptions i =
  Sk_sampling_options
    { fMaxAniso = fromIntegral i.maxAniso
    , fUseCubic = fromBool $ isJust i.cubicResampler
    , fCubic = maybe dummyCubicResampler marshalSKCubicResampler i.cubicResampler
    , fFilter = marshalSKEnum i.filterMode
    , fMipmap = marshalSKEnum i.mipmapMode
    }
 where
  dummyCubicResampler = Sk_cubic_resampler{fB = 0, fC = 0}
