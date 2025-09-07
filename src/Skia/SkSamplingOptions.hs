module Skia.SkSamplingOptions where

import Skia.Internal.Prelude
import Skia.Internal.THUtils
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkSamplingOptions.h"

$( qGenerateSkEnum
  "SKFilterMode"
  ""
  [ ("Nearest", "SkFilterMode::kNearest", "")
  , ("Linear", "SkFilterMode::kLinear", "")
  ]
 )

$( qGenerateSkEnum
  "SKMipmapMode"
  ""
  [ ("None", "SkMipmapMode::kNone", "")
  , ("Nearest", "SkMipmapMode::kNearest", "")
  , ("Linear", "SkMipmapMode::kLinear", "")
  ]
 )

-- DEVNOTE: These two structs are placed in Google Skia's
-- include/core/SkSamplingOptions.h. I don't know a good place to put these
-- datatypes, so here is 'SkSamplingOptions.hs'.

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
data CubicResampler = CubicResampler
  { b :: Float
  , c :: Float
  }
  deriving (Show, Eq, Ord)

data SamplingOptions = SamplingOptions
  { maxAniso :: Int
  , cubicResampler :: Maybe CubicResampler
  , filterMode :: SKFilterMode
  , mipmapMode :: SKMipmapMode
  }
  deriving (Show, Eq, Ord)

-- * Marshal utils

marshalSkSamplingOptions :: SamplingOptions -> Managed (Ptr C'SkSamplingOptions)
marshalSkSamplingOptions i = do
  let maxAniso = fromIntegral i.maxAniso
  let filterMode = marshalSkEnum i.filterMode
  let mipmapMode = marshalSkEnum i.mipmapMode

  coptions <- managed $ bracket
    [C.block|SkSamplingOptions* {
      return new SkSamplingOptions;
    }|]
    ( \p -> do
      [C.block|void {
        delete $(SkSamplingOptions* p);
      }|]
    )

  -- NOTE: SkSamplingOptions' fields are all constant.

  liftIO [C.block|void {
    *((int*) &($(SkSamplingOptions* coptions)->maxAniso)) = $(int maxAniso);
    *((SkFilterMode*) &($(SkSamplingOptions* coptions)->filter)) = (SkFilterMode) $(int filterMode);
    *((SkMipmapMode*) &($(SkSamplingOptions* coptions)->mipmap)) = (SkMipmapMode) $(int mipmapMode);
  }|]

  whenJust i.cubicResampler \cubicResampler -> do
    let fb :: CFloat = coerce cubicResampler.b
    let fc :: CFloat = coerce cubicResampler.c
    liftIO [C.block|void {
      *((bool*) &$(SkSamplingOptions* coptions)->useCubic) = true;
      *((SkCubicResampler*) &$(SkSamplingOptions* coptions)->cubic) = { $(float fb), $(float fc) };
    }|]

  pure coptions