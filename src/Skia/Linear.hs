module Skia.Linear where

import Skia.Internal.Prelude
import Language.C.Inline.Cpp qualified as C

C.context $ C.cppCtx <> cppSkiaObjectTypes

C.include "core/SkMatrix.h"
C.include "core/SkM44.h"

marshalSkMatrix :: M33 Float -> Managed (Ptr C'SkMatrix)
marshalSkMatrix ( coerce ->
  V3
    (V3 scaleX skewX transX)
    (V3 skewY scaleY transY)
    (V3 persp0 persp1 persp2)
  ) = managed $ bracket
    [C.block| SkMatrix* {
      auto mat = new SkMatrix;
      mat->setAll(
        $(float scaleX), $(float skewX), $(float transX),
        $(float skewY), $(float scaleY), $(float transY),
        $(float persp0), $(float persp1), $(float persp2)
      );
      return mat;
    }|]
    ( \p -> do
      [C.block|void {
        delete $(SkMatrix* p);
      }|]
    )

allocaSkM44 :: Managed (Ptr C'SkM44)
allocaSkM44 = managed $ bracket
  [C.block| SkM44* {
    return new SkM44();
  }|]
  ( \p -> do
    [C.block|void {
      delete $(SkM44* p);
    }|]
  )

marshalSkM44 :: M44 Float -> Managed (Ptr C'SkM44)
marshalSkM44 ( coerce ->
  V4
    (V4 m0 m1 m2 m3)
    (V4 m4 m5 m6 m7)
    (V4 m8 m9 m10 m11)
    (V4 m12 m13 m14 m15)
  ) = managed $ bracket
    [C.block| SkM44* {
      return new SkM44(
        $(float m0), $(float m4), $(float m8),  $(float m12),
        $(float m1), $(float m5), $(float m9),  $(float m13),
        $(float m2), $(float m6), $(float m10), $(float m14),
        $(float m3), $(float m7), $(float m11), $(float m15)
      );
    }|]
    ( \p -> do
      [C.block|void {
        delete $(SkM44* p);
      }|]
    )

peekSkM44 :: MonadIO m => Ptr C'SkM44 -> m (M44 Float)
peekSkM44 p = evalManaged do
  vec <- managed $ allocaArray @CFloat 16
  -- NOTE: SkM44's fMat is private and it is really annoying
  liftIO [C.block| void {
    SkM44* p = $(SkM44* p);
    float* vec = $(float* vec);
    vec[0] = p->rc(0, 0);
    vec[1] = p->rc(0, 1);
    vec[2] = p->rc(0, 2);
    vec[3] = p->rc(0, 3);
    vec[4] = p->rc(1, 0);
    vec[5] = p->rc(1, 1);
    vec[6] = p->rc(1, 2);
    vec[7] = p->rc(1, 3);
    vec[8] = p->rc(2, 0);
    vec[9] = p->rc(2, 1);
    vec[10] = p->rc(2, 2);
    vec[11] = p->rc(2, 3);
    vec[12] = p->rc(3, 0);
    vec[13] = p->rc(3, 1);
    vec[14] = p->rc(3, 2);
    vec[15] = p->rc(3, 3);
  }|]

  [m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15] <- liftIO $ peekArray 16 vec
  pure $ coerce $ V4
    (V4 m0 m1 m2 m3)
    (V4 m4 m5 m6 m7)
    (V4 m8 m9 m10 m11)
    (V4 m12 m13 m14 m15)

allocaSkPoint :: Managed (Ptr C'SkPoint)
allocaSkPoint = managed $ bracket
  [C.block| SkPoint* {
    return new SkPoint();
  }|]
  ( \p -> do
    [C.block|void {
      delete $(SkPoint* p);
    }|]
  )

marshalSkPoint :: V2 Float -> Managed (Ptr C'SkPoint)
marshalSkPoint (coerce -> V2 x y) =
  managed $ bracket
    [C.exp| SkPoint* {
      new SkPoint { .fX = $(float x), .fY = $(float y) }
    }|]
    ( \p -> [C.block|void { delete $(SkPoint* p); }|])

peekSkPoint :: MonadIO m => Ptr C'SkPoint -> m (V2 Float)
peekSkPoint p = liftIO do
  x <- [C.exp| float { $(SkPoint* p)->fX }|]
  y <- [C.exp| float { $(SkPoint* p)->fY }|]
  pure $ coerce $ V2 x y

allocaSkSize :: Managed (Ptr C'SkSize)
allocaSkSize = managed $ bracket
  [C.block| SkSize* {
    return new SkSize();
  }|]
  ( \p -> do
    [C.block|void {
      delete $(SkSize* p);
    }|]
  )

marshalSkSize :: V2 Float -> Managed (Ptr C'SkSize)
marshalSkSize (coerce -> V2 w h) =
  managed $ bracket
    [C.exp| SkSize* {
      new SkSize { .fWidth = $(float w), .fHeight = $(float h) }
    }|]
    ( \p -> [C.block|void { delete $(SkSize* p); }|])

peekSkSize :: MonadIO m => Ptr C'SkSize -> m (V2 Float)
peekSkSize p = liftIO do
  x <- [C.exp| float { $(SkSize* p)->fWidth }|]
  y <- [C.exp| float { $(SkSize* p)->fHeight }|]
  pure $ coerce $ V2 x y
