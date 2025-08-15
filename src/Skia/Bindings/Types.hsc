{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | This is an auto-generated FFI binding module.
module Skia.Bindings.Types where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

#include "skia_capi/sk_types.h"

{- | Opaque C struct: @"sk_refcnt_t"@
-}
data Sk_refcnt = Sk_refcnt

{- | Opaque C struct: @"sk_nvrefcnt_t"@
-}
data Sk_nvrefcnt = Sk_nvrefcnt

{- | Opaque C struct: @"sk_flattenable_t"@
-}
data Sk_flattenable = Sk_flattenable

{- | C type alias: @sk_color_t@

@
typedef uint32_t sk_color_t
@
-}
type Sk_color = Word32

{- | C type alias: @sk_pmcolor_t@

@
typedef uint32_t sk_pmcolor_t
@
-}
type Sk_pmcolor = Word32

{- | C type alias: @sk_scalar_t@

@
typedef float sk_scalar_t
@
-}
type Sk_scalar = CFloat

{- | C struct: @"sk_color4f_t"@

@
typedef struct sk_color4f_t
{
  float fR;
  float fG;
  float fB;
  float fA;
} sk_color4f_t
@
-}
data Sk_color4f = Sk_color4f
  { fR :: CFloat -- ^ C field: @"float fR"@
  , fG :: CFloat -- ^ C field: @"float fG"@
  , fB :: CFloat -- ^ C field: @"float fB"@
  , fA :: CFloat -- ^ C field: @"float fA"@
  }
instance Foreign.Storable.Offset.Offset "fR" Sk_color4f where
  rawOffset = (#offset sk_color4f_t, fR)
instance Foreign.Storable.Offset.Offset "fG" Sk_color4f where
  rawOffset = (#offset sk_color4f_t, fG)
instance Foreign.Storable.Offset.Offset "fB" Sk_color4f where
  rawOffset = (#offset sk_color4f_t, fB)
instance Foreign.Storable.Offset.Offset "fA" Sk_color4f where
  rawOffset = (#offset sk_color4f_t, fA)
instance Foreign.Storable.Storable Sk_color4f where
  sizeOf _ = (#size sk_color4f_t)
  alignment _ = (#alignment sk_color4f_t)
  peek p' = do
    fR <- (#peek sk_color4f_t, fR) p'
    fG <- (#peek sk_color4f_t, fG) p'
    fB <- (#peek sk_color4f_t, fB) p'
    fA <- (#peek sk_color4f_t, fA) p'
    pure Sk_color4f{..}
  poke p' Sk_color4f{..} = do
    (#poke sk_color4f_t, fR) p' fR
    (#poke sk_color4f_t, fG) p' fG
    (#poke sk_color4f_t, fB) p' fB
    (#poke sk_color4f_t, fA) p' fA

{- | C enum: @"sk_colortype_t"@

@
typedef enum 
{
  UNKNOWN_SK_COLORTYPE = 0,
  ALPHA_8_SK_COLORTYPE,
  RGB_565_SK_COLORTYPE,
  ARGB_4444_SK_COLORTYPE,
  RGBA_8888_SK_COLORTYPE,
  RGB_888X_SK_COLORTYPE,
  BGRA_8888_SK_COLORTYPE,
  RGBA_1010102_SK_COLORTYPE,
  BGRA_1010102_SK_COLORTYPE,
  RGB_101010X_SK_COLORTYPE,
  BGR_101010X_SK_COLORTYPE,
  BGR_101010X_XR_SK_COLORTYPE,
  BGRA_10101010_XR_SK_COLORTYPE,
  RGBA_10X6_SK_COLORTYPE,
  GRAY_8_SK_COLORTYPE,
  RGBA_F16_NORM_SK_COLORTYPE,
  RGBA_F16_SK_COLORTYPE,
  RGB_F16F16F16X_SK_COLORTYPE,
  RGBA_F32_SK_COLORTYPE,
  R8G8_UNORM_SK_COLORTYPE,
  A16_FLOAT_SK_COLORTYPE,
  R16G16_FLOAT_SK_COLORTYPE,
  A16_UNORM_SK_COLORTYPE,
  R16G16_UNORM_SK_COLORTYPE,
  R16G16B16A16_UNORM_SK_COLORTYPE,
  SRGBA_8888_SK_COLORTYPE,
  R8_UNORM_SK_COLORTYPE
} sk_colortype_t
@

-}
newtype Sk_colortype = Sk_colortype (#type sk_colortype_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_colortype_t"@ value (1/27): @"UNKNOWN_SK_COLORTYPE"@
pattern UNKNOWN_SK_COLORTYPE :: Sk_colortype
pattern UNKNOWN_SK_COLORTYPE = (#const UNKNOWN_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (2/27): @"ALPHA_8_SK_COLORTYPE"@
pattern ALPHA_8_SK_COLORTYPE :: Sk_colortype
pattern ALPHA_8_SK_COLORTYPE = (#const ALPHA_8_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (3/27): @"RGB_565_SK_COLORTYPE"@
pattern RGB_565_SK_COLORTYPE :: Sk_colortype
pattern RGB_565_SK_COLORTYPE = (#const RGB_565_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (4/27): @"ARGB_4444_SK_COLORTYPE"@
pattern ARGB_4444_SK_COLORTYPE :: Sk_colortype
pattern ARGB_4444_SK_COLORTYPE = (#const ARGB_4444_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (5/27): @"RGBA_8888_SK_COLORTYPE"@
pattern RGBA_8888_SK_COLORTYPE :: Sk_colortype
pattern RGBA_8888_SK_COLORTYPE = (#const RGBA_8888_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (6/27): @"RGB_888X_SK_COLORTYPE"@
pattern RGB_888X_SK_COLORTYPE :: Sk_colortype
pattern RGB_888X_SK_COLORTYPE = (#const RGB_888X_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (7/27): @"BGRA_8888_SK_COLORTYPE"@
pattern BGRA_8888_SK_COLORTYPE :: Sk_colortype
pattern BGRA_8888_SK_COLORTYPE = (#const BGRA_8888_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (8/27): @"RGBA_1010102_SK_COLORTYPE"@
pattern RGBA_1010102_SK_COLORTYPE :: Sk_colortype
pattern RGBA_1010102_SK_COLORTYPE = (#const RGBA_1010102_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (9/27): @"BGRA_1010102_SK_COLORTYPE"@
pattern BGRA_1010102_SK_COLORTYPE :: Sk_colortype
pattern BGRA_1010102_SK_COLORTYPE = (#const BGRA_1010102_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (10/27): @"RGB_101010X_SK_COLORTYPE"@
pattern RGB_101010X_SK_COLORTYPE :: Sk_colortype
pattern RGB_101010X_SK_COLORTYPE = (#const RGB_101010X_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (11/27): @"BGR_101010X_SK_COLORTYPE"@
pattern BGR_101010X_SK_COLORTYPE :: Sk_colortype
pattern BGR_101010X_SK_COLORTYPE = (#const BGR_101010X_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (12/27): @"BGR_101010X_XR_SK_COLORTYPE"@
pattern BGR_101010X_XR_SK_COLORTYPE :: Sk_colortype
pattern BGR_101010X_XR_SK_COLORTYPE = (#const BGR_101010X_XR_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (13/27): @"BGRA_10101010_XR_SK_COLORTYPE"@
pattern BGRA_10101010_XR_SK_COLORTYPE :: Sk_colortype
pattern BGRA_10101010_XR_SK_COLORTYPE = (#const BGRA_10101010_XR_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (14/27): @"RGBA_10X6_SK_COLORTYPE"@
pattern RGBA_10X6_SK_COLORTYPE :: Sk_colortype
pattern RGBA_10X6_SK_COLORTYPE = (#const RGBA_10X6_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (15/27): @"GRAY_8_SK_COLORTYPE"@
pattern GRAY_8_SK_COLORTYPE :: Sk_colortype
pattern GRAY_8_SK_COLORTYPE = (#const GRAY_8_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (16/27): @"RGBA_F16_NORM_SK_COLORTYPE"@
pattern RGBA_F16_NORM_SK_COLORTYPE :: Sk_colortype
pattern RGBA_F16_NORM_SK_COLORTYPE = (#const RGBA_F16_NORM_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (17/27): @"RGBA_F16_SK_COLORTYPE"@
pattern RGBA_F16_SK_COLORTYPE :: Sk_colortype
pattern RGBA_F16_SK_COLORTYPE = (#const RGBA_F16_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (18/27): @"RGB_F16F16F16X_SK_COLORTYPE"@
pattern RGB_F16F16F16X_SK_COLORTYPE :: Sk_colortype
pattern RGB_F16F16F16X_SK_COLORTYPE = (#const RGB_F16F16F16X_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (19/27): @"RGBA_F32_SK_COLORTYPE"@
pattern RGBA_F32_SK_COLORTYPE :: Sk_colortype
pattern RGBA_F32_SK_COLORTYPE = (#const RGBA_F32_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (20/27): @"R8G8_UNORM_SK_COLORTYPE"@
pattern R8G8_UNORM_SK_COLORTYPE :: Sk_colortype
pattern R8G8_UNORM_SK_COLORTYPE = (#const R8G8_UNORM_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (21/27): @"A16_FLOAT_SK_COLORTYPE"@
pattern A16_FLOAT_SK_COLORTYPE :: Sk_colortype
pattern A16_FLOAT_SK_COLORTYPE = (#const A16_FLOAT_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (22/27): @"R16G16_FLOAT_SK_COLORTYPE"@
pattern R16G16_FLOAT_SK_COLORTYPE :: Sk_colortype
pattern R16G16_FLOAT_SK_COLORTYPE = (#const R16G16_FLOAT_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (23/27): @"A16_UNORM_SK_COLORTYPE"@
pattern A16_UNORM_SK_COLORTYPE :: Sk_colortype
pattern A16_UNORM_SK_COLORTYPE = (#const A16_UNORM_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (24/27): @"R16G16_UNORM_SK_COLORTYPE"@
pattern R16G16_UNORM_SK_COLORTYPE :: Sk_colortype
pattern R16G16_UNORM_SK_COLORTYPE = (#const R16G16_UNORM_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (25/27): @"R16G16B16A16_UNORM_SK_COLORTYPE"@
pattern R16G16B16A16_UNORM_SK_COLORTYPE :: Sk_colortype
pattern R16G16B16A16_UNORM_SK_COLORTYPE = (#const R16G16B16A16_UNORM_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (26/27): @"SRGBA_8888_SK_COLORTYPE"@
pattern SRGBA_8888_SK_COLORTYPE :: Sk_colortype
pattern SRGBA_8888_SK_COLORTYPE = (#const SRGBA_8888_SK_COLORTYPE)

-- | C enum @"sk_colortype_t"@ value (27/27): @"R8_UNORM_SK_COLORTYPE"@
pattern R8_UNORM_SK_COLORTYPE :: Sk_colortype
pattern R8_UNORM_SK_COLORTYPE = (#const R8_UNORM_SK_COLORTYPE)

{- | C enum: @"sk_alphatype_t"@

@
typedef enum 
{
  UNKNOWN_SK_ALPHATYPE,
  OPAQUE_SK_ALPHATYPE,
  PREMUL_SK_ALPHATYPE,
  UNPREMUL_SK_ALPHATYPE
} sk_alphatype_t
@

-}
newtype Sk_alphatype = Sk_alphatype (#type sk_alphatype_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_alphatype_t"@ value (1/4): @"UNKNOWN_SK_ALPHATYPE"@
pattern UNKNOWN_SK_ALPHATYPE :: Sk_alphatype
pattern UNKNOWN_SK_ALPHATYPE = (#const UNKNOWN_SK_ALPHATYPE)

-- | C enum @"sk_alphatype_t"@ value (2/4): @"OPAQUE_SK_ALPHATYPE"@
pattern OPAQUE_SK_ALPHATYPE :: Sk_alphatype
pattern OPAQUE_SK_ALPHATYPE = (#const OPAQUE_SK_ALPHATYPE)

-- | C enum @"sk_alphatype_t"@ value (3/4): @"PREMUL_SK_ALPHATYPE"@
pattern PREMUL_SK_ALPHATYPE :: Sk_alphatype
pattern PREMUL_SK_ALPHATYPE = (#const PREMUL_SK_ALPHATYPE)

-- | C enum @"sk_alphatype_t"@ value (4/4): @"UNPREMUL_SK_ALPHATYPE"@
pattern UNPREMUL_SK_ALPHATYPE :: Sk_alphatype
pattern UNPREMUL_SK_ALPHATYPE = (#const UNPREMUL_SK_ALPHATYPE)

{- | C enum: @"sk_pixelgeometry_t"@

@
typedef enum 
{
  UNKNOWN_SK_PIXELGEOMETRY,
  RGB_H_SK_PIXELGEOMETRY,
  BGR_H_SK_PIXELGEOMETRY,
  RGB_V_SK_PIXELGEOMETRY,
  BGR_V_SK_PIXELGEOMETRY
} sk_pixelgeometry_t
@

-}
newtype Sk_pixelgeometry = Sk_pixelgeometry (#type sk_pixelgeometry_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_pixelgeometry_t"@ value (1/5): @"UNKNOWN_SK_PIXELGEOMETRY"@
pattern UNKNOWN_SK_PIXELGEOMETRY :: Sk_pixelgeometry
pattern UNKNOWN_SK_PIXELGEOMETRY = (#const UNKNOWN_SK_PIXELGEOMETRY)

-- | C enum @"sk_pixelgeometry_t"@ value (2/5): @"RGB_H_SK_PIXELGEOMETRY"@
pattern RGB_H_SK_PIXELGEOMETRY :: Sk_pixelgeometry
pattern RGB_H_SK_PIXELGEOMETRY = (#const RGB_H_SK_PIXELGEOMETRY)

-- | C enum @"sk_pixelgeometry_t"@ value (3/5): @"BGR_H_SK_PIXELGEOMETRY"@
pattern BGR_H_SK_PIXELGEOMETRY :: Sk_pixelgeometry
pattern BGR_H_SK_PIXELGEOMETRY = (#const BGR_H_SK_PIXELGEOMETRY)

-- | C enum @"sk_pixelgeometry_t"@ value (4/5): @"RGB_V_SK_PIXELGEOMETRY"@
pattern RGB_V_SK_PIXELGEOMETRY :: Sk_pixelgeometry
pattern RGB_V_SK_PIXELGEOMETRY = (#const RGB_V_SK_PIXELGEOMETRY)

-- | C enum @"sk_pixelgeometry_t"@ value (5/5): @"BGR_V_SK_PIXELGEOMETRY"@
pattern BGR_V_SK_PIXELGEOMETRY :: Sk_pixelgeometry
pattern BGR_V_SK_PIXELGEOMETRY = (#const BGR_V_SK_PIXELGEOMETRY)

{- | C enum: @"sk_surfaceprops_flags_t"@

@
typedef enum 
{
  NONE_SK_SURFACE_PROPS_FLAGS = 0,
  USE_DEVICE_INDEPENDENT_FONTS_SK_SURFACE_PROPS_FLAGS = 1 << 0,
  DYNAMIC_MSAA_SK_SURFACE_PROPS_FLAGS = 1 << 1,
  ALWAYS_DITHER_SK_SURFACE_PROPS_FLAGS = 1 << 2
} sk_surfaceprops_flags_t
@

-}
newtype Sk_surfaceprops_flags = Sk_surfaceprops_flags (#type sk_surfaceprops_flags_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_surfaceprops_flags_t"@ value (1/4): @"NONE_SK_SURFACE_PROPS_FLAGS"@
pattern NONE_SK_SURFACE_PROPS_FLAGS :: Sk_surfaceprops_flags
pattern NONE_SK_SURFACE_PROPS_FLAGS = (#const NONE_SK_SURFACE_PROPS_FLAGS)

-- | C enum @"sk_surfaceprops_flags_t"@ value (2/4): @"USE_DEVICE_INDEPENDENT_FONTS_SK_SURFACE_PROPS_FLAGS"@
pattern USE_DEVICE_INDEPENDENT_FONTS_SK_SURFACE_PROPS_FLAGS :: Sk_surfaceprops_flags
pattern USE_DEVICE_INDEPENDENT_FONTS_SK_SURFACE_PROPS_FLAGS = (#const USE_DEVICE_INDEPENDENT_FONTS_SK_SURFACE_PROPS_FLAGS)

-- | C enum @"sk_surfaceprops_flags_t"@ value (3/4): @"DYNAMIC_MSAA_SK_SURFACE_PROPS_FLAGS"@
pattern DYNAMIC_MSAA_SK_SURFACE_PROPS_FLAGS :: Sk_surfaceprops_flags
pattern DYNAMIC_MSAA_SK_SURFACE_PROPS_FLAGS = (#const DYNAMIC_MSAA_SK_SURFACE_PROPS_FLAGS)

-- | C enum @"sk_surfaceprops_flags_t"@ value (4/4): @"ALWAYS_DITHER_SK_SURFACE_PROPS_FLAGS"@
pattern ALWAYS_DITHER_SK_SURFACE_PROPS_FLAGS :: Sk_surfaceprops_flags
pattern ALWAYS_DITHER_SK_SURFACE_PROPS_FLAGS = (#const ALWAYS_DITHER_SK_SURFACE_PROPS_FLAGS)

{- | Opaque C struct: @"sk_surfaceprops_t"@
-}
data Sk_surfaceprops = Sk_surfaceprops

{- | C struct: @"sk_point_t"@

@
typedef struct 
{
  float x;
  float y;
} sk_point_t
@
-}
data Sk_point = Sk_point
  { x :: CFloat -- ^ C field: @"float x"@
  , y :: CFloat -- ^ C field: @"float y"@
  }
instance Foreign.Storable.Offset.Offset "x" Sk_point where
  rawOffset = (#offset sk_point_t, x)
instance Foreign.Storable.Offset.Offset "y" Sk_point where
  rawOffset = (#offset sk_point_t, y)
instance Foreign.Storable.Storable Sk_point where
  sizeOf _ = (#size sk_point_t)
  alignment _ = (#alignment sk_point_t)
  peek p' = do
    x <- (#peek sk_point_t, x) p'
    y <- (#peek sk_point_t, y) p'
    pure Sk_point{..}
  poke p' Sk_point{..} = do
    (#poke sk_point_t, x) p' x
    (#poke sk_point_t, y) p' y

{- | C type alias: @sk_vector_t@

@
typedef sk_point_t sk_vector_t
@
-}
type Sk_vector = Sk_point

{- | C struct: @"sk_irect_t"@

@
typedef struct 
{
  int32_t left;
  int32_t top;
  int32_t right;
  int32_t bottom;
} sk_irect_t
@
-}
data Sk_irect = Sk_irect
  { left :: Int32 -- ^ C field: @"int32_t left"@
  , top :: Int32 -- ^ C field: @"int32_t top"@
  , right :: Int32 -- ^ C field: @"int32_t right"@
  , bottom :: Int32 -- ^ C field: @"int32_t bottom"@
  }
instance Foreign.Storable.Offset.Offset "left" Sk_irect where
  rawOffset = (#offset sk_irect_t, left)
instance Foreign.Storable.Offset.Offset "top" Sk_irect where
  rawOffset = (#offset sk_irect_t, top)
instance Foreign.Storable.Offset.Offset "right" Sk_irect where
  rawOffset = (#offset sk_irect_t, right)
instance Foreign.Storable.Offset.Offset "bottom" Sk_irect where
  rawOffset = (#offset sk_irect_t, bottom)
instance Foreign.Storable.Storable Sk_irect where
  sizeOf _ = (#size sk_irect_t)
  alignment _ = (#alignment sk_irect_t)
  peek p' = do
    left <- (#peek sk_irect_t, left) p'
    top <- (#peek sk_irect_t, top) p'
    right <- (#peek sk_irect_t, right) p'
    bottom <- (#peek sk_irect_t, bottom) p'
    pure Sk_irect{..}
  poke p' Sk_irect{..} = do
    (#poke sk_irect_t, left) p' left
    (#poke sk_irect_t, top) p' top
    (#poke sk_irect_t, right) p' right
    (#poke sk_irect_t, bottom) p' bottom

{- | C struct: @"sk_rect_t"@

@
typedef struct 
{
  float left;
  float top;
  float right;
  float bottom;
} sk_rect_t
@
-}
data Sk_rect = Sk_rect
  { left :: CFloat -- ^ C field: @"float left"@
  , top :: CFloat -- ^ C field: @"float top"@
  , right :: CFloat -- ^ C field: @"float right"@
  , bottom :: CFloat -- ^ C field: @"float bottom"@
  }
instance Foreign.Storable.Offset.Offset "left" Sk_rect where
  rawOffset = (#offset sk_rect_t, left)
instance Foreign.Storable.Offset.Offset "top" Sk_rect where
  rawOffset = (#offset sk_rect_t, top)
instance Foreign.Storable.Offset.Offset "right" Sk_rect where
  rawOffset = (#offset sk_rect_t, right)
instance Foreign.Storable.Offset.Offset "bottom" Sk_rect where
  rawOffset = (#offset sk_rect_t, bottom)
instance Foreign.Storable.Storable Sk_rect where
  sizeOf _ = (#size sk_rect_t)
  alignment _ = (#alignment sk_rect_t)
  peek p' = do
    left <- (#peek sk_rect_t, left) p'
    top <- (#peek sk_rect_t, top) p'
    right <- (#peek sk_rect_t, right) p'
    bottom <- (#peek sk_rect_t, bottom) p'
    pure Sk_rect{..}
  poke p' Sk_rect{..} = do
    (#poke sk_rect_t, left) p' left
    (#poke sk_rect_t, top) p' top
    (#poke sk_rect_t, right) p' right
    (#poke sk_rect_t, bottom) p' bottom

{- | C struct: @"sk_matrix_t"@

@
typedef struct 
{
  float scaleX;
  float skewX;
  float transX;
  float skewY;
  float scaleY;
  float transY;
  float persp0;
  float persp1;
  float persp2;
} sk_matrix_t
@
-}
data Sk_matrix = Sk_matrix
  { scaleX :: CFloat -- ^ C field: @"float scaleX"@
  , skewX :: CFloat -- ^ C field: @"float skewX"@
  , transX :: CFloat -- ^ C field: @"float transX"@
  , skewY :: CFloat -- ^ C field: @"float skewY"@
  , scaleY :: CFloat -- ^ C field: @"float scaleY"@
  , transY :: CFloat -- ^ C field: @"float transY"@
  , persp0 :: CFloat -- ^ C field: @"float persp0"@
  , persp1 :: CFloat -- ^ C field: @"float persp1"@
  , persp2 :: CFloat -- ^ C field: @"float persp2"@
  }
instance Foreign.Storable.Offset.Offset "scaleX" Sk_matrix where
  rawOffset = (#offset sk_matrix_t, scaleX)
instance Foreign.Storable.Offset.Offset "skewX" Sk_matrix where
  rawOffset = (#offset sk_matrix_t, skewX)
instance Foreign.Storable.Offset.Offset "transX" Sk_matrix where
  rawOffset = (#offset sk_matrix_t, transX)
instance Foreign.Storable.Offset.Offset "skewY" Sk_matrix where
  rawOffset = (#offset sk_matrix_t, skewY)
instance Foreign.Storable.Offset.Offset "scaleY" Sk_matrix where
  rawOffset = (#offset sk_matrix_t, scaleY)
instance Foreign.Storable.Offset.Offset "transY" Sk_matrix where
  rawOffset = (#offset sk_matrix_t, transY)
instance Foreign.Storable.Offset.Offset "persp0" Sk_matrix where
  rawOffset = (#offset sk_matrix_t, persp0)
instance Foreign.Storable.Offset.Offset "persp1" Sk_matrix where
  rawOffset = (#offset sk_matrix_t, persp1)
instance Foreign.Storable.Offset.Offset "persp2" Sk_matrix where
  rawOffset = (#offset sk_matrix_t, persp2)
instance Foreign.Storable.Storable Sk_matrix where
  sizeOf _ = (#size sk_matrix_t)
  alignment _ = (#alignment sk_matrix_t)
  peek p' = do
    scaleX <- (#peek sk_matrix_t, scaleX) p'
    skewX <- (#peek sk_matrix_t, skewX) p'
    transX <- (#peek sk_matrix_t, transX) p'
    skewY <- (#peek sk_matrix_t, skewY) p'
    scaleY <- (#peek sk_matrix_t, scaleY) p'
    transY <- (#peek sk_matrix_t, transY) p'
    persp0 <- (#peek sk_matrix_t, persp0) p'
    persp1 <- (#peek sk_matrix_t, persp1) p'
    persp2 <- (#peek sk_matrix_t, persp2) p'
    pure Sk_matrix{..}
  poke p' Sk_matrix{..} = do
    (#poke sk_matrix_t, scaleX) p' scaleX
    (#poke sk_matrix_t, skewX) p' skewX
    (#poke sk_matrix_t, transX) p' transX
    (#poke sk_matrix_t, skewY) p' skewY
    (#poke sk_matrix_t, scaleY) p' scaleY
    (#poke sk_matrix_t, transY) p' transY
    (#poke sk_matrix_t, persp0) p' persp0
    (#poke sk_matrix_t, persp1) p' persp1
    (#poke sk_matrix_t, persp2) p' persp2

{- | C struct: @"sk_matrix44_t"@

@
typedef struct 
{
  float m00;
  float m01;
  float m02;
  float m03;
  float m10;
  float m11;
  float m12;
  float m13;
  float m20;
  float m21;
  float m22;
  float m23;
  float m30;
  float m31;
  float m32;
  float m33;
} sk_matrix44_t
@
-}
data Sk_matrix44 = Sk_matrix44
  { m00 :: CFloat -- ^ C field: @"float m00"@
  , m01 :: CFloat -- ^ C field: @"float m01"@
  , m02 :: CFloat -- ^ C field: @"float m02"@
  , m03 :: CFloat -- ^ C field: @"float m03"@
  , m10 :: CFloat -- ^ C field: @"float m10"@
  , m11 :: CFloat -- ^ C field: @"float m11"@
  , m12 :: CFloat -- ^ C field: @"float m12"@
  , m13 :: CFloat -- ^ C field: @"float m13"@
  , m20 :: CFloat -- ^ C field: @"float m20"@
  , m21 :: CFloat -- ^ C field: @"float m21"@
  , m22 :: CFloat -- ^ C field: @"float m22"@
  , m23 :: CFloat -- ^ C field: @"float m23"@
  , m30 :: CFloat -- ^ C field: @"float m30"@
  , m31 :: CFloat -- ^ C field: @"float m31"@
  , m32 :: CFloat -- ^ C field: @"float m32"@
  , m33 :: CFloat -- ^ C field: @"float m33"@
  }
instance Foreign.Storable.Offset.Offset "m00" Sk_matrix44 where
  rawOffset = (#offset sk_matrix44_t, m00)
instance Foreign.Storable.Offset.Offset "m01" Sk_matrix44 where
  rawOffset = (#offset sk_matrix44_t, m01)
instance Foreign.Storable.Offset.Offset "m02" Sk_matrix44 where
  rawOffset = (#offset sk_matrix44_t, m02)
instance Foreign.Storable.Offset.Offset "m03" Sk_matrix44 where
  rawOffset = (#offset sk_matrix44_t, m03)
instance Foreign.Storable.Offset.Offset "m10" Sk_matrix44 where
  rawOffset = (#offset sk_matrix44_t, m10)
instance Foreign.Storable.Offset.Offset "m11" Sk_matrix44 where
  rawOffset = (#offset sk_matrix44_t, m11)
instance Foreign.Storable.Offset.Offset "m12" Sk_matrix44 where
  rawOffset = (#offset sk_matrix44_t, m12)
instance Foreign.Storable.Offset.Offset "m13" Sk_matrix44 where
  rawOffset = (#offset sk_matrix44_t, m13)
instance Foreign.Storable.Offset.Offset "m20" Sk_matrix44 where
  rawOffset = (#offset sk_matrix44_t, m20)
instance Foreign.Storable.Offset.Offset "m21" Sk_matrix44 where
  rawOffset = (#offset sk_matrix44_t, m21)
instance Foreign.Storable.Offset.Offset "m22" Sk_matrix44 where
  rawOffset = (#offset sk_matrix44_t, m22)
instance Foreign.Storable.Offset.Offset "m23" Sk_matrix44 where
  rawOffset = (#offset sk_matrix44_t, m23)
instance Foreign.Storable.Offset.Offset "m30" Sk_matrix44 where
  rawOffset = (#offset sk_matrix44_t, m30)
instance Foreign.Storable.Offset.Offset "m31" Sk_matrix44 where
  rawOffset = (#offset sk_matrix44_t, m31)
instance Foreign.Storable.Offset.Offset "m32" Sk_matrix44 where
  rawOffset = (#offset sk_matrix44_t, m32)
instance Foreign.Storable.Offset.Offset "m33" Sk_matrix44 where
  rawOffset = (#offset sk_matrix44_t, m33)
instance Foreign.Storable.Storable Sk_matrix44 where
  sizeOf _ = (#size sk_matrix44_t)
  alignment _ = (#alignment sk_matrix44_t)
  peek p' = do
    m00 <- (#peek sk_matrix44_t, m00) p'
    m01 <- (#peek sk_matrix44_t, m01) p'
    m02 <- (#peek sk_matrix44_t, m02) p'
    m03 <- (#peek sk_matrix44_t, m03) p'
    m10 <- (#peek sk_matrix44_t, m10) p'
    m11 <- (#peek sk_matrix44_t, m11) p'
    m12 <- (#peek sk_matrix44_t, m12) p'
    m13 <- (#peek sk_matrix44_t, m13) p'
    m20 <- (#peek sk_matrix44_t, m20) p'
    m21 <- (#peek sk_matrix44_t, m21) p'
    m22 <- (#peek sk_matrix44_t, m22) p'
    m23 <- (#peek sk_matrix44_t, m23) p'
    m30 <- (#peek sk_matrix44_t, m30) p'
    m31 <- (#peek sk_matrix44_t, m31) p'
    m32 <- (#peek sk_matrix44_t, m32) p'
    m33 <- (#peek sk_matrix44_t, m33) p'
    pure Sk_matrix44{..}
  poke p' Sk_matrix44{..} = do
    (#poke sk_matrix44_t, m00) p' m00
    (#poke sk_matrix44_t, m01) p' m01
    (#poke sk_matrix44_t, m02) p' m02
    (#poke sk_matrix44_t, m03) p' m03
    (#poke sk_matrix44_t, m10) p' m10
    (#poke sk_matrix44_t, m11) p' m11
    (#poke sk_matrix44_t, m12) p' m12
    (#poke sk_matrix44_t, m13) p' m13
    (#poke sk_matrix44_t, m20) p' m20
    (#poke sk_matrix44_t, m21) p' m21
    (#poke sk_matrix44_t, m22) p' m22
    (#poke sk_matrix44_t, m23) p' m23
    (#poke sk_matrix44_t, m30) p' m30
    (#poke sk_matrix44_t, m31) p' m31
    (#poke sk_matrix44_t, m32) p' m32
    (#poke sk_matrix44_t, m33) p' m33

{- | Opaque C struct: @"sk_canvas_t"@
-}
data Sk_canvas = Sk_canvas

{- | Opaque C struct: @"sk_nodraw_canvas_t"@
-}
data Sk_nodraw_canvas = Sk_nodraw_canvas

{- | Opaque C struct: @"sk_nway_canvas_t"@
-}
data Sk_nway_canvas = Sk_nway_canvas

{- | Opaque C struct: @"sk_overdraw_canvas_t"@
-}
data Sk_overdraw_canvas = Sk_overdraw_canvas

{- | Opaque C struct: @"sk_data_t"@
-}
data Sk_data = Sk_data

{- | Opaque C struct: @"sk_drawable_t"@
-}
data Sk_drawable = Sk_drawable

{- | Opaque C struct: @"sk_image_t"@
-}
data Sk_image = Sk_image

{- | Opaque C struct: @"sk_maskfilter_t"@
-}
data Sk_maskfilter = Sk_maskfilter

{- | Opaque C struct: @"sk_paint_t"@
-}
data Sk_paint = Sk_paint

{- | Opaque C struct: @"sk_font_t"@
-}
data Sk_font = Sk_font

{- | Opaque C struct: @"sk_path_t"@
-}
data Sk_path = Sk_path

{- | Opaque C struct: @"sk_picture_t"@
-}
data Sk_picture = Sk_picture

{- | Opaque C struct: @"sk_picture_recorder_t"@
-}
data Sk_picture_recorder = Sk_picture_recorder

{- | Opaque C struct: @"sk_bbh_factory_t"@
-}
data Sk_bbh_factory = Sk_bbh_factory

{- | Opaque C struct: @"sk_rtree_factory_t"@
-}
data Sk_rtree_factory = Sk_rtree_factory

{- | Opaque C struct: @"sk_shader_t"@
-}
data Sk_shader = Sk_shader

{- | Opaque C struct: @"sk_surface_t"@
-}
data Sk_surface = Sk_surface

{- | Opaque C struct: @"sk_region_t"@
-}
data Sk_region = Sk_region

{- | Opaque C struct: @"sk_region_iterator_t"@
-}
data Sk_region_iterator = Sk_region_iterator

{- | Opaque C struct: @"sk_region_cliperator_t"@
-}
data Sk_region_cliperator = Sk_region_cliperator

{- | Opaque C struct: @"sk_region_spanerator_t"@
-}
data Sk_region_spanerator = Sk_region_spanerator

{- | C enum: @"sk_blendmode_t"@

@
typedef enum 
{
  CLEAR_SK_BLENDMODE,
  SRC_SK_BLENDMODE,
  DST_SK_BLENDMODE,
  SRCOVER_SK_BLENDMODE,
  DSTOVER_SK_BLENDMODE,
  SRCIN_SK_BLENDMODE,
  DSTIN_SK_BLENDMODE,
  SRCOUT_SK_BLENDMODE,
  DSTOUT_SK_BLENDMODE,
  SRCATOP_SK_BLENDMODE,
  DSTATOP_SK_BLENDMODE,
  XOR_SK_BLENDMODE,
  PLUS_SK_BLENDMODE,
  MODULATE_SK_BLENDMODE,
  SCREEN_SK_BLENDMODE,
  OVERLAY_SK_BLENDMODE,
  DARKEN_SK_BLENDMODE,
  LIGHTEN_SK_BLENDMODE,
  COLORDODGE_SK_BLENDMODE,
  COLORBURN_SK_BLENDMODE,
  HARDLIGHT_SK_BLENDMODE,
  SOFTLIGHT_SK_BLENDMODE,
  DIFFERENCE_SK_BLENDMODE,
  EXCLUSION_SK_BLENDMODE,
  MULTIPLY_SK_BLENDMODE,
  HUE_SK_BLENDMODE,
  SATURATION_SK_BLENDMODE,
  COLOR_SK_BLENDMODE,
  LUMINOSITY_SK_BLENDMODE
} sk_blendmode_t
@

-}
newtype Sk_blendmode = Sk_blendmode (#type sk_blendmode_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_blendmode_t"@ value (1/29): @"CLEAR_SK_BLENDMODE"@
pattern CLEAR_SK_BLENDMODE :: Sk_blendmode
pattern CLEAR_SK_BLENDMODE = (#const CLEAR_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (2/29): @"SRC_SK_BLENDMODE"@
pattern SRC_SK_BLENDMODE :: Sk_blendmode
pattern SRC_SK_BLENDMODE = (#const SRC_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (3/29): @"DST_SK_BLENDMODE"@
pattern DST_SK_BLENDMODE :: Sk_blendmode
pattern DST_SK_BLENDMODE = (#const DST_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (4/29): @"SRCOVER_SK_BLENDMODE"@
pattern SRCOVER_SK_BLENDMODE :: Sk_blendmode
pattern SRCOVER_SK_BLENDMODE = (#const SRCOVER_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (5/29): @"DSTOVER_SK_BLENDMODE"@
pattern DSTOVER_SK_BLENDMODE :: Sk_blendmode
pattern DSTOVER_SK_BLENDMODE = (#const DSTOVER_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (6/29): @"SRCIN_SK_BLENDMODE"@
pattern SRCIN_SK_BLENDMODE :: Sk_blendmode
pattern SRCIN_SK_BLENDMODE = (#const SRCIN_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (7/29): @"DSTIN_SK_BLENDMODE"@
pattern DSTIN_SK_BLENDMODE :: Sk_blendmode
pattern DSTIN_SK_BLENDMODE = (#const DSTIN_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (8/29): @"SRCOUT_SK_BLENDMODE"@
pattern SRCOUT_SK_BLENDMODE :: Sk_blendmode
pattern SRCOUT_SK_BLENDMODE = (#const SRCOUT_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (9/29): @"DSTOUT_SK_BLENDMODE"@
pattern DSTOUT_SK_BLENDMODE :: Sk_blendmode
pattern DSTOUT_SK_BLENDMODE = (#const DSTOUT_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (10/29): @"SRCATOP_SK_BLENDMODE"@
pattern SRCATOP_SK_BLENDMODE :: Sk_blendmode
pattern SRCATOP_SK_BLENDMODE = (#const SRCATOP_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (11/29): @"DSTATOP_SK_BLENDMODE"@
pattern DSTATOP_SK_BLENDMODE :: Sk_blendmode
pattern DSTATOP_SK_BLENDMODE = (#const DSTATOP_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (12/29): @"XOR_SK_BLENDMODE"@
pattern XOR_SK_BLENDMODE :: Sk_blendmode
pattern XOR_SK_BLENDMODE = (#const XOR_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (13/29): @"PLUS_SK_BLENDMODE"@
pattern PLUS_SK_BLENDMODE :: Sk_blendmode
pattern PLUS_SK_BLENDMODE = (#const PLUS_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (14/29): @"MODULATE_SK_BLENDMODE"@
pattern MODULATE_SK_BLENDMODE :: Sk_blendmode
pattern MODULATE_SK_BLENDMODE = (#const MODULATE_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (15/29): @"SCREEN_SK_BLENDMODE"@
pattern SCREEN_SK_BLENDMODE :: Sk_blendmode
pattern SCREEN_SK_BLENDMODE = (#const SCREEN_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (16/29): @"OVERLAY_SK_BLENDMODE"@
pattern OVERLAY_SK_BLENDMODE :: Sk_blendmode
pattern OVERLAY_SK_BLENDMODE = (#const OVERLAY_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (17/29): @"DARKEN_SK_BLENDMODE"@
pattern DARKEN_SK_BLENDMODE :: Sk_blendmode
pattern DARKEN_SK_BLENDMODE = (#const DARKEN_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (18/29): @"LIGHTEN_SK_BLENDMODE"@
pattern LIGHTEN_SK_BLENDMODE :: Sk_blendmode
pattern LIGHTEN_SK_BLENDMODE = (#const LIGHTEN_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (19/29): @"COLORDODGE_SK_BLENDMODE"@
pattern COLORDODGE_SK_BLENDMODE :: Sk_blendmode
pattern COLORDODGE_SK_BLENDMODE = (#const COLORDODGE_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (20/29): @"COLORBURN_SK_BLENDMODE"@
pattern COLORBURN_SK_BLENDMODE :: Sk_blendmode
pattern COLORBURN_SK_BLENDMODE = (#const COLORBURN_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (21/29): @"HARDLIGHT_SK_BLENDMODE"@
pattern HARDLIGHT_SK_BLENDMODE :: Sk_blendmode
pattern HARDLIGHT_SK_BLENDMODE = (#const HARDLIGHT_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (22/29): @"SOFTLIGHT_SK_BLENDMODE"@
pattern SOFTLIGHT_SK_BLENDMODE :: Sk_blendmode
pattern SOFTLIGHT_SK_BLENDMODE = (#const SOFTLIGHT_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (23/29): @"DIFFERENCE_SK_BLENDMODE"@
pattern DIFFERENCE_SK_BLENDMODE :: Sk_blendmode
pattern DIFFERENCE_SK_BLENDMODE = (#const DIFFERENCE_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (24/29): @"EXCLUSION_SK_BLENDMODE"@
pattern EXCLUSION_SK_BLENDMODE :: Sk_blendmode
pattern EXCLUSION_SK_BLENDMODE = (#const EXCLUSION_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (25/29): @"MULTIPLY_SK_BLENDMODE"@
pattern MULTIPLY_SK_BLENDMODE :: Sk_blendmode
pattern MULTIPLY_SK_BLENDMODE = (#const MULTIPLY_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (26/29): @"HUE_SK_BLENDMODE"@
pattern HUE_SK_BLENDMODE :: Sk_blendmode
pattern HUE_SK_BLENDMODE = (#const HUE_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (27/29): @"SATURATION_SK_BLENDMODE"@
pattern SATURATION_SK_BLENDMODE :: Sk_blendmode
pattern SATURATION_SK_BLENDMODE = (#const SATURATION_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (28/29): @"COLOR_SK_BLENDMODE"@
pattern COLOR_SK_BLENDMODE :: Sk_blendmode
pattern COLOR_SK_BLENDMODE = (#const COLOR_SK_BLENDMODE)

-- | C enum @"sk_blendmode_t"@ value (29/29): @"LUMINOSITY_SK_BLENDMODE"@
pattern LUMINOSITY_SK_BLENDMODE :: Sk_blendmode
pattern LUMINOSITY_SK_BLENDMODE = (#const LUMINOSITY_SK_BLENDMODE)

{- | C struct: @"sk_point3_t"@

@
typedef struct 
{
  float x;
  float y;
  float z;
} sk_point3_t
@
-}
data Sk_point3 = Sk_point3
  { x :: CFloat -- ^ C field: @"float x"@
  , y :: CFloat -- ^ C field: @"float y"@
  , z :: CFloat -- ^ C field: @"float z"@
  }
instance Foreign.Storable.Offset.Offset "x" Sk_point3 where
  rawOffset = (#offset sk_point3_t, x)
instance Foreign.Storable.Offset.Offset "y" Sk_point3 where
  rawOffset = (#offset sk_point3_t, y)
instance Foreign.Storable.Offset.Offset "z" Sk_point3 where
  rawOffset = (#offset sk_point3_t, z)
instance Foreign.Storable.Storable Sk_point3 where
  sizeOf _ = (#size sk_point3_t)
  alignment _ = (#alignment sk_point3_t)
  peek p' = do
    x <- (#peek sk_point3_t, x) p'
    y <- (#peek sk_point3_t, y) p'
    z <- (#peek sk_point3_t, z) p'
    pure Sk_point3{..}
  poke p' Sk_point3{..} = do
    (#poke sk_point3_t, x) p' x
    (#poke sk_point3_t, y) p' y
    (#poke sk_point3_t, z) p' z

{- | C struct: @"sk_ipoint_t"@

@
typedef struct 
{
  int32_t x;
  int32_t y;
} sk_ipoint_t
@
-}
data Sk_ipoint = Sk_ipoint
  { x :: Int32 -- ^ C field: @"int32_t x"@
  , y :: Int32 -- ^ C field: @"int32_t y"@
  }
instance Foreign.Storable.Offset.Offset "x" Sk_ipoint where
  rawOffset = (#offset sk_ipoint_t, x)
instance Foreign.Storable.Offset.Offset "y" Sk_ipoint where
  rawOffset = (#offset sk_ipoint_t, y)
instance Foreign.Storable.Storable Sk_ipoint where
  sizeOf _ = (#size sk_ipoint_t)
  alignment _ = (#alignment sk_ipoint_t)
  peek p' = do
    x <- (#peek sk_ipoint_t, x) p'
    y <- (#peek sk_ipoint_t, y) p'
    pure Sk_ipoint{..}
  poke p' Sk_ipoint{..} = do
    (#poke sk_ipoint_t, x) p' x
    (#poke sk_ipoint_t, y) p' y

{- | C struct: @"sk_size_t"@

@
typedef struct 
{
  float w;
  float h;
} sk_size_t
@
-}
data Sk_size = Sk_size
  { w :: CFloat -- ^ C field: @"float w"@
  , h :: CFloat -- ^ C field: @"float h"@
  }
instance Foreign.Storable.Offset.Offset "w" Sk_size where
  rawOffset = (#offset sk_size_t, w)
instance Foreign.Storable.Offset.Offset "h" Sk_size where
  rawOffset = (#offset sk_size_t, h)
instance Foreign.Storable.Storable Sk_size where
  sizeOf _ = (#size sk_size_t)
  alignment _ = (#alignment sk_size_t)
  peek p' = do
    w <- (#peek sk_size_t, w) p'
    h <- (#peek sk_size_t, h) p'
    pure Sk_size{..}
  poke p' Sk_size{..} = do
    (#poke sk_size_t, w) p' w
    (#poke sk_size_t, h) p' h

{- | C struct: @"sk_isize_t"@

@
typedef struct 
{
  int32_t w;
  int32_t h;
} sk_isize_t
@
-}
data Sk_isize = Sk_isize
  { w :: Int32 -- ^ C field: @"int32_t w"@
  , h :: Int32 -- ^ C field: @"int32_t h"@
  }
instance Foreign.Storable.Offset.Offset "w" Sk_isize where
  rawOffset = (#offset sk_isize_t, w)
instance Foreign.Storable.Offset.Offset "h" Sk_isize where
  rawOffset = (#offset sk_isize_t, h)
instance Foreign.Storable.Storable Sk_isize where
  sizeOf _ = (#size sk_isize_t)
  alignment _ = (#alignment sk_isize_t)
  peek p' = do
    w <- (#peek sk_isize_t, w) p'
    h <- (#peek sk_isize_t, h) p'
    pure Sk_isize{..}
  poke p' Sk_isize{..} = do
    (#poke sk_isize_t, w) p' w
    (#poke sk_isize_t, h) p' h

{- | C struct: @"sk_fontmetrics_t"@

@
typedef struct 
{
  uint32_t fFlags;
  float fTop;
  float fAscent;
  float fDescent;
  float fBottom;
  float fLeading;
  float fAvgCharWidth;
  float fMaxCharWidth;
  float fXMin;
  float fXMax;
  float fXHeight;
  float fCapHeight;
  float fUnderlineThickness;
  float fUnderlinePosition;
  float fStrikeoutThickness;
  float fStrikeoutPosition;
} sk_fontmetrics_t
@
-}
data Sk_fontmetrics = Sk_fontmetrics
  { fFlags :: Word32 -- ^ C field: @"uint32_t fFlags"@
  , fTop :: CFloat -- ^ C field: @"float fTop"@
  , fAscent :: CFloat -- ^ C field: @"float fAscent"@
  , fDescent :: CFloat -- ^ C field: @"float fDescent"@
  , fBottom :: CFloat -- ^ C field: @"float fBottom"@
  , fLeading :: CFloat -- ^ C field: @"float fLeading"@
  , fAvgCharWidth :: CFloat -- ^ C field: @"float fAvgCharWidth"@
  , fMaxCharWidth :: CFloat -- ^ C field: @"float fMaxCharWidth"@
  , fXMin :: CFloat -- ^ C field: @"float fXMin"@
  , fXMax :: CFloat -- ^ C field: @"float fXMax"@
  , fXHeight :: CFloat -- ^ C field: @"float fXHeight"@
  , fCapHeight :: CFloat -- ^ C field: @"float fCapHeight"@
  , fUnderlineThickness :: CFloat -- ^ C field: @"float fUnderlineThickness"@
  , fUnderlinePosition :: CFloat -- ^ C field: @"float fUnderlinePosition"@
  , fStrikeoutThickness :: CFloat -- ^ C field: @"float fStrikeoutThickness"@
  , fStrikeoutPosition :: CFloat -- ^ C field: @"float fStrikeoutPosition"@
  }
instance Foreign.Storable.Offset.Offset "fFlags" Sk_fontmetrics where
  rawOffset = (#offset sk_fontmetrics_t, fFlags)
instance Foreign.Storable.Offset.Offset "fTop" Sk_fontmetrics where
  rawOffset = (#offset sk_fontmetrics_t, fTop)
instance Foreign.Storable.Offset.Offset "fAscent" Sk_fontmetrics where
  rawOffset = (#offset sk_fontmetrics_t, fAscent)
instance Foreign.Storable.Offset.Offset "fDescent" Sk_fontmetrics where
  rawOffset = (#offset sk_fontmetrics_t, fDescent)
instance Foreign.Storable.Offset.Offset "fBottom" Sk_fontmetrics where
  rawOffset = (#offset sk_fontmetrics_t, fBottom)
instance Foreign.Storable.Offset.Offset "fLeading" Sk_fontmetrics where
  rawOffset = (#offset sk_fontmetrics_t, fLeading)
instance Foreign.Storable.Offset.Offset "fAvgCharWidth" Sk_fontmetrics where
  rawOffset = (#offset sk_fontmetrics_t, fAvgCharWidth)
instance Foreign.Storable.Offset.Offset "fMaxCharWidth" Sk_fontmetrics where
  rawOffset = (#offset sk_fontmetrics_t, fMaxCharWidth)
instance Foreign.Storable.Offset.Offset "fXMin" Sk_fontmetrics where
  rawOffset = (#offset sk_fontmetrics_t, fXMin)
instance Foreign.Storable.Offset.Offset "fXMax" Sk_fontmetrics where
  rawOffset = (#offset sk_fontmetrics_t, fXMax)
instance Foreign.Storable.Offset.Offset "fXHeight" Sk_fontmetrics where
  rawOffset = (#offset sk_fontmetrics_t, fXHeight)
instance Foreign.Storable.Offset.Offset "fCapHeight" Sk_fontmetrics where
  rawOffset = (#offset sk_fontmetrics_t, fCapHeight)
instance Foreign.Storable.Offset.Offset "fUnderlineThickness" Sk_fontmetrics where
  rawOffset = (#offset sk_fontmetrics_t, fUnderlineThickness)
instance Foreign.Storable.Offset.Offset "fUnderlinePosition" Sk_fontmetrics where
  rawOffset = (#offset sk_fontmetrics_t, fUnderlinePosition)
instance Foreign.Storable.Offset.Offset "fStrikeoutThickness" Sk_fontmetrics where
  rawOffset = (#offset sk_fontmetrics_t, fStrikeoutThickness)
instance Foreign.Storable.Offset.Offset "fStrikeoutPosition" Sk_fontmetrics where
  rawOffset = (#offset sk_fontmetrics_t, fStrikeoutPosition)
instance Foreign.Storable.Storable Sk_fontmetrics where
  sizeOf _ = (#size sk_fontmetrics_t)
  alignment _ = (#alignment sk_fontmetrics_t)
  peek p' = do
    fFlags <- (#peek sk_fontmetrics_t, fFlags) p'
    fTop <- (#peek sk_fontmetrics_t, fTop) p'
    fAscent <- (#peek sk_fontmetrics_t, fAscent) p'
    fDescent <- (#peek sk_fontmetrics_t, fDescent) p'
    fBottom <- (#peek sk_fontmetrics_t, fBottom) p'
    fLeading <- (#peek sk_fontmetrics_t, fLeading) p'
    fAvgCharWidth <- (#peek sk_fontmetrics_t, fAvgCharWidth) p'
    fMaxCharWidth <- (#peek sk_fontmetrics_t, fMaxCharWidth) p'
    fXMin <- (#peek sk_fontmetrics_t, fXMin) p'
    fXMax <- (#peek sk_fontmetrics_t, fXMax) p'
    fXHeight <- (#peek sk_fontmetrics_t, fXHeight) p'
    fCapHeight <- (#peek sk_fontmetrics_t, fCapHeight) p'
    fUnderlineThickness <- (#peek sk_fontmetrics_t, fUnderlineThickness) p'
    fUnderlinePosition <- (#peek sk_fontmetrics_t, fUnderlinePosition) p'
    fStrikeoutThickness <- (#peek sk_fontmetrics_t, fStrikeoutThickness) p'
    fStrikeoutPosition <- (#peek sk_fontmetrics_t, fStrikeoutPosition) p'
    pure Sk_fontmetrics{..}
  poke p' Sk_fontmetrics{..} = do
    (#poke sk_fontmetrics_t, fFlags) p' fFlags
    (#poke sk_fontmetrics_t, fTop) p' fTop
    (#poke sk_fontmetrics_t, fAscent) p' fAscent
    (#poke sk_fontmetrics_t, fDescent) p' fDescent
    (#poke sk_fontmetrics_t, fBottom) p' fBottom
    (#poke sk_fontmetrics_t, fLeading) p' fLeading
    (#poke sk_fontmetrics_t, fAvgCharWidth) p' fAvgCharWidth
    (#poke sk_fontmetrics_t, fMaxCharWidth) p' fMaxCharWidth
    (#poke sk_fontmetrics_t, fXMin) p' fXMin
    (#poke sk_fontmetrics_t, fXMax) p' fXMax
    (#poke sk_fontmetrics_t, fXHeight) p' fXHeight
    (#poke sk_fontmetrics_t, fCapHeight) p' fCapHeight
    (#poke sk_fontmetrics_t, fUnderlineThickness) p' fUnderlineThickness
    (#poke sk_fontmetrics_t, fUnderlinePosition) p' fUnderlinePosition
    (#poke sk_fontmetrics_t, fStrikeoutThickness) p' fStrikeoutThickness
    (#poke sk_fontmetrics_t, fStrikeoutPosition) p' fStrikeoutPosition

{- | Opaque C struct: @"sk_string_t"@
-}
data Sk_string = Sk_string

{- | Opaque C struct: @"sk_bitmap_t"@
-}
data Sk_bitmap = Sk_bitmap

{- | Opaque C struct: @"sk_pixmap_t"@
-}
data Sk_pixmap = Sk_pixmap

{- | Opaque C struct: @"sk_colorfilter_t"@
-}
data Sk_colorfilter = Sk_colorfilter

{- | Opaque C struct: @"sk_imagefilter_t"@
-}
data Sk_imagefilter = Sk_imagefilter

{- | Opaque C struct: @"sk_blender_t"@
-}
data Sk_blender = Sk_blender

{- | Opaque C struct: @"sk_typeface_t"@
-}
data Sk_typeface = Sk_typeface

{- | C type alias: @sk_font_table_tag_t@

@
typedef uint32_t sk_font_table_tag_t
@
-}
type Sk_font_table_tag = Word32

{- | Opaque C struct: @"sk_fontmgr_t"@
-}
data Sk_fontmgr = Sk_fontmgr

{- | Opaque C struct: @"sk_fontstyle_t"@
-}
data Sk_fontstyle = Sk_fontstyle

{- | Opaque C struct: @"sk_fontstyleset_t"@
-}
data Sk_fontstyleset = Sk_fontstyleset

{- | Opaque C struct: @"sk_fontarguments_t"@
-}
data Sk_fontarguments = Sk_fontarguments

{- | Opaque C struct: @"sk_codec_t"@
-}
data Sk_codec = Sk_codec

{- | Opaque C struct: @"sk_colorspace_t"@
-}
data Sk_colorspace = Sk_colorspace

{- | Opaque C struct: @"sk_stream_t"@
-}
data Sk_stream = Sk_stream

{- | Opaque C struct: @"sk_stream_filestream_t"@
-}
data Sk_stream_filestream = Sk_stream_filestream

{- | Opaque C struct: @"sk_stream_asset_t"@
-}
data Sk_stream_asset = Sk_stream_asset

{- | Opaque C struct: @"sk_stream_memorystream_t"@
-}
data Sk_stream_memorystream = Sk_stream_memorystream

{- | Opaque C struct: @"sk_stream_streamrewindable_t"@
-}
data Sk_stream_streamrewindable = Sk_stream_streamrewindable

{- | Opaque C struct: @"sk_wstream_t"@
-}
data Sk_wstream = Sk_wstream

{- | Opaque C struct: @"sk_wstream_filestream_t"@
-}
data Sk_wstream_filestream = Sk_wstream_filestream

{- | Opaque C struct: @"sk_wstream_dynamicmemorystream_t"@
-}
data Sk_wstream_dynamicmemorystream = Sk_wstream_dynamicmemorystream

{- | Opaque C struct: @"sk_document_t"@
-}
data Sk_document = Sk_document

{- | C enum: @"sk_point_mode_t"@

@
typedef enum 
{
  POINTS_SK_POINT_MODE,
  LINES_SK_POINT_MODE,
  POLYGON_SK_POINT_MODE
} sk_point_mode_t
@

-}
newtype Sk_point_mode = Sk_point_mode (#type sk_point_mode_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_point_mode_t"@ value (1/3): @"POINTS_SK_POINT_MODE"@
pattern POINTS_SK_POINT_MODE :: Sk_point_mode
pattern POINTS_SK_POINT_MODE = (#const POINTS_SK_POINT_MODE)

-- | C enum @"sk_point_mode_t"@ value (2/3): @"LINES_SK_POINT_MODE"@
pattern LINES_SK_POINT_MODE :: Sk_point_mode
pattern LINES_SK_POINT_MODE = (#const LINES_SK_POINT_MODE)

-- | C enum @"sk_point_mode_t"@ value (3/3): @"POLYGON_SK_POINT_MODE"@
pattern POLYGON_SK_POINT_MODE :: Sk_point_mode
pattern POLYGON_SK_POINT_MODE = (#const POLYGON_SK_POINT_MODE)

{- | C enum: @"sk_text_align_t"@

@
typedef enum 
{
  LEFT_SK_TEXT_ALIGN,
  CENTER_SK_TEXT_ALIGN,
  RIGHT_SK_TEXT_ALIGN
} sk_text_align_t
@

-}
newtype Sk_text_align = Sk_text_align (#type sk_text_align_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_text_align_t"@ value (1/3): @"LEFT_SK_TEXT_ALIGN"@
pattern LEFT_SK_TEXT_ALIGN :: Sk_text_align
pattern LEFT_SK_TEXT_ALIGN = (#const LEFT_SK_TEXT_ALIGN)

-- | C enum @"sk_text_align_t"@ value (2/3): @"CENTER_SK_TEXT_ALIGN"@
pattern CENTER_SK_TEXT_ALIGN :: Sk_text_align
pattern CENTER_SK_TEXT_ALIGN = (#const CENTER_SK_TEXT_ALIGN)

-- | C enum @"sk_text_align_t"@ value (3/3): @"RIGHT_SK_TEXT_ALIGN"@
pattern RIGHT_SK_TEXT_ALIGN :: Sk_text_align
pattern RIGHT_SK_TEXT_ALIGN = (#const RIGHT_SK_TEXT_ALIGN)

{- | C enum: @"sk_text_encoding_t"@

@
typedef enum 
{
  UTF8_SK_TEXT_ENCODING,
  UTF16_SK_TEXT_ENCODING,
  UTF32_SK_TEXT_ENCODING,
  GLYPH_ID_SK_TEXT_ENCODING
} sk_text_encoding_t
@

-}
newtype Sk_text_encoding = Sk_text_encoding (#type sk_text_encoding_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_text_encoding_t"@ value (1/4): @"UTF8_SK_TEXT_ENCODING"@
pattern UTF8_SK_TEXT_ENCODING :: Sk_text_encoding
pattern UTF8_SK_TEXT_ENCODING = (#const UTF8_SK_TEXT_ENCODING)

-- | C enum @"sk_text_encoding_t"@ value (2/4): @"UTF16_SK_TEXT_ENCODING"@
pattern UTF16_SK_TEXT_ENCODING :: Sk_text_encoding
pattern UTF16_SK_TEXT_ENCODING = (#const UTF16_SK_TEXT_ENCODING)

-- | C enum @"sk_text_encoding_t"@ value (3/4): @"UTF32_SK_TEXT_ENCODING"@
pattern UTF32_SK_TEXT_ENCODING :: Sk_text_encoding
pattern UTF32_SK_TEXT_ENCODING = (#const UTF32_SK_TEXT_ENCODING)

-- | C enum @"sk_text_encoding_t"@ value (4/4): @"GLYPH_ID_SK_TEXT_ENCODING"@
pattern GLYPH_ID_SK_TEXT_ENCODING :: Sk_text_encoding
pattern GLYPH_ID_SK_TEXT_ENCODING = (#const GLYPH_ID_SK_TEXT_ENCODING)

{- | C enum: @"sk_path_filltype_t"@

@
typedef enum 
{
  WINDING_SK_PATH_FILLTYPE,
  EVENODD_SK_PATH_FILLTYPE,
  INVERSE_WINDING_SK_PATH_FILLTYPE,
  INVERSE_EVENODD_SK_PATH_FILLTYPE
} sk_path_filltype_t
@

-}
newtype Sk_path_filltype = Sk_path_filltype (#type sk_path_filltype_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_path_filltype_t"@ value (1/4): @"WINDING_SK_PATH_FILLTYPE"@
pattern WINDING_SK_PATH_FILLTYPE :: Sk_path_filltype
pattern WINDING_SK_PATH_FILLTYPE = (#const WINDING_SK_PATH_FILLTYPE)

-- | C enum @"sk_path_filltype_t"@ value (2/4): @"EVENODD_SK_PATH_FILLTYPE"@
pattern EVENODD_SK_PATH_FILLTYPE :: Sk_path_filltype
pattern EVENODD_SK_PATH_FILLTYPE = (#const EVENODD_SK_PATH_FILLTYPE)

-- | C enum @"sk_path_filltype_t"@ value (3/4): @"INVERSE_WINDING_SK_PATH_FILLTYPE"@
pattern INVERSE_WINDING_SK_PATH_FILLTYPE :: Sk_path_filltype
pattern INVERSE_WINDING_SK_PATH_FILLTYPE = (#const INVERSE_WINDING_SK_PATH_FILLTYPE)

-- | C enum @"sk_path_filltype_t"@ value (4/4): @"INVERSE_EVENODD_SK_PATH_FILLTYPE"@
pattern INVERSE_EVENODD_SK_PATH_FILLTYPE :: Sk_path_filltype
pattern INVERSE_EVENODD_SK_PATH_FILLTYPE = (#const INVERSE_EVENODD_SK_PATH_FILLTYPE)

{- | C enum: @"sk_font_style_slant_t"@

@
typedef enum 
{
  UPRIGHT_SK_FONT_STYLE_SLANT = 0,
  ITALIC_SK_FONT_STYLE_SLANT = 1,
  OBLIQUE_SK_FONT_STYLE_SLANT = 2
} sk_font_style_slant_t
@

-}
newtype Sk_font_style_slant = Sk_font_style_slant (#type sk_font_style_slant_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_font_style_slant_t"@ value (1/3): @"UPRIGHT_SK_FONT_STYLE_SLANT"@
pattern UPRIGHT_SK_FONT_STYLE_SLANT :: Sk_font_style_slant
pattern UPRIGHT_SK_FONT_STYLE_SLANT = (#const UPRIGHT_SK_FONT_STYLE_SLANT)

-- | C enum @"sk_font_style_slant_t"@ value (2/3): @"ITALIC_SK_FONT_STYLE_SLANT"@
pattern ITALIC_SK_FONT_STYLE_SLANT :: Sk_font_style_slant
pattern ITALIC_SK_FONT_STYLE_SLANT = (#const ITALIC_SK_FONT_STYLE_SLANT)

-- | C enum @"sk_font_style_slant_t"@ value (3/3): @"OBLIQUE_SK_FONT_STYLE_SLANT"@
pattern OBLIQUE_SK_FONT_STYLE_SLANT :: Sk_font_style_slant
pattern OBLIQUE_SK_FONT_STYLE_SLANT = (#const OBLIQUE_SK_FONT_STYLE_SLANT)

{- | C enum: @"sk_color_channel_t"@

@
typedef enum 
{
  R_SK_COLOR_CHANNEL,
  G_SK_COLOR_CHANNEL,
  B_SK_COLOR_CHANNEL,
  A_SK_COLOR_CHANNEL
} sk_color_channel_t
@

-}
newtype Sk_color_channel = Sk_color_channel (#type sk_color_channel_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_color_channel_t"@ value (1/4): @"R_SK_COLOR_CHANNEL"@
pattern R_SK_COLOR_CHANNEL :: Sk_color_channel
pattern R_SK_COLOR_CHANNEL = (#const R_SK_COLOR_CHANNEL)

-- | C enum @"sk_color_channel_t"@ value (2/4): @"G_SK_COLOR_CHANNEL"@
pattern G_SK_COLOR_CHANNEL :: Sk_color_channel
pattern G_SK_COLOR_CHANNEL = (#const G_SK_COLOR_CHANNEL)

-- | C enum @"sk_color_channel_t"@ value (3/4): @"B_SK_COLOR_CHANNEL"@
pattern B_SK_COLOR_CHANNEL :: Sk_color_channel
pattern B_SK_COLOR_CHANNEL = (#const B_SK_COLOR_CHANNEL)

-- | C enum @"sk_color_channel_t"@ value (4/4): @"A_SK_COLOR_CHANNEL"@
pattern A_SK_COLOR_CHANNEL :: Sk_color_channel
pattern A_SK_COLOR_CHANNEL = (#const A_SK_COLOR_CHANNEL)

{- | C enum: @"sk_region_op_t"@

@
typedef enum 
{
  DIFFERENCE_SK_REGION_OP,
  INTERSECT_SK_REGION_OP,
  UNION_SK_REGION_OP,
  XOR_SK_REGION_OP,
  REVERSE_DIFFERENCE_SK_REGION_OP,
  REPLACE_SK_REGION_OP
} sk_region_op_t
@

-}
newtype Sk_region_op = Sk_region_op (#type sk_region_op_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_region_op_t"@ value (1/6): @"DIFFERENCE_SK_REGION_OP"@
pattern DIFFERENCE_SK_REGION_OP :: Sk_region_op
pattern DIFFERENCE_SK_REGION_OP = (#const DIFFERENCE_SK_REGION_OP)

-- | C enum @"sk_region_op_t"@ value (2/6): @"INTERSECT_SK_REGION_OP"@
pattern INTERSECT_SK_REGION_OP :: Sk_region_op
pattern INTERSECT_SK_REGION_OP = (#const INTERSECT_SK_REGION_OP)

-- | C enum @"sk_region_op_t"@ value (3/6): @"UNION_SK_REGION_OP"@
pattern UNION_SK_REGION_OP :: Sk_region_op
pattern UNION_SK_REGION_OP = (#const UNION_SK_REGION_OP)

-- | C enum @"sk_region_op_t"@ value (4/6): @"XOR_SK_REGION_OP"@
pattern XOR_SK_REGION_OP :: Sk_region_op
pattern XOR_SK_REGION_OP = (#const XOR_SK_REGION_OP)

-- | C enum @"sk_region_op_t"@ value (5/6): @"REVERSE_DIFFERENCE_SK_REGION_OP"@
pattern REVERSE_DIFFERENCE_SK_REGION_OP :: Sk_region_op
pattern REVERSE_DIFFERENCE_SK_REGION_OP = (#const REVERSE_DIFFERENCE_SK_REGION_OP)

-- | C enum @"sk_region_op_t"@ value (6/6): @"REPLACE_SK_REGION_OP"@
pattern REPLACE_SK_REGION_OP :: Sk_region_op
pattern REPLACE_SK_REGION_OP = (#const REPLACE_SK_REGION_OP)

{- | C enum: @"sk_clipop_t"@

@
typedef enum 
{
  DIFFERENCE_SK_CLIPOP,
  INTERSECT_SK_CLIPOP
} sk_clipop_t
@

-}
newtype Sk_clipop = Sk_clipop (#type sk_clipop_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_clipop_t"@ value (1/2): @"DIFFERENCE_SK_CLIPOP"@
pattern DIFFERENCE_SK_CLIPOP :: Sk_clipop
pattern DIFFERENCE_SK_CLIPOP = (#const DIFFERENCE_SK_CLIPOP)

-- | C enum @"sk_clipop_t"@ value (2/2): @"INTERSECT_SK_CLIPOP"@
pattern INTERSECT_SK_CLIPOP :: Sk_clipop
pattern INTERSECT_SK_CLIPOP = (#const INTERSECT_SK_CLIPOP)

{- | C enum: @"sk_encoded_image_format_t"@

@
typedef enum 
{
  BMP_SK_ENCODED_FORMAT,
  GIF_SK_ENCODED_FORMAT,
  ICO_SK_ENCODED_FORMAT,
  JPEG_SK_ENCODED_FORMAT,
  PNG_SK_ENCODED_FORMAT,
  WBMP_SK_ENCODED_FORMAT,
  WEBP_SK_ENCODED_FORMAT,
  PKM_SK_ENCODED_FORMAT,
  KTX_SK_ENCODED_FORMAT,
  ASTC_SK_ENCODED_FORMAT,
  DNG_SK_ENCODED_FORMAT,
  HEIF_SK_ENCODED_FORMAT,
  AVIF_SK_ENCODED_FORMAT,
  JPEGXL_SK_ENCODED_FORMAT
} sk_encoded_image_format_t
@

-}
newtype Sk_encoded_image_format = Sk_encoded_image_format (#type sk_encoded_image_format_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_encoded_image_format_t"@ value (1/14): @"BMP_SK_ENCODED_FORMAT"@
pattern BMP_SK_ENCODED_FORMAT :: Sk_encoded_image_format
pattern BMP_SK_ENCODED_FORMAT = (#const BMP_SK_ENCODED_FORMAT)

-- | C enum @"sk_encoded_image_format_t"@ value (2/14): @"GIF_SK_ENCODED_FORMAT"@
pattern GIF_SK_ENCODED_FORMAT :: Sk_encoded_image_format
pattern GIF_SK_ENCODED_FORMAT = (#const GIF_SK_ENCODED_FORMAT)

-- | C enum @"sk_encoded_image_format_t"@ value (3/14): @"ICO_SK_ENCODED_FORMAT"@
pattern ICO_SK_ENCODED_FORMAT :: Sk_encoded_image_format
pattern ICO_SK_ENCODED_FORMAT = (#const ICO_SK_ENCODED_FORMAT)

-- | C enum @"sk_encoded_image_format_t"@ value (4/14): @"JPEG_SK_ENCODED_FORMAT"@
pattern JPEG_SK_ENCODED_FORMAT :: Sk_encoded_image_format
pattern JPEG_SK_ENCODED_FORMAT = (#const JPEG_SK_ENCODED_FORMAT)

-- | C enum @"sk_encoded_image_format_t"@ value (5/14): @"PNG_SK_ENCODED_FORMAT"@
pattern PNG_SK_ENCODED_FORMAT :: Sk_encoded_image_format
pattern PNG_SK_ENCODED_FORMAT = (#const PNG_SK_ENCODED_FORMAT)

-- | C enum @"sk_encoded_image_format_t"@ value (6/14): @"WBMP_SK_ENCODED_FORMAT"@
pattern WBMP_SK_ENCODED_FORMAT :: Sk_encoded_image_format
pattern WBMP_SK_ENCODED_FORMAT = (#const WBMP_SK_ENCODED_FORMAT)

-- | C enum @"sk_encoded_image_format_t"@ value (7/14): @"WEBP_SK_ENCODED_FORMAT"@
pattern WEBP_SK_ENCODED_FORMAT :: Sk_encoded_image_format
pattern WEBP_SK_ENCODED_FORMAT = (#const WEBP_SK_ENCODED_FORMAT)

-- | C enum @"sk_encoded_image_format_t"@ value (8/14): @"PKM_SK_ENCODED_FORMAT"@
pattern PKM_SK_ENCODED_FORMAT :: Sk_encoded_image_format
pattern PKM_SK_ENCODED_FORMAT = (#const PKM_SK_ENCODED_FORMAT)

-- | C enum @"sk_encoded_image_format_t"@ value (9/14): @"KTX_SK_ENCODED_FORMAT"@
pattern KTX_SK_ENCODED_FORMAT :: Sk_encoded_image_format
pattern KTX_SK_ENCODED_FORMAT = (#const KTX_SK_ENCODED_FORMAT)

-- | C enum @"sk_encoded_image_format_t"@ value (10/14): @"ASTC_SK_ENCODED_FORMAT"@
pattern ASTC_SK_ENCODED_FORMAT :: Sk_encoded_image_format
pattern ASTC_SK_ENCODED_FORMAT = (#const ASTC_SK_ENCODED_FORMAT)

-- | C enum @"sk_encoded_image_format_t"@ value (11/14): @"DNG_SK_ENCODED_FORMAT"@
pattern DNG_SK_ENCODED_FORMAT :: Sk_encoded_image_format
pattern DNG_SK_ENCODED_FORMAT = (#const DNG_SK_ENCODED_FORMAT)

-- | C enum @"sk_encoded_image_format_t"@ value (12/14): @"HEIF_SK_ENCODED_FORMAT"@
pattern HEIF_SK_ENCODED_FORMAT :: Sk_encoded_image_format
pattern HEIF_SK_ENCODED_FORMAT = (#const HEIF_SK_ENCODED_FORMAT)

-- | C enum @"sk_encoded_image_format_t"@ value (13/14): @"AVIF_SK_ENCODED_FORMAT"@
pattern AVIF_SK_ENCODED_FORMAT :: Sk_encoded_image_format
pattern AVIF_SK_ENCODED_FORMAT = (#const AVIF_SK_ENCODED_FORMAT)

-- | C enum @"sk_encoded_image_format_t"@ value (14/14): @"JPEGXL_SK_ENCODED_FORMAT"@
pattern JPEGXL_SK_ENCODED_FORMAT :: Sk_encoded_image_format
pattern JPEGXL_SK_ENCODED_FORMAT = (#const JPEGXL_SK_ENCODED_FORMAT)

{- | C enum: @"sk_encodedorigin_t"@

@
typedef enum 
{
  TOP_LEFT_SK_ENCODED_ORIGIN = 1,
  TOP_RIGHT_SK_ENCODED_ORIGIN = 2,
  BOTTOM_RIGHT_SK_ENCODED_ORIGIN = 3,
  BOTTOM_LEFT_SK_ENCODED_ORIGIN = 4,
  LEFT_TOP_SK_ENCODED_ORIGIN = 5,
  RIGHT_TOP_SK_ENCODED_ORIGIN = 6,
  RIGHT_BOTTOM_SK_ENCODED_ORIGIN = 7,
  LEFT_BOTTOM_SK_ENCODED_ORIGIN = 8,
  DEFAULT_SK_ENCODED_ORIGIN = TOP_LEFT_SK_ENCODED_ORIGIN
} sk_encodedorigin_t
@

-}
newtype Sk_encodedorigin = Sk_encodedorigin (#type sk_encodedorigin_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_encodedorigin_t"@ value (1/9): @"TOP_LEFT_SK_ENCODED_ORIGIN"@
pattern TOP_LEFT_SK_ENCODED_ORIGIN :: Sk_encodedorigin
pattern TOP_LEFT_SK_ENCODED_ORIGIN = (#const TOP_LEFT_SK_ENCODED_ORIGIN)

-- | C enum @"sk_encodedorigin_t"@ value (2/9): @"TOP_RIGHT_SK_ENCODED_ORIGIN"@
pattern TOP_RIGHT_SK_ENCODED_ORIGIN :: Sk_encodedorigin
pattern TOP_RIGHT_SK_ENCODED_ORIGIN = (#const TOP_RIGHT_SK_ENCODED_ORIGIN)

-- | C enum @"sk_encodedorigin_t"@ value (3/9): @"BOTTOM_RIGHT_SK_ENCODED_ORIGIN"@
pattern BOTTOM_RIGHT_SK_ENCODED_ORIGIN :: Sk_encodedorigin
pattern BOTTOM_RIGHT_SK_ENCODED_ORIGIN = (#const BOTTOM_RIGHT_SK_ENCODED_ORIGIN)

-- | C enum @"sk_encodedorigin_t"@ value (4/9): @"BOTTOM_LEFT_SK_ENCODED_ORIGIN"@
pattern BOTTOM_LEFT_SK_ENCODED_ORIGIN :: Sk_encodedorigin
pattern BOTTOM_LEFT_SK_ENCODED_ORIGIN = (#const BOTTOM_LEFT_SK_ENCODED_ORIGIN)

-- | C enum @"sk_encodedorigin_t"@ value (5/9): @"LEFT_TOP_SK_ENCODED_ORIGIN"@
pattern LEFT_TOP_SK_ENCODED_ORIGIN :: Sk_encodedorigin
pattern LEFT_TOP_SK_ENCODED_ORIGIN = (#const LEFT_TOP_SK_ENCODED_ORIGIN)

-- | C enum @"sk_encodedorigin_t"@ value (6/9): @"RIGHT_TOP_SK_ENCODED_ORIGIN"@
pattern RIGHT_TOP_SK_ENCODED_ORIGIN :: Sk_encodedorigin
pattern RIGHT_TOP_SK_ENCODED_ORIGIN = (#const RIGHT_TOP_SK_ENCODED_ORIGIN)

-- | C enum @"sk_encodedorigin_t"@ value (7/9): @"RIGHT_BOTTOM_SK_ENCODED_ORIGIN"@
pattern RIGHT_BOTTOM_SK_ENCODED_ORIGIN :: Sk_encodedorigin
pattern RIGHT_BOTTOM_SK_ENCODED_ORIGIN = (#const RIGHT_BOTTOM_SK_ENCODED_ORIGIN)

-- | C enum @"sk_encodedorigin_t"@ value (8/9): @"LEFT_BOTTOM_SK_ENCODED_ORIGIN"@
pattern LEFT_BOTTOM_SK_ENCODED_ORIGIN :: Sk_encodedorigin
pattern LEFT_BOTTOM_SK_ENCODED_ORIGIN = (#const LEFT_BOTTOM_SK_ENCODED_ORIGIN)

-- | C enum @"sk_encodedorigin_t"@ value (9/9): @"DEFAULT_SK_ENCODED_ORIGIN"@
pattern DEFAULT_SK_ENCODED_ORIGIN :: Sk_encodedorigin
pattern DEFAULT_SK_ENCODED_ORIGIN = (#const DEFAULT_SK_ENCODED_ORIGIN)

{- | C enum: @"sk_codec_result_t"@

@
typedef enum 
{
  SUCCESS_SK_CODEC_RESULT,
  INCOMPLETE_INPUT_SK_CODEC_RESULT,
  ERROR_IN_INPUT_SK_CODEC_RESULT,
  INVALID_CONVERSION_SK_CODEC_RESULT,
  INVALID_SCALE_SK_CODEC_RESULT,
  INVALID_PARAMETERS_SK_CODEC_RESULT,
  INVALID_INPUT_SK_CODEC_RESULT,
  COULD_NOT_REWIND_SK_CODEC_RESULT,
  INTERNAL_ERROR_SK_CODEC_RESULT,
  UNIMPLEMENTED_SK_CODEC_RESULT
} sk_codec_result_t
@

-}
newtype Sk_codec_result = Sk_codec_result (#type sk_codec_result_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_codec_result_t"@ value (1/10): @"SUCCESS_SK_CODEC_RESULT"@
pattern SUCCESS_SK_CODEC_RESULT :: Sk_codec_result
pattern SUCCESS_SK_CODEC_RESULT = (#const SUCCESS_SK_CODEC_RESULT)

-- | C enum @"sk_codec_result_t"@ value (2/10): @"INCOMPLETE_INPUT_SK_CODEC_RESULT"@
pattern INCOMPLETE_INPUT_SK_CODEC_RESULT :: Sk_codec_result
pattern INCOMPLETE_INPUT_SK_CODEC_RESULT = (#const INCOMPLETE_INPUT_SK_CODEC_RESULT)

-- | C enum @"sk_codec_result_t"@ value (3/10): @"ERROR_IN_INPUT_SK_CODEC_RESULT"@
pattern ERROR_IN_INPUT_SK_CODEC_RESULT :: Sk_codec_result
pattern ERROR_IN_INPUT_SK_CODEC_RESULT = (#const ERROR_IN_INPUT_SK_CODEC_RESULT)

-- | C enum @"sk_codec_result_t"@ value (4/10): @"INVALID_CONVERSION_SK_CODEC_RESULT"@
pattern INVALID_CONVERSION_SK_CODEC_RESULT :: Sk_codec_result
pattern INVALID_CONVERSION_SK_CODEC_RESULT = (#const INVALID_CONVERSION_SK_CODEC_RESULT)

-- | C enum @"sk_codec_result_t"@ value (5/10): @"INVALID_SCALE_SK_CODEC_RESULT"@
pattern INVALID_SCALE_SK_CODEC_RESULT :: Sk_codec_result
pattern INVALID_SCALE_SK_CODEC_RESULT = (#const INVALID_SCALE_SK_CODEC_RESULT)

-- | C enum @"sk_codec_result_t"@ value (6/10): @"INVALID_PARAMETERS_SK_CODEC_RESULT"@
pattern INVALID_PARAMETERS_SK_CODEC_RESULT :: Sk_codec_result
pattern INVALID_PARAMETERS_SK_CODEC_RESULT = (#const INVALID_PARAMETERS_SK_CODEC_RESULT)

-- | C enum @"sk_codec_result_t"@ value (7/10): @"INVALID_INPUT_SK_CODEC_RESULT"@
pattern INVALID_INPUT_SK_CODEC_RESULT :: Sk_codec_result
pattern INVALID_INPUT_SK_CODEC_RESULT = (#const INVALID_INPUT_SK_CODEC_RESULT)

-- | C enum @"sk_codec_result_t"@ value (8/10): @"COULD_NOT_REWIND_SK_CODEC_RESULT"@
pattern COULD_NOT_REWIND_SK_CODEC_RESULT :: Sk_codec_result
pattern COULD_NOT_REWIND_SK_CODEC_RESULT = (#const COULD_NOT_REWIND_SK_CODEC_RESULT)

-- | C enum @"sk_codec_result_t"@ value (9/10): @"INTERNAL_ERROR_SK_CODEC_RESULT"@
pattern INTERNAL_ERROR_SK_CODEC_RESULT :: Sk_codec_result
pattern INTERNAL_ERROR_SK_CODEC_RESULT = (#const INTERNAL_ERROR_SK_CODEC_RESULT)

-- | C enum @"sk_codec_result_t"@ value (10/10): @"UNIMPLEMENTED_SK_CODEC_RESULT"@
pattern UNIMPLEMENTED_SK_CODEC_RESULT :: Sk_codec_result
pattern UNIMPLEMENTED_SK_CODEC_RESULT = (#const UNIMPLEMENTED_SK_CODEC_RESULT)

{- | C enum: @"sk_codec_zero_initialized_t"@

@
typedef enum 
{
  YES_SK_CODEC_ZERO_INITIALIZED,
  NO_SK_CODEC_ZERO_INITIALIZED
} sk_codec_zero_initialized_t
@

-}
newtype Sk_codec_zero_initialized = Sk_codec_zero_initialized (#type sk_codec_zero_initialized_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_codec_zero_initialized_t"@ value (1/2): @"YES_SK_CODEC_ZERO_INITIALIZED"@
pattern YES_SK_CODEC_ZERO_INITIALIZED :: Sk_codec_zero_initialized
pattern YES_SK_CODEC_ZERO_INITIALIZED = (#const YES_SK_CODEC_ZERO_INITIALIZED)

-- | C enum @"sk_codec_zero_initialized_t"@ value (2/2): @"NO_SK_CODEC_ZERO_INITIALIZED"@
pattern NO_SK_CODEC_ZERO_INITIALIZED :: Sk_codec_zero_initialized
pattern NO_SK_CODEC_ZERO_INITIALIZED = (#const NO_SK_CODEC_ZERO_INITIALIZED)

{- | C struct: @"sk_codec_options_t"@

@
typedef struct 
{
  sk_codec_zero_initialized_t fZeroInitialized;
  sk_irect_t *fSubset;
  int fFrameIndex;
  int fPriorFrame;
} sk_codec_options_t
@
-}
data Sk_codec_options = Sk_codec_options
  { fZeroInitialized :: Sk_codec_zero_initialized -- ^ C field: @"sk_codec_zero_initialized_t fZeroInitialized"@
  , fSubset :: Ptr (Sk_irect) -- ^ C field: @"sk_irect_t *fSubset"@
  , fFrameIndex :: CInt -- ^ C field: @"int fFrameIndex"@
  , fPriorFrame :: CInt -- ^ C field: @"int fPriorFrame"@
  }
instance Foreign.Storable.Offset.Offset "fZeroInitialized" Sk_codec_options where
  rawOffset = (#offset sk_codec_options_t, fZeroInitialized)
instance Foreign.Storable.Offset.Offset "fSubset" Sk_codec_options where
  rawOffset = (#offset sk_codec_options_t, fSubset)
instance Foreign.Storable.Offset.Offset "fFrameIndex" Sk_codec_options where
  rawOffset = (#offset sk_codec_options_t, fFrameIndex)
instance Foreign.Storable.Offset.Offset "fPriorFrame" Sk_codec_options where
  rawOffset = (#offset sk_codec_options_t, fPriorFrame)
instance Foreign.Storable.Storable Sk_codec_options where
  sizeOf _ = (#size sk_codec_options_t)
  alignment _ = (#alignment sk_codec_options_t)
  peek p' = do
    fZeroInitialized <- (#peek sk_codec_options_t, fZeroInitialized) p'
    fSubset <- (#peek sk_codec_options_t, fSubset) p'
    fFrameIndex <- (#peek sk_codec_options_t, fFrameIndex) p'
    fPriorFrame <- (#peek sk_codec_options_t, fPriorFrame) p'
    pure Sk_codec_options{..}
  poke p' Sk_codec_options{..} = do
    (#poke sk_codec_options_t, fZeroInitialized) p' fZeroInitialized
    (#poke sk_codec_options_t, fSubset) p' fSubset
    (#poke sk_codec_options_t, fFrameIndex) p' fFrameIndex
    (#poke sk_codec_options_t, fPriorFrame) p' fPriorFrame

{- | C enum: @"sk_codec_scanline_order_t"@

@
typedef enum 
{
  TOP_DOWN_SK_CODEC_SCANLINE_ORDER,
  BOTTOM_UP_SK_CODEC_SCANLINE_ORDER
} sk_codec_scanline_order_t
@

-}
newtype Sk_codec_scanline_order = Sk_codec_scanline_order (#type sk_codec_scanline_order_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_codec_scanline_order_t"@ value (1/2): @"TOP_DOWN_SK_CODEC_SCANLINE_ORDER"@
pattern TOP_DOWN_SK_CODEC_SCANLINE_ORDER :: Sk_codec_scanline_order
pattern TOP_DOWN_SK_CODEC_SCANLINE_ORDER = (#const TOP_DOWN_SK_CODEC_SCANLINE_ORDER)

-- | C enum @"sk_codec_scanline_order_t"@ value (2/2): @"BOTTOM_UP_SK_CODEC_SCANLINE_ORDER"@
pattern BOTTOM_UP_SK_CODEC_SCANLINE_ORDER :: Sk_codec_scanline_order
pattern BOTTOM_UP_SK_CODEC_SCANLINE_ORDER = (#const BOTTOM_UP_SK_CODEC_SCANLINE_ORDER)

{- | C enum: @"sk_path_verb_t"@

@
typedef enum 
{
  MOVE_SK_PATH_VERB,
  LINE_SK_PATH_VERB,
  QUAD_SK_PATH_VERB,
  CONIC_SK_PATH_VERB,
  CUBIC_SK_PATH_VERB,
  CLOSE_SK_PATH_VERB,
  DONE_SK_PATH_VERB
} sk_path_verb_t
@

-}
newtype Sk_path_verb = Sk_path_verb (#type sk_path_verb_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_path_verb_t"@ value (1/7): @"MOVE_SK_PATH_VERB"@
pattern MOVE_SK_PATH_VERB :: Sk_path_verb
pattern MOVE_SK_PATH_VERB = (#const MOVE_SK_PATH_VERB)

-- | C enum @"sk_path_verb_t"@ value (2/7): @"LINE_SK_PATH_VERB"@
pattern LINE_SK_PATH_VERB :: Sk_path_verb
pattern LINE_SK_PATH_VERB = (#const LINE_SK_PATH_VERB)

-- | C enum @"sk_path_verb_t"@ value (3/7): @"QUAD_SK_PATH_VERB"@
pattern QUAD_SK_PATH_VERB :: Sk_path_verb
pattern QUAD_SK_PATH_VERB = (#const QUAD_SK_PATH_VERB)

-- | C enum @"sk_path_verb_t"@ value (4/7): @"CONIC_SK_PATH_VERB"@
pattern CONIC_SK_PATH_VERB :: Sk_path_verb
pattern CONIC_SK_PATH_VERB = (#const CONIC_SK_PATH_VERB)

-- | C enum @"sk_path_verb_t"@ value (5/7): @"CUBIC_SK_PATH_VERB"@
pattern CUBIC_SK_PATH_VERB :: Sk_path_verb
pattern CUBIC_SK_PATH_VERB = (#const CUBIC_SK_PATH_VERB)

-- | C enum @"sk_path_verb_t"@ value (6/7): @"CLOSE_SK_PATH_VERB"@
pattern CLOSE_SK_PATH_VERB :: Sk_path_verb
pattern CLOSE_SK_PATH_VERB = (#const CLOSE_SK_PATH_VERB)

-- | C enum @"sk_path_verb_t"@ value (7/7): @"DONE_SK_PATH_VERB"@
pattern DONE_SK_PATH_VERB :: Sk_path_verb
pattern DONE_SK_PATH_VERB = (#const DONE_SK_PATH_VERB)

{- | Opaque C struct: @"sk_path_iterator_t"@
-}
data Sk_path_iterator = Sk_path_iterator

{- | Opaque C struct: @"sk_path_rawiterator_t"@
-}
data Sk_path_rawiterator = Sk_path_rawiterator

{- | C enum: @"sk_path_add_mode_t"@

@
typedef enum 
{
  APPEND_SK_PATH_ADD_MODE,
  EXTEND_SK_PATH_ADD_MODE
} sk_path_add_mode_t
@

-}
newtype Sk_path_add_mode = Sk_path_add_mode (#type sk_path_add_mode_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_path_add_mode_t"@ value (1/2): @"APPEND_SK_PATH_ADD_MODE"@
pattern APPEND_SK_PATH_ADD_MODE :: Sk_path_add_mode
pattern APPEND_SK_PATH_ADD_MODE = (#const APPEND_SK_PATH_ADD_MODE)

-- | C enum @"sk_path_add_mode_t"@ value (2/2): @"EXTEND_SK_PATH_ADD_MODE"@
pattern EXTEND_SK_PATH_ADD_MODE :: Sk_path_add_mode
pattern EXTEND_SK_PATH_ADD_MODE = (#const EXTEND_SK_PATH_ADD_MODE)

{- | C enum: @"sk_path_segment_mask_t"@

@
typedef enum 
{
  LINE_SK_PATH_SEGMENT_MASK = 1 << 0,
  QUAD_SK_PATH_SEGMENT_MASK = 1 << 1,
  CONIC_SK_PATH_SEGMENT_MASK = 1 << 2,
  CUBIC_SK_PATH_SEGMENT_MASK = 1 << 3
} sk_path_segment_mask_t
@

-}
newtype Sk_path_segment_mask = Sk_path_segment_mask (#type sk_path_segment_mask_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_path_segment_mask_t"@ value (1/4): @"LINE_SK_PATH_SEGMENT_MASK"@
pattern LINE_SK_PATH_SEGMENT_MASK :: Sk_path_segment_mask
pattern LINE_SK_PATH_SEGMENT_MASK = (#const LINE_SK_PATH_SEGMENT_MASK)

-- | C enum @"sk_path_segment_mask_t"@ value (2/4): @"QUAD_SK_PATH_SEGMENT_MASK"@
pattern QUAD_SK_PATH_SEGMENT_MASK :: Sk_path_segment_mask
pattern QUAD_SK_PATH_SEGMENT_MASK = (#const QUAD_SK_PATH_SEGMENT_MASK)

-- | C enum @"sk_path_segment_mask_t"@ value (3/4): @"CONIC_SK_PATH_SEGMENT_MASK"@
pattern CONIC_SK_PATH_SEGMENT_MASK :: Sk_path_segment_mask
pattern CONIC_SK_PATH_SEGMENT_MASK = (#const CONIC_SK_PATH_SEGMENT_MASK)

-- | C enum @"sk_path_segment_mask_t"@ value (4/4): @"CUBIC_SK_PATH_SEGMENT_MASK"@
pattern CUBIC_SK_PATH_SEGMENT_MASK :: Sk_path_segment_mask
pattern CUBIC_SK_PATH_SEGMENT_MASK = (#const CUBIC_SK_PATH_SEGMENT_MASK)

{- | C enum: @"sk_path_effect_1d_style_t"@

@
typedef enum 
{
  TRANSLATE_SK_PATH_EFFECT_1D_STYLE,
  ROTATE_SK_PATH_EFFECT_1D_STYLE,
  MORPH_SK_PATH_EFFECT_1D_STYLE
} sk_path_effect_1d_style_t
@

-}
newtype Sk_path_effect_1d_style = Sk_path_effect_1d_style (#type sk_path_effect_1d_style_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_path_effect_1d_style_t"@ value (1/3): @"TRANSLATE_SK_PATH_EFFECT_1D_STYLE"@
pattern TRANSLATE_SK_PATH_EFFECT_1D_STYLE :: Sk_path_effect_1d_style
pattern TRANSLATE_SK_PATH_EFFECT_1D_STYLE = (#const TRANSLATE_SK_PATH_EFFECT_1D_STYLE)

-- | C enum @"sk_path_effect_1d_style_t"@ value (2/3): @"ROTATE_SK_PATH_EFFECT_1D_STYLE"@
pattern ROTATE_SK_PATH_EFFECT_1D_STYLE :: Sk_path_effect_1d_style
pattern ROTATE_SK_PATH_EFFECT_1D_STYLE = (#const ROTATE_SK_PATH_EFFECT_1D_STYLE)

-- | C enum @"sk_path_effect_1d_style_t"@ value (3/3): @"MORPH_SK_PATH_EFFECT_1D_STYLE"@
pattern MORPH_SK_PATH_EFFECT_1D_STYLE :: Sk_path_effect_1d_style
pattern MORPH_SK_PATH_EFFECT_1D_STYLE = (#const MORPH_SK_PATH_EFFECT_1D_STYLE)

{- | C enum: @"sk_path_effect_trim_mode_t"@

@
typedef enum 
{
  NORMAL_SK_PATH_EFFECT_TRIM_MODE,
  INVERTED_SK_PATH_EFFECT_TRIM_MODE
} sk_path_effect_trim_mode_t
@

-}
newtype Sk_path_effect_trim_mode = Sk_path_effect_trim_mode (#type sk_path_effect_trim_mode_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_path_effect_trim_mode_t"@ value (1/2): @"NORMAL_SK_PATH_EFFECT_TRIM_MODE"@
pattern NORMAL_SK_PATH_EFFECT_TRIM_MODE :: Sk_path_effect_trim_mode
pattern NORMAL_SK_PATH_EFFECT_TRIM_MODE = (#const NORMAL_SK_PATH_EFFECT_TRIM_MODE)

-- | C enum @"sk_path_effect_trim_mode_t"@ value (2/2): @"INVERTED_SK_PATH_EFFECT_TRIM_MODE"@
pattern INVERTED_SK_PATH_EFFECT_TRIM_MODE :: Sk_path_effect_trim_mode
pattern INVERTED_SK_PATH_EFFECT_TRIM_MODE = (#const INVERTED_SK_PATH_EFFECT_TRIM_MODE)

{- | Opaque C struct: @"sk_path_effect_t"@
-}
data Sk_path_effect = Sk_path_effect

{- | C enum: @"sk_stroke_cap_t"@

@
typedef enum 
{
  BUTT_SK_STROKE_CAP,
  ROUND_SK_STROKE_CAP,
  SQUARE_SK_STROKE_CAP
} sk_stroke_cap_t
@

-}
newtype Sk_stroke_cap = Sk_stroke_cap (#type sk_stroke_cap_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_stroke_cap_t"@ value (1/3): @"BUTT_SK_STROKE_CAP"@
pattern BUTT_SK_STROKE_CAP :: Sk_stroke_cap
pattern BUTT_SK_STROKE_CAP = (#const BUTT_SK_STROKE_CAP)

-- | C enum @"sk_stroke_cap_t"@ value (2/3): @"ROUND_SK_STROKE_CAP"@
pattern ROUND_SK_STROKE_CAP :: Sk_stroke_cap
pattern ROUND_SK_STROKE_CAP = (#const ROUND_SK_STROKE_CAP)

-- | C enum @"sk_stroke_cap_t"@ value (3/3): @"SQUARE_SK_STROKE_CAP"@
pattern SQUARE_SK_STROKE_CAP :: Sk_stroke_cap
pattern SQUARE_SK_STROKE_CAP = (#const SQUARE_SK_STROKE_CAP)

{- | C enum: @"sk_stroke_join_t"@

@
typedef enum 
{
  MITER_SK_STROKE_JOIN,
  ROUND_SK_STROKE_JOIN,
  BEVEL_SK_STROKE_JOIN
} sk_stroke_join_t
@

-}
newtype Sk_stroke_join = Sk_stroke_join (#type sk_stroke_join_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_stroke_join_t"@ value (1/3): @"MITER_SK_STROKE_JOIN"@
pattern MITER_SK_STROKE_JOIN :: Sk_stroke_join
pattern MITER_SK_STROKE_JOIN = (#const MITER_SK_STROKE_JOIN)

-- | C enum @"sk_stroke_join_t"@ value (2/3): @"ROUND_SK_STROKE_JOIN"@
pattern ROUND_SK_STROKE_JOIN :: Sk_stroke_join
pattern ROUND_SK_STROKE_JOIN = (#const ROUND_SK_STROKE_JOIN)

-- | C enum @"sk_stroke_join_t"@ value (3/3): @"BEVEL_SK_STROKE_JOIN"@
pattern BEVEL_SK_STROKE_JOIN :: Sk_stroke_join
pattern BEVEL_SK_STROKE_JOIN = (#const BEVEL_SK_STROKE_JOIN)

{- | C enum: @"sk_shader_tilemode_t"@

@
typedef enum 
{
  CLAMP_SK_SHADER_TILEMODE,
  REPEAT_SK_SHADER_TILEMODE,
  MIRROR_SK_SHADER_TILEMODE,
  DECAL_SK_SHADER_TILEMODE
} sk_shader_tilemode_t
@

-}
newtype Sk_shader_tilemode = Sk_shader_tilemode (#type sk_shader_tilemode_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_shader_tilemode_t"@ value (1/4): @"CLAMP_SK_SHADER_TILEMODE"@
pattern CLAMP_SK_SHADER_TILEMODE :: Sk_shader_tilemode
pattern CLAMP_SK_SHADER_TILEMODE = (#const CLAMP_SK_SHADER_TILEMODE)

-- | C enum @"sk_shader_tilemode_t"@ value (2/4): @"REPEAT_SK_SHADER_TILEMODE"@
pattern REPEAT_SK_SHADER_TILEMODE :: Sk_shader_tilemode
pattern REPEAT_SK_SHADER_TILEMODE = (#const REPEAT_SK_SHADER_TILEMODE)

-- | C enum @"sk_shader_tilemode_t"@ value (3/4): @"MIRROR_SK_SHADER_TILEMODE"@
pattern MIRROR_SK_SHADER_TILEMODE :: Sk_shader_tilemode
pattern MIRROR_SK_SHADER_TILEMODE = (#const MIRROR_SK_SHADER_TILEMODE)

-- | C enum @"sk_shader_tilemode_t"@ value (4/4): @"DECAL_SK_SHADER_TILEMODE"@
pattern DECAL_SK_SHADER_TILEMODE :: Sk_shader_tilemode
pattern DECAL_SK_SHADER_TILEMODE = (#const DECAL_SK_SHADER_TILEMODE)

{- | C enum: @"sk_blurstyle_t"@

@
typedef enum 
{
  NORMAL_SK_BLUR_STYLE,
  SOLID_SK_BLUR_STYLE,
  OUTER_SK_BLUR_STYLE,
  INNER_SK_BLUR_STYLE
} sk_blurstyle_t
@

-}
newtype Sk_blurstyle = Sk_blurstyle (#type sk_blurstyle_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_blurstyle_t"@ value (1/4): @"NORMAL_SK_BLUR_STYLE"@
pattern NORMAL_SK_BLUR_STYLE :: Sk_blurstyle
pattern NORMAL_SK_BLUR_STYLE = (#const NORMAL_SK_BLUR_STYLE)

-- | C enum @"sk_blurstyle_t"@ value (2/4): @"SOLID_SK_BLUR_STYLE"@
pattern SOLID_SK_BLUR_STYLE :: Sk_blurstyle
pattern SOLID_SK_BLUR_STYLE = (#const SOLID_SK_BLUR_STYLE)

-- | C enum @"sk_blurstyle_t"@ value (3/4): @"OUTER_SK_BLUR_STYLE"@
pattern OUTER_SK_BLUR_STYLE :: Sk_blurstyle
pattern OUTER_SK_BLUR_STYLE = (#const OUTER_SK_BLUR_STYLE)

-- | C enum @"sk_blurstyle_t"@ value (4/4): @"INNER_SK_BLUR_STYLE"@
pattern INNER_SK_BLUR_STYLE :: Sk_blurstyle
pattern INNER_SK_BLUR_STYLE = (#const INNER_SK_BLUR_STYLE)

{- | C enum: @"sk_path_direction_t"@

@
typedef enum 
{
  CW_SK_PATH_DIRECTION,
  CCW_SK_PATH_DIRECTION
} sk_path_direction_t
@

-}
newtype Sk_path_direction = Sk_path_direction (#type sk_path_direction_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_path_direction_t"@ value (1/2): @"CW_SK_PATH_DIRECTION"@
pattern CW_SK_PATH_DIRECTION :: Sk_path_direction
pattern CW_SK_PATH_DIRECTION = (#const CW_SK_PATH_DIRECTION)

-- | C enum @"sk_path_direction_t"@ value (2/2): @"CCW_SK_PATH_DIRECTION"@
pattern CCW_SK_PATH_DIRECTION :: Sk_path_direction
pattern CCW_SK_PATH_DIRECTION = (#const CCW_SK_PATH_DIRECTION)

{- | C enum: @"sk_path_arc_size_t"@

@
typedef enum 
{
  SMALL_SK_PATH_ARC_SIZE,
  LARGE_SK_PATH_ARC_SIZE
} sk_path_arc_size_t
@

-}
newtype Sk_path_arc_size = Sk_path_arc_size (#type sk_path_arc_size_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_path_arc_size_t"@ value (1/2): @"SMALL_SK_PATH_ARC_SIZE"@
pattern SMALL_SK_PATH_ARC_SIZE :: Sk_path_arc_size
pattern SMALL_SK_PATH_ARC_SIZE = (#const SMALL_SK_PATH_ARC_SIZE)

-- | C enum @"sk_path_arc_size_t"@ value (2/2): @"LARGE_SK_PATH_ARC_SIZE"@
pattern LARGE_SK_PATH_ARC_SIZE :: Sk_path_arc_size
pattern LARGE_SK_PATH_ARC_SIZE = (#const LARGE_SK_PATH_ARC_SIZE)

{- | C enum: @"sk_paint_style_t"@

@
typedef enum 
{
  FILL_SK_PAINT_STYLE,
  STROKE_SK_PAINT_STYLE,
  STROKE_AND_FILL_SK_PAINT_STYLE
} sk_paint_style_t
@

-}
newtype Sk_paint_style = Sk_paint_style (#type sk_paint_style_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_paint_style_t"@ value (1/3): @"FILL_SK_PAINT_STYLE"@
pattern FILL_SK_PAINT_STYLE :: Sk_paint_style
pattern FILL_SK_PAINT_STYLE = (#const FILL_SK_PAINT_STYLE)

-- | C enum @"sk_paint_style_t"@ value (2/3): @"STROKE_SK_PAINT_STYLE"@
pattern STROKE_SK_PAINT_STYLE :: Sk_paint_style
pattern STROKE_SK_PAINT_STYLE = (#const STROKE_SK_PAINT_STYLE)

-- | C enum @"sk_paint_style_t"@ value (3/3): @"STROKE_AND_FILL_SK_PAINT_STYLE"@
pattern STROKE_AND_FILL_SK_PAINT_STYLE :: Sk_paint_style
pattern STROKE_AND_FILL_SK_PAINT_STYLE = (#const STROKE_AND_FILL_SK_PAINT_STYLE)

{- | C enum: @"sk_font_hinting_t"@

@
typedef enum 
{
  NONE_SK_FONT_HINTING,
  SLIGHT_SK_FONT_HINTING,
  NORMAL_SK_FONT_HINTING,
  FULL_SK_FONT_HINTING
} sk_font_hinting_t
@

-}
newtype Sk_font_hinting = Sk_font_hinting (#type sk_font_hinting_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_font_hinting_t"@ value (1/4): @"NONE_SK_FONT_HINTING"@
pattern NONE_SK_FONT_HINTING :: Sk_font_hinting
pattern NONE_SK_FONT_HINTING = (#const NONE_SK_FONT_HINTING)

-- | C enum @"sk_font_hinting_t"@ value (2/4): @"SLIGHT_SK_FONT_HINTING"@
pattern SLIGHT_SK_FONT_HINTING :: Sk_font_hinting
pattern SLIGHT_SK_FONT_HINTING = (#const SLIGHT_SK_FONT_HINTING)

-- | C enum @"sk_font_hinting_t"@ value (3/4): @"NORMAL_SK_FONT_HINTING"@
pattern NORMAL_SK_FONT_HINTING :: Sk_font_hinting
pattern NORMAL_SK_FONT_HINTING = (#const NORMAL_SK_FONT_HINTING)

-- | C enum @"sk_font_hinting_t"@ value (4/4): @"FULL_SK_FONT_HINTING"@
pattern FULL_SK_FONT_HINTING :: Sk_font_hinting
pattern FULL_SK_FONT_HINTING = (#const FULL_SK_FONT_HINTING)

{- | C enum: @"sk_font_edging_t"@

@
typedef enum 
{
  ALIAS_SK_FONT_EDGING,
  ANTIALIAS_SK_FONT_EDGING,
  SUBPIXEL_ANTIALIAS_SK_FONT_EDGING
} sk_font_edging_t
@

-}
newtype Sk_font_edging = Sk_font_edging (#type sk_font_edging_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_font_edging_t"@ value (1/3): @"ALIAS_SK_FONT_EDGING"@
pattern ALIAS_SK_FONT_EDGING :: Sk_font_edging
pattern ALIAS_SK_FONT_EDGING = (#const ALIAS_SK_FONT_EDGING)

-- | C enum @"sk_font_edging_t"@ value (2/3): @"ANTIALIAS_SK_FONT_EDGING"@
pattern ANTIALIAS_SK_FONT_EDGING :: Sk_font_edging
pattern ANTIALIAS_SK_FONT_EDGING = (#const ANTIALIAS_SK_FONT_EDGING)

-- | C enum @"sk_font_edging_t"@ value (3/3): @"SUBPIXEL_ANTIALIAS_SK_FONT_EDGING"@
pattern SUBPIXEL_ANTIALIAS_SK_FONT_EDGING :: Sk_font_edging
pattern SUBPIXEL_ANTIALIAS_SK_FONT_EDGING = (#const SUBPIXEL_ANTIALIAS_SK_FONT_EDGING)

{- | Opaque C struct: @"sk_pixelref_factory_t"@
-}
data Sk_pixelref_factory = Sk_pixelref_factory

{- | C enum: @"sk_pathop_t"@

@
typedef enum 
{
  DIFFERENCE_SK_PATHOP,
  INTERSECT_SK_PATHOP,
  UNION_SK_PATHOP,
  XOR_SK_PATHOP,
  REVERSE_DIFFERENCE_SK_PATHOP
} sk_pathop_t
@

-}
newtype Sk_pathop = Sk_pathop (#type sk_pathop_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_pathop_t"@ value (1/5): @"DIFFERENCE_SK_PATHOP"@
pattern DIFFERENCE_SK_PATHOP :: Sk_pathop
pattern DIFFERENCE_SK_PATHOP = (#const DIFFERENCE_SK_PATHOP)

-- | C enum @"sk_pathop_t"@ value (2/5): @"INTERSECT_SK_PATHOP"@
pattern INTERSECT_SK_PATHOP :: Sk_pathop
pattern INTERSECT_SK_PATHOP = (#const INTERSECT_SK_PATHOP)

-- | C enum @"sk_pathop_t"@ value (3/5): @"UNION_SK_PATHOP"@
pattern UNION_SK_PATHOP :: Sk_pathop
pattern UNION_SK_PATHOP = (#const UNION_SK_PATHOP)

-- | C enum @"sk_pathop_t"@ value (4/5): @"XOR_SK_PATHOP"@
pattern XOR_SK_PATHOP :: Sk_pathop
pattern XOR_SK_PATHOP = (#const XOR_SK_PATHOP)

-- | C enum @"sk_pathop_t"@ value (5/5): @"REVERSE_DIFFERENCE_SK_PATHOP"@
pattern REVERSE_DIFFERENCE_SK_PATHOP :: Sk_pathop
pattern REVERSE_DIFFERENCE_SK_PATHOP = (#const REVERSE_DIFFERENCE_SK_PATHOP)

{- | Opaque C struct: @"sk_opbuilder_t"@
-}
data Sk_opbuilder = Sk_opbuilder

{- | C enum: @"sk_lattice_recttype_t"@

@
typedef enum 
{
  DEFAULT_SK_LATTICE_RECT_TYPE,
  TRANSPARENT_SK_LATTICE_RECT_TYPE,
  FIXED_COLOR_SK_LATTICE_RECT_TYPE
} sk_lattice_recttype_t
@

-}
newtype Sk_lattice_recttype = Sk_lattice_recttype (#type sk_lattice_recttype_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_lattice_recttype_t"@ value (1/3): @"DEFAULT_SK_LATTICE_RECT_TYPE"@
pattern DEFAULT_SK_LATTICE_RECT_TYPE :: Sk_lattice_recttype
pattern DEFAULT_SK_LATTICE_RECT_TYPE = (#const DEFAULT_SK_LATTICE_RECT_TYPE)

-- | C enum @"sk_lattice_recttype_t"@ value (2/3): @"TRANSPARENT_SK_LATTICE_RECT_TYPE"@
pattern TRANSPARENT_SK_LATTICE_RECT_TYPE :: Sk_lattice_recttype
pattern TRANSPARENT_SK_LATTICE_RECT_TYPE = (#const TRANSPARENT_SK_LATTICE_RECT_TYPE)

-- | C enum @"sk_lattice_recttype_t"@ value (3/3): @"FIXED_COLOR_SK_LATTICE_RECT_TYPE"@
pattern FIXED_COLOR_SK_LATTICE_RECT_TYPE :: Sk_lattice_recttype
pattern FIXED_COLOR_SK_LATTICE_RECT_TYPE = (#const FIXED_COLOR_SK_LATTICE_RECT_TYPE)

{- | C struct: @"sk_lattice_t"@

@
typedef struct 
{
  const int *fXDivs;
  const int *fYDivs;
  const sk_lattice_recttype_t *fRectTypes;
  int fXCount;
  int fYCount;
  const sk_irect_t *fBounds;
  const sk_color_t *fColors;
} sk_lattice_t
@
-}
data Sk_lattice = Sk_lattice
  { fXDivs :: Ptr (CInt) -- ^ C field: @"const int *fXDivs"@
  , fYDivs :: Ptr (CInt) -- ^ C field: @"const int *fYDivs"@
  , fRectTypes :: Ptr (Sk_lattice_recttype) -- ^ C field: @"const sk_lattice_recttype_t *fRectTypes"@
  , fXCount :: CInt -- ^ C field: @"int fXCount"@
  , fYCount :: CInt -- ^ C field: @"int fYCount"@
  , fBounds :: Ptr (Sk_irect) -- ^ C field: @"const sk_irect_t *fBounds"@
  , fColors :: Ptr (Sk_color) -- ^ C field: @"const sk_color_t *fColors"@
  }
instance Foreign.Storable.Offset.Offset "fXDivs" Sk_lattice where
  rawOffset = (#offset sk_lattice_t, fXDivs)
instance Foreign.Storable.Offset.Offset "fYDivs" Sk_lattice where
  rawOffset = (#offset sk_lattice_t, fYDivs)
instance Foreign.Storable.Offset.Offset "fRectTypes" Sk_lattice where
  rawOffset = (#offset sk_lattice_t, fRectTypes)
instance Foreign.Storable.Offset.Offset "fXCount" Sk_lattice where
  rawOffset = (#offset sk_lattice_t, fXCount)
instance Foreign.Storable.Offset.Offset "fYCount" Sk_lattice where
  rawOffset = (#offset sk_lattice_t, fYCount)
instance Foreign.Storable.Offset.Offset "fBounds" Sk_lattice where
  rawOffset = (#offset sk_lattice_t, fBounds)
instance Foreign.Storable.Offset.Offset "fColors" Sk_lattice where
  rawOffset = (#offset sk_lattice_t, fColors)
instance Foreign.Storable.Storable Sk_lattice where
  sizeOf _ = (#size sk_lattice_t)
  alignment _ = (#alignment sk_lattice_t)
  peek p' = do
    fXDivs <- (#peek sk_lattice_t, fXDivs) p'
    fYDivs <- (#peek sk_lattice_t, fYDivs) p'
    fRectTypes <- (#peek sk_lattice_t, fRectTypes) p'
    fXCount <- (#peek sk_lattice_t, fXCount) p'
    fYCount <- (#peek sk_lattice_t, fYCount) p'
    fBounds <- (#peek sk_lattice_t, fBounds) p'
    fColors <- (#peek sk_lattice_t, fColors) p'
    pure Sk_lattice{..}
  poke p' Sk_lattice{..} = do
    (#poke sk_lattice_t, fXDivs) p' fXDivs
    (#poke sk_lattice_t, fYDivs) p' fYDivs
    (#poke sk_lattice_t, fRectTypes) p' fRectTypes
    (#poke sk_lattice_t, fXCount) p' fXCount
    (#poke sk_lattice_t, fYCount) p' fYCount
    (#poke sk_lattice_t, fBounds) p' fBounds
    (#poke sk_lattice_t, fColors) p' fColors

{- | Opaque C struct: @"sk_pathmeasure_t"@
-}
data Sk_pathmeasure = Sk_pathmeasure

{- | C enum: @"sk_pathmeasure_matrixflags_t"@

@
typedef enum 
{
  GET_POSITION_SK_PATHMEASURE_MATRIXFLAGS = 0x01,
  GET_TANGENT_SK_PATHMEASURE_MATRIXFLAGS = 0x02,
  GET_POS_AND_TAN_SK_PATHMEASURE_MATRIXFLAGS = GET_POSITION_SK_PATHMEASURE_MATRIXFLAGS | GET_TANGENT_SK_PATHMEASURE_MATRIXFLAGS
} sk_pathmeasure_matrixflags_t
@

-}
newtype Sk_pathmeasure_matrixflags = Sk_pathmeasure_matrixflags (#type sk_pathmeasure_matrixflags_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_pathmeasure_matrixflags_t"@ value (1/3): @"GET_POSITION_SK_PATHMEASURE_MATRIXFLAGS"@
pattern GET_POSITION_SK_PATHMEASURE_MATRIXFLAGS :: Sk_pathmeasure_matrixflags
pattern GET_POSITION_SK_PATHMEASURE_MATRIXFLAGS = (#const GET_POSITION_SK_PATHMEASURE_MATRIXFLAGS)

-- | C enum @"sk_pathmeasure_matrixflags_t"@ value (2/3): @"GET_TANGENT_SK_PATHMEASURE_MATRIXFLAGS"@
pattern GET_TANGENT_SK_PATHMEASURE_MATRIXFLAGS :: Sk_pathmeasure_matrixflags
pattern GET_TANGENT_SK_PATHMEASURE_MATRIXFLAGS = (#const GET_TANGENT_SK_PATHMEASURE_MATRIXFLAGS)

-- | C enum @"sk_pathmeasure_matrixflags_t"@ value (3/3): @"GET_POS_AND_TAN_SK_PATHMEASURE_MATRIXFLAGS"@
pattern GET_POS_AND_TAN_SK_PATHMEASURE_MATRIXFLAGS :: Sk_pathmeasure_matrixflags
pattern GET_POS_AND_TAN_SK_PATHMEASURE_MATRIXFLAGS = (#const GET_POS_AND_TAN_SK_PATHMEASURE_MATRIXFLAGS)

-- | C function pointer type: @typedef void (*sk_bitmap_release_proc)(void *addr, void *context)@
type Sk_bitmap_release_proc = Ptr (()) -> Ptr (()) -> IO (())

-- | Creates a 'FunPtr' of @"sk_bitmap_release_proc"@.
foreign import ccall "wrapper" mkFunPtr'Sk_bitmap_release_proc :: Sk_bitmap_release_proc -> IO (FunPtr Sk_bitmap_release_proc)

-- | C function pointer type: @typedef void (*sk_data_release_proc)(const void *ptr, void *context)@
type Sk_data_release_proc = Ptr (()) -> Ptr (()) -> IO (())

-- | Creates a 'FunPtr' of @"sk_data_release_proc"@.
foreign import ccall "wrapper" mkFunPtr'Sk_data_release_proc :: Sk_data_release_proc -> IO (FunPtr Sk_data_release_proc)

-- | C function pointer type: @typedef void (*sk_image_raster_release_proc)(const void *addr, void *context)@
type Sk_image_raster_release_proc = Ptr (()) -> Ptr (()) -> IO (())

-- | Creates a 'FunPtr' of @"sk_image_raster_release_proc"@.
foreign import ccall "wrapper" mkFunPtr'Sk_image_raster_release_proc :: Sk_image_raster_release_proc -> IO (FunPtr Sk_image_raster_release_proc)

-- | C function pointer type: @typedef void (*sk_image_texture_release_proc)(void *context)@
type Sk_image_texture_release_proc = Ptr (()) -> IO (())

-- | Creates a 'FunPtr' of @"sk_image_texture_release_proc"@.
foreign import ccall "wrapper" mkFunPtr'Sk_image_texture_release_proc :: Sk_image_texture_release_proc -> IO (FunPtr Sk_image_texture_release_proc)

-- | C function pointer type: @typedef void (*sk_surface_raster_release_proc)(void *addr, void *context)@
type Sk_surface_raster_release_proc = Ptr (()) -> Ptr (()) -> IO (())

-- | Creates a 'FunPtr' of @"sk_surface_raster_release_proc"@.
foreign import ccall "wrapper" mkFunPtr'Sk_surface_raster_release_proc :: Sk_surface_raster_release_proc -> IO (FunPtr Sk_surface_raster_release_proc)

-- | C function pointer type: @typedef void (*sk_glyph_path_proc)(const sk_path_t *pathOrNull, const sk_matrix_t *matrix, void *context)@
type Sk_glyph_path_proc = Ptr (Sk_path) -> Ptr (Sk_matrix) -> Ptr (()) -> IO (())

-- | Creates a 'FunPtr' of @"sk_glyph_path_proc"@.
foreign import ccall "wrapper" mkFunPtr'Sk_glyph_path_proc :: Sk_glyph_path_proc -> IO (FunPtr Sk_glyph_path_proc)

{- | C enum: @"sk_image_caching_hint_t"@

@
typedef enum 
{
  ALLOW_SK_IMAGE_CACHING_HINT,
  DISALLOW_SK_IMAGE_CACHING_HINT
} sk_image_caching_hint_t
@

-}
newtype Sk_image_caching_hint = Sk_image_caching_hint (#type sk_image_caching_hint_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_image_caching_hint_t"@ value (1/2): @"ALLOW_SK_IMAGE_CACHING_HINT"@
pattern ALLOW_SK_IMAGE_CACHING_HINT :: Sk_image_caching_hint
pattern ALLOW_SK_IMAGE_CACHING_HINT = (#const ALLOW_SK_IMAGE_CACHING_HINT)

-- | C enum @"sk_image_caching_hint_t"@ value (2/2): @"DISALLOW_SK_IMAGE_CACHING_HINT"@
pattern DISALLOW_SK_IMAGE_CACHING_HINT :: Sk_image_caching_hint
pattern DISALLOW_SK_IMAGE_CACHING_HINT = (#const DISALLOW_SK_IMAGE_CACHING_HINT)

{- | C enum: @"sk_bitmap_allocflags_t"@

@
typedef enum 
{
  NONE_SK_BITMAP_ALLOC_FLAGS = 0,
  ZERO_PIXELS_SK_BITMAP_ALLOC_FLAGS = 1 << 0
} sk_bitmap_allocflags_t
@

-}
newtype Sk_bitmap_allocflags = Sk_bitmap_allocflags (#type sk_bitmap_allocflags_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_bitmap_allocflags_t"@ value (1/2): @"NONE_SK_BITMAP_ALLOC_FLAGS"@
pattern NONE_SK_BITMAP_ALLOC_FLAGS :: Sk_bitmap_allocflags
pattern NONE_SK_BITMAP_ALLOC_FLAGS = (#const NONE_SK_BITMAP_ALLOC_FLAGS)

-- | C enum @"sk_bitmap_allocflags_t"@ value (2/2): @"ZERO_PIXELS_SK_BITMAP_ALLOC_FLAGS"@
pattern ZERO_PIXELS_SK_BITMAP_ALLOC_FLAGS :: Sk_bitmap_allocflags
pattern ZERO_PIXELS_SK_BITMAP_ALLOC_FLAGS = (#const ZERO_PIXELS_SK_BITMAP_ALLOC_FLAGS)

{- | C struct: @"sk_document_pdf_datetime_t"@

@
typedef struct 
{
  int16_t fTimeZoneMinutes;
  uint16_t fYear;
  uint8_t fMonth;
  uint8_t fDayOfWeek;
  uint8_t fDay;
  uint8_t fHour;
  uint8_t fMinute;
  uint8_t fSecond;
} sk_document_pdf_datetime_t
@
-}
data Sk_document_pdf_datetime = Sk_document_pdf_datetime
  { fTimeZoneMinutes :: Int16 -- ^ C field: @"int16_t fTimeZoneMinutes"@
  , fYear :: Word16 -- ^ C field: @"uint16_t fYear"@
  , fMonth :: Word8 -- ^ C field: @"uint8_t fMonth"@
  , fDayOfWeek :: Word8 -- ^ C field: @"uint8_t fDayOfWeek"@
  , fDay :: Word8 -- ^ C field: @"uint8_t fDay"@
  , fHour :: Word8 -- ^ C field: @"uint8_t fHour"@
  , fMinute :: Word8 -- ^ C field: @"uint8_t fMinute"@
  , fSecond :: Word8 -- ^ C field: @"uint8_t fSecond"@
  }
instance Foreign.Storable.Offset.Offset "fTimeZoneMinutes" Sk_document_pdf_datetime where
  rawOffset = (#offset sk_document_pdf_datetime_t, fTimeZoneMinutes)
instance Foreign.Storable.Offset.Offset "fYear" Sk_document_pdf_datetime where
  rawOffset = (#offset sk_document_pdf_datetime_t, fYear)
instance Foreign.Storable.Offset.Offset "fMonth" Sk_document_pdf_datetime where
  rawOffset = (#offset sk_document_pdf_datetime_t, fMonth)
instance Foreign.Storable.Offset.Offset "fDayOfWeek" Sk_document_pdf_datetime where
  rawOffset = (#offset sk_document_pdf_datetime_t, fDayOfWeek)
instance Foreign.Storable.Offset.Offset "fDay" Sk_document_pdf_datetime where
  rawOffset = (#offset sk_document_pdf_datetime_t, fDay)
instance Foreign.Storable.Offset.Offset "fHour" Sk_document_pdf_datetime where
  rawOffset = (#offset sk_document_pdf_datetime_t, fHour)
instance Foreign.Storable.Offset.Offset "fMinute" Sk_document_pdf_datetime where
  rawOffset = (#offset sk_document_pdf_datetime_t, fMinute)
instance Foreign.Storable.Offset.Offset "fSecond" Sk_document_pdf_datetime where
  rawOffset = (#offset sk_document_pdf_datetime_t, fSecond)
instance Foreign.Storable.Storable Sk_document_pdf_datetime where
  sizeOf _ = (#size sk_document_pdf_datetime_t)
  alignment _ = (#alignment sk_document_pdf_datetime_t)
  peek p' = do
    fTimeZoneMinutes <- (#peek sk_document_pdf_datetime_t, fTimeZoneMinutes) p'
    fYear <- (#peek sk_document_pdf_datetime_t, fYear) p'
    fMonth <- (#peek sk_document_pdf_datetime_t, fMonth) p'
    fDayOfWeek <- (#peek sk_document_pdf_datetime_t, fDayOfWeek) p'
    fDay <- (#peek sk_document_pdf_datetime_t, fDay) p'
    fHour <- (#peek sk_document_pdf_datetime_t, fHour) p'
    fMinute <- (#peek sk_document_pdf_datetime_t, fMinute) p'
    fSecond <- (#peek sk_document_pdf_datetime_t, fSecond) p'
    pure Sk_document_pdf_datetime{..}
  poke p' Sk_document_pdf_datetime{..} = do
    (#poke sk_document_pdf_datetime_t, fTimeZoneMinutes) p' fTimeZoneMinutes
    (#poke sk_document_pdf_datetime_t, fYear) p' fYear
    (#poke sk_document_pdf_datetime_t, fMonth) p' fMonth
    (#poke sk_document_pdf_datetime_t, fDayOfWeek) p' fDayOfWeek
    (#poke sk_document_pdf_datetime_t, fDay) p' fDay
    (#poke sk_document_pdf_datetime_t, fHour) p' fHour
    (#poke sk_document_pdf_datetime_t, fMinute) p' fMinute
    (#poke sk_document_pdf_datetime_t, fSecond) p' fSecond

{- | C struct: @"sk_document_pdf_metadata_t"@

@
typedef struct 
{
  sk_string_t *fTitle;
  sk_string_t *fAuthor;
  sk_string_t *fSubject;
  sk_string_t *fKeywords;
  sk_string_t *fCreator;
  sk_string_t *fProducer;
  sk_document_pdf_datetime_t *fCreation;
  sk_document_pdf_datetime_t *fModified;
  float fRasterDPI;
  _Bool fPDFA;
  int fEncodingQuality;
} sk_document_pdf_metadata_t
@
-}
data Sk_document_pdf_metadata = Sk_document_pdf_metadata
  { fTitle :: Ptr (Sk_string) -- ^ C field: @"sk_string_t *fTitle"@
  , fAuthor :: Ptr (Sk_string) -- ^ C field: @"sk_string_t *fAuthor"@
  , fSubject :: Ptr (Sk_string) -- ^ C field: @"sk_string_t *fSubject"@
  , fKeywords :: Ptr (Sk_string) -- ^ C field: @"sk_string_t *fKeywords"@
  , fCreator :: Ptr (Sk_string) -- ^ C field: @"sk_string_t *fCreator"@
  , fProducer :: Ptr (Sk_string) -- ^ C field: @"sk_string_t *fProducer"@
  , fCreation :: Ptr (Sk_document_pdf_datetime) -- ^ C field: @"sk_document_pdf_datetime_t *fCreation"@
  , fModified :: Ptr (Sk_document_pdf_datetime) -- ^ C field: @"sk_document_pdf_datetime_t *fModified"@
  , fRasterDPI :: CFloat -- ^ C field: @"float fRasterDPI"@
  , fPDFA :: CBool -- ^ C field: @"_Bool fPDFA"@
  , fEncodingQuality :: CInt -- ^ C field: @"int fEncodingQuality"@
  }
instance Foreign.Storable.Offset.Offset "fTitle" Sk_document_pdf_metadata where
  rawOffset = (#offset sk_document_pdf_metadata_t, fTitle)
instance Foreign.Storable.Offset.Offset "fAuthor" Sk_document_pdf_metadata where
  rawOffset = (#offset sk_document_pdf_metadata_t, fAuthor)
instance Foreign.Storable.Offset.Offset "fSubject" Sk_document_pdf_metadata where
  rawOffset = (#offset sk_document_pdf_metadata_t, fSubject)
instance Foreign.Storable.Offset.Offset "fKeywords" Sk_document_pdf_metadata where
  rawOffset = (#offset sk_document_pdf_metadata_t, fKeywords)
instance Foreign.Storable.Offset.Offset "fCreator" Sk_document_pdf_metadata where
  rawOffset = (#offset sk_document_pdf_metadata_t, fCreator)
instance Foreign.Storable.Offset.Offset "fProducer" Sk_document_pdf_metadata where
  rawOffset = (#offset sk_document_pdf_metadata_t, fProducer)
instance Foreign.Storable.Offset.Offset "fCreation" Sk_document_pdf_metadata where
  rawOffset = (#offset sk_document_pdf_metadata_t, fCreation)
instance Foreign.Storable.Offset.Offset "fModified" Sk_document_pdf_metadata where
  rawOffset = (#offset sk_document_pdf_metadata_t, fModified)
instance Foreign.Storable.Offset.Offset "fRasterDPI" Sk_document_pdf_metadata where
  rawOffset = (#offset sk_document_pdf_metadata_t, fRasterDPI)
instance Foreign.Storable.Offset.Offset "fPDFA" Sk_document_pdf_metadata where
  rawOffset = (#offset sk_document_pdf_metadata_t, fPDFA)
instance Foreign.Storable.Offset.Offset "fEncodingQuality" Sk_document_pdf_metadata where
  rawOffset = (#offset sk_document_pdf_metadata_t, fEncodingQuality)
instance Foreign.Storable.Storable Sk_document_pdf_metadata where
  sizeOf _ = (#size sk_document_pdf_metadata_t)
  alignment _ = (#alignment sk_document_pdf_metadata_t)
  peek p' = do
    fTitle <- (#peek sk_document_pdf_metadata_t, fTitle) p'
    fAuthor <- (#peek sk_document_pdf_metadata_t, fAuthor) p'
    fSubject <- (#peek sk_document_pdf_metadata_t, fSubject) p'
    fKeywords <- (#peek sk_document_pdf_metadata_t, fKeywords) p'
    fCreator <- (#peek sk_document_pdf_metadata_t, fCreator) p'
    fProducer <- (#peek sk_document_pdf_metadata_t, fProducer) p'
    fCreation <- (#peek sk_document_pdf_metadata_t, fCreation) p'
    fModified <- (#peek sk_document_pdf_metadata_t, fModified) p'
    fRasterDPI <- (#peek sk_document_pdf_metadata_t, fRasterDPI) p'
    fPDFA <- (#peek sk_document_pdf_metadata_t, fPDFA) p'
    fEncodingQuality <- (#peek sk_document_pdf_metadata_t, fEncodingQuality) p'
    pure Sk_document_pdf_metadata{..}
  poke p' Sk_document_pdf_metadata{..} = do
    (#poke sk_document_pdf_metadata_t, fTitle) p' fTitle
    (#poke sk_document_pdf_metadata_t, fAuthor) p' fAuthor
    (#poke sk_document_pdf_metadata_t, fSubject) p' fSubject
    (#poke sk_document_pdf_metadata_t, fKeywords) p' fKeywords
    (#poke sk_document_pdf_metadata_t, fCreator) p' fCreator
    (#poke sk_document_pdf_metadata_t, fProducer) p' fProducer
    (#poke sk_document_pdf_metadata_t, fCreation) p' fCreation
    (#poke sk_document_pdf_metadata_t, fModified) p' fModified
    (#poke sk_document_pdf_metadata_t, fRasterDPI) p' fRasterDPI
    (#poke sk_document_pdf_metadata_t, fPDFA) p' fPDFA
    (#poke sk_document_pdf_metadata_t, fEncodingQuality) p' fEncodingQuality

{- | C struct: @"sk_imageinfo_t"@

@
typedef struct 
{
  sk_colorspace_t *colorspace;
  int32_t width;
  int32_t height;
  sk_colortype_t colorType;
  sk_alphatype_t alphaType;
} sk_imageinfo_t
@
-}
data Sk_imageinfo = Sk_imageinfo
  { colorspace :: Ptr (Sk_colorspace) -- ^ C field: @"sk_colorspace_t *colorspace"@
  , width :: Int32 -- ^ C field: @"int32_t width"@
  , height :: Int32 -- ^ C field: @"int32_t height"@
  , colorType :: Sk_colortype -- ^ C field: @"sk_colortype_t colorType"@
  , alphaType :: Sk_alphatype -- ^ C field: @"sk_alphatype_t alphaType"@
  }
instance Foreign.Storable.Offset.Offset "colorspace" Sk_imageinfo where
  rawOffset = (#offset sk_imageinfo_t, colorspace)
instance Foreign.Storable.Offset.Offset "width" Sk_imageinfo where
  rawOffset = (#offset sk_imageinfo_t, width)
instance Foreign.Storable.Offset.Offset "height" Sk_imageinfo where
  rawOffset = (#offset sk_imageinfo_t, height)
instance Foreign.Storable.Offset.Offset "colorType" Sk_imageinfo where
  rawOffset = (#offset sk_imageinfo_t, colorType)
instance Foreign.Storable.Offset.Offset "alphaType" Sk_imageinfo where
  rawOffset = (#offset sk_imageinfo_t, alphaType)
instance Foreign.Storable.Storable Sk_imageinfo where
  sizeOf _ = (#size sk_imageinfo_t)
  alignment _ = (#alignment sk_imageinfo_t)
  peek p' = do
    colorspace <- (#peek sk_imageinfo_t, colorspace) p'
    width <- (#peek sk_imageinfo_t, width) p'
    height <- (#peek sk_imageinfo_t, height) p'
    colorType <- (#peek sk_imageinfo_t, colorType) p'
    alphaType <- (#peek sk_imageinfo_t, alphaType) p'
    pure Sk_imageinfo{..}
  poke p' Sk_imageinfo{..} = do
    (#poke sk_imageinfo_t, colorspace) p' colorspace
    (#poke sk_imageinfo_t, width) p' width
    (#poke sk_imageinfo_t, height) p' height
    (#poke sk_imageinfo_t, colorType) p' colorType
    (#poke sk_imageinfo_t, alphaType) p' alphaType

{- | C enum: @"sk_codecanimation_disposalmethod_t"@

@
typedef enum 
{
  KEEP_SK_CODEC_ANIMATION_DISPOSAL_METHOD = 1,
  RESTORE_BG_COLOR_SK_CODEC_ANIMATION_DISPOSAL_METHOD = 2,
  RESTORE_PREVIOUS_SK_CODEC_ANIMATION_DISPOSAL_METHOD = 3
} sk_codecanimation_disposalmethod_t
@

-}
newtype Sk_codecanimation_disposalmethod = Sk_codecanimation_disposalmethod (#type sk_codecanimation_disposalmethod_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_codecanimation_disposalmethod_t"@ value (1/3): @"KEEP_SK_CODEC_ANIMATION_DISPOSAL_METHOD"@
pattern KEEP_SK_CODEC_ANIMATION_DISPOSAL_METHOD :: Sk_codecanimation_disposalmethod
pattern KEEP_SK_CODEC_ANIMATION_DISPOSAL_METHOD = (#const KEEP_SK_CODEC_ANIMATION_DISPOSAL_METHOD)

-- | C enum @"sk_codecanimation_disposalmethod_t"@ value (2/3): @"RESTORE_BG_COLOR_SK_CODEC_ANIMATION_DISPOSAL_METHOD"@
pattern RESTORE_BG_COLOR_SK_CODEC_ANIMATION_DISPOSAL_METHOD :: Sk_codecanimation_disposalmethod
pattern RESTORE_BG_COLOR_SK_CODEC_ANIMATION_DISPOSAL_METHOD = (#const RESTORE_BG_COLOR_SK_CODEC_ANIMATION_DISPOSAL_METHOD)

-- | C enum @"sk_codecanimation_disposalmethod_t"@ value (3/3): @"RESTORE_PREVIOUS_SK_CODEC_ANIMATION_DISPOSAL_METHOD"@
pattern RESTORE_PREVIOUS_SK_CODEC_ANIMATION_DISPOSAL_METHOD :: Sk_codecanimation_disposalmethod
pattern RESTORE_PREVIOUS_SK_CODEC_ANIMATION_DISPOSAL_METHOD = (#const RESTORE_PREVIOUS_SK_CODEC_ANIMATION_DISPOSAL_METHOD)

{- | C enum: @"sk_codecanimation_blend_t"@

@
typedef enum 
{
  SRC_OVER_SK_CODEC_ANIMATION_BLEND = 0,
  SRC_SK_CODEC_ANIMATION_BLEND = 1
} sk_codecanimation_blend_t
@

-}
newtype Sk_codecanimation_blend = Sk_codecanimation_blend (#type sk_codecanimation_blend_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_codecanimation_blend_t"@ value (1/2): @"SRC_OVER_SK_CODEC_ANIMATION_BLEND"@
pattern SRC_OVER_SK_CODEC_ANIMATION_BLEND :: Sk_codecanimation_blend
pattern SRC_OVER_SK_CODEC_ANIMATION_BLEND = (#const SRC_OVER_SK_CODEC_ANIMATION_BLEND)

-- | C enum @"sk_codecanimation_blend_t"@ value (2/2): @"SRC_SK_CODEC_ANIMATION_BLEND"@
pattern SRC_SK_CODEC_ANIMATION_BLEND :: Sk_codecanimation_blend
pattern SRC_SK_CODEC_ANIMATION_BLEND = (#const SRC_SK_CODEC_ANIMATION_BLEND)

{- | C struct: @"sk_codec_frameinfo_t"@

@
typedef struct 
{
  int fRequiredFrame;
  int fDuration;
  _Bool fFullyReceived;
  sk_alphatype_t fAlphaType;
  _Bool fHasAlphaWithinBounds;
  sk_codecanimation_disposalmethod_t fDisposalMethod;
  sk_codecanimation_blend_t fBlend;
  sk_irect_t fFrameRect;
} sk_codec_frameinfo_t
@
-}
data Sk_codec_frameinfo = Sk_codec_frameinfo
  { fRequiredFrame :: CInt -- ^ C field: @"int fRequiredFrame"@
  , fDuration :: CInt -- ^ C field: @"int fDuration"@
  , fFullyReceived :: CBool -- ^ C field: @"_Bool fFullyReceived"@
  , fAlphaType :: Sk_alphatype -- ^ C field: @"sk_alphatype_t fAlphaType"@
  , fHasAlphaWithinBounds :: CBool -- ^ C field: @"_Bool fHasAlphaWithinBounds"@
  , fDisposalMethod :: Sk_codecanimation_disposalmethod -- ^ C field: @"sk_codecanimation_disposalmethod_t fDisposalMethod"@
  , fBlend :: Sk_codecanimation_blend -- ^ C field: @"sk_codecanimation_blend_t fBlend"@
  , fFrameRect :: Sk_irect -- ^ C field: @"sk_irect_t fFrameRect"@
  }
instance Foreign.Storable.Offset.Offset "fRequiredFrame" Sk_codec_frameinfo where
  rawOffset = (#offset sk_codec_frameinfo_t, fRequiredFrame)
instance Foreign.Storable.Offset.Offset "fDuration" Sk_codec_frameinfo where
  rawOffset = (#offset sk_codec_frameinfo_t, fDuration)
instance Foreign.Storable.Offset.Offset "fFullyReceived" Sk_codec_frameinfo where
  rawOffset = (#offset sk_codec_frameinfo_t, fFullyReceived)
instance Foreign.Storable.Offset.Offset "fAlphaType" Sk_codec_frameinfo where
  rawOffset = (#offset sk_codec_frameinfo_t, fAlphaType)
instance Foreign.Storable.Offset.Offset "fHasAlphaWithinBounds" Sk_codec_frameinfo where
  rawOffset = (#offset sk_codec_frameinfo_t, fHasAlphaWithinBounds)
instance Foreign.Storable.Offset.Offset "fDisposalMethod" Sk_codec_frameinfo where
  rawOffset = (#offset sk_codec_frameinfo_t, fDisposalMethod)
instance Foreign.Storable.Offset.Offset "fBlend" Sk_codec_frameinfo where
  rawOffset = (#offset sk_codec_frameinfo_t, fBlend)
instance Foreign.Storable.Offset.Offset "fFrameRect" Sk_codec_frameinfo where
  rawOffset = (#offset sk_codec_frameinfo_t, fFrameRect)
instance Foreign.Storable.Storable Sk_codec_frameinfo where
  sizeOf _ = (#size sk_codec_frameinfo_t)
  alignment _ = (#alignment sk_codec_frameinfo_t)
  peek p' = do
    fRequiredFrame <- (#peek sk_codec_frameinfo_t, fRequiredFrame) p'
    fDuration <- (#peek sk_codec_frameinfo_t, fDuration) p'
    fFullyReceived <- (#peek sk_codec_frameinfo_t, fFullyReceived) p'
    fAlphaType <- (#peek sk_codec_frameinfo_t, fAlphaType) p'
    fHasAlphaWithinBounds <- (#peek sk_codec_frameinfo_t, fHasAlphaWithinBounds) p'
    fDisposalMethod <- (#peek sk_codec_frameinfo_t, fDisposalMethod) p'
    fBlend <- (#peek sk_codec_frameinfo_t, fBlend) p'
    fFrameRect <- (#peek sk_codec_frameinfo_t, fFrameRect) p'
    pure Sk_codec_frameinfo{..}
  poke p' Sk_codec_frameinfo{..} = do
    (#poke sk_codec_frameinfo_t, fRequiredFrame) p' fRequiredFrame
    (#poke sk_codec_frameinfo_t, fDuration) p' fDuration
    (#poke sk_codec_frameinfo_t, fFullyReceived) p' fFullyReceived
    (#poke sk_codec_frameinfo_t, fAlphaType) p' fAlphaType
    (#poke sk_codec_frameinfo_t, fHasAlphaWithinBounds) p' fHasAlphaWithinBounds
    (#poke sk_codec_frameinfo_t, fDisposalMethod) p' fDisposalMethod
    (#poke sk_codec_frameinfo_t, fBlend) p' fBlend
    (#poke sk_codec_frameinfo_t, fFrameRect) p' fFrameRect

{- | Opaque C struct: @"sk_svgcanvas_t"@
-}
data Sk_svgcanvas = Sk_svgcanvas

{- | C enum: @"sk_vertices_vertex_mode_t"@

@
typedef enum 
{
  TRIANGLES_SK_VERTICES_VERTEX_MODE,
  TRIANGLE_STRIP_SK_VERTICES_VERTEX_MODE,
  TRIANGLE_FAN_SK_VERTICES_VERTEX_MODE
} sk_vertices_vertex_mode_t
@

-}
newtype Sk_vertices_vertex_mode = Sk_vertices_vertex_mode (#type sk_vertices_vertex_mode_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_vertices_vertex_mode_t"@ value (1/3): @"TRIANGLES_SK_VERTICES_VERTEX_MODE"@
pattern TRIANGLES_SK_VERTICES_VERTEX_MODE :: Sk_vertices_vertex_mode
pattern TRIANGLES_SK_VERTICES_VERTEX_MODE = (#const TRIANGLES_SK_VERTICES_VERTEX_MODE)

-- | C enum @"sk_vertices_vertex_mode_t"@ value (2/3): @"TRIANGLE_STRIP_SK_VERTICES_VERTEX_MODE"@
pattern TRIANGLE_STRIP_SK_VERTICES_VERTEX_MODE :: Sk_vertices_vertex_mode
pattern TRIANGLE_STRIP_SK_VERTICES_VERTEX_MODE = (#const TRIANGLE_STRIP_SK_VERTICES_VERTEX_MODE)

-- | C enum @"sk_vertices_vertex_mode_t"@ value (3/3): @"TRIANGLE_FAN_SK_VERTICES_VERTEX_MODE"@
pattern TRIANGLE_FAN_SK_VERTICES_VERTEX_MODE :: Sk_vertices_vertex_mode
pattern TRIANGLE_FAN_SK_VERTICES_VERTEX_MODE = (#const TRIANGLE_FAN_SK_VERTICES_VERTEX_MODE)

{- | Opaque C struct: @"sk_vertices_t"@
-}
data Sk_vertices = Sk_vertices

{- | C struct: @"sk_colorspace_transfer_fn_t"@

@
typedef struct sk_colorspace_transfer_fn_t
{
  float fG;
  float fA;
  float fB;
  float fC;
  float fD;
  float fE;
  float fF;
} sk_colorspace_transfer_fn_t
@
-}
data Sk_colorspace_transfer_fn = Sk_colorspace_transfer_fn
  { fG :: CFloat -- ^ C field: @"float fG"@
  , fA :: CFloat -- ^ C field: @"float fA"@
  , fB :: CFloat -- ^ C field: @"float fB"@
  , fC :: CFloat -- ^ C field: @"float fC"@
  , fD :: CFloat -- ^ C field: @"float fD"@
  , fE :: CFloat -- ^ C field: @"float fE"@
  , fF :: CFloat -- ^ C field: @"float fF"@
  }
instance Foreign.Storable.Offset.Offset "fG" Sk_colorspace_transfer_fn where
  rawOffset = (#offset sk_colorspace_transfer_fn_t, fG)
instance Foreign.Storable.Offset.Offset "fA" Sk_colorspace_transfer_fn where
  rawOffset = (#offset sk_colorspace_transfer_fn_t, fA)
instance Foreign.Storable.Offset.Offset "fB" Sk_colorspace_transfer_fn where
  rawOffset = (#offset sk_colorspace_transfer_fn_t, fB)
instance Foreign.Storable.Offset.Offset "fC" Sk_colorspace_transfer_fn where
  rawOffset = (#offset sk_colorspace_transfer_fn_t, fC)
instance Foreign.Storable.Offset.Offset "fD" Sk_colorspace_transfer_fn where
  rawOffset = (#offset sk_colorspace_transfer_fn_t, fD)
instance Foreign.Storable.Offset.Offset "fE" Sk_colorspace_transfer_fn where
  rawOffset = (#offset sk_colorspace_transfer_fn_t, fE)
instance Foreign.Storable.Offset.Offset "fF" Sk_colorspace_transfer_fn where
  rawOffset = (#offset sk_colorspace_transfer_fn_t, fF)
instance Foreign.Storable.Storable Sk_colorspace_transfer_fn where
  sizeOf _ = (#size sk_colorspace_transfer_fn_t)
  alignment _ = (#alignment sk_colorspace_transfer_fn_t)
  peek p' = do
    fG <- (#peek sk_colorspace_transfer_fn_t, fG) p'
    fA <- (#peek sk_colorspace_transfer_fn_t, fA) p'
    fB <- (#peek sk_colorspace_transfer_fn_t, fB) p'
    fC <- (#peek sk_colorspace_transfer_fn_t, fC) p'
    fD <- (#peek sk_colorspace_transfer_fn_t, fD) p'
    fE <- (#peek sk_colorspace_transfer_fn_t, fE) p'
    fF <- (#peek sk_colorspace_transfer_fn_t, fF) p'
    pure Sk_colorspace_transfer_fn{..}
  poke p' Sk_colorspace_transfer_fn{..} = do
    (#poke sk_colorspace_transfer_fn_t, fG) p' fG
    (#poke sk_colorspace_transfer_fn_t, fA) p' fA
    (#poke sk_colorspace_transfer_fn_t, fB) p' fB
    (#poke sk_colorspace_transfer_fn_t, fC) p' fC
    (#poke sk_colorspace_transfer_fn_t, fD) p' fD
    (#poke sk_colorspace_transfer_fn_t, fE) p' fE
    (#poke sk_colorspace_transfer_fn_t, fF) p' fF

{- | C struct: @"sk_colorspace_primaries_t"@

@
typedef struct sk_colorspace_primaries_t
{
  float fRX;
  float fRY;
  float fGX;
  float fGY;
  float fBX;
  float fBY;
  float fWX;
  float fWY;
} sk_colorspace_primaries_t
@
-}
data Sk_colorspace_primaries = Sk_colorspace_primaries
  { fRX :: CFloat -- ^ C field: @"float fRX"@
  , fRY :: CFloat -- ^ C field: @"float fRY"@
  , fGX :: CFloat -- ^ C field: @"float fGX"@
  , fGY :: CFloat -- ^ C field: @"float fGY"@
  , fBX :: CFloat -- ^ C field: @"float fBX"@
  , fBY :: CFloat -- ^ C field: @"float fBY"@
  , fWX :: CFloat -- ^ C field: @"float fWX"@
  , fWY :: CFloat -- ^ C field: @"float fWY"@
  }
instance Foreign.Storable.Offset.Offset "fRX" Sk_colorspace_primaries where
  rawOffset = (#offset sk_colorspace_primaries_t, fRX)
instance Foreign.Storable.Offset.Offset "fRY" Sk_colorspace_primaries where
  rawOffset = (#offset sk_colorspace_primaries_t, fRY)
instance Foreign.Storable.Offset.Offset "fGX" Sk_colorspace_primaries where
  rawOffset = (#offset sk_colorspace_primaries_t, fGX)
instance Foreign.Storable.Offset.Offset "fGY" Sk_colorspace_primaries where
  rawOffset = (#offset sk_colorspace_primaries_t, fGY)
instance Foreign.Storable.Offset.Offset "fBX" Sk_colorspace_primaries where
  rawOffset = (#offset sk_colorspace_primaries_t, fBX)
instance Foreign.Storable.Offset.Offset "fBY" Sk_colorspace_primaries where
  rawOffset = (#offset sk_colorspace_primaries_t, fBY)
instance Foreign.Storable.Offset.Offset "fWX" Sk_colorspace_primaries where
  rawOffset = (#offset sk_colorspace_primaries_t, fWX)
instance Foreign.Storable.Offset.Offset "fWY" Sk_colorspace_primaries where
  rawOffset = (#offset sk_colorspace_primaries_t, fWY)
instance Foreign.Storable.Storable Sk_colorspace_primaries where
  sizeOf _ = (#size sk_colorspace_primaries_t)
  alignment _ = (#alignment sk_colorspace_primaries_t)
  peek p' = do
    fRX <- (#peek sk_colorspace_primaries_t, fRX) p'
    fRY <- (#peek sk_colorspace_primaries_t, fRY) p'
    fGX <- (#peek sk_colorspace_primaries_t, fGX) p'
    fGY <- (#peek sk_colorspace_primaries_t, fGY) p'
    fBX <- (#peek sk_colorspace_primaries_t, fBX) p'
    fBY <- (#peek sk_colorspace_primaries_t, fBY) p'
    fWX <- (#peek sk_colorspace_primaries_t, fWX) p'
    fWY <- (#peek sk_colorspace_primaries_t, fWY) p'
    pure Sk_colorspace_primaries{..}
  poke p' Sk_colorspace_primaries{..} = do
    (#poke sk_colorspace_primaries_t, fRX) p' fRX
    (#poke sk_colorspace_primaries_t, fRY) p' fRY
    (#poke sk_colorspace_primaries_t, fGX) p' fGX
    (#poke sk_colorspace_primaries_t, fGY) p' fGY
    (#poke sk_colorspace_primaries_t, fBX) p' fBX
    (#poke sk_colorspace_primaries_t, fBY) p' fBY
    (#poke sk_colorspace_primaries_t, fWX) p' fWX
    (#poke sk_colorspace_primaries_t, fWY) p' fWY

{- | C struct: @"sk_colorspace_xyz_t"@

@
typedef struct sk_colorspace_xyz_t
{
  float fM00;
  float fM01;
  float fM02;
  float fM10;
  float fM11;
  float fM12;
  float fM20;
  float fM21;
  float fM22;
} sk_colorspace_xyz_t
@
-}
data Sk_colorspace_xyz = Sk_colorspace_xyz
  { fM00 :: CFloat -- ^ C field: @"float fM00"@
  , fM01 :: CFloat -- ^ C field: @"float fM01"@
  , fM02 :: CFloat -- ^ C field: @"float fM02"@
  , fM10 :: CFloat -- ^ C field: @"float fM10"@
  , fM11 :: CFloat -- ^ C field: @"float fM11"@
  , fM12 :: CFloat -- ^ C field: @"float fM12"@
  , fM20 :: CFloat -- ^ C field: @"float fM20"@
  , fM21 :: CFloat -- ^ C field: @"float fM21"@
  , fM22 :: CFloat -- ^ C field: @"float fM22"@
  }
instance Foreign.Storable.Offset.Offset "fM00" Sk_colorspace_xyz where
  rawOffset = (#offset sk_colorspace_xyz_t, fM00)
instance Foreign.Storable.Offset.Offset "fM01" Sk_colorspace_xyz where
  rawOffset = (#offset sk_colorspace_xyz_t, fM01)
instance Foreign.Storable.Offset.Offset "fM02" Sk_colorspace_xyz where
  rawOffset = (#offset sk_colorspace_xyz_t, fM02)
instance Foreign.Storable.Offset.Offset "fM10" Sk_colorspace_xyz where
  rawOffset = (#offset sk_colorspace_xyz_t, fM10)
instance Foreign.Storable.Offset.Offset "fM11" Sk_colorspace_xyz where
  rawOffset = (#offset sk_colorspace_xyz_t, fM11)
instance Foreign.Storable.Offset.Offset "fM12" Sk_colorspace_xyz where
  rawOffset = (#offset sk_colorspace_xyz_t, fM12)
instance Foreign.Storable.Offset.Offset "fM20" Sk_colorspace_xyz where
  rawOffset = (#offset sk_colorspace_xyz_t, fM20)
instance Foreign.Storable.Offset.Offset "fM21" Sk_colorspace_xyz where
  rawOffset = (#offset sk_colorspace_xyz_t, fM21)
instance Foreign.Storable.Offset.Offset "fM22" Sk_colorspace_xyz where
  rawOffset = (#offset sk_colorspace_xyz_t, fM22)
instance Foreign.Storable.Storable Sk_colorspace_xyz where
  sizeOf _ = (#size sk_colorspace_xyz_t)
  alignment _ = (#alignment sk_colorspace_xyz_t)
  peek p' = do
    fM00 <- (#peek sk_colorspace_xyz_t, fM00) p'
    fM01 <- (#peek sk_colorspace_xyz_t, fM01) p'
    fM02 <- (#peek sk_colorspace_xyz_t, fM02) p'
    fM10 <- (#peek sk_colorspace_xyz_t, fM10) p'
    fM11 <- (#peek sk_colorspace_xyz_t, fM11) p'
    fM12 <- (#peek sk_colorspace_xyz_t, fM12) p'
    fM20 <- (#peek sk_colorspace_xyz_t, fM20) p'
    fM21 <- (#peek sk_colorspace_xyz_t, fM21) p'
    fM22 <- (#peek sk_colorspace_xyz_t, fM22) p'
    pure Sk_colorspace_xyz{..}
  poke p' Sk_colorspace_xyz{..} = do
    (#poke sk_colorspace_xyz_t, fM00) p' fM00
    (#poke sk_colorspace_xyz_t, fM01) p' fM01
    (#poke sk_colorspace_xyz_t, fM02) p' fM02
    (#poke sk_colorspace_xyz_t, fM10) p' fM10
    (#poke sk_colorspace_xyz_t, fM11) p' fM11
    (#poke sk_colorspace_xyz_t, fM12) p' fM12
    (#poke sk_colorspace_xyz_t, fM20) p' fM20
    (#poke sk_colorspace_xyz_t, fM21) p' fM21
    (#poke sk_colorspace_xyz_t, fM22) p' fM22

{- | Opaque C struct: @"sk_colorspace_icc_profile_t"@
-}
data Sk_colorspace_icc_profile = Sk_colorspace_icc_profile

{- | C enum: @"sk_highcontrastconfig_invertstyle_t"@

@
typedef enum 
{
  NO_INVERT_SK_HIGH_CONTRAST_CONFIG_INVERT_STYLE,
  INVERT_BRIGHTNESS_SK_HIGH_CONTRAST_CONFIG_INVERT_STYLE,
  INVERT_LIGHTNESS_SK_HIGH_CONTRAST_CONFIG_INVERT_STYLE
} sk_highcontrastconfig_invertstyle_t
@

-}
newtype Sk_highcontrastconfig_invertstyle = Sk_highcontrastconfig_invertstyle (#type sk_highcontrastconfig_invertstyle_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_highcontrastconfig_invertstyle_t"@ value (1/3): @"NO_INVERT_SK_HIGH_CONTRAST_CONFIG_INVERT_STYLE"@
pattern NO_INVERT_SK_HIGH_CONTRAST_CONFIG_INVERT_STYLE :: Sk_highcontrastconfig_invertstyle
pattern NO_INVERT_SK_HIGH_CONTRAST_CONFIG_INVERT_STYLE = (#const NO_INVERT_SK_HIGH_CONTRAST_CONFIG_INVERT_STYLE)

-- | C enum @"sk_highcontrastconfig_invertstyle_t"@ value (2/3): @"INVERT_BRIGHTNESS_SK_HIGH_CONTRAST_CONFIG_INVERT_STYLE"@
pattern INVERT_BRIGHTNESS_SK_HIGH_CONTRAST_CONFIG_INVERT_STYLE :: Sk_highcontrastconfig_invertstyle
pattern INVERT_BRIGHTNESS_SK_HIGH_CONTRAST_CONFIG_INVERT_STYLE = (#const INVERT_BRIGHTNESS_SK_HIGH_CONTRAST_CONFIG_INVERT_STYLE)

-- | C enum @"sk_highcontrastconfig_invertstyle_t"@ value (3/3): @"INVERT_LIGHTNESS_SK_HIGH_CONTRAST_CONFIG_INVERT_STYLE"@
pattern INVERT_LIGHTNESS_SK_HIGH_CONTRAST_CONFIG_INVERT_STYLE :: Sk_highcontrastconfig_invertstyle
pattern INVERT_LIGHTNESS_SK_HIGH_CONTRAST_CONFIG_INVERT_STYLE = (#const INVERT_LIGHTNESS_SK_HIGH_CONTRAST_CONFIG_INVERT_STYLE)

{- | C struct: @"sk_highcontrastconfig_t"@

@
typedef struct 
{
  _Bool fGrayscale;
  sk_highcontrastconfig_invertstyle_t fInvertStyle;
  float fContrast;
} sk_highcontrastconfig_t
@
-}
data Sk_highcontrastconfig = Sk_highcontrastconfig
  { fGrayscale :: CBool -- ^ C field: @"_Bool fGrayscale"@
  , fInvertStyle :: Sk_highcontrastconfig_invertstyle -- ^ C field: @"sk_highcontrastconfig_invertstyle_t fInvertStyle"@
  , fContrast :: CFloat -- ^ C field: @"float fContrast"@
  }
instance Foreign.Storable.Offset.Offset "fGrayscale" Sk_highcontrastconfig where
  rawOffset = (#offset sk_highcontrastconfig_t, fGrayscale)
instance Foreign.Storable.Offset.Offset "fInvertStyle" Sk_highcontrastconfig where
  rawOffset = (#offset sk_highcontrastconfig_t, fInvertStyle)
instance Foreign.Storable.Offset.Offset "fContrast" Sk_highcontrastconfig where
  rawOffset = (#offset sk_highcontrastconfig_t, fContrast)
instance Foreign.Storable.Storable Sk_highcontrastconfig where
  sizeOf _ = (#size sk_highcontrastconfig_t)
  alignment _ = (#alignment sk_highcontrastconfig_t)
  peek p' = do
    fGrayscale <- (#peek sk_highcontrastconfig_t, fGrayscale) p'
    fInvertStyle <- (#peek sk_highcontrastconfig_t, fInvertStyle) p'
    fContrast <- (#peek sk_highcontrastconfig_t, fContrast) p'
    pure Sk_highcontrastconfig{..}
  poke p' Sk_highcontrastconfig{..} = do
    (#poke sk_highcontrastconfig_t, fGrayscale) p' fGrayscale
    (#poke sk_highcontrastconfig_t, fInvertStyle) p' fInvertStyle
    (#poke sk_highcontrastconfig_t, fContrast) p' fContrast

{- | C enum: @"sk_pngencoder_filterflags_t"@

@
typedef enum 
{
  ZERO_SK_PNGENCODER_FILTER_FLAGS = 0x00,
  NONE_SK_PNGENCODER_FILTER_FLAGS = 0x08,
  SUB_SK_PNGENCODER_FILTER_FLAGS = 0x10,
  UP_SK_PNGENCODER_FILTER_FLAGS = 0x20,
  AVG_SK_PNGENCODER_FILTER_FLAGS = 0x40,
  PAETH_SK_PNGENCODER_FILTER_FLAGS = 0x80,
  ALL_SK_PNGENCODER_FILTER_FLAGS = (((NONE_SK_PNGENCODER_FILTER_FLAGS | SUB_SK_PNGENCODER_FILTER_FLAGS) | UP_SK_PNGENCODER_FILTER_FLAGS) | AVG_SK_PNGENCODER_FILTER_FLAGS) | PAETH_SK_PNGENCODER_FILTER_FLAGS
} sk_pngencoder_filterflags_t
@

-}
newtype Sk_pngencoder_filterflags = Sk_pngencoder_filterflags (#type sk_pngencoder_filterflags_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_pngencoder_filterflags_t"@ value (1/7): @"ZERO_SK_PNGENCODER_FILTER_FLAGS"@
pattern ZERO_SK_PNGENCODER_FILTER_FLAGS :: Sk_pngencoder_filterflags
pattern ZERO_SK_PNGENCODER_FILTER_FLAGS = (#const ZERO_SK_PNGENCODER_FILTER_FLAGS)

-- | C enum @"sk_pngencoder_filterflags_t"@ value (2/7): @"NONE_SK_PNGENCODER_FILTER_FLAGS"@
pattern NONE_SK_PNGENCODER_FILTER_FLAGS :: Sk_pngencoder_filterflags
pattern NONE_SK_PNGENCODER_FILTER_FLAGS = (#const NONE_SK_PNGENCODER_FILTER_FLAGS)

-- | C enum @"sk_pngencoder_filterflags_t"@ value (3/7): @"SUB_SK_PNGENCODER_FILTER_FLAGS"@
pattern SUB_SK_PNGENCODER_FILTER_FLAGS :: Sk_pngencoder_filterflags
pattern SUB_SK_PNGENCODER_FILTER_FLAGS = (#const SUB_SK_PNGENCODER_FILTER_FLAGS)

-- | C enum @"sk_pngencoder_filterflags_t"@ value (4/7): @"UP_SK_PNGENCODER_FILTER_FLAGS"@
pattern UP_SK_PNGENCODER_FILTER_FLAGS :: Sk_pngencoder_filterflags
pattern UP_SK_PNGENCODER_FILTER_FLAGS = (#const UP_SK_PNGENCODER_FILTER_FLAGS)

-- | C enum @"sk_pngencoder_filterflags_t"@ value (5/7): @"AVG_SK_PNGENCODER_FILTER_FLAGS"@
pattern AVG_SK_PNGENCODER_FILTER_FLAGS :: Sk_pngencoder_filterflags
pattern AVG_SK_PNGENCODER_FILTER_FLAGS = (#const AVG_SK_PNGENCODER_FILTER_FLAGS)

-- | C enum @"sk_pngencoder_filterflags_t"@ value (6/7): @"PAETH_SK_PNGENCODER_FILTER_FLAGS"@
pattern PAETH_SK_PNGENCODER_FILTER_FLAGS :: Sk_pngencoder_filterflags
pattern PAETH_SK_PNGENCODER_FILTER_FLAGS = (#const PAETH_SK_PNGENCODER_FILTER_FLAGS)

-- | C enum @"sk_pngencoder_filterflags_t"@ value (7/7): @"ALL_SK_PNGENCODER_FILTER_FLAGS"@
pattern ALL_SK_PNGENCODER_FILTER_FLAGS :: Sk_pngencoder_filterflags
pattern ALL_SK_PNGENCODER_FILTER_FLAGS = (#const ALL_SK_PNGENCODER_FILTER_FLAGS)

{- | C struct: @"sk_pngencoder_options_t"@

@
typedef struct 
{
  sk_pngencoder_filterflags_t fFilterFlags;
  int fZLibLevel;
  void *fComments;
  const sk_colorspace_icc_profile_t *fICCProfile;
  const char *fICCProfileDescription;
} sk_pngencoder_options_t
@
-}
data Sk_pngencoder_options = Sk_pngencoder_options
  { fFilterFlags :: Sk_pngencoder_filterflags -- ^ C field: @"sk_pngencoder_filterflags_t fFilterFlags"@
  , fZLibLevel :: CInt -- ^ C field: @"int fZLibLevel"@
  , fComments :: Ptr (()) -- ^ C field: @"void *fComments"@
  , fICCProfile :: Ptr (Sk_colorspace_icc_profile) -- ^ C field: @"const sk_colorspace_icc_profile_t *fICCProfile"@
  , fICCProfileDescription :: Ptr (CChar) -- ^ C field: @"const char *fICCProfileDescription"@
  }
instance Foreign.Storable.Offset.Offset "fFilterFlags" Sk_pngencoder_options where
  rawOffset = (#offset sk_pngencoder_options_t, fFilterFlags)
instance Foreign.Storable.Offset.Offset "fZLibLevel" Sk_pngencoder_options where
  rawOffset = (#offset sk_pngencoder_options_t, fZLibLevel)
instance Foreign.Storable.Offset.Offset "fComments" Sk_pngencoder_options where
  rawOffset = (#offset sk_pngencoder_options_t, fComments)
instance Foreign.Storable.Offset.Offset "fICCProfile" Sk_pngencoder_options where
  rawOffset = (#offset sk_pngencoder_options_t, fICCProfile)
instance Foreign.Storable.Offset.Offset "fICCProfileDescription" Sk_pngencoder_options where
  rawOffset = (#offset sk_pngencoder_options_t, fICCProfileDescription)
instance Foreign.Storable.Storable Sk_pngencoder_options where
  sizeOf _ = (#size sk_pngencoder_options_t)
  alignment _ = (#alignment sk_pngencoder_options_t)
  peek p' = do
    fFilterFlags <- (#peek sk_pngencoder_options_t, fFilterFlags) p'
    fZLibLevel <- (#peek sk_pngencoder_options_t, fZLibLevel) p'
    fComments <- (#peek sk_pngencoder_options_t, fComments) p'
    fICCProfile <- (#peek sk_pngencoder_options_t, fICCProfile) p'
    fICCProfileDescription <- (#peek sk_pngencoder_options_t, fICCProfileDescription) p'
    pure Sk_pngencoder_options{..}
  poke p' Sk_pngencoder_options{..} = do
    (#poke sk_pngencoder_options_t, fFilterFlags) p' fFilterFlags
    (#poke sk_pngencoder_options_t, fZLibLevel) p' fZLibLevel
    (#poke sk_pngencoder_options_t, fComments) p' fComments
    (#poke sk_pngencoder_options_t, fICCProfile) p' fICCProfile
    (#poke sk_pngencoder_options_t, fICCProfileDescription) p' fICCProfileDescription

{- | C enum: @"sk_jpegencoder_downsample_t"@

@
typedef enum 
{
  DOWNSAMPLE_420_SK_JPEGENCODER_DOWNSAMPLE,
  DOWNSAMPLE_422_SK_JPEGENCODER_DOWNSAMPLE,
  DOWNSAMPLE_444_SK_JPEGENCODER_DOWNSAMPLE
} sk_jpegencoder_downsample_t
@

-}
newtype Sk_jpegencoder_downsample = Sk_jpegencoder_downsample (#type sk_jpegencoder_downsample_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_jpegencoder_downsample_t"@ value (1/3): @"DOWNSAMPLE_420_SK_JPEGENCODER_DOWNSAMPLE"@
pattern DOWNSAMPLE_420_SK_JPEGENCODER_DOWNSAMPLE :: Sk_jpegencoder_downsample
pattern DOWNSAMPLE_420_SK_JPEGENCODER_DOWNSAMPLE = (#const DOWNSAMPLE_420_SK_JPEGENCODER_DOWNSAMPLE)

-- | C enum @"sk_jpegencoder_downsample_t"@ value (2/3): @"DOWNSAMPLE_422_SK_JPEGENCODER_DOWNSAMPLE"@
pattern DOWNSAMPLE_422_SK_JPEGENCODER_DOWNSAMPLE :: Sk_jpegencoder_downsample
pattern DOWNSAMPLE_422_SK_JPEGENCODER_DOWNSAMPLE = (#const DOWNSAMPLE_422_SK_JPEGENCODER_DOWNSAMPLE)

-- | C enum @"sk_jpegencoder_downsample_t"@ value (3/3): @"DOWNSAMPLE_444_SK_JPEGENCODER_DOWNSAMPLE"@
pattern DOWNSAMPLE_444_SK_JPEGENCODER_DOWNSAMPLE :: Sk_jpegencoder_downsample
pattern DOWNSAMPLE_444_SK_JPEGENCODER_DOWNSAMPLE = (#const DOWNSAMPLE_444_SK_JPEGENCODER_DOWNSAMPLE)

{- | C enum: @"sk_jpegencoder_alphaoption_t"@

@
typedef enum 
{
  IGNORE_SK_JPEGENCODER_ALPHA_OPTION,
  BLEND_ON_BLACK_SK_JPEGENCODER_ALPHA_OPTION
} sk_jpegencoder_alphaoption_t
@

-}
newtype Sk_jpegencoder_alphaoption = Sk_jpegencoder_alphaoption (#type sk_jpegencoder_alphaoption_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_jpegencoder_alphaoption_t"@ value (1/2): @"IGNORE_SK_JPEGENCODER_ALPHA_OPTION"@
pattern IGNORE_SK_JPEGENCODER_ALPHA_OPTION :: Sk_jpegencoder_alphaoption
pattern IGNORE_SK_JPEGENCODER_ALPHA_OPTION = (#const IGNORE_SK_JPEGENCODER_ALPHA_OPTION)

-- | C enum @"sk_jpegencoder_alphaoption_t"@ value (2/2): @"BLEND_ON_BLACK_SK_JPEGENCODER_ALPHA_OPTION"@
pattern BLEND_ON_BLACK_SK_JPEGENCODER_ALPHA_OPTION :: Sk_jpegencoder_alphaoption
pattern BLEND_ON_BLACK_SK_JPEGENCODER_ALPHA_OPTION = (#const BLEND_ON_BLACK_SK_JPEGENCODER_ALPHA_OPTION)

{- | C struct: @"sk_jpegencoder_options_t"@

@
typedef struct 
{
  int fQuality;
  sk_jpegencoder_downsample_t fDownsample;
  sk_jpegencoder_alphaoption_t fAlphaOption;
  const sk_data_t *xmpMetadata;
  const sk_colorspace_icc_profile_t *fICCProfile;
  const char *fICCProfileDescription;
} sk_jpegencoder_options_t
@
-}
data Sk_jpegencoder_options = Sk_jpegencoder_options
  { fQuality :: CInt -- ^ C field: @"int fQuality"@
  , fDownsample :: Sk_jpegencoder_downsample -- ^ C field: @"sk_jpegencoder_downsample_t fDownsample"@
  , fAlphaOption :: Sk_jpegencoder_alphaoption -- ^ C field: @"sk_jpegencoder_alphaoption_t fAlphaOption"@
  , xmpMetadata :: Ptr (Sk_data) -- ^ C field: @"const sk_data_t *xmpMetadata"@
  , fICCProfile :: Ptr (Sk_colorspace_icc_profile) -- ^ C field: @"const sk_colorspace_icc_profile_t *fICCProfile"@
  , fICCProfileDescription :: Ptr (CChar) -- ^ C field: @"const char *fICCProfileDescription"@
  }
instance Foreign.Storable.Offset.Offset "fQuality" Sk_jpegencoder_options where
  rawOffset = (#offset sk_jpegencoder_options_t, fQuality)
instance Foreign.Storable.Offset.Offset "fDownsample" Sk_jpegencoder_options where
  rawOffset = (#offset sk_jpegencoder_options_t, fDownsample)
instance Foreign.Storable.Offset.Offset "fAlphaOption" Sk_jpegencoder_options where
  rawOffset = (#offset sk_jpegencoder_options_t, fAlphaOption)
instance Foreign.Storable.Offset.Offset "xmpMetadata" Sk_jpegencoder_options where
  rawOffset = (#offset sk_jpegencoder_options_t, xmpMetadata)
instance Foreign.Storable.Offset.Offset "fICCProfile" Sk_jpegencoder_options where
  rawOffset = (#offset sk_jpegencoder_options_t, fICCProfile)
instance Foreign.Storable.Offset.Offset "fICCProfileDescription" Sk_jpegencoder_options where
  rawOffset = (#offset sk_jpegencoder_options_t, fICCProfileDescription)
instance Foreign.Storable.Storable Sk_jpegencoder_options where
  sizeOf _ = (#size sk_jpegencoder_options_t)
  alignment _ = (#alignment sk_jpegencoder_options_t)
  peek p' = do
    fQuality <- (#peek sk_jpegencoder_options_t, fQuality) p'
    fDownsample <- (#peek sk_jpegencoder_options_t, fDownsample) p'
    fAlphaOption <- (#peek sk_jpegencoder_options_t, fAlphaOption) p'
    xmpMetadata <- (#peek sk_jpegencoder_options_t, xmpMetadata) p'
    fICCProfile <- (#peek sk_jpegencoder_options_t, fICCProfile) p'
    fICCProfileDescription <- (#peek sk_jpegencoder_options_t, fICCProfileDescription) p'
    pure Sk_jpegencoder_options{..}
  poke p' Sk_jpegencoder_options{..} = do
    (#poke sk_jpegencoder_options_t, fQuality) p' fQuality
    (#poke sk_jpegencoder_options_t, fDownsample) p' fDownsample
    (#poke sk_jpegencoder_options_t, fAlphaOption) p' fAlphaOption
    (#poke sk_jpegencoder_options_t, xmpMetadata) p' xmpMetadata
    (#poke sk_jpegencoder_options_t, fICCProfile) p' fICCProfile
    (#poke sk_jpegencoder_options_t, fICCProfileDescription) p' fICCProfileDescription

{- | C enum: @"sk_webpencoder_compression_t"@

@
typedef enum 
{
  LOSSY_SK_WEBPENCODER_COMPTRESSION,
  LOSSLESS_SK_WEBPENCODER_COMPTRESSION
} sk_webpencoder_compression_t
@

-}
newtype Sk_webpencoder_compression = Sk_webpencoder_compression (#type sk_webpencoder_compression_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_webpencoder_compression_t"@ value (1/2): @"LOSSY_SK_WEBPENCODER_COMPTRESSION"@
pattern LOSSY_SK_WEBPENCODER_COMPTRESSION :: Sk_webpencoder_compression
pattern LOSSY_SK_WEBPENCODER_COMPTRESSION = (#const LOSSY_SK_WEBPENCODER_COMPTRESSION)

-- | C enum @"sk_webpencoder_compression_t"@ value (2/2): @"LOSSLESS_SK_WEBPENCODER_COMPTRESSION"@
pattern LOSSLESS_SK_WEBPENCODER_COMPTRESSION :: Sk_webpencoder_compression
pattern LOSSLESS_SK_WEBPENCODER_COMPTRESSION = (#const LOSSLESS_SK_WEBPENCODER_COMPTRESSION)

{- | C struct: @"sk_webpencoder_options_t"@

@
typedef struct 
{
  sk_webpencoder_compression_t fCompression;
  float fQuality;
  const sk_colorspace_icc_profile_t *fICCProfile;
  const char *fICCProfileDescription;
} sk_webpencoder_options_t
@
-}
data Sk_webpencoder_options = Sk_webpencoder_options
  { fCompression :: Sk_webpencoder_compression -- ^ C field: @"sk_webpencoder_compression_t fCompression"@
  , fQuality :: CFloat -- ^ C field: @"float fQuality"@
  , fICCProfile :: Ptr (Sk_colorspace_icc_profile) -- ^ C field: @"const sk_colorspace_icc_profile_t *fICCProfile"@
  , fICCProfileDescription :: Ptr (CChar) -- ^ C field: @"const char *fICCProfileDescription"@
  }
instance Foreign.Storable.Offset.Offset "fCompression" Sk_webpencoder_options where
  rawOffset = (#offset sk_webpencoder_options_t, fCompression)
instance Foreign.Storable.Offset.Offset "fQuality" Sk_webpencoder_options where
  rawOffset = (#offset sk_webpencoder_options_t, fQuality)
instance Foreign.Storable.Offset.Offset "fICCProfile" Sk_webpencoder_options where
  rawOffset = (#offset sk_webpencoder_options_t, fICCProfile)
instance Foreign.Storable.Offset.Offset "fICCProfileDescription" Sk_webpencoder_options where
  rawOffset = (#offset sk_webpencoder_options_t, fICCProfileDescription)
instance Foreign.Storable.Storable Sk_webpencoder_options where
  sizeOf _ = (#size sk_webpencoder_options_t)
  alignment _ = (#alignment sk_webpencoder_options_t)
  peek p' = do
    fCompression <- (#peek sk_webpencoder_options_t, fCompression) p'
    fQuality <- (#peek sk_webpencoder_options_t, fQuality) p'
    fICCProfile <- (#peek sk_webpencoder_options_t, fICCProfile) p'
    fICCProfileDescription <- (#peek sk_webpencoder_options_t, fICCProfileDescription) p'
    pure Sk_webpencoder_options{..}
  poke p' Sk_webpencoder_options{..} = do
    (#poke sk_webpencoder_options_t, fCompression) p' fCompression
    (#poke sk_webpencoder_options_t, fQuality) p' fQuality
    (#poke sk_webpencoder_options_t, fICCProfile) p' fICCProfile
    (#poke sk_webpencoder_options_t, fICCProfileDescription) p' fICCProfileDescription

{- | Opaque C struct: @"sk_rrect_t"@
-}
data Sk_rrect = Sk_rrect

{- | C enum: @"sk_rrect_type_t"@

@
typedef enum 
{
  EMPTY_SK_RRECT_TYPE,
  RECT_SK_RRECT_TYPE,
  OVAL_SK_RRECT_TYPE,
  SIMPLE_SK_RRECT_TYPE,
  NINE_PATCH_SK_RRECT_TYPE,
  COMPLEX_SK_RRECT_TYPE
} sk_rrect_type_t
@

-}
newtype Sk_rrect_type = Sk_rrect_type (#type sk_rrect_type_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_rrect_type_t"@ value (1/6): @"EMPTY_SK_RRECT_TYPE"@
pattern EMPTY_SK_RRECT_TYPE :: Sk_rrect_type
pattern EMPTY_SK_RRECT_TYPE = (#const EMPTY_SK_RRECT_TYPE)

-- | C enum @"sk_rrect_type_t"@ value (2/6): @"RECT_SK_RRECT_TYPE"@
pattern RECT_SK_RRECT_TYPE :: Sk_rrect_type
pattern RECT_SK_RRECT_TYPE = (#const RECT_SK_RRECT_TYPE)

-- | C enum @"sk_rrect_type_t"@ value (3/6): @"OVAL_SK_RRECT_TYPE"@
pattern OVAL_SK_RRECT_TYPE :: Sk_rrect_type
pattern OVAL_SK_RRECT_TYPE = (#const OVAL_SK_RRECT_TYPE)

-- | C enum @"sk_rrect_type_t"@ value (4/6): @"SIMPLE_SK_RRECT_TYPE"@
pattern SIMPLE_SK_RRECT_TYPE :: Sk_rrect_type
pattern SIMPLE_SK_RRECT_TYPE = (#const SIMPLE_SK_RRECT_TYPE)

-- | C enum @"sk_rrect_type_t"@ value (5/6): @"NINE_PATCH_SK_RRECT_TYPE"@
pattern NINE_PATCH_SK_RRECT_TYPE :: Sk_rrect_type
pattern NINE_PATCH_SK_RRECT_TYPE = (#const NINE_PATCH_SK_RRECT_TYPE)

-- | C enum @"sk_rrect_type_t"@ value (6/6): @"COMPLEX_SK_RRECT_TYPE"@
pattern COMPLEX_SK_RRECT_TYPE :: Sk_rrect_type
pattern COMPLEX_SK_RRECT_TYPE = (#const COMPLEX_SK_RRECT_TYPE)

{- | C enum: @"sk_rrect_corner_t"@

@
typedef enum 
{
  UPPER_LEFT_SK_RRECT_CORNER,
  UPPER_RIGHT_SK_RRECT_CORNER,
  LOWER_RIGHT_SK_RRECT_CORNER,
  LOWER_LEFT_SK_RRECT_CORNER
} sk_rrect_corner_t
@

-}
newtype Sk_rrect_corner = Sk_rrect_corner (#type sk_rrect_corner_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_rrect_corner_t"@ value (1/4): @"UPPER_LEFT_SK_RRECT_CORNER"@
pattern UPPER_LEFT_SK_RRECT_CORNER :: Sk_rrect_corner
pattern UPPER_LEFT_SK_RRECT_CORNER = (#const UPPER_LEFT_SK_RRECT_CORNER)

-- | C enum @"sk_rrect_corner_t"@ value (2/4): @"UPPER_RIGHT_SK_RRECT_CORNER"@
pattern UPPER_RIGHT_SK_RRECT_CORNER :: Sk_rrect_corner
pattern UPPER_RIGHT_SK_RRECT_CORNER = (#const UPPER_RIGHT_SK_RRECT_CORNER)

-- | C enum @"sk_rrect_corner_t"@ value (3/4): @"LOWER_RIGHT_SK_RRECT_CORNER"@
pattern LOWER_RIGHT_SK_RRECT_CORNER :: Sk_rrect_corner
pattern LOWER_RIGHT_SK_RRECT_CORNER = (#const LOWER_RIGHT_SK_RRECT_CORNER)

-- | C enum @"sk_rrect_corner_t"@ value (4/4): @"LOWER_LEFT_SK_RRECT_CORNER"@
pattern LOWER_LEFT_SK_RRECT_CORNER :: Sk_rrect_corner
pattern LOWER_LEFT_SK_RRECT_CORNER = (#const LOWER_LEFT_SK_RRECT_CORNER)

{- | Opaque C struct: @"sk_textblob_t"@
-}
data Sk_textblob = Sk_textblob

{- | Opaque C struct: @"sk_textblob_builder_t"@
-}
data Sk_textblob_builder = Sk_textblob_builder

{- | C struct: @"sk_textblob_builder_runbuffer_t"@

@
typedef struct 
{
  void *glyphs;
  void *pos;
  void *utf8text;
  void *clusters;
} sk_textblob_builder_runbuffer_t
@
-}
data Sk_textblob_builder_runbuffer = Sk_textblob_builder_runbuffer
  { glyphs :: Ptr (()) -- ^ C field: @"void *glyphs"@
  , pos :: Ptr (()) -- ^ C field: @"void *pos"@
  , utf8text :: Ptr (()) -- ^ C field: @"void *utf8text"@
  , clusters :: Ptr (()) -- ^ C field: @"void *clusters"@
  }
instance Foreign.Storable.Offset.Offset "glyphs" Sk_textblob_builder_runbuffer where
  rawOffset = (#offset sk_textblob_builder_runbuffer_t, glyphs)
instance Foreign.Storable.Offset.Offset "pos" Sk_textblob_builder_runbuffer where
  rawOffset = (#offset sk_textblob_builder_runbuffer_t, pos)
instance Foreign.Storable.Offset.Offset "utf8text" Sk_textblob_builder_runbuffer where
  rawOffset = (#offset sk_textblob_builder_runbuffer_t, utf8text)
instance Foreign.Storable.Offset.Offset "clusters" Sk_textblob_builder_runbuffer where
  rawOffset = (#offset sk_textblob_builder_runbuffer_t, clusters)
instance Foreign.Storable.Storable Sk_textblob_builder_runbuffer where
  sizeOf _ = (#size sk_textblob_builder_runbuffer_t)
  alignment _ = (#alignment sk_textblob_builder_runbuffer_t)
  peek p' = do
    glyphs <- (#peek sk_textblob_builder_runbuffer_t, glyphs) p'
    pos <- (#peek sk_textblob_builder_runbuffer_t, pos) p'
    utf8text <- (#peek sk_textblob_builder_runbuffer_t, utf8text) p'
    clusters <- (#peek sk_textblob_builder_runbuffer_t, clusters) p'
    pure Sk_textblob_builder_runbuffer{..}
  poke p' Sk_textblob_builder_runbuffer{..} = do
    (#poke sk_textblob_builder_runbuffer_t, glyphs) p' glyphs
    (#poke sk_textblob_builder_runbuffer_t, pos) p' pos
    (#poke sk_textblob_builder_runbuffer_t, utf8text) p' utf8text
    (#poke sk_textblob_builder_runbuffer_t, clusters) p' clusters

{- | C struct: @"sk_rsxform_t"@

@
typedef struct 
{
  float fSCos;
  float fSSin;
  float fTX;
  float fTY;
} sk_rsxform_t
@
-}
data Sk_rsxform = Sk_rsxform
  { fSCos :: CFloat -- ^ C field: @"float fSCos"@
  , fSSin :: CFloat -- ^ C field: @"float fSSin"@
  , fTX :: CFloat -- ^ C field: @"float fTX"@
  , fTY :: CFloat -- ^ C field: @"float fTY"@
  }
instance Foreign.Storable.Offset.Offset "fSCos" Sk_rsxform where
  rawOffset = (#offset sk_rsxform_t, fSCos)
instance Foreign.Storable.Offset.Offset "fSSin" Sk_rsxform where
  rawOffset = (#offset sk_rsxform_t, fSSin)
instance Foreign.Storable.Offset.Offset "fTX" Sk_rsxform where
  rawOffset = (#offset sk_rsxform_t, fTX)
instance Foreign.Storable.Offset.Offset "fTY" Sk_rsxform where
  rawOffset = (#offset sk_rsxform_t, fTY)
instance Foreign.Storable.Storable Sk_rsxform where
  sizeOf _ = (#size sk_rsxform_t)
  alignment _ = (#alignment sk_rsxform_t)
  peek p' = do
    fSCos <- (#peek sk_rsxform_t, fSCos) p'
    fSSin <- (#peek sk_rsxform_t, fSSin) p'
    fTX <- (#peek sk_rsxform_t, fTX) p'
    fTY <- (#peek sk_rsxform_t, fTY) p'
    pure Sk_rsxform{..}
  poke p' Sk_rsxform{..} = do
    (#poke sk_rsxform_t, fSCos) p' fSCos
    (#poke sk_rsxform_t, fSSin) p' fSSin
    (#poke sk_rsxform_t, fTX) p' fTX
    (#poke sk_rsxform_t, fTY) p' fTY

{- | Opaque C struct: @"sk_tracememorydump_t"@
-}
data Sk_tracememorydump = Sk_tracememorydump

{- | Opaque C struct: @"sk_runtimeeffect_t"@
-}
data Sk_runtimeeffect = Sk_runtimeeffect

{- | C enum: @"sk_runtimeeffect_uniform_type_t"@

@
typedef enum 
{
  FLOAT_SK_RUNTIMEEFFECT_UNIFORM_TYPE,
  FLOAT2_SK_RUNTIMEEFFECT_UNIFORM_TYPE,
  FLOAT3_SK_RUNTIMEEFFECT_UNIFORM_TYPE,
  FLOAT4_SK_RUNTIMEEFFECT_UNIFORM_TYPE,
  FLOAT2X2_SK_RUNTIMEEFFECT_UNIFORM_TYPE,
  FLOAT3X3_SK_RUNTIMEEFFECT_UNIFORM_TYPE,
  FLOAT4X4_SK_RUNTIMEEFFECT_UNIFORM_TYPE,
  INT_SK_RUNTIMEEFFECT_UNIFORM_TYPE,
  INT2_SK_RUNTIMEEFFECT_UNIFORM_TYPE,
  INT3_SK_RUNTIMEEFFECT_UNIFORM_TYPE,
  INT4_SK_RUNTIMEEFFECT_UNIFORM_TYPE
} sk_runtimeeffect_uniform_type_t
@

-}
newtype Sk_runtimeeffect_uniform_type = Sk_runtimeeffect_uniform_type (#type sk_runtimeeffect_uniform_type_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_runtimeeffect_uniform_type_t"@ value (1/11): @"FLOAT_SK_RUNTIMEEFFECT_UNIFORM_TYPE"@
pattern FLOAT_SK_RUNTIMEEFFECT_UNIFORM_TYPE :: Sk_runtimeeffect_uniform_type
pattern FLOAT_SK_RUNTIMEEFFECT_UNIFORM_TYPE = (#const FLOAT_SK_RUNTIMEEFFECT_UNIFORM_TYPE)

-- | C enum @"sk_runtimeeffect_uniform_type_t"@ value (2/11): @"FLOAT2_SK_RUNTIMEEFFECT_UNIFORM_TYPE"@
pattern FLOAT2_SK_RUNTIMEEFFECT_UNIFORM_TYPE :: Sk_runtimeeffect_uniform_type
pattern FLOAT2_SK_RUNTIMEEFFECT_UNIFORM_TYPE = (#const FLOAT2_SK_RUNTIMEEFFECT_UNIFORM_TYPE)

-- | C enum @"sk_runtimeeffect_uniform_type_t"@ value (3/11): @"FLOAT3_SK_RUNTIMEEFFECT_UNIFORM_TYPE"@
pattern FLOAT3_SK_RUNTIMEEFFECT_UNIFORM_TYPE :: Sk_runtimeeffect_uniform_type
pattern FLOAT3_SK_RUNTIMEEFFECT_UNIFORM_TYPE = (#const FLOAT3_SK_RUNTIMEEFFECT_UNIFORM_TYPE)

-- | C enum @"sk_runtimeeffect_uniform_type_t"@ value (4/11): @"FLOAT4_SK_RUNTIMEEFFECT_UNIFORM_TYPE"@
pattern FLOAT4_SK_RUNTIMEEFFECT_UNIFORM_TYPE :: Sk_runtimeeffect_uniform_type
pattern FLOAT4_SK_RUNTIMEEFFECT_UNIFORM_TYPE = (#const FLOAT4_SK_RUNTIMEEFFECT_UNIFORM_TYPE)

-- | C enum @"sk_runtimeeffect_uniform_type_t"@ value (5/11): @"FLOAT2X2_SK_RUNTIMEEFFECT_UNIFORM_TYPE"@
pattern FLOAT2X2_SK_RUNTIMEEFFECT_UNIFORM_TYPE :: Sk_runtimeeffect_uniform_type
pattern FLOAT2X2_SK_RUNTIMEEFFECT_UNIFORM_TYPE = (#const FLOAT2X2_SK_RUNTIMEEFFECT_UNIFORM_TYPE)

-- | C enum @"sk_runtimeeffect_uniform_type_t"@ value (6/11): @"FLOAT3X3_SK_RUNTIMEEFFECT_UNIFORM_TYPE"@
pattern FLOAT3X3_SK_RUNTIMEEFFECT_UNIFORM_TYPE :: Sk_runtimeeffect_uniform_type
pattern FLOAT3X3_SK_RUNTIMEEFFECT_UNIFORM_TYPE = (#const FLOAT3X3_SK_RUNTIMEEFFECT_UNIFORM_TYPE)

-- | C enum @"sk_runtimeeffect_uniform_type_t"@ value (7/11): @"FLOAT4X4_SK_RUNTIMEEFFECT_UNIFORM_TYPE"@
pattern FLOAT4X4_SK_RUNTIMEEFFECT_UNIFORM_TYPE :: Sk_runtimeeffect_uniform_type
pattern FLOAT4X4_SK_RUNTIMEEFFECT_UNIFORM_TYPE = (#const FLOAT4X4_SK_RUNTIMEEFFECT_UNIFORM_TYPE)

-- | C enum @"sk_runtimeeffect_uniform_type_t"@ value (8/11): @"INT_SK_RUNTIMEEFFECT_UNIFORM_TYPE"@
pattern INT_SK_RUNTIMEEFFECT_UNIFORM_TYPE :: Sk_runtimeeffect_uniform_type
pattern INT_SK_RUNTIMEEFFECT_UNIFORM_TYPE = (#const INT_SK_RUNTIMEEFFECT_UNIFORM_TYPE)

-- | C enum @"sk_runtimeeffect_uniform_type_t"@ value (9/11): @"INT2_SK_RUNTIMEEFFECT_UNIFORM_TYPE"@
pattern INT2_SK_RUNTIMEEFFECT_UNIFORM_TYPE :: Sk_runtimeeffect_uniform_type
pattern INT2_SK_RUNTIMEEFFECT_UNIFORM_TYPE = (#const INT2_SK_RUNTIMEEFFECT_UNIFORM_TYPE)

-- | C enum @"sk_runtimeeffect_uniform_type_t"@ value (10/11): @"INT3_SK_RUNTIMEEFFECT_UNIFORM_TYPE"@
pattern INT3_SK_RUNTIMEEFFECT_UNIFORM_TYPE :: Sk_runtimeeffect_uniform_type
pattern INT3_SK_RUNTIMEEFFECT_UNIFORM_TYPE = (#const INT3_SK_RUNTIMEEFFECT_UNIFORM_TYPE)

-- | C enum @"sk_runtimeeffect_uniform_type_t"@ value (11/11): @"INT4_SK_RUNTIMEEFFECT_UNIFORM_TYPE"@
pattern INT4_SK_RUNTIMEEFFECT_UNIFORM_TYPE :: Sk_runtimeeffect_uniform_type
pattern INT4_SK_RUNTIMEEFFECT_UNIFORM_TYPE = (#const INT4_SK_RUNTIMEEFFECT_UNIFORM_TYPE)

{- | C enum: @"sk_runtimeeffect_child_type_t"@

@
typedef enum 
{
  SHADER_SK_RUNTIMEEFFECT_CHILD_TYPE,
  COLOR_FILTER_SK_RUNTIMEEFFECT_CHILD_TYPE,
  BLENDER_SK_RUNTIMEEFFECT_CHILD_TYPE
} sk_runtimeeffect_child_type_t
@

-}
newtype Sk_runtimeeffect_child_type = Sk_runtimeeffect_child_type (#type sk_runtimeeffect_child_type_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_runtimeeffect_child_type_t"@ value (1/3): @"SHADER_SK_RUNTIMEEFFECT_CHILD_TYPE"@
pattern SHADER_SK_RUNTIMEEFFECT_CHILD_TYPE :: Sk_runtimeeffect_child_type
pattern SHADER_SK_RUNTIMEEFFECT_CHILD_TYPE = (#const SHADER_SK_RUNTIMEEFFECT_CHILD_TYPE)

-- | C enum @"sk_runtimeeffect_child_type_t"@ value (2/3): @"COLOR_FILTER_SK_RUNTIMEEFFECT_CHILD_TYPE"@
pattern COLOR_FILTER_SK_RUNTIMEEFFECT_CHILD_TYPE :: Sk_runtimeeffect_child_type
pattern COLOR_FILTER_SK_RUNTIMEEFFECT_CHILD_TYPE = (#const COLOR_FILTER_SK_RUNTIMEEFFECT_CHILD_TYPE)

-- | C enum @"sk_runtimeeffect_child_type_t"@ value (3/3): @"BLENDER_SK_RUNTIMEEFFECT_CHILD_TYPE"@
pattern BLENDER_SK_RUNTIMEEFFECT_CHILD_TYPE :: Sk_runtimeeffect_child_type
pattern BLENDER_SK_RUNTIMEEFFECT_CHILD_TYPE = (#const BLENDER_SK_RUNTIMEEFFECT_CHILD_TYPE)

{- | C enum: @"sk_runtimeeffect_uniform_flags_t"@

@
typedef enum 
{
  NONE_SK_RUNTIMEEFFECT_UNIFORM_FLAGS = 0x00,
  ARRAY_SK_RUNTIMEEFFECT_UNIFORM_FLAGS = 0x01,
  COLOR_SK_RUNTIMEEFFECT_UNIFORM_FLAGS = 0x02,
  VERTEX_SK_RUNTIMEEFFECT_UNIFORM_FLAGS = 0x04,
  FRAGMENT_SK_RUNTIMEEFFECT_UNIFORM_FLAGS = 0x08,
  HALF_PRECISION_SK_RUNTIMEEFFECT_UNIFORM_FLAGS = 0x10
} sk_runtimeeffect_uniform_flags_t
@

-}
newtype Sk_runtimeeffect_uniform_flags = Sk_runtimeeffect_uniform_flags (#type sk_runtimeeffect_uniform_flags_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_runtimeeffect_uniform_flags_t"@ value (1/6): @"NONE_SK_RUNTIMEEFFECT_UNIFORM_FLAGS"@
pattern NONE_SK_RUNTIMEEFFECT_UNIFORM_FLAGS :: Sk_runtimeeffect_uniform_flags
pattern NONE_SK_RUNTIMEEFFECT_UNIFORM_FLAGS = (#const NONE_SK_RUNTIMEEFFECT_UNIFORM_FLAGS)

-- | C enum @"sk_runtimeeffect_uniform_flags_t"@ value (2/6): @"ARRAY_SK_RUNTIMEEFFECT_UNIFORM_FLAGS"@
pattern ARRAY_SK_RUNTIMEEFFECT_UNIFORM_FLAGS :: Sk_runtimeeffect_uniform_flags
pattern ARRAY_SK_RUNTIMEEFFECT_UNIFORM_FLAGS = (#const ARRAY_SK_RUNTIMEEFFECT_UNIFORM_FLAGS)

-- | C enum @"sk_runtimeeffect_uniform_flags_t"@ value (3/6): @"COLOR_SK_RUNTIMEEFFECT_UNIFORM_FLAGS"@
pattern COLOR_SK_RUNTIMEEFFECT_UNIFORM_FLAGS :: Sk_runtimeeffect_uniform_flags
pattern COLOR_SK_RUNTIMEEFFECT_UNIFORM_FLAGS = (#const COLOR_SK_RUNTIMEEFFECT_UNIFORM_FLAGS)

-- | C enum @"sk_runtimeeffect_uniform_flags_t"@ value (4/6): @"VERTEX_SK_RUNTIMEEFFECT_UNIFORM_FLAGS"@
pattern VERTEX_SK_RUNTIMEEFFECT_UNIFORM_FLAGS :: Sk_runtimeeffect_uniform_flags
pattern VERTEX_SK_RUNTIMEEFFECT_UNIFORM_FLAGS = (#const VERTEX_SK_RUNTIMEEFFECT_UNIFORM_FLAGS)

-- | C enum @"sk_runtimeeffect_uniform_flags_t"@ value (5/6): @"FRAGMENT_SK_RUNTIMEEFFECT_UNIFORM_FLAGS"@
pattern FRAGMENT_SK_RUNTIMEEFFECT_UNIFORM_FLAGS :: Sk_runtimeeffect_uniform_flags
pattern FRAGMENT_SK_RUNTIMEEFFECT_UNIFORM_FLAGS = (#const FRAGMENT_SK_RUNTIMEEFFECT_UNIFORM_FLAGS)

-- | C enum @"sk_runtimeeffect_uniform_flags_t"@ value (6/6): @"HALF_PRECISION_SK_RUNTIMEEFFECT_UNIFORM_FLAGS"@
pattern HALF_PRECISION_SK_RUNTIMEEFFECT_UNIFORM_FLAGS :: Sk_runtimeeffect_uniform_flags
pattern HALF_PRECISION_SK_RUNTIMEEFFECT_UNIFORM_FLAGS = (#const HALF_PRECISION_SK_RUNTIMEEFFECT_UNIFORM_FLAGS)

{- | C struct: @"sk_runtimeeffect_uniform_t"@

@
typedef struct 
{
  const char *fName;
  size_t fNameLength;
  size_t fOffset;
  sk_runtimeeffect_uniform_type_t fType;
  int fCount;
  sk_runtimeeffect_uniform_flags_t fFlags;
} sk_runtimeeffect_uniform_t
@
-}
data Sk_runtimeeffect_uniform = Sk_runtimeeffect_uniform
  { fName :: Ptr (CChar) -- ^ C field: @"const char *fName"@
  , fNameLength :: CSize -- ^ C field: @"size_t fNameLength"@
  , fOffset :: CSize -- ^ C field: @"size_t fOffset"@
  , fType :: Sk_runtimeeffect_uniform_type -- ^ C field: @"sk_runtimeeffect_uniform_type_t fType"@
  , fCount :: CInt -- ^ C field: @"int fCount"@
  , fFlags :: Sk_runtimeeffect_uniform_flags -- ^ C field: @"sk_runtimeeffect_uniform_flags_t fFlags"@
  }
instance Foreign.Storable.Offset.Offset "fName" Sk_runtimeeffect_uniform where
  rawOffset = (#offset sk_runtimeeffect_uniform_t, fName)
instance Foreign.Storable.Offset.Offset "fNameLength" Sk_runtimeeffect_uniform where
  rawOffset = (#offset sk_runtimeeffect_uniform_t, fNameLength)
instance Foreign.Storable.Offset.Offset "fOffset" Sk_runtimeeffect_uniform where
  rawOffset = (#offset sk_runtimeeffect_uniform_t, fOffset)
instance Foreign.Storable.Offset.Offset "fType" Sk_runtimeeffect_uniform where
  rawOffset = (#offset sk_runtimeeffect_uniform_t, fType)
instance Foreign.Storable.Offset.Offset "fCount" Sk_runtimeeffect_uniform where
  rawOffset = (#offset sk_runtimeeffect_uniform_t, fCount)
instance Foreign.Storable.Offset.Offset "fFlags" Sk_runtimeeffect_uniform where
  rawOffset = (#offset sk_runtimeeffect_uniform_t, fFlags)
instance Foreign.Storable.Storable Sk_runtimeeffect_uniform where
  sizeOf _ = (#size sk_runtimeeffect_uniform_t)
  alignment _ = (#alignment sk_runtimeeffect_uniform_t)
  peek p' = do
    fName <- (#peek sk_runtimeeffect_uniform_t, fName) p'
    fNameLength <- (#peek sk_runtimeeffect_uniform_t, fNameLength) p'
    fOffset <- (#peek sk_runtimeeffect_uniform_t, fOffset) p'
    fType <- (#peek sk_runtimeeffect_uniform_t, fType) p'
    fCount <- (#peek sk_runtimeeffect_uniform_t, fCount) p'
    fFlags <- (#peek sk_runtimeeffect_uniform_t, fFlags) p'
    pure Sk_runtimeeffect_uniform{..}
  poke p' Sk_runtimeeffect_uniform{..} = do
    (#poke sk_runtimeeffect_uniform_t, fName) p' fName
    (#poke sk_runtimeeffect_uniform_t, fNameLength) p' fNameLength
    (#poke sk_runtimeeffect_uniform_t, fOffset) p' fOffset
    (#poke sk_runtimeeffect_uniform_t, fType) p' fType
    (#poke sk_runtimeeffect_uniform_t, fCount) p' fCount
    (#poke sk_runtimeeffect_uniform_t, fFlags) p' fFlags

{- | C struct: @"sk_runtimeeffect_child_t"@

@
typedef struct 
{
  const char *fName;
  size_t fNameLength;
  sk_runtimeeffect_child_type_t fType;
  int fIndex;
} sk_runtimeeffect_child_t
@
-}
data Sk_runtimeeffect_child = Sk_runtimeeffect_child
  { fName :: Ptr (CChar) -- ^ C field: @"const char *fName"@
  , fNameLength :: CSize -- ^ C field: @"size_t fNameLength"@
  , fType :: Sk_runtimeeffect_child_type -- ^ C field: @"sk_runtimeeffect_child_type_t fType"@
  , fIndex :: CInt -- ^ C field: @"int fIndex"@
  }
instance Foreign.Storable.Offset.Offset "fName" Sk_runtimeeffect_child where
  rawOffset = (#offset sk_runtimeeffect_child_t, fName)
instance Foreign.Storable.Offset.Offset "fNameLength" Sk_runtimeeffect_child where
  rawOffset = (#offset sk_runtimeeffect_child_t, fNameLength)
instance Foreign.Storable.Offset.Offset "fType" Sk_runtimeeffect_child where
  rawOffset = (#offset sk_runtimeeffect_child_t, fType)
instance Foreign.Storable.Offset.Offset "fIndex" Sk_runtimeeffect_child where
  rawOffset = (#offset sk_runtimeeffect_child_t, fIndex)
instance Foreign.Storable.Storable Sk_runtimeeffect_child where
  sizeOf _ = (#size sk_runtimeeffect_child_t)
  alignment _ = (#alignment sk_runtimeeffect_child_t)
  peek p' = do
    fName <- (#peek sk_runtimeeffect_child_t, fName) p'
    fNameLength <- (#peek sk_runtimeeffect_child_t, fNameLength) p'
    fType <- (#peek sk_runtimeeffect_child_t, fType) p'
    fIndex <- (#peek sk_runtimeeffect_child_t, fIndex) p'
    pure Sk_runtimeeffect_child{..}
  poke p' Sk_runtimeeffect_child{..} = do
    (#poke sk_runtimeeffect_child_t, fName) p' fName
    (#poke sk_runtimeeffect_child_t, fNameLength) p' fNameLength
    (#poke sk_runtimeeffect_child_t, fType) p' fType
    (#poke sk_runtimeeffect_child_t, fIndex) p' fIndex

{- | C enum: @"sk_filter_mode_t"@

@
typedef enum 
{
  NEAREST_SK_FILTER_MODE,
  LINEAR_SK_FILTER_MODE
} sk_filter_mode_t
@

-}
newtype Sk_filter_mode = Sk_filter_mode (#type sk_filter_mode_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_filter_mode_t"@ value (1/2): @"NEAREST_SK_FILTER_MODE"@
pattern NEAREST_SK_FILTER_MODE :: Sk_filter_mode
pattern NEAREST_SK_FILTER_MODE = (#const NEAREST_SK_FILTER_MODE)

-- | C enum @"sk_filter_mode_t"@ value (2/2): @"LINEAR_SK_FILTER_MODE"@
pattern LINEAR_SK_FILTER_MODE :: Sk_filter_mode
pattern LINEAR_SK_FILTER_MODE = (#const LINEAR_SK_FILTER_MODE)

{- | C enum: @"sk_mipmap_mode_t"@

@
typedef enum 
{
  NONE_SK_MIPMAP_MODE,
  NEAREST_SK_MIPMAP_MODE,
  LINEAR_SK_MIPMAP_MODE
} sk_mipmap_mode_t
@

-}
newtype Sk_mipmap_mode = Sk_mipmap_mode (#type sk_mipmap_mode_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_mipmap_mode_t"@ value (1/3): @"NONE_SK_MIPMAP_MODE"@
pattern NONE_SK_MIPMAP_MODE :: Sk_mipmap_mode
pattern NONE_SK_MIPMAP_MODE = (#const NONE_SK_MIPMAP_MODE)

-- | C enum @"sk_mipmap_mode_t"@ value (2/3): @"NEAREST_SK_MIPMAP_MODE"@
pattern NEAREST_SK_MIPMAP_MODE :: Sk_mipmap_mode
pattern NEAREST_SK_MIPMAP_MODE = (#const NEAREST_SK_MIPMAP_MODE)

-- | C enum @"sk_mipmap_mode_t"@ value (3/3): @"LINEAR_SK_MIPMAP_MODE"@
pattern LINEAR_SK_MIPMAP_MODE :: Sk_mipmap_mode
pattern LINEAR_SK_MIPMAP_MODE = (#const LINEAR_SK_MIPMAP_MODE)

{- | C struct: @"sk_cubic_resampler_t"@

@
typedef struct 
{
  float fB;
  float fC;
} sk_cubic_resampler_t
@
-}
data Sk_cubic_resampler = Sk_cubic_resampler
  { fB :: CFloat -- ^ C field: @"float fB"@
  , fC :: CFloat -- ^ C field: @"float fC"@
  }
instance Foreign.Storable.Offset.Offset "fB" Sk_cubic_resampler where
  rawOffset = (#offset sk_cubic_resampler_t, fB)
instance Foreign.Storable.Offset.Offset "fC" Sk_cubic_resampler where
  rawOffset = (#offset sk_cubic_resampler_t, fC)
instance Foreign.Storable.Storable Sk_cubic_resampler where
  sizeOf _ = (#size sk_cubic_resampler_t)
  alignment _ = (#alignment sk_cubic_resampler_t)
  peek p' = do
    fB <- (#peek sk_cubic_resampler_t, fB) p'
    fC <- (#peek sk_cubic_resampler_t, fC) p'
    pure Sk_cubic_resampler{..}
  poke p' Sk_cubic_resampler{..} = do
    (#poke sk_cubic_resampler_t, fB) p' fB
    (#poke sk_cubic_resampler_t, fC) p' fC

{- | C struct: @"sk_sampling_options_t"@

@
typedef struct 
{
  int fMaxAniso;
  _Bool fUseCubic;
  sk_cubic_resampler_t fCubic;
  sk_filter_mode_t fFilter;
  sk_mipmap_mode_t fMipmap;
} sk_sampling_options_t
@
-}
data Sk_sampling_options = Sk_sampling_options
  { fMaxAniso :: CInt -- ^ C field: @"int fMaxAniso"@
  , fUseCubic :: CBool -- ^ C field: @"_Bool fUseCubic"@
  , fCubic :: Sk_cubic_resampler -- ^ C field: @"sk_cubic_resampler_t fCubic"@
  , fFilter :: Sk_filter_mode -- ^ C field: @"sk_filter_mode_t fFilter"@
  , fMipmap :: Sk_mipmap_mode -- ^ C field: @"sk_mipmap_mode_t fMipmap"@
  }
instance Foreign.Storable.Offset.Offset "fMaxAniso" Sk_sampling_options where
  rawOffset = (#offset sk_sampling_options_t, fMaxAniso)
instance Foreign.Storable.Offset.Offset "fUseCubic" Sk_sampling_options where
  rawOffset = (#offset sk_sampling_options_t, fUseCubic)
instance Foreign.Storable.Offset.Offset "fCubic" Sk_sampling_options where
  rawOffset = (#offset sk_sampling_options_t, fCubic)
instance Foreign.Storable.Offset.Offset "fFilter" Sk_sampling_options where
  rawOffset = (#offset sk_sampling_options_t, fFilter)
instance Foreign.Storable.Offset.Offset "fMipmap" Sk_sampling_options where
  rawOffset = (#offset sk_sampling_options_t, fMipmap)
instance Foreign.Storable.Storable Sk_sampling_options where
  sizeOf _ = (#size sk_sampling_options_t)
  alignment _ = (#alignment sk_sampling_options_t)
  peek p' = do
    fMaxAniso <- (#peek sk_sampling_options_t, fMaxAniso) p'
    fUseCubic <- (#peek sk_sampling_options_t, fUseCubic) p'
    fCubic <- (#peek sk_sampling_options_t, fCubic) p'
    fFilter <- (#peek sk_sampling_options_t, fFilter) p'
    fMipmap <- (#peek sk_sampling_options_t, fMipmap) p'
    pure Sk_sampling_options{..}
  poke p' Sk_sampling_options{..} = do
    (#poke sk_sampling_options_t, fMaxAniso) p' fMaxAniso
    (#poke sk_sampling_options_t, fUseCubic) p' fUseCubic
    (#poke sk_sampling_options_t, fCubic) p' fCubic
    (#poke sk_sampling_options_t, fFilter) p' fFilter
    (#poke sk_sampling_options_t, fMipmap) p' fMipmap

{- | C enum: @"sk_canvas_savelayerrec_flags_t"@

@
typedef enum 
{
  NONE_SK_CANVAS_SAVELAYERREC_FLAGS = 0,
  PRESERVE_LCD_TEXT_SK_CANVAS_SAVELAYERREC_FLAGS = 1 << 1,
  INITIALIZE_WITH_PREVIOUS_SK_CANVAS_SAVELAYERREC_FLAGS = 1 << 2,
  F16_COLOR_TYPE_SK_CANVAS_SAVELAYERREC_FLAGS = 1 << 4
} sk_canvas_savelayerrec_flags_t
@

-}
newtype Sk_canvas_savelayerrec_flags = Sk_canvas_savelayerrec_flags (#type sk_canvas_savelayerrec_flags_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"sk_canvas_savelayerrec_flags_t"@ value (1/4): @"NONE_SK_CANVAS_SAVELAYERREC_FLAGS"@
pattern NONE_SK_CANVAS_SAVELAYERREC_FLAGS :: Sk_canvas_savelayerrec_flags
pattern NONE_SK_CANVAS_SAVELAYERREC_FLAGS = (#const NONE_SK_CANVAS_SAVELAYERREC_FLAGS)

-- | C enum @"sk_canvas_savelayerrec_flags_t"@ value (2/4): @"PRESERVE_LCD_TEXT_SK_CANVAS_SAVELAYERREC_FLAGS"@
pattern PRESERVE_LCD_TEXT_SK_CANVAS_SAVELAYERREC_FLAGS :: Sk_canvas_savelayerrec_flags
pattern PRESERVE_LCD_TEXT_SK_CANVAS_SAVELAYERREC_FLAGS = (#const PRESERVE_LCD_TEXT_SK_CANVAS_SAVELAYERREC_FLAGS)

-- | C enum @"sk_canvas_savelayerrec_flags_t"@ value (3/4): @"INITIALIZE_WITH_PREVIOUS_SK_CANVAS_SAVELAYERREC_FLAGS"@
pattern INITIALIZE_WITH_PREVIOUS_SK_CANVAS_SAVELAYERREC_FLAGS :: Sk_canvas_savelayerrec_flags
pattern INITIALIZE_WITH_PREVIOUS_SK_CANVAS_SAVELAYERREC_FLAGS = (#const INITIALIZE_WITH_PREVIOUS_SK_CANVAS_SAVELAYERREC_FLAGS)

-- | C enum @"sk_canvas_savelayerrec_flags_t"@ value (4/4): @"F16_COLOR_TYPE_SK_CANVAS_SAVELAYERREC_FLAGS"@
pattern F16_COLOR_TYPE_SK_CANVAS_SAVELAYERREC_FLAGS :: Sk_canvas_savelayerrec_flags
pattern F16_COLOR_TYPE_SK_CANVAS_SAVELAYERREC_FLAGS = (#const F16_COLOR_TYPE_SK_CANVAS_SAVELAYERREC_FLAGS)

{- | C struct: @"sk_canvas_savelayerrec_t"@

@
typedef struct 
{
  sk_rect_t *fBounds;
  sk_paint_t *fPaint;
  sk_imagefilter_t *fBackdrop;
  sk_canvas_savelayerrec_flags_t fFlags;
} sk_canvas_savelayerrec_t
@
-}
data Sk_canvas_savelayerrec = Sk_canvas_savelayerrec
  { fBounds :: Ptr (Sk_rect) -- ^ C field: @"sk_rect_t *fBounds"@
  , fPaint :: Ptr (Sk_paint) -- ^ C field: @"sk_paint_t *fPaint"@
  , fBackdrop :: Ptr (Sk_imagefilter) -- ^ C field: @"sk_imagefilter_t *fBackdrop"@
  , fFlags :: Sk_canvas_savelayerrec_flags -- ^ C field: @"sk_canvas_savelayerrec_flags_t fFlags"@
  }
instance Foreign.Storable.Offset.Offset "fBounds" Sk_canvas_savelayerrec where
  rawOffset = (#offset sk_canvas_savelayerrec_t, fBounds)
instance Foreign.Storable.Offset.Offset "fPaint" Sk_canvas_savelayerrec where
  rawOffset = (#offset sk_canvas_savelayerrec_t, fPaint)
instance Foreign.Storable.Offset.Offset "fBackdrop" Sk_canvas_savelayerrec where
  rawOffset = (#offset sk_canvas_savelayerrec_t, fBackdrop)
instance Foreign.Storable.Offset.Offset "fFlags" Sk_canvas_savelayerrec where
  rawOffset = (#offset sk_canvas_savelayerrec_t, fFlags)
instance Foreign.Storable.Storable Sk_canvas_savelayerrec where
  sizeOf _ = (#size sk_canvas_savelayerrec_t)
  alignment _ = (#alignment sk_canvas_savelayerrec_t)
  peek p' = do
    fBounds <- (#peek sk_canvas_savelayerrec_t, fBounds) p'
    fPaint <- (#peek sk_canvas_savelayerrec_t, fPaint) p'
    fBackdrop <- (#peek sk_canvas_savelayerrec_t, fBackdrop) p'
    fFlags <- (#peek sk_canvas_savelayerrec_t, fFlags) p'
    pure Sk_canvas_savelayerrec{..}
  poke p' Sk_canvas_savelayerrec{..} = do
    (#poke sk_canvas_savelayerrec_t, fBounds) p' fBounds
    (#poke sk_canvas_savelayerrec_t, fPaint) p' fPaint
    (#poke sk_canvas_savelayerrec_t, fBackdrop) p' fBackdrop
    (#poke sk_canvas_savelayerrec_t, fFlags) p' fFlags

{- | C enum: @"gr_surfaceorigin_t"@

@
typedef enum 
{
  TOP_LEFT_GR_SURFACE_ORIGIN,
  BOTTOM_LEFT_GR_SURFACE_ORIGIN
} gr_surfaceorigin_t
@

-}
newtype Gr_surfaceorigin = Gr_surfaceorigin (#type gr_surfaceorigin_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"gr_surfaceorigin_t"@ value (1/2): @"TOP_LEFT_GR_SURFACE_ORIGIN"@
pattern TOP_LEFT_GR_SURFACE_ORIGIN :: Gr_surfaceorigin
pattern TOP_LEFT_GR_SURFACE_ORIGIN = (#const TOP_LEFT_GR_SURFACE_ORIGIN)

-- | C enum @"gr_surfaceorigin_t"@ value (2/2): @"BOTTOM_LEFT_GR_SURFACE_ORIGIN"@
pattern BOTTOM_LEFT_GR_SURFACE_ORIGIN :: Gr_surfaceorigin
pattern BOTTOM_LEFT_GR_SURFACE_ORIGIN = (#const BOTTOM_LEFT_GR_SURFACE_ORIGIN)

{- | C struct: @"gr_context_options_t"@

@
typedef struct 
{
  _Bool fAvoidStencilBuffers;
  int fRuntimeProgramCacheSize;
  size_t fGlyphCacheTextureMaximumBytes;
  _Bool fAllowPathMaskCaching;
  _Bool fDoManualMipmapping;
  int fBufferMapThreshold;
} gr_context_options_t
@
-}
data Gr_context_options = Gr_context_options
  { fAvoidStencilBuffers :: CBool -- ^ C field: @"_Bool fAvoidStencilBuffers"@
  , fRuntimeProgramCacheSize :: CInt -- ^ C field: @"int fRuntimeProgramCacheSize"@
  , fGlyphCacheTextureMaximumBytes :: CSize -- ^ C field: @"size_t fGlyphCacheTextureMaximumBytes"@
  , fAllowPathMaskCaching :: CBool -- ^ C field: @"_Bool fAllowPathMaskCaching"@
  , fDoManualMipmapping :: CBool -- ^ C field: @"_Bool fDoManualMipmapping"@
  , fBufferMapThreshold :: CInt -- ^ C field: @"int fBufferMapThreshold"@
  }
instance Foreign.Storable.Offset.Offset "fAvoidStencilBuffers" Gr_context_options where
  rawOffset = (#offset gr_context_options_t, fAvoidStencilBuffers)
instance Foreign.Storable.Offset.Offset "fRuntimeProgramCacheSize" Gr_context_options where
  rawOffset = (#offset gr_context_options_t, fRuntimeProgramCacheSize)
instance Foreign.Storable.Offset.Offset "fGlyphCacheTextureMaximumBytes" Gr_context_options where
  rawOffset = (#offset gr_context_options_t, fGlyphCacheTextureMaximumBytes)
instance Foreign.Storable.Offset.Offset "fAllowPathMaskCaching" Gr_context_options where
  rawOffset = (#offset gr_context_options_t, fAllowPathMaskCaching)
instance Foreign.Storable.Offset.Offset "fDoManualMipmapping" Gr_context_options where
  rawOffset = (#offset gr_context_options_t, fDoManualMipmapping)
instance Foreign.Storable.Offset.Offset "fBufferMapThreshold" Gr_context_options where
  rawOffset = (#offset gr_context_options_t, fBufferMapThreshold)
instance Foreign.Storable.Storable Gr_context_options where
  sizeOf _ = (#size gr_context_options_t)
  alignment _ = (#alignment gr_context_options_t)
  peek p' = do
    fAvoidStencilBuffers <- (#peek gr_context_options_t, fAvoidStencilBuffers) p'
    fRuntimeProgramCacheSize <- (#peek gr_context_options_t, fRuntimeProgramCacheSize) p'
    fGlyphCacheTextureMaximumBytes <- (#peek gr_context_options_t, fGlyphCacheTextureMaximumBytes) p'
    fAllowPathMaskCaching <- (#peek gr_context_options_t, fAllowPathMaskCaching) p'
    fDoManualMipmapping <- (#peek gr_context_options_t, fDoManualMipmapping) p'
    fBufferMapThreshold <- (#peek gr_context_options_t, fBufferMapThreshold) p'
    pure Gr_context_options{..}
  poke p' Gr_context_options{..} = do
    (#poke gr_context_options_t, fAvoidStencilBuffers) p' fAvoidStencilBuffers
    (#poke gr_context_options_t, fRuntimeProgramCacheSize) p' fRuntimeProgramCacheSize
    (#poke gr_context_options_t, fGlyphCacheTextureMaximumBytes) p' fGlyphCacheTextureMaximumBytes
    (#poke gr_context_options_t, fAllowPathMaskCaching) p' fAllowPathMaskCaching
    (#poke gr_context_options_t, fDoManualMipmapping) p' fDoManualMipmapping
    (#poke gr_context_options_t, fBufferMapThreshold) p' fBufferMapThreshold

{- | C type alias: @gr_backendobject_t@

@
typedef intptr_t gr_backendobject_t
@
-}
type Gr_backendobject = CIntPtr

{- | Opaque C struct: @"gr_backendrendertarget_t"@
-}
data Gr_backendrendertarget = Gr_backendrendertarget

{- | Opaque C struct: @"gr_backendtexture_t"@
-}
data Gr_backendtexture = Gr_backendtexture

{- | Opaque C struct: @"gr_direct_context_t"@
-}
data Gr_direct_context = Gr_direct_context

{- | Opaque C struct: @"gr_recording_context_t"@
-}
data Gr_recording_context = Gr_recording_context

{- | C enum: @"gr_backend_t"@

@
typedef enum 
{
  OPENGL_GR_BACKEND = 0,
  VULKAN_GR_BACKEND = 1,
  METAL_GR_BACKEND = 2,
  DIRECT3D_GR_BACKEND = 3,
  UNSUPPORTED_GR_BACKEND = 5
} gr_backend_t
@

-}
newtype Gr_backend = Gr_backend (#type gr_backend_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"gr_backend_t"@ value (1/5): @"OPENGL_GR_BACKEND"@
pattern OPENGL_GR_BACKEND :: Gr_backend
pattern OPENGL_GR_BACKEND = (#const OPENGL_GR_BACKEND)

-- | C enum @"gr_backend_t"@ value (2/5): @"VULKAN_GR_BACKEND"@
pattern VULKAN_GR_BACKEND :: Gr_backend
pattern VULKAN_GR_BACKEND = (#const VULKAN_GR_BACKEND)

-- | C enum @"gr_backend_t"@ value (3/5): @"METAL_GR_BACKEND"@
pattern METAL_GR_BACKEND :: Gr_backend
pattern METAL_GR_BACKEND = (#const METAL_GR_BACKEND)

-- | C enum @"gr_backend_t"@ value (4/5): @"DIRECT3D_GR_BACKEND"@
pattern DIRECT3D_GR_BACKEND :: Gr_backend
pattern DIRECT3D_GR_BACKEND = (#const DIRECT3D_GR_BACKEND)

-- | C enum @"gr_backend_t"@ value (5/5): @"UNSUPPORTED_GR_BACKEND"@
pattern UNSUPPORTED_GR_BACKEND :: Gr_backend
pattern UNSUPPORTED_GR_BACKEND = (#const UNSUPPORTED_GR_BACKEND)

{- | C type alias: @gr_backendcontext_t@

@
typedef intptr_t gr_backendcontext_t
@
-}
type Gr_backendcontext = CIntPtr

{- | Opaque C struct: @"gr_glinterface_t"@
-}
data Gr_glinterface = Gr_glinterface

-- | C function pointer type: @typedef void (*gr_gl_func_ptr)(void)@
type Gr_gl_func_ptr = IO (())

-- | Creates a 'FunPtr' of @"gr_gl_func_ptr"@.
foreign import ccall "wrapper" mkFunPtr'Gr_gl_func_ptr :: Gr_gl_func_ptr -> IO (FunPtr Gr_gl_func_ptr)

-- | C function pointer type: @typedef gr_gl_func_ptr (*gr_gl_get_proc)(void *ctx, const char *name)@
type Gr_gl_get_proc = Ptr (()) -> Ptr (CChar) -> IO (FunPtr Gr_gl_func_ptr)

-- | Creates a 'FunPtr' of @"gr_gl_get_proc"@.
foreign import ccall "wrapper" mkFunPtr'Gr_gl_get_proc :: Gr_gl_get_proc -> IO (FunPtr Gr_gl_get_proc)

{- | C struct: @"gr_gl_textureinfo_t"@

@
typedef struct 
{
  unsigned int fTarget;
  unsigned int fID;
  unsigned int fFormat;
  _Bool fProtected;
} gr_gl_textureinfo_t
@
-}
data Gr_gl_textureinfo = Gr_gl_textureinfo
  { fTarget :: CUInt -- ^ C field: @"unsigned int fTarget"@
  , fID :: CUInt -- ^ C field: @"unsigned int fID"@
  , fFormat :: CUInt -- ^ C field: @"unsigned int fFormat"@
  , fProtected :: CBool -- ^ C field: @"_Bool fProtected"@
  }
instance Foreign.Storable.Offset.Offset "fTarget" Gr_gl_textureinfo where
  rawOffset = (#offset gr_gl_textureinfo_t, fTarget)
instance Foreign.Storable.Offset.Offset "fID" Gr_gl_textureinfo where
  rawOffset = (#offset gr_gl_textureinfo_t, fID)
instance Foreign.Storable.Offset.Offset "fFormat" Gr_gl_textureinfo where
  rawOffset = (#offset gr_gl_textureinfo_t, fFormat)
instance Foreign.Storable.Offset.Offset "fProtected" Gr_gl_textureinfo where
  rawOffset = (#offset gr_gl_textureinfo_t, fProtected)
instance Foreign.Storable.Storable Gr_gl_textureinfo where
  sizeOf _ = (#size gr_gl_textureinfo_t)
  alignment _ = (#alignment gr_gl_textureinfo_t)
  peek p' = do
    fTarget <- (#peek gr_gl_textureinfo_t, fTarget) p'
    fID <- (#peek gr_gl_textureinfo_t, fID) p'
    fFormat <- (#peek gr_gl_textureinfo_t, fFormat) p'
    fProtected <- (#peek gr_gl_textureinfo_t, fProtected) p'
    pure Gr_gl_textureinfo{..}
  poke p' Gr_gl_textureinfo{..} = do
    (#poke gr_gl_textureinfo_t, fTarget) p' fTarget
    (#poke gr_gl_textureinfo_t, fID) p' fID
    (#poke gr_gl_textureinfo_t, fFormat) p' fFormat
    (#poke gr_gl_textureinfo_t, fProtected) p' fProtected

{- | C struct: @"gr_gl_framebufferinfo_t"@

@
typedef struct 
{
  unsigned int fFBOID;
  unsigned int fFormat;
  _Bool fProtected;
} gr_gl_framebufferinfo_t
@
-}
data Gr_gl_framebufferinfo = Gr_gl_framebufferinfo
  { fFBOID :: CUInt -- ^ C field: @"unsigned int fFBOID"@
  , fFormat :: CUInt -- ^ C field: @"unsigned int fFormat"@
  , fProtected :: CBool -- ^ C field: @"_Bool fProtected"@
  }
instance Foreign.Storable.Offset.Offset "fFBOID" Gr_gl_framebufferinfo where
  rawOffset = (#offset gr_gl_framebufferinfo_t, fFBOID)
instance Foreign.Storable.Offset.Offset "fFormat" Gr_gl_framebufferinfo where
  rawOffset = (#offset gr_gl_framebufferinfo_t, fFormat)
instance Foreign.Storable.Offset.Offset "fProtected" Gr_gl_framebufferinfo where
  rawOffset = (#offset gr_gl_framebufferinfo_t, fProtected)
instance Foreign.Storable.Storable Gr_gl_framebufferinfo where
  sizeOf _ = (#size gr_gl_framebufferinfo_t)
  alignment _ = (#alignment gr_gl_framebufferinfo_t)
  peek p' = do
    fFBOID <- (#peek gr_gl_framebufferinfo_t, fFBOID) p'
    fFormat <- (#peek gr_gl_framebufferinfo_t, fFormat) p'
    fProtected <- (#peek gr_gl_framebufferinfo_t, fProtected) p'
    pure Gr_gl_framebufferinfo{..}
  poke p' Gr_gl_framebufferinfo{..} = do
    (#poke gr_gl_framebufferinfo_t, fFBOID) p' fFBOID
    (#poke gr_gl_framebufferinfo_t, fFormat) p' fFormat
    (#poke gr_gl_framebufferinfo_t, fProtected) p' fProtected

{- | Opaque C struct: @"vk_instance_t"@
-}
data Vk_instance = Vk_instance

{- | Opaque C struct: @"gr_vkinterface_t"@
-}
data Gr_vkinterface = Gr_vkinterface

{- | Opaque C struct: @"vk_physical_device_t"@
-}
data Vk_physical_device = Vk_physical_device

{- | Opaque C struct: @"vk_physical_device_features_t"@
-}
data Vk_physical_device_features = Vk_physical_device_features

{- | Opaque C struct: @"vk_physical_device_features_2_t"@
-}
data Vk_physical_device_features_2 = Vk_physical_device_features_2

{- | Opaque C struct: @"vk_device_t"@
-}
data Vk_device = Vk_device

{- | Opaque C struct: @"vk_queue_t"@
-}
data Vk_queue = Vk_queue

{- | Opaque C struct: @"gr_vk_extensions_t"@
-}
data Gr_vk_extensions = Gr_vk_extensions

{- | Opaque C struct: @"gr_vk_memory_allocator_t"@
-}
data Gr_vk_memory_allocator = Gr_vk_memory_allocator

-- | C function pointer type: @typedef void (*gr_vk_func_ptr)(void)@
type Gr_vk_func_ptr = IO (())

-- | Creates a 'FunPtr' of @"gr_vk_func_ptr"@.
foreign import ccall "wrapper" mkFunPtr'Gr_vk_func_ptr :: Gr_vk_func_ptr -> IO (FunPtr Gr_vk_func_ptr)

-- | C function pointer type: @typedef gr_vk_func_ptr (*gr_vk_get_proc)(void *ctx, const char *name, vk_instance_t *instance, vk_device_t *device)@
type Gr_vk_get_proc = Ptr (()) -> Ptr (CChar) -> Ptr (Vk_instance) -> Ptr (Vk_device) -> IO (FunPtr Gr_vk_func_ptr)

-- | Creates a 'FunPtr' of @"gr_vk_get_proc"@.
foreign import ccall "wrapper" mkFunPtr'Gr_vk_get_proc :: Gr_vk_get_proc -> IO (FunPtr Gr_vk_get_proc)

{- | C struct: @"gr_vk_backendcontext_t"@

@
typedef struct 
{
  vk_instance_t *fInstance;
  vk_physical_device_t *fPhysicalDevice;
  vk_device_t *fDevice;
  vk_queue_t *fQueue;
  uint32_t fGraphicsQueueIndex;
  uint32_t fMinAPIVersion;
  uint32_t fInstanceVersion;
  uint32_t fMaxAPIVersion;
  uint32_t fExtensions;
  const gr_vk_extensions_t *fVkExtensions;
  uint32_t fFeatures;
  const vk_physical_device_features_t *fDeviceFeatures;
  const vk_physical_device_features_2_t *fDeviceFeatures2;
  gr_vk_memory_allocator_t *fMemoryAllocator;
  gr_vk_get_proc fGetProc;
  void *fGetProcUserData;
  _Bool fOwnsInstanceAndDevice;
  _Bool fProtectedContext;
} gr_vk_backendcontext_t
@
-}
data Gr_vk_backendcontext = Gr_vk_backendcontext
  { fInstance :: Ptr (Vk_instance) -- ^ C field: @"vk_instance_t *fInstance"@
  , fPhysicalDevice :: Ptr (Vk_physical_device) -- ^ C field: @"vk_physical_device_t *fPhysicalDevice"@
  , fDevice :: Ptr (Vk_device) -- ^ C field: @"vk_device_t *fDevice"@
  , fQueue :: Ptr (Vk_queue) -- ^ C field: @"vk_queue_t *fQueue"@
  , fGraphicsQueueIndex :: Word32 -- ^ C field: @"uint32_t fGraphicsQueueIndex"@
  , fMinAPIVersion :: Word32 -- ^ C field: @"uint32_t fMinAPIVersion"@
  , fInstanceVersion :: Word32 -- ^ C field: @"uint32_t fInstanceVersion"@
  , fMaxAPIVersion :: Word32 -- ^ C field: @"uint32_t fMaxAPIVersion"@
  , fExtensions :: Word32 -- ^ C field: @"uint32_t fExtensions"@
  , fVkExtensions :: Ptr (Gr_vk_extensions) -- ^ C field: @"const gr_vk_extensions_t *fVkExtensions"@
  , fFeatures :: Word32 -- ^ C field: @"uint32_t fFeatures"@
  , fDeviceFeatures :: Ptr (Vk_physical_device_features) -- ^ C field: @"const vk_physical_device_features_t *fDeviceFeatures"@
  , fDeviceFeatures2 :: Ptr (Vk_physical_device_features_2) -- ^ C field: @"const vk_physical_device_features_2_t *fDeviceFeatures2"@
  , fMemoryAllocator :: Ptr (Gr_vk_memory_allocator) -- ^ C field: @"gr_vk_memory_allocator_t *fMemoryAllocator"@
  , fGetProc :: FunPtr Gr_vk_get_proc -- ^ C field: @"gr_vk_get_proc fGetProc"@
  , fGetProcUserData :: Ptr (()) -- ^ C field: @"void *fGetProcUserData"@
  , fOwnsInstanceAndDevice :: CBool -- ^ C field: @"_Bool fOwnsInstanceAndDevice"@
  , fProtectedContext :: CBool -- ^ C field: @"_Bool fProtectedContext"@
  }
instance Foreign.Storable.Offset.Offset "fInstance" Gr_vk_backendcontext where
  rawOffset = (#offset gr_vk_backendcontext_t, fInstance)
instance Foreign.Storable.Offset.Offset "fPhysicalDevice" Gr_vk_backendcontext where
  rawOffset = (#offset gr_vk_backendcontext_t, fPhysicalDevice)
instance Foreign.Storable.Offset.Offset "fDevice" Gr_vk_backendcontext where
  rawOffset = (#offset gr_vk_backendcontext_t, fDevice)
instance Foreign.Storable.Offset.Offset "fQueue" Gr_vk_backendcontext where
  rawOffset = (#offset gr_vk_backendcontext_t, fQueue)
instance Foreign.Storable.Offset.Offset "fGraphicsQueueIndex" Gr_vk_backendcontext where
  rawOffset = (#offset gr_vk_backendcontext_t, fGraphicsQueueIndex)
instance Foreign.Storable.Offset.Offset "fMinAPIVersion" Gr_vk_backendcontext where
  rawOffset = (#offset gr_vk_backendcontext_t, fMinAPIVersion)
instance Foreign.Storable.Offset.Offset "fInstanceVersion" Gr_vk_backendcontext where
  rawOffset = (#offset gr_vk_backendcontext_t, fInstanceVersion)
instance Foreign.Storable.Offset.Offset "fMaxAPIVersion" Gr_vk_backendcontext where
  rawOffset = (#offset gr_vk_backendcontext_t, fMaxAPIVersion)
instance Foreign.Storable.Offset.Offset "fExtensions" Gr_vk_backendcontext where
  rawOffset = (#offset gr_vk_backendcontext_t, fExtensions)
instance Foreign.Storable.Offset.Offset "fVkExtensions" Gr_vk_backendcontext where
  rawOffset = (#offset gr_vk_backendcontext_t, fVkExtensions)
instance Foreign.Storable.Offset.Offset "fFeatures" Gr_vk_backendcontext where
  rawOffset = (#offset gr_vk_backendcontext_t, fFeatures)
instance Foreign.Storable.Offset.Offset "fDeviceFeatures" Gr_vk_backendcontext where
  rawOffset = (#offset gr_vk_backendcontext_t, fDeviceFeatures)
instance Foreign.Storable.Offset.Offset "fDeviceFeatures2" Gr_vk_backendcontext where
  rawOffset = (#offset gr_vk_backendcontext_t, fDeviceFeatures2)
instance Foreign.Storable.Offset.Offset "fMemoryAllocator" Gr_vk_backendcontext where
  rawOffset = (#offset gr_vk_backendcontext_t, fMemoryAllocator)
instance Foreign.Storable.Offset.Offset "fGetProc" Gr_vk_backendcontext where
  rawOffset = (#offset gr_vk_backendcontext_t, fGetProc)
instance Foreign.Storable.Offset.Offset "fGetProcUserData" Gr_vk_backendcontext where
  rawOffset = (#offset gr_vk_backendcontext_t, fGetProcUserData)
instance Foreign.Storable.Offset.Offset "fOwnsInstanceAndDevice" Gr_vk_backendcontext where
  rawOffset = (#offset gr_vk_backendcontext_t, fOwnsInstanceAndDevice)
instance Foreign.Storable.Offset.Offset "fProtectedContext" Gr_vk_backendcontext where
  rawOffset = (#offset gr_vk_backendcontext_t, fProtectedContext)
instance Foreign.Storable.Storable Gr_vk_backendcontext where
  sizeOf _ = (#size gr_vk_backendcontext_t)
  alignment _ = (#alignment gr_vk_backendcontext_t)
  peek p' = do
    fInstance <- (#peek gr_vk_backendcontext_t, fInstance) p'
    fPhysicalDevice <- (#peek gr_vk_backendcontext_t, fPhysicalDevice) p'
    fDevice <- (#peek gr_vk_backendcontext_t, fDevice) p'
    fQueue <- (#peek gr_vk_backendcontext_t, fQueue) p'
    fGraphicsQueueIndex <- (#peek gr_vk_backendcontext_t, fGraphicsQueueIndex) p'
    fMinAPIVersion <- (#peek gr_vk_backendcontext_t, fMinAPIVersion) p'
    fInstanceVersion <- (#peek gr_vk_backendcontext_t, fInstanceVersion) p'
    fMaxAPIVersion <- (#peek gr_vk_backendcontext_t, fMaxAPIVersion) p'
    fExtensions <- (#peek gr_vk_backendcontext_t, fExtensions) p'
    fVkExtensions <- (#peek gr_vk_backendcontext_t, fVkExtensions) p'
    fFeatures <- (#peek gr_vk_backendcontext_t, fFeatures) p'
    fDeviceFeatures <- (#peek gr_vk_backendcontext_t, fDeviceFeatures) p'
    fDeviceFeatures2 <- (#peek gr_vk_backendcontext_t, fDeviceFeatures2) p'
    fMemoryAllocator <- (#peek gr_vk_backendcontext_t, fMemoryAllocator) p'
    fGetProc <- (#peek gr_vk_backendcontext_t, fGetProc) p'
    fGetProcUserData <- (#peek gr_vk_backendcontext_t, fGetProcUserData) p'
    fOwnsInstanceAndDevice <- (#peek gr_vk_backendcontext_t, fOwnsInstanceAndDevice) p'
    fProtectedContext <- (#peek gr_vk_backendcontext_t, fProtectedContext) p'
    pure Gr_vk_backendcontext{..}
  poke p' Gr_vk_backendcontext{..} = do
    (#poke gr_vk_backendcontext_t, fInstance) p' fInstance
    (#poke gr_vk_backendcontext_t, fPhysicalDevice) p' fPhysicalDevice
    (#poke gr_vk_backendcontext_t, fDevice) p' fDevice
    (#poke gr_vk_backendcontext_t, fQueue) p' fQueue
    (#poke gr_vk_backendcontext_t, fGraphicsQueueIndex) p' fGraphicsQueueIndex
    (#poke gr_vk_backendcontext_t, fMinAPIVersion) p' fMinAPIVersion
    (#poke gr_vk_backendcontext_t, fInstanceVersion) p' fInstanceVersion
    (#poke gr_vk_backendcontext_t, fMaxAPIVersion) p' fMaxAPIVersion
    (#poke gr_vk_backendcontext_t, fExtensions) p' fExtensions
    (#poke gr_vk_backendcontext_t, fVkExtensions) p' fVkExtensions
    (#poke gr_vk_backendcontext_t, fFeatures) p' fFeatures
    (#poke gr_vk_backendcontext_t, fDeviceFeatures) p' fDeviceFeatures
    (#poke gr_vk_backendcontext_t, fDeviceFeatures2) p' fDeviceFeatures2
    (#poke gr_vk_backendcontext_t, fMemoryAllocator) p' fMemoryAllocator
    (#poke gr_vk_backendcontext_t, fGetProc) p' fGetProc
    (#poke gr_vk_backendcontext_t, fGetProcUserData) p' fGetProcUserData
    (#poke gr_vk_backendcontext_t, fOwnsInstanceAndDevice) p' fOwnsInstanceAndDevice
    (#poke gr_vk_backendcontext_t, fProtectedContext) p' fProtectedContext

{- | C type alias: @gr_vk_backendmemory_t@

@
typedef intptr_t gr_vk_backendmemory_t
@
-}
type Gr_vk_backendmemory = CIntPtr

{- | C struct: @"gr_vk_alloc_t"@

@
typedef struct 
{
  uint64_t fMemory;
  uint64_t fOffset;
  uint64_t fSize;
  uint32_t fFlags;
  gr_vk_backendmemory_t fBackendMemory;
  _Bool _private_fUsesSystemHeap;
} gr_vk_alloc_t
@
-}
data Gr_vk_alloc = Gr_vk_alloc
  { fMemory :: Word64 -- ^ C field: @"uint64_t fMemory"@
  , fOffset :: Word64 -- ^ C field: @"uint64_t fOffset"@
  , fSize :: Word64 -- ^ C field: @"uint64_t fSize"@
  , fFlags :: Word32 -- ^ C field: @"uint32_t fFlags"@
  , fBackendMemory :: Gr_vk_backendmemory -- ^ C field: @"gr_vk_backendmemory_t fBackendMemory"@
  , _private_fUsesSystemHeap :: CBool -- ^ C field: @"_Bool _private_fUsesSystemHeap"@
  }
instance Foreign.Storable.Offset.Offset "fMemory" Gr_vk_alloc where
  rawOffset = (#offset gr_vk_alloc_t, fMemory)
instance Foreign.Storable.Offset.Offset "fOffset" Gr_vk_alloc where
  rawOffset = (#offset gr_vk_alloc_t, fOffset)
instance Foreign.Storable.Offset.Offset "fSize" Gr_vk_alloc where
  rawOffset = (#offset gr_vk_alloc_t, fSize)
instance Foreign.Storable.Offset.Offset "fFlags" Gr_vk_alloc where
  rawOffset = (#offset gr_vk_alloc_t, fFlags)
instance Foreign.Storable.Offset.Offset "fBackendMemory" Gr_vk_alloc where
  rawOffset = (#offset gr_vk_alloc_t, fBackendMemory)
instance Foreign.Storable.Offset.Offset "_private_fUsesSystemHeap" Gr_vk_alloc where
  rawOffset = (#offset gr_vk_alloc_t, _private_fUsesSystemHeap)
instance Foreign.Storable.Storable Gr_vk_alloc where
  sizeOf _ = (#size gr_vk_alloc_t)
  alignment _ = (#alignment gr_vk_alloc_t)
  peek p' = do
    fMemory <- (#peek gr_vk_alloc_t, fMemory) p'
    fOffset <- (#peek gr_vk_alloc_t, fOffset) p'
    fSize <- (#peek gr_vk_alloc_t, fSize) p'
    fFlags <- (#peek gr_vk_alloc_t, fFlags) p'
    fBackendMemory <- (#peek gr_vk_alloc_t, fBackendMemory) p'
    _private_fUsesSystemHeap <- (#peek gr_vk_alloc_t, _private_fUsesSystemHeap) p'
    pure Gr_vk_alloc{..}
  poke p' Gr_vk_alloc{..} = do
    (#poke gr_vk_alloc_t, fMemory) p' fMemory
    (#poke gr_vk_alloc_t, fOffset) p' fOffset
    (#poke gr_vk_alloc_t, fSize) p' fSize
    (#poke gr_vk_alloc_t, fFlags) p' fFlags
    (#poke gr_vk_alloc_t, fBackendMemory) p' fBackendMemory
    (#poke gr_vk_alloc_t, _private_fUsesSystemHeap) p' _private_fUsesSystemHeap

{- | C struct: @"gr_vk_ycbcrconversioninfo_t"@

@
typedef struct 
{
  uint32_t fFormat;
  uint64_t fExternalFormat;
  uint32_t fYcbcrModel;
  uint32_t fYcbcrRange;
  uint32_t fXChromaOffset;
  uint32_t fYChromaOffset;
  uint32_t fChromaFilter;
  uint32_t fForceExplicitReconstruction;
  uint32_t fFormatFeatures;
} gr_vk_ycbcrconversioninfo_t
@
-}
data Gr_vk_ycbcrconversioninfo = Gr_vk_ycbcrconversioninfo
  { fFormat :: Word32 -- ^ C field: @"uint32_t fFormat"@
  , fExternalFormat :: Word64 -- ^ C field: @"uint64_t fExternalFormat"@
  , fYcbcrModel :: Word32 -- ^ C field: @"uint32_t fYcbcrModel"@
  , fYcbcrRange :: Word32 -- ^ C field: @"uint32_t fYcbcrRange"@
  , fXChromaOffset :: Word32 -- ^ C field: @"uint32_t fXChromaOffset"@
  , fYChromaOffset :: Word32 -- ^ C field: @"uint32_t fYChromaOffset"@
  , fChromaFilter :: Word32 -- ^ C field: @"uint32_t fChromaFilter"@
  , fForceExplicitReconstruction :: Word32 -- ^ C field: @"uint32_t fForceExplicitReconstruction"@
  , fFormatFeatures :: Word32 -- ^ C field: @"uint32_t fFormatFeatures"@
  }
instance Foreign.Storable.Offset.Offset "fFormat" Gr_vk_ycbcrconversioninfo where
  rawOffset = (#offset gr_vk_ycbcrconversioninfo_t, fFormat)
instance Foreign.Storable.Offset.Offset "fExternalFormat" Gr_vk_ycbcrconversioninfo where
  rawOffset = (#offset gr_vk_ycbcrconversioninfo_t, fExternalFormat)
instance Foreign.Storable.Offset.Offset "fYcbcrModel" Gr_vk_ycbcrconversioninfo where
  rawOffset = (#offset gr_vk_ycbcrconversioninfo_t, fYcbcrModel)
instance Foreign.Storable.Offset.Offset "fYcbcrRange" Gr_vk_ycbcrconversioninfo where
  rawOffset = (#offset gr_vk_ycbcrconversioninfo_t, fYcbcrRange)
instance Foreign.Storable.Offset.Offset "fXChromaOffset" Gr_vk_ycbcrconversioninfo where
  rawOffset = (#offset gr_vk_ycbcrconversioninfo_t, fXChromaOffset)
instance Foreign.Storable.Offset.Offset "fYChromaOffset" Gr_vk_ycbcrconversioninfo where
  rawOffset = (#offset gr_vk_ycbcrconversioninfo_t, fYChromaOffset)
instance Foreign.Storable.Offset.Offset "fChromaFilter" Gr_vk_ycbcrconversioninfo where
  rawOffset = (#offset gr_vk_ycbcrconversioninfo_t, fChromaFilter)
instance Foreign.Storable.Offset.Offset "fForceExplicitReconstruction" Gr_vk_ycbcrconversioninfo where
  rawOffset = (#offset gr_vk_ycbcrconversioninfo_t, fForceExplicitReconstruction)
instance Foreign.Storable.Offset.Offset "fFormatFeatures" Gr_vk_ycbcrconversioninfo where
  rawOffset = (#offset gr_vk_ycbcrconversioninfo_t, fFormatFeatures)
instance Foreign.Storable.Storable Gr_vk_ycbcrconversioninfo where
  sizeOf _ = (#size gr_vk_ycbcrconversioninfo_t)
  alignment _ = (#alignment gr_vk_ycbcrconversioninfo_t)
  peek p' = do
    fFormat <- (#peek gr_vk_ycbcrconversioninfo_t, fFormat) p'
    fExternalFormat <- (#peek gr_vk_ycbcrconversioninfo_t, fExternalFormat) p'
    fYcbcrModel <- (#peek gr_vk_ycbcrconversioninfo_t, fYcbcrModel) p'
    fYcbcrRange <- (#peek gr_vk_ycbcrconversioninfo_t, fYcbcrRange) p'
    fXChromaOffset <- (#peek gr_vk_ycbcrconversioninfo_t, fXChromaOffset) p'
    fYChromaOffset <- (#peek gr_vk_ycbcrconversioninfo_t, fYChromaOffset) p'
    fChromaFilter <- (#peek gr_vk_ycbcrconversioninfo_t, fChromaFilter) p'
    fForceExplicitReconstruction <- (#peek gr_vk_ycbcrconversioninfo_t, fForceExplicitReconstruction) p'
    fFormatFeatures <- (#peek gr_vk_ycbcrconversioninfo_t, fFormatFeatures) p'
    pure Gr_vk_ycbcrconversioninfo{..}
  poke p' Gr_vk_ycbcrconversioninfo{..} = do
    (#poke gr_vk_ycbcrconversioninfo_t, fFormat) p' fFormat
    (#poke gr_vk_ycbcrconversioninfo_t, fExternalFormat) p' fExternalFormat
    (#poke gr_vk_ycbcrconversioninfo_t, fYcbcrModel) p' fYcbcrModel
    (#poke gr_vk_ycbcrconversioninfo_t, fYcbcrRange) p' fYcbcrRange
    (#poke gr_vk_ycbcrconversioninfo_t, fXChromaOffset) p' fXChromaOffset
    (#poke gr_vk_ycbcrconversioninfo_t, fYChromaOffset) p' fYChromaOffset
    (#poke gr_vk_ycbcrconversioninfo_t, fChromaFilter) p' fChromaFilter
    (#poke gr_vk_ycbcrconversioninfo_t, fForceExplicitReconstruction) p' fForceExplicitReconstruction
    (#poke gr_vk_ycbcrconversioninfo_t, fFormatFeatures) p' fFormatFeatures

{- | C struct: @"gr_vk_imageinfo_t"@

@
typedef struct 
{
  uint64_t fImage;
  gr_vk_alloc_t fAlloc;
  uint32_t fImageTiling;
  uint32_t fImageLayout;
  uint32_t fFormat;
  uint32_t fImageUsageFlags;
  uint32_t fSampleCount;
  uint32_t fLevelCount;
  uint32_t fCurrentQueueFamily;
  _Bool fProtected;
  gr_vk_ycbcrconversioninfo_t fYcbcrConversionInfo;
  uint32_t fSharingMode;
} gr_vk_imageinfo_t
@
-}
data Gr_vk_imageinfo = Gr_vk_imageinfo
  { fImage :: Word64 -- ^ C field: @"uint64_t fImage"@
  , fAlloc :: Gr_vk_alloc -- ^ C field: @"gr_vk_alloc_t fAlloc"@
  , fImageTiling :: Word32 -- ^ C field: @"uint32_t fImageTiling"@
  , fImageLayout :: Word32 -- ^ C field: @"uint32_t fImageLayout"@
  , fFormat :: Word32 -- ^ C field: @"uint32_t fFormat"@
  , fImageUsageFlags :: Word32 -- ^ C field: @"uint32_t fImageUsageFlags"@
  , fSampleCount :: Word32 -- ^ C field: @"uint32_t fSampleCount"@
  , fLevelCount :: Word32 -- ^ C field: @"uint32_t fLevelCount"@
  , fCurrentQueueFamily :: Word32 -- ^ C field: @"uint32_t fCurrentQueueFamily"@
  , fProtected :: CBool -- ^ C field: @"_Bool fProtected"@
  , fYcbcrConversionInfo :: Gr_vk_ycbcrconversioninfo -- ^ C field: @"gr_vk_ycbcrconversioninfo_t fYcbcrConversionInfo"@
  , fSharingMode :: Word32 -- ^ C field: @"uint32_t fSharingMode"@
  }
instance Foreign.Storable.Offset.Offset "fImage" Gr_vk_imageinfo where
  rawOffset = (#offset gr_vk_imageinfo_t, fImage)
instance Foreign.Storable.Offset.Offset "fAlloc" Gr_vk_imageinfo where
  rawOffset = (#offset gr_vk_imageinfo_t, fAlloc)
instance Foreign.Storable.Offset.Offset "fImageTiling" Gr_vk_imageinfo where
  rawOffset = (#offset gr_vk_imageinfo_t, fImageTiling)
instance Foreign.Storable.Offset.Offset "fImageLayout" Gr_vk_imageinfo where
  rawOffset = (#offset gr_vk_imageinfo_t, fImageLayout)
instance Foreign.Storable.Offset.Offset "fFormat" Gr_vk_imageinfo where
  rawOffset = (#offset gr_vk_imageinfo_t, fFormat)
instance Foreign.Storable.Offset.Offset "fImageUsageFlags" Gr_vk_imageinfo where
  rawOffset = (#offset gr_vk_imageinfo_t, fImageUsageFlags)
instance Foreign.Storable.Offset.Offset "fSampleCount" Gr_vk_imageinfo where
  rawOffset = (#offset gr_vk_imageinfo_t, fSampleCount)
instance Foreign.Storable.Offset.Offset "fLevelCount" Gr_vk_imageinfo where
  rawOffset = (#offset gr_vk_imageinfo_t, fLevelCount)
instance Foreign.Storable.Offset.Offset "fCurrentQueueFamily" Gr_vk_imageinfo where
  rawOffset = (#offset gr_vk_imageinfo_t, fCurrentQueueFamily)
instance Foreign.Storable.Offset.Offset "fProtected" Gr_vk_imageinfo where
  rawOffset = (#offset gr_vk_imageinfo_t, fProtected)
instance Foreign.Storable.Offset.Offset "fYcbcrConversionInfo" Gr_vk_imageinfo where
  rawOffset = (#offset gr_vk_imageinfo_t, fYcbcrConversionInfo)
instance Foreign.Storable.Offset.Offset "fSharingMode" Gr_vk_imageinfo where
  rawOffset = (#offset gr_vk_imageinfo_t, fSharingMode)
instance Foreign.Storable.Storable Gr_vk_imageinfo where
  sizeOf _ = (#size gr_vk_imageinfo_t)
  alignment _ = (#alignment gr_vk_imageinfo_t)
  peek p' = do
    fImage <- (#peek gr_vk_imageinfo_t, fImage) p'
    fAlloc <- (#peek gr_vk_imageinfo_t, fAlloc) p'
    fImageTiling <- (#peek gr_vk_imageinfo_t, fImageTiling) p'
    fImageLayout <- (#peek gr_vk_imageinfo_t, fImageLayout) p'
    fFormat <- (#peek gr_vk_imageinfo_t, fFormat) p'
    fImageUsageFlags <- (#peek gr_vk_imageinfo_t, fImageUsageFlags) p'
    fSampleCount <- (#peek gr_vk_imageinfo_t, fSampleCount) p'
    fLevelCount <- (#peek gr_vk_imageinfo_t, fLevelCount) p'
    fCurrentQueueFamily <- (#peek gr_vk_imageinfo_t, fCurrentQueueFamily) p'
    fProtected <- (#peek gr_vk_imageinfo_t, fProtected) p'
    fYcbcrConversionInfo <- (#peek gr_vk_imageinfo_t, fYcbcrConversionInfo) p'
    fSharingMode <- (#peek gr_vk_imageinfo_t, fSharingMode) p'
    pure Gr_vk_imageinfo{..}
  poke p' Gr_vk_imageinfo{..} = do
    (#poke gr_vk_imageinfo_t, fImage) p' fImage
    (#poke gr_vk_imageinfo_t, fAlloc) p' fAlloc
    (#poke gr_vk_imageinfo_t, fImageTiling) p' fImageTiling
    (#poke gr_vk_imageinfo_t, fImageLayout) p' fImageLayout
    (#poke gr_vk_imageinfo_t, fFormat) p' fFormat
    (#poke gr_vk_imageinfo_t, fImageUsageFlags) p' fImageUsageFlags
    (#poke gr_vk_imageinfo_t, fSampleCount) p' fSampleCount
    (#poke gr_vk_imageinfo_t, fLevelCount) p' fLevelCount
    (#poke gr_vk_imageinfo_t, fCurrentQueueFamily) p' fCurrentQueueFamily
    (#poke gr_vk_imageinfo_t, fProtected) p' fProtected
    (#poke gr_vk_imageinfo_t, fYcbcrConversionInfo) p' fYcbcrConversionInfo
    (#poke gr_vk_imageinfo_t, fSharingMode) p' fSharingMode

{- | C struct: @"gr_mtl_textureinfo_t"@

@
typedef struct 
{
  const void *fTexture;
} gr_mtl_textureinfo_t
@
-}
data Gr_mtl_textureinfo = Gr_mtl_textureinfo
  { fTexture :: Ptr (()) -- ^ C field: @"const void *fTexture"@
  }
instance Foreign.Storable.Offset.Offset "fTexture" Gr_mtl_textureinfo where
  rawOffset = (#offset gr_mtl_textureinfo_t, fTexture)
instance Foreign.Storable.Storable Gr_mtl_textureinfo where
  sizeOf _ = (#size gr_mtl_textureinfo_t)
  alignment _ = (#alignment gr_mtl_textureinfo_t)
  peek p' = do
    fTexture <- (#peek gr_mtl_textureinfo_t, fTexture) p'
    pure Gr_mtl_textureinfo{..}
  poke p' Gr_mtl_textureinfo{..} = do
    (#poke gr_mtl_textureinfo_t, fTexture) p' fTexture

{- | Opaque C struct: @"d3d_dxgi_adapter_t"@
-}
data D3d_dxgi_adapter = D3d_dxgi_adapter

{- | Opaque C struct: @"d3d_d12_device_t"@
-}
data D3d_d12_device = D3d_d12_device

{- | Opaque C struct: @"d3d_d12_command_queue_t"@
-}
data D3d_d12_command_queue = D3d_d12_command_queue

{- | Opaque C struct: @"gr_d3d_memory_allocator_t"@
-}
data Gr_d3d_memory_allocator = Gr_d3d_memory_allocator

{- | C struct: @"gr_d3d_backendcontext_t"@

@
typedef struct 
{
  d3d_dxgi_adapter_t *fAdapter;
  d3d_d12_device_t *fDevice;
  d3d_d12_command_queue_t *fQueue;
  gr_d3d_memory_allocator_t *fMemoryAllocator;
  _Bool fProtectedContext;
} gr_d3d_backendcontext_t
@
-}
data Gr_d3d_backendcontext = Gr_d3d_backendcontext
  { fAdapter :: Ptr (D3d_dxgi_adapter) -- ^ C field: @"d3d_dxgi_adapter_t *fAdapter"@
  , fDevice :: Ptr (D3d_d12_device) -- ^ C field: @"d3d_d12_device_t *fDevice"@
  , fQueue :: Ptr (D3d_d12_command_queue) -- ^ C field: @"d3d_d12_command_queue_t *fQueue"@
  , fMemoryAllocator :: Ptr (Gr_d3d_memory_allocator) -- ^ C field: @"gr_d3d_memory_allocator_t *fMemoryAllocator"@
  , fProtectedContext :: CBool -- ^ C field: @"_Bool fProtectedContext"@
  }
instance Foreign.Storable.Offset.Offset "fAdapter" Gr_d3d_backendcontext where
  rawOffset = (#offset gr_d3d_backendcontext_t, fAdapter)
instance Foreign.Storable.Offset.Offset "fDevice" Gr_d3d_backendcontext where
  rawOffset = (#offset gr_d3d_backendcontext_t, fDevice)
instance Foreign.Storable.Offset.Offset "fQueue" Gr_d3d_backendcontext where
  rawOffset = (#offset gr_d3d_backendcontext_t, fQueue)
instance Foreign.Storable.Offset.Offset "fMemoryAllocator" Gr_d3d_backendcontext where
  rawOffset = (#offset gr_d3d_backendcontext_t, fMemoryAllocator)
instance Foreign.Storable.Offset.Offset "fProtectedContext" Gr_d3d_backendcontext where
  rawOffset = (#offset gr_d3d_backendcontext_t, fProtectedContext)
instance Foreign.Storable.Storable Gr_d3d_backendcontext where
  sizeOf _ = (#size gr_d3d_backendcontext_t)
  alignment _ = (#alignment gr_d3d_backendcontext_t)
  peek p' = do
    fAdapter <- (#peek gr_d3d_backendcontext_t, fAdapter) p'
    fDevice <- (#peek gr_d3d_backendcontext_t, fDevice) p'
    fQueue <- (#peek gr_d3d_backendcontext_t, fQueue) p'
    fMemoryAllocator <- (#peek gr_d3d_backendcontext_t, fMemoryAllocator) p'
    fProtectedContext <- (#peek gr_d3d_backendcontext_t, fProtectedContext) p'
    pure Gr_d3d_backendcontext{..}
  poke p' Gr_d3d_backendcontext{..} = do
    (#poke gr_d3d_backendcontext_t, fAdapter) p' fAdapter
    (#poke gr_d3d_backendcontext_t, fDevice) p' fDevice
    (#poke gr_d3d_backendcontext_t, fQueue) p' fQueue
    (#poke gr_d3d_backendcontext_t, fMemoryAllocator) p' fMemoryAllocator
    (#poke gr_d3d_backendcontext_t, fProtectedContext) p' fProtectedContext

{- | Opaque C struct: @"d3d_d12_resource_t"@
-}
data D3d_d12_resource = D3d_d12_resource

{- | Opaque C struct: @"d3d_alloc_t"@
-}
data D3d_alloc = D3d_alloc

{- | C struct: @"gr_d3d_textureresourceinfo_t"@

@
typedef struct 
{
  d3d_d12_resource_t *fResource;
  d3d_alloc_t *fAlloc;
  uint32_t fResourceState;
  uint32_t fFormat;
  uint32_t fSampleCount;
  uint32_t fLevelCount;
  unsigned int fSampleQualityPattern;
  _Bool fProtected;
} gr_d3d_textureresourceinfo_t
@
-}
data Gr_d3d_textureresourceinfo = Gr_d3d_textureresourceinfo
  { fResource :: Ptr (D3d_d12_resource) -- ^ C field: @"d3d_d12_resource_t *fResource"@
  , fAlloc :: Ptr (D3d_alloc) -- ^ C field: @"d3d_alloc_t *fAlloc"@
  , fResourceState :: Word32 -- ^ C field: @"uint32_t fResourceState"@
  , fFormat :: Word32 -- ^ C field: @"uint32_t fFormat"@
  , fSampleCount :: Word32 -- ^ C field: @"uint32_t fSampleCount"@
  , fLevelCount :: Word32 -- ^ C field: @"uint32_t fLevelCount"@
  , fSampleQualityPattern :: CUInt -- ^ C field: @"unsigned int fSampleQualityPattern"@
  , fProtected :: CBool -- ^ C field: @"_Bool fProtected"@
  }
instance Foreign.Storable.Offset.Offset "fResource" Gr_d3d_textureresourceinfo where
  rawOffset = (#offset gr_d3d_textureresourceinfo_t, fResource)
instance Foreign.Storable.Offset.Offset "fAlloc" Gr_d3d_textureresourceinfo where
  rawOffset = (#offset gr_d3d_textureresourceinfo_t, fAlloc)
instance Foreign.Storable.Offset.Offset "fResourceState" Gr_d3d_textureresourceinfo where
  rawOffset = (#offset gr_d3d_textureresourceinfo_t, fResourceState)
instance Foreign.Storable.Offset.Offset "fFormat" Gr_d3d_textureresourceinfo where
  rawOffset = (#offset gr_d3d_textureresourceinfo_t, fFormat)
instance Foreign.Storable.Offset.Offset "fSampleCount" Gr_d3d_textureresourceinfo where
  rawOffset = (#offset gr_d3d_textureresourceinfo_t, fSampleCount)
instance Foreign.Storable.Offset.Offset "fLevelCount" Gr_d3d_textureresourceinfo where
  rawOffset = (#offset gr_d3d_textureresourceinfo_t, fLevelCount)
instance Foreign.Storable.Offset.Offset "fSampleQualityPattern" Gr_d3d_textureresourceinfo where
  rawOffset = (#offset gr_d3d_textureresourceinfo_t, fSampleQualityPattern)
instance Foreign.Storable.Offset.Offset "fProtected" Gr_d3d_textureresourceinfo where
  rawOffset = (#offset gr_d3d_textureresourceinfo_t, fProtected)
instance Foreign.Storable.Storable Gr_d3d_textureresourceinfo where
  sizeOf _ = (#size gr_d3d_textureresourceinfo_t)
  alignment _ = (#alignment gr_d3d_textureresourceinfo_t)
  peek p' = do
    fResource <- (#peek gr_d3d_textureresourceinfo_t, fResource) p'
    fAlloc <- (#peek gr_d3d_textureresourceinfo_t, fAlloc) p'
    fResourceState <- (#peek gr_d3d_textureresourceinfo_t, fResourceState) p'
    fFormat <- (#peek gr_d3d_textureresourceinfo_t, fFormat) p'
    fSampleCount <- (#peek gr_d3d_textureresourceinfo_t, fSampleCount) p'
    fLevelCount <- (#peek gr_d3d_textureresourceinfo_t, fLevelCount) p'
    fSampleQualityPattern <- (#peek gr_d3d_textureresourceinfo_t, fSampleQualityPattern) p'
    fProtected <- (#peek gr_d3d_textureresourceinfo_t, fProtected) p'
    pure Gr_d3d_textureresourceinfo{..}
  poke p' Gr_d3d_textureresourceinfo{..} = do
    (#poke gr_d3d_textureresourceinfo_t, fResource) p' fResource
    (#poke gr_d3d_textureresourceinfo_t, fAlloc) p' fAlloc
    (#poke gr_d3d_textureresourceinfo_t, fResourceState) p' fResourceState
    (#poke gr_d3d_textureresourceinfo_t, fFormat) p' fFormat
    (#poke gr_d3d_textureresourceinfo_t, fSampleCount) p' fSampleCount
    (#poke gr_d3d_textureresourceinfo_t, fLevelCount) p' fLevelCount
    (#poke gr_d3d_textureresourceinfo_t, fSampleQualityPattern) p' fSampleQualityPattern
    (#poke gr_d3d_textureresourceinfo_t, fProtected) p' fProtected

{- | Opaque C struct: @"skottie_animation_t"@
-}
data Skottie_animation = Skottie_animation

{- | Opaque C struct: @"skottie_animation_builder_t"@
-}
data Skottie_animation_builder = Skottie_animation_builder

{- | Opaque C struct: @"skottie_property_observer_t"@
-}
data Skottie_property_observer = Skottie_property_observer

{- | Opaque C struct: @"skottie_logger_t"@
-}
data Skottie_logger = Skottie_logger

{- | Opaque C struct: @"skottie_marker_observer_t"@
-}
data Skottie_marker_observer = Skottie_marker_observer

{- | Opaque C struct: @"sksg_invalidation_controller_t"@
-}
data Sksg_invalidation_controller = Sksg_invalidation_controller

{- | C enum: @"skottie_animation_renderflags_t"@

@
typedef enum 
{
  SKIP_TOP_LEVEL_ISOLATION = 0x01,
  DISABLE_TOP_LEVEL_CLIPPING = 0x02
} skottie_animation_renderflags_t
@

-}
newtype Skottie_animation_renderflags = Skottie_animation_renderflags (#type skottie_animation_renderflags_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"skottie_animation_renderflags_t"@ value (1/2): @"SKIP_TOP_LEVEL_ISOLATION"@
pattern SKIP_TOP_LEVEL_ISOLATION :: Skottie_animation_renderflags
pattern SKIP_TOP_LEVEL_ISOLATION = (#const SKIP_TOP_LEVEL_ISOLATION)

-- | C enum @"skottie_animation_renderflags_t"@ value (2/2): @"DISABLE_TOP_LEVEL_CLIPPING"@
pattern DISABLE_TOP_LEVEL_CLIPPING :: Skottie_animation_renderflags
pattern DISABLE_TOP_LEVEL_CLIPPING = (#const DISABLE_TOP_LEVEL_CLIPPING)

{- | C enum: @"skottie_animation_builder_flags_t"@

@
typedef enum 
{
  NONE_SKOTTIE_ANIMATION_BUILDER_FLAGS = 0,
  DEFER_IMAGE_LOADING_SKOTTIE_ANIMATION_BUILDER_FLAGS = 0x01,
  PREFER_EMBEDDED_FONTS_SKOTTIE_ANIMATION_BUILDER_FLAGS = 0x02
} skottie_animation_builder_flags_t
@

-}
newtype Skottie_animation_builder_flags = Skottie_animation_builder_flags (#type skottie_animation_builder_flags_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"skottie_animation_builder_flags_t"@ value (1/3): @"NONE_SKOTTIE_ANIMATION_BUILDER_FLAGS"@
pattern NONE_SKOTTIE_ANIMATION_BUILDER_FLAGS :: Skottie_animation_builder_flags
pattern NONE_SKOTTIE_ANIMATION_BUILDER_FLAGS = (#const NONE_SKOTTIE_ANIMATION_BUILDER_FLAGS)

-- | C enum @"skottie_animation_builder_flags_t"@ value (2/3): @"DEFER_IMAGE_LOADING_SKOTTIE_ANIMATION_BUILDER_FLAGS"@
pattern DEFER_IMAGE_LOADING_SKOTTIE_ANIMATION_BUILDER_FLAGS :: Skottie_animation_builder_flags
pattern DEFER_IMAGE_LOADING_SKOTTIE_ANIMATION_BUILDER_FLAGS = (#const DEFER_IMAGE_LOADING_SKOTTIE_ANIMATION_BUILDER_FLAGS)

-- | C enum @"skottie_animation_builder_flags_t"@ value (3/3): @"PREFER_EMBEDDED_FONTS_SKOTTIE_ANIMATION_BUILDER_FLAGS"@
pattern PREFER_EMBEDDED_FONTS_SKOTTIE_ANIMATION_BUILDER_FLAGS :: Skottie_animation_builder_flags
pattern PREFER_EMBEDDED_FONTS_SKOTTIE_ANIMATION_BUILDER_FLAGS = (#const PREFER_EMBEDDED_FONTS_SKOTTIE_ANIMATION_BUILDER_FLAGS)

{- | C struct: @"skottie_animation_builder_stats_t"@

@
typedef struct 
{
  float fTotalLoadTimeMS;
  float fJsonParseTimeMS;
  float fSceneParseTimeMS;
  size_t fJsonSize;
  size_t fAnimatorCount;
} skottie_animation_builder_stats_t
@
-}
data Skottie_animation_builder_stats = Skottie_animation_builder_stats
  { fTotalLoadTimeMS :: CFloat -- ^ C field: @"float fTotalLoadTimeMS"@
  , fJsonParseTimeMS :: CFloat -- ^ C field: @"float fJsonParseTimeMS"@
  , fSceneParseTimeMS :: CFloat -- ^ C field: @"float fSceneParseTimeMS"@
  , fJsonSize :: CSize -- ^ C field: @"size_t fJsonSize"@
  , fAnimatorCount :: CSize -- ^ C field: @"size_t fAnimatorCount"@
  }
instance Foreign.Storable.Offset.Offset "fTotalLoadTimeMS" Skottie_animation_builder_stats where
  rawOffset = (#offset skottie_animation_builder_stats_t, fTotalLoadTimeMS)
instance Foreign.Storable.Offset.Offset "fJsonParseTimeMS" Skottie_animation_builder_stats where
  rawOffset = (#offset skottie_animation_builder_stats_t, fJsonParseTimeMS)
instance Foreign.Storable.Offset.Offset "fSceneParseTimeMS" Skottie_animation_builder_stats where
  rawOffset = (#offset skottie_animation_builder_stats_t, fSceneParseTimeMS)
instance Foreign.Storable.Offset.Offset "fJsonSize" Skottie_animation_builder_stats where
  rawOffset = (#offset skottie_animation_builder_stats_t, fJsonSize)
instance Foreign.Storable.Offset.Offset "fAnimatorCount" Skottie_animation_builder_stats where
  rawOffset = (#offset skottie_animation_builder_stats_t, fAnimatorCount)
instance Foreign.Storable.Storable Skottie_animation_builder_stats where
  sizeOf _ = (#size skottie_animation_builder_stats_t)
  alignment _ = (#alignment skottie_animation_builder_stats_t)
  peek p' = do
    fTotalLoadTimeMS <- (#peek skottie_animation_builder_stats_t, fTotalLoadTimeMS) p'
    fJsonParseTimeMS <- (#peek skottie_animation_builder_stats_t, fJsonParseTimeMS) p'
    fSceneParseTimeMS <- (#peek skottie_animation_builder_stats_t, fSceneParseTimeMS) p'
    fJsonSize <- (#peek skottie_animation_builder_stats_t, fJsonSize) p'
    fAnimatorCount <- (#peek skottie_animation_builder_stats_t, fAnimatorCount) p'
    pure Skottie_animation_builder_stats{..}
  poke p' Skottie_animation_builder_stats{..} = do
    (#poke skottie_animation_builder_stats_t, fTotalLoadTimeMS) p' fTotalLoadTimeMS
    (#poke skottie_animation_builder_stats_t, fJsonParseTimeMS) p' fJsonParseTimeMS
    (#poke skottie_animation_builder_stats_t, fSceneParseTimeMS) p' fSceneParseTimeMS
    (#poke skottie_animation_builder_stats_t, fJsonSize) p' fJsonSize
    (#poke skottie_animation_builder_stats_t, fAnimatorCount) p' fAnimatorCount

{- | C enum: @"skparagraph_affinity_t"@

@
typedef enum 
{
  UPSTREAM_SKPARAGRAPH_AFFINITY,
  DOWNSTREAM_SKPARAGRAPH_AFFINITY
} skparagraph_affinity_t
@

-}
newtype Skparagraph_affinity = Skparagraph_affinity (#type skparagraph_affinity_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"skparagraph_affinity_t"@ value (1/2): @"UPSTREAM_SKPARAGRAPH_AFFINITY"@
pattern UPSTREAM_SKPARAGRAPH_AFFINITY :: Skparagraph_affinity
pattern UPSTREAM_SKPARAGRAPH_AFFINITY = (#const UPSTREAM_SKPARAGRAPH_AFFINITY)

-- | C enum @"skparagraph_affinity_t"@ value (2/2): @"DOWNSTREAM_SKPARAGRAPH_AFFINITY"@
pattern DOWNSTREAM_SKPARAGRAPH_AFFINITY :: Skparagraph_affinity
pattern DOWNSTREAM_SKPARAGRAPH_AFFINITY = (#const DOWNSTREAM_SKPARAGRAPH_AFFINITY)

{- | C enum: @"skparagraph_rect_height_style_t"@

@
typedef enum 
{
  TIGHT_SKPARAGRAPH_RECT_HEIGHT_STYLE,
  MAX_SKPARAGRAPH_RECT_HEIGHT_STYLE,
  INCLUDE_LINE_SPACING_MIDDLE_SKPARAGRAPH_RECT_HEIGHT_STYLE,
  INCLUDE_LINE_SPACING_TOP_SKPARAGRAPH_RECT_HEIGHT_STYLE,
  INCLUDE_LINE_SPACING_BOTTOM_SKPARAGRAPH_RECT_HEIGHT_STYLE,
  STRUT_SKPARAGRAPH_RECT_HEIGHT_STYLE
} skparagraph_rect_height_style_t
@

-}
newtype Skparagraph_rect_height_style = Skparagraph_rect_height_style (#type skparagraph_rect_height_style_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"skparagraph_rect_height_style_t"@ value (1/6): @"TIGHT_SKPARAGRAPH_RECT_HEIGHT_STYLE"@
pattern TIGHT_SKPARAGRAPH_RECT_HEIGHT_STYLE :: Skparagraph_rect_height_style
pattern TIGHT_SKPARAGRAPH_RECT_HEIGHT_STYLE = (#const TIGHT_SKPARAGRAPH_RECT_HEIGHT_STYLE)

-- | C enum @"skparagraph_rect_height_style_t"@ value (2/6): @"MAX_SKPARAGRAPH_RECT_HEIGHT_STYLE"@
pattern MAX_SKPARAGRAPH_RECT_HEIGHT_STYLE :: Skparagraph_rect_height_style
pattern MAX_SKPARAGRAPH_RECT_HEIGHT_STYLE = (#const MAX_SKPARAGRAPH_RECT_HEIGHT_STYLE)

-- | C enum @"skparagraph_rect_height_style_t"@ value (3/6): @"INCLUDE_LINE_SPACING_MIDDLE_SKPARAGRAPH_RECT_HEIGHT_STYLE"@
pattern INCLUDE_LINE_SPACING_MIDDLE_SKPARAGRAPH_RECT_HEIGHT_STYLE :: Skparagraph_rect_height_style
pattern INCLUDE_LINE_SPACING_MIDDLE_SKPARAGRAPH_RECT_HEIGHT_STYLE = (#const INCLUDE_LINE_SPACING_MIDDLE_SKPARAGRAPH_RECT_HEIGHT_STYLE)

-- | C enum @"skparagraph_rect_height_style_t"@ value (4/6): @"INCLUDE_LINE_SPACING_TOP_SKPARAGRAPH_RECT_HEIGHT_STYLE"@
pattern INCLUDE_LINE_SPACING_TOP_SKPARAGRAPH_RECT_HEIGHT_STYLE :: Skparagraph_rect_height_style
pattern INCLUDE_LINE_SPACING_TOP_SKPARAGRAPH_RECT_HEIGHT_STYLE = (#const INCLUDE_LINE_SPACING_TOP_SKPARAGRAPH_RECT_HEIGHT_STYLE)

-- | C enum @"skparagraph_rect_height_style_t"@ value (5/6): @"INCLUDE_LINE_SPACING_BOTTOM_SKPARAGRAPH_RECT_HEIGHT_STYLE"@
pattern INCLUDE_LINE_SPACING_BOTTOM_SKPARAGRAPH_RECT_HEIGHT_STYLE :: Skparagraph_rect_height_style
pattern INCLUDE_LINE_SPACING_BOTTOM_SKPARAGRAPH_RECT_HEIGHT_STYLE = (#const INCLUDE_LINE_SPACING_BOTTOM_SKPARAGRAPH_RECT_HEIGHT_STYLE)

-- | C enum @"skparagraph_rect_height_style_t"@ value (6/6): @"STRUT_SKPARAGRAPH_RECT_HEIGHT_STYLE"@
pattern STRUT_SKPARAGRAPH_RECT_HEIGHT_STYLE :: Skparagraph_rect_height_style
pattern STRUT_SKPARAGRAPH_RECT_HEIGHT_STYLE = (#const STRUT_SKPARAGRAPH_RECT_HEIGHT_STYLE)

{- | C enum: @"skparagraph_rect_width_style_t"@

@
typedef enum 
{
  TIGHT_SKPARAGRAPH_RECT_WIDTH_STYLE,
  MAX_SKPARAGRAPH_RECT_WIDTH_STYLE
} skparagraph_rect_width_style_t
@

-}
newtype Skparagraph_rect_width_style = Skparagraph_rect_width_style (#type skparagraph_rect_width_style_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"skparagraph_rect_width_style_t"@ value (1/2): @"TIGHT_SKPARAGRAPH_RECT_WIDTH_STYLE"@
pattern TIGHT_SKPARAGRAPH_RECT_WIDTH_STYLE :: Skparagraph_rect_width_style
pattern TIGHT_SKPARAGRAPH_RECT_WIDTH_STYLE = (#const TIGHT_SKPARAGRAPH_RECT_WIDTH_STYLE)

-- | C enum @"skparagraph_rect_width_style_t"@ value (2/2): @"MAX_SKPARAGRAPH_RECT_WIDTH_STYLE"@
pattern MAX_SKPARAGRAPH_RECT_WIDTH_STYLE :: Skparagraph_rect_width_style
pattern MAX_SKPARAGRAPH_RECT_WIDTH_STYLE = (#const MAX_SKPARAGRAPH_RECT_WIDTH_STYLE)

{- | C enum: @"skparagraph_text_align_t"@

@
typedef enum 
{
  LEFT_SKPARAGRAPH_TEXT_ALIGN,
  RIGHT_SKPARAGRAPH_TEXT_ALIGN,
  CENTER_SKPARAGRAPH_TEXT_ALIGN,
  JUSTIFY_SKPARAGRAPH_TEXT_ALIGN,
  START_SKPARAGRAPH_TEXT_ALIGN,
  END_SKPARAGRAPH_TEXT_ALIGN
} skparagraph_text_align_t
@

-}
newtype Skparagraph_text_align = Skparagraph_text_align (#type skparagraph_text_align_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"skparagraph_text_align_t"@ value (1/6): @"LEFT_SKPARAGRAPH_TEXT_ALIGN"@
pattern LEFT_SKPARAGRAPH_TEXT_ALIGN :: Skparagraph_text_align
pattern LEFT_SKPARAGRAPH_TEXT_ALIGN = (#const LEFT_SKPARAGRAPH_TEXT_ALIGN)

-- | C enum @"skparagraph_text_align_t"@ value (2/6): @"RIGHT_SKPARAGRAPH_TEXT_ALIGN"@
pattern RIGHT_SKPARAGRAPH_TEXT_ALIGN :: Skparagraph_text_align
pattern RIGHT_SKPARAGRAPH_TEXT_ALIGN = (#const RIGHT_SKPARAGRAPH_TEXT_ALIGN)

-- | C enum @"skparagraph_text_align_t"@ value (3/6): @"CENTER_SKPARAGRAPH_TEXT_ALIGN"@
pattern CENTER_SKPARAGRAPH_TEXT_ALIGN :: Skparagraph_text_align
pattern CENTER_SKPARAGRAPH_TEXT_ALIGN = (#const CENTER_SKPARAGRAPH_TEXT_ALIGN)

-- | C enum @"skparagraph_text_align_t"@ value (4/6): @"JUSTIFY_SKPARAGRAPH_TEXT_ALIGN"@
pattern JUSTIFY_SKPARAGRAPH_TEXT_ALIGN :: Skparagraph_text_align
pattern JUSTIFY_SKPARAGRAPH_TEXT_ALIGN = (#const JUSTIFY_SKPARAGRAPH_TEXT_ALIGN)

-- | C enum @"skparagraph_text_align_t"@ value (5/6): @"START_SKPARAGRAPH_TEXT_ALIGN"@
pattern START_SKPARAGRAPH_TEXT_ALIGN :: Skparagraph_text_align
pattern START_SKPARAGRAPH_TEXT_ALIGN = (#const START_SKPARAGRAPH_TEXT_ALIGN)

-- | C enum @"skparagraph_text_align_t"@ value (6/6): @"END_SKPARAGRAPH_TEXT_ALIGN"@
pattern END_SKPARAGRAPH_TEXT_ALIGN :: Skparagraph_text_align
pattern END_SKPARAGRAPH_TEXT_ALIGN = (#const END_SKPARAGRAPH_TEXT_ALIGN)

{- | C enum: @"skparagraph_text_direction_t"@

@
typedef enum 
{
  RTL_SKPARAGRAPH_TEXT_DIRECTION,
  LTR_SKPARAGRAPH_TEXT_DIRECTION
} skparagraph_text_direction_t
@

-}
newtype Skparagraph_text_direction = Skparagraph_text_direction (#type skparagraph_text_direction_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"skparagraph_text_direction_t"@ value (1/2): @"RTL_SKPARAGRAPH_TEXT_DIRECTION"@
pattern RTL_SKPARAGRAPH_TEXT_DIRECTION :: Skparagraph_text_direction
pattern RTL_SKPARAGRAPH_TEXT_DIRECTION = (#const RTL_SKPARAGRAPH_TEXT_DIRECTION)

-- | C enum @"skparagraph_text_direction_t"@ value (2/2): @"LTR_SKPARAGRAPH_TEXT_DIRECTION"@
pattern LTR_SKPARAGRAPH_TEXT_DIRECTION :: Skparagraph_text_direction
pattern LTR_SKPARAGRAPH_TEXT_DIRECTION = (#const LTR_SKPARAGRAPH_TEXT_DIRECTION)

{- | C struct: @"skparagraph_position_with_affinity_t"@

@
typedef struct 
{
  int32_t position;
  skparagraph_affinity_t affinity;
} skparagraph_position_with_affinity_t
@
-}
data Skparagraph_position_with_affinity = Skparagraph_position_with_affinity
  { position :: Int32 -- ^ C field: @"int32_t position"@
  , affinity :: Skparagraph_affinity -- ^ C field: @"skparagraph_affinity_t affinity"@
  }
instance Foreign.Storable.Offset.Offset "position" Skparagraph_position_with_affinity where
  rawOffset = (#offset skparagraph_position_with_affinity_t, position)
instance Foreign.Storable.Offset.Offset "affinity" Skparagraph_position_with_affinity where
  rawOffset = (#offset skparagraph_position_with_affinity_t, affinity)
instance Foreign.Storable.Storable Skparagraph_position_with_affinity where
  sizeOf _ = (#size skparagraph_position_with_affinity_t)
  alignment _ = (#alignment skparagraph_position_with_affinity_t)
  peek p' = do
    position <- (#peek skparagraph_position_with_affinity_t, position) p'
    affinity <- (#peek skparagraph_position_with_affinity_t, affinity) p'
    pure Skparagraph_position_with_affinity{..}
  poke p' Skparagraph_position_with_affinity{..} = do
    (#poke skparagraph_position_with_affinity_t, position) p' position
    (#poke skparagraph_position_with_affinity_t, affinity) p' affinity

{- | C struct: @"skparagraph_text_box_t"@

@
typedef struct 
{
  sk_rect_t *rect;
  skparagraph_text_direction_t direction;
} skparagraph_text_box_t
@
-}
data Skparagraph_text_box = Skparagraph_text_box
  { rect :: Ptr (Sk_rect) -- ^ C field: @"sk_rect_t *rect"@
  , direction :: Skparagraph_text_direction -- ^ C field: @"skparagraph_text_direction_t direction"@
  }
instance Foreign.Storable.Offset.Offset "rect" Skparagraph_text_box where
  rawOffset = (#offset skparagraph_text_box_t, rect)
instance Foreign.Storable.Offset.Offset "direction" Skparagraph_text_box where
  rawOffset = (#offset skparagraph_text_box_t, direction)
instance Foreign.Storable.Storable Skparagraph_text_box where
  sizeOf _ = (#size skparagraph_text_box_t)
  alignment _ = (#alignment skparagraph_text_box_t)
  peek p' = do
    rect <- (#peek skparagraph_text_box_t, rect) p'
    direction <- (#peek skparagraph_text_box_t, direction) p'
    pure Skparagraph_text_box{..}
  poke p' Skparagraph_text_box{..} = do
    (#poke skparagraph_text_box_t, rect) p' rect
    (#poke skparagraph_text_box_t, direction) p' direction

{- | C enum: @"skparagraph_text_baseline_t"@

@
typedef enum 
{
  ALPHABETIC_SKPARAGRAPH_TEXT_BASELINE,
  IDEOGRAPHIC_SKPARAGRAPH_TEXT_BASELINE
} skparagraph_text_baseline_t
@

-}
newtype Skparagraph_text_baseline = Skparagraph_text_baseline (#type skparagraph_text_baseline_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"skparagraph_text_baseline_t"@ value (1/2): @"ALPHABETIC_SKPARAGRAPH_TEXT_BASELINE"@
pattern ALPHABETIC_SKPARAGRAPH_TEXT_BASELINE :: Skparagraph_text_baseline
pattern ALPHABETIC_SKPARAGRAPH_TEXT_BASELINE = (#const ALPHABETIC_SKPARAGRAPH_TEXT_BASELINE)

-- | C enum @"skparagraph_text_baseline_t"@ value (2/2): @"IDEOGRAPHIC_SKPARAGRAPH_TEXT_BASELINE"@
pattern IDEOGRAPHIC_SKPARAGRAPH_TEXT_BASELINE :: Skparagraph_text_baseline
pattern IDEOGRAPHIC_SKPARAGRAPH_TEXT_BASELINE = (#const IDEOGRAPHIC_SKPARAGRAPH_TEXT_BASELINE)

{- | C enum: @"skparagraph_text_height_behavior_t"@

@
typedef enum 
{
  ALL_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR = 0x0,
  DISABLE_FIRST_ASCENT_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR = 0x1,
  DISABLE_LAST_DESCENT_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR = 0x2,
  DISABLE_ALL_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR = 0x1 | 0x2
} skparagraph_text_height_behavior_t
@

-}
newtype Skparagraph_text_height_behavior = Skparagraph_text_height_behavior (#type skparagraph_text_height_behavior_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"skparagraph_text_height_behavior_t"@ value (1/4): @"ALL_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR"@
pattern ALL_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR :: Skparagraph_text_height_behavior
pattern ALL_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR = (#const ALL_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR)

-- | C enum @"skparagraph_text_height_behavior_t"@ value (2/4): @"DISABLE_FIRST_ASCENT_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR"@
pattern DISABLE_FIRST_ASCENT_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR :: Skparagraph_text_height_behavior
pattern DISABLE_FIRST_ASCENT_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR = (#const DISABLE_FIRST_ASCENT_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR)

-- | C enum @"skparagraph_text_height_behavior_t"@ value (3/4): @"DISABLE_LAST_DESCENT_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR"@
pattern DISABLE_LAST_DESCENT_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR :: Skparagraph_text_height_behavior
pattern DISABLE_LAST_DESCENT_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR = (#const DISABLE_LAST_DESCENT_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR)

-- | C enum @"skparagraph_text_height_behavior_t"@ value (4/4): @"DISABLE_ALL_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR"@
pattern DISABLE_ALL_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR :: Skparagraph_text_height_behavior
pattern DISABLE_ALL_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR = (#const DISABLE_ALL_SKPARAGRAPH_TEXT_HEIGHT_BEHAVIOR)

{- | C enum: @"skparagraph_line_metric_style_t"@

@
typedef enum 
{
  TYPOGRAPHIC_SKPARAGRAPH_LINE_METRIC_STYLE,
  CSS_SKPARAGRAPH_LINE_METRIC_STYLE
} skparagraph_line_metric_style_t
@

-}
newtype Skparagraph_line_metric_style = Skparagraph_line_metric_style (#type skparagraph_line_metric_style_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"skparagraph_line_metric_style_t"@ value (1/2): @"TYPOGRAPHIC_SKPARAGRAPH_LINE_METRIC_STYLE"@
pattern TYPOGRAPHIC_SKPARAGRAPH_LINE_METRIC_STYLE :: Skparagraph_line_metric_style
pattern TYPOGRAPHIC_SKPARAGRAPH_LINE_METRIC_STYLE = (#const TYPOGRAPHIC_SKPARAGRAPH_LINE_METRIC_STYLE)

-- | C enum @"skparagraph_line_metric_style_t"@ value (2/2): @"CSS_SKPARAGRAPH_LINE_METRIC_STYLE"@
pattern CSS_SKPARAGRAPH_LINE_METRIC_STYLE :: Skparagraph_line_metric_style
pattern CSS_SKPARAGRAPH_LINE_METRIC_STYLE = (#const CSS_SKPARAGRAPH_LINE_METRIC_STYLE)

{- | C enum: @"skparagraph_text_decoration_flags_t"@

@
typedef enum 
{
  NO_DECORATION_SKPARAGRAPH_TEXT_DECORATION_FLAGS = 0x0,
  UNDERLINE_SKPARAGRAPH_TEXT_DECORATION_FLAGS = 0x1,
  OVERLINE_SKPARAGRAPH_TEXT_DECORATION_FLAGS = 0x2,
  LINETHROUGH_SKPARAGRAPH_TEXT_DECORATION_FLAGS = 0x4
} skparagraph_text_decoration_flags_t
@

-}
newtype Skparagraph_text_decoration_flags = Skparagraph_text_decoration_flags (#type skparagraph_text_decoration_flags_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"skparagraph_text_decoration_flags_t"@ value (1/4): @"NO_DECORATION_SKPARAGRAPH_TEXT_DECORATION_FLAGS"@
pattern NO_DECORATION_SKPARAGRAPH_TEXT_DECORATION_FLAGS :: Skparagraph_text_decoration_flags
pattern NO_DECORATION_SKPARAGRAPH_TEXT_DECORATION_FLAGS = (#const NO_DECORATION_SKPARAGRAPH_TEXT_DECORATION_FLAGS)

-- | C enum @"skparagraph_text_decoration_flags_t"@ value (2/4): @"UNDERLINE_SKPARAGRAPH_TEXT_DECORATION_FLAGS"@
pattern UNDERLINE_SKPARAGRAPH_TEXT_DECORATION_FLAGS :: Skparagraph_text_decoration_flags
pattern UNDERLINE_SKPARAGRAPH_TEXT_DECORATION_FLAGS = (#const UNDERLINE_SKPARAGRAPH_TEXT_DECORATION_FLAGS)

-- | C enum @"skparagraph_text_decoration_flags_t"@ value (3/4): @"OVERLINE_SKPARAGRAPH_TEXT_DECORATION_FLAGS"@
pattern OVERLINE_SKPARAGRAPH_TEXT_DECORATION_FLAGS :: Skparagraph_text_decoration_flags
pattern OVERLINE_SKPARAGRAPH_TEXT_DECORATION_FLAGS = (#const OVERLINE_SKPARAGRAPH_TEXT_DECORATION_FLAGS)

-- | C enum @"skparagraph_text_decoration_flags_t"@ value (4/4): @"LINETHROUGH_SKPARAGRAPH_TEXT_DECORATION_FLAGS"@
pattern LINETHROUGH_SKPARAGRAPH_TEXT_DECORATION_FLAGS :: Skparagraph_text_decoration_flags
pattern LINETHROUGH_SKPARAGRAPH_TEXT_DECORATION_FLAGS = (#const LINETHROUGH_SKPARAGRAPH_TEXT_DECORATION_FLAGS)

{- | C enum: @"skparagraph_text_decoration_style_t"@

@
typedef enum 
{
  SOLID_SKPARAGRAPH_TEXT_DECORATION_STYLE,
  DOUBLE_SKPARAGRAPH_TEXT_DECORATION_STYLE,
  DOTTED_SKPARAGRAPH_TEXT_DECORATION_STYLE,
  DASHED_SKPARAGRAPH_TEXT_DECORATION_STYLE,
  WAVY_SKPARAGRAPH_TEXT_DECORATION_STYLE
} skparagraph_text_decoration_style_t
@

-}
newtype Skparagraph_text_decoration_style = Skparagraph_text_decoration_style (#type skparagraph_text_decoration_style_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"skparagraph_text_decoration_style_t"@ value (1/5): @"SOLID_SKPARAGRAPH_TEXT_DECORATION_STYLE"@
pattern SOLID_SKPARAGRAPH_TEXT_DECORATION_STYLE :: Skparagraph_text_decoration_style
pattern SOLID_SKPARAGRAPH_TEXT_DECORATION_STYLE = (#const SOLID_SKPARAGRAPH_TEXT_DECORATION_STYLE)

-- | C enum @"skparagraph_text_decoration_style_t"@ value (2/5): @"DOUBLE_SKPARAGRAPH_TEXT_DECORATION_STYLE"@
pattern DOUBLE_SKPARAGRAPH_TEXT_DECORATION_STYLE :: Skparagraph_text_decoration_style
pattern DOUBLE_SKPARAGRAPH_TEXT_DECORATION_STYLE = (#const DOUBLE_SKPARAGRAPH_TEXT_DECORATION_STYLE)

-- | C enum @"skparagraph_text_decoration_style_t"@ value (3/5): @"DOTTED_SKPARAGRAPH_TEXT_DECORATION_STYLE"@
pattern DOTTED_SKPARAGRAPH_TEXT_DECORATION_STYLE :: Skparagraph_text_decoration_style
pattern DOTTED_SKPARAGRAPH_TEXT_DECORATION_STYLE = (#const DOTTED_SKPARAGRAPH_TEXT_DECORATION_STYLE)

-- | C enum @"skparagraph_text_decoration_style_t"@ value (4/5): @"DASHED_SKPARAGRAPH_TEXT_DECORATION_STYLE"@
pattern DASHED_SKPARAGRAPH_TEXT_DECORATION_STYLE :: Skparagraph_text_decoration_style
pattern DASHED_SKPARAGRAPH_TEXT_DECORATION_STYLE = (#const DASHED_SKPARAGRAPH_TEXT_DECORATION_STYLE)

-- | C enum @"skparagraph_text_decoration_style_t"@ value (5/5): @"WAVY_SKPARAGRAPH_TEXT_DECORATION_STYLE"@
pattern WAVY_SKPARAGRAPH_TEXT_DECORATION_STYLE :: Skparagraph_text_decoration_style
pattern WAVY_SKPARAGRAPH_TEXT_DECORATION_STYLE = (#const WAVY_SKPARAGRAPH_TEXT_DECORATION_STYLE)

{- | C enum: @"skparagraph_text_decoration_mode_t"@

@
typedef enum 
{
  GAPS_SKPARAGRAPH_TEXT_DECORATION_MODE,
  THROUGH_SKPARAGRAPH_TEXT_DECORATION_MODE
} skparagraph_text_decoration_mode_t
@

-}
newtype Skparagraph_text_decoration_mode = Skparagraph_text_decoration_mode (#type skparagraph_text_decoration_mode_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"skparagraph_text_decoration_mode_t"@ value (1/2): @"GAPS_SKPARAGRAPH_TEXT_DECORATION_MODE"@
pattern GAPS_SKPARAGRAPH_TEXT_DECORATION_MODE :: Skparagraph_text_decoration_mode
pattern GAPS_SKPARAGRAPH_TEXT_DECORATION_MODE = (#const GAPS_SKPARAGRAPH_TEXT_DECORATION_MODE)

-- | C enum @"skparagraph_text_decoration_mode_t"@ value (2/2): @"THROUGH_SKPARAGRAPH_TEXT_DECORATION_MODE"@
pattern THROUGH_SKPARAGRAPH_TEXT_DECORATION_MODE :: Skparagraph_text_decoration_mode
pattern THROUGH_SKPARAGRAPH_TEXT_DECORATION_MODE = (#const THROUGH_SKPARAGRAPH_TEXT_DECORATION_MODE)

{- | C enum: @"skparagraph_style_type_t"@

@
typedef enum 
{
  NONE_SKPARAGRAPH_STYLE_TYPE,
  ALL_ATTRIBUTES_SKPARAGRAPH_STYLE_TYPE,
  FONT_SKPARAGRAPH_STYLE_TYPE,
  FOREGROUND_SKPARAGRAPH_STYLE_TYPE,
  BACKGROUND_SKPARAGRAPH_STYLE_TYPE,
  SHADOW_SKPARAGRAPH_STYLE_TYPE,
  DECORATIONS_SKPARAGRAPH_STYLE_TYPE,
  LETTER_SPACING_SKPARAGRAPH_STYLE_TYPE,
  WORD_SPACING_SKPARAGRAPH_STYLE_TYPE
} skparagraph_style_type_t
@

-}
newtype Skparagraph_style_type = Skparagraph_style_type (#type skparagraph_style_type_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"skparagraph_style_type_t"@ value (1/9): @"NONE_SKPARAGRAPH_STYLE_TYPE"@
pattern NONE_SKPARAGRAPH_STYLE_TYPE :: Skparagraph_style_type
pattern NONE_SKPARAGRAPH_STYLE_TYPE = (#const NONE_SKPARAGRAPH_STYLE_TYPE)

-- | C enum @"skparagraph_style_type_t"@ value (2/9): @"ALL_ATTRIBUTES_SKPARAGRAPH_STYLE_TYPE"@
pattern ALL_ATTRIBUTES_SKPARAGRAPH_STYLE_TYPE :: Skparagraph_style_type
pattern ALL_ATTRIBUTES_SKPARAGRAPH_STYLE_TYPE = (#const ALL_ATTRIBUTES_SKPARAGRAPH_STYLE_TYPE)

-- | C enum @"skparagraph_style_type_t"@ value (3/9): @"FONT_SKPARAGRAPH_STYLE_TYPE"@
pattern FONT_SKPARAGRAPH_STYLE_TYPE :: Skparagraph_style_type
pattern FONT_SKPARAGRAPH_STYLE_TYPE = (#const FONT_SKPARAGRAPH_STYLE_TYPE)

-- | C enum @"skparagraph_style_type_t"@ value (4/9): @"FOREGROUND_SKPARAGRAPH_STYLE_TYPE"@
pattern FOREGROUND_SKPARAGRAPH_STYLE_TYPE :: Skparagraph_style_type
pattern FOREGROUND_SKPARAGRAPH_STYLE_TYPE = (#const FOREGROUND_SKPARAGRAPH_STYLE_TYPE)

-- | C enum @"skparagraph_style_type_t"@ value (5/9): @"BACKGROUND_SKPARAGRAPH_STYLE_TYPE"@
pattern BACKGROUND_SKPARAGRAPH_STYLE_TYPE :: Skparagraph_style_type
pattern BACKGROUND_SKPARAGRAPH_STYLE_TYPE = (#const BACKGROUND_SKPARAGRAPH_STYLE_TYPE)

-- | C enum @"skparagraph_style_type_t"@ value (6/9): @"SHADOW_SKPARAGRAPH_STYLE_TYPE"@
pattern SHADOW_SKPARAGRAPH_STYLE_TYPE :: Skparagraph_style_type
pattern SHADOW_SKPARAGRAPH_STYLE_TYPE = (#const SHADOW_SKPARAGRAPH_STYLE_TYPE)

-- | C enum @"skparagraph_style_type_t"@ value (7/9): @"DECORATIONS_SKPARAGRAPH_STYLE_TYPE"@
pattern DECORATIONS_SKPARAGRAPH_STYLE_TYPE :: Skparagraph_style_type
pattern DECORATIONS_SKPARAGRAPH_STYLE_TYPE = (#const DECORATIONS_SKPARAGRAPH_STYLE_TYPE)

-- | C enum @"skparagraph_style_type_t"@ value (8/9): @"LETTER_SPACING_SKPARAGRAPH_STYLE_TYPE"@
pattern LETTER_SPACING_SKPARAGRAPH_STYLE_TYPE :: Skparagraph_style_type
pattern LETTER_SPACING_SKPARAGRAPH_STYLE_TYPE = (#const LETTER_SPACING_SKPARAGRAPH_STYLE_TYPE)

-- | C enum @"skparagraph_style_type_t"@ value (9/9): @"WORD_SPACING_SKPARAGRAPH_STYLE_TYPE"@
pattern WORD_SPACING_SKPARAGRAPH_STYLE_TYPE :: Skparagraph_style_type
pattern WORD_SPACING_SKPARAGRAPH_STYLE_TYPE = (#const WORD_SPACING_SKPARAGRAPH_STYLE_TYPE)

{- | C enum: @"skparagraph_placeholder_alignment_t"@

@
typedef enum 
{
  BASELINE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT,
  ABOVE_BASELINE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT,
  BELOW_BASELINE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT,
  TOP_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT,
  BOTTOM_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT,
  MIDDLE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT
} skparagraph_placeholder_alignment_t
@

-}
newtype Skparagraph_placeholder_alignment = Skparagraph_placeholder_alignment (#type skparagraph_placeholder_alignment_t)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Bits, Storable)

-- | C enum @"skparagraph_placeholder_alignment_t"@ value (1/6): @"BASELINE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT"@
pattern BASELINE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT :: Skparagraph_placeholder_alignment
pattern BASELINE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT = (#const BASELINE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT)

-- | C enum @"skparagraph_placeholder_alignment_t"@ value (2/6): @"ABOVE_BASELINE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT"@
pattern ABOVE_BASELINE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT :: Skparagraph_placeholder_alignment
pattern ABOVE_BASELINE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT = (#const ABOVE_BASELINE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT)

-- | C enum @"skparagraph_placeholder_alignment_t"@ value (3/6): @"BELOW_BASELINE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT"@
pattern BELOW_BASELINE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT :: Skparagraph_placeholder_alignment
pattern BELOW_BASELINE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT = (#const BELOW_BASELINE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT)

-- | C enum @"skparagraph_placeholder_alignment_t"@ value (4/6): @"TOP_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT"@
pattern TOP_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT :: Skparagraph_placeholder_alignment
pattern TOP_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT = (#const TOP_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT)

-- | C enum @"skparagraph_placeholder_alignment_t"@ value (5/6): @"BOTTOM_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT"@
pattern BOTTOM_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT :: Skparagraph_placeholder_alignment
pattern BOTTOM_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT = (#const BOTTOM_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT)

-- | C enum @"skparagraph_placeholder_alignment_t"@ value (6/6): @"MIDDLE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT"@
pattern MIDDLE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT :: Skparagraph_placeholder_alignment
pattern MIDDLE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT = (#const MIDDLE_SKPARAGRAPH_PLACEHOLDER_ALIGNMENT)

{- | C struct: @"skparagraph_placeholder_style_t"@

@
typedef struct 
{
  sk_scalar_t fWidth;
  sk_scalar_t fHeight;
  skparagraph_placeholder_alignment_t fAlignment;
  skparagraph_text_baseline_t fBaseline;
  sk_scalar_t fBaselineOffset;
} skparagraph_placeholder_style_t
@
-}
data Skparagraph_placeholder_style = Skparagraph_placeholder_style
  { fWidth :: Sk_scalar -- ^ C field: @"sk_scalar_t fWidth"@
  , fHeight :: Sk_scalar -- ^ C field: @"sk_scalar_t fHeight"@
  , fAlignment :: Skparagraph_placeholder_alignment -- ^ C field: @"skparagraph_placeholder_alignment_t fAlignment"@
  , fBaseline :: Skparagraph_text_baseline -- ^ C field: @"skparagraph_text_baseline_t fBaseline"@
  , fBaselineOffset :: Sk_scalar -- ^ C field: @"sk_scalar_t fBaselineOffset"@
  }
instance Foreign.Storable.Offset.Offset "fWidth" Skparagraph_placeholder_style where
  rawOffset = (#offset skparagraph_placeholder_style_t, fWidth)
instance Foreign.Storable.Offset.Offset "fHeight" Skparagraph_placeholder_style where
  rawOffset = (#offset skparagraph_placeholder_style_t, fHeight)
instance Foreign.Storable.Offset.Offset "fAlignment" Skparagraph_placeholder_style where
  rawOffset = (#offset skparagraph_placeholder_style_t, fAlignment)
instance Foreign.Storable.Offset.Offset "fBaseline" Skparagraph_placeholder_style where
  rawOffset = (#offset skparagraph_placeholder_style_t, fBaseline)
instance Foreign.Storable.Offset.Offset "fBaselineOffset" Skparagraph_placeholder_style where
  rawOffset = (#offset skparagraph_placeholder_style_t, fBaselineOffset)
instance Foreign.Storable.Storable Skparagraph_placeholder_style where
  sizeOf _ = (#size skparagraph_placeholder_style_t)
  alignment _ = (#alignment skparagraph_placeholder_style_t)
  peek p' = do
    fWidth <- (#peek skparagraph_placeholder_style_t, fWidth) p'
    fHeight <- (#peek skparagraph_placeholder_style_t, fHeight) p'
    fAlignment <- (#peek skparagraph_placeholder_style_t, fAlignment) p'
    fBaseline <- (#peek skparagraph_placeholder_style_t, fBaseline) p'
    fBaselineOffset <- (#peek skparagraph_placeholder_style_t, fBaselineOffset) p'
    pure Skparagraph_placeholder_style{..}
  poke p' Skparagraph_placeholder_style{..} = do
    (#poke skparagraph_placeholder_style_t, fWidth) p' fWidth
    (#poke skparagraph_placeholder_style_t, fHeight) p' fHeight
    (#poke skparagraph_placeholder_style_t, fAlignment) p' fAlignment
    (#poke skparagraph_placeholder_style_t, fBaseline) p' fBaseline
    (#poke skparagraph_placeholder_style_t, fBaselineOffset) p' fBaselineOffset

{- | Opaque C struct: @"skparagraph_text_style_t"@
-}
data Skparagraph_text_style = Skparagraph_text_style

{- | C struct: @"skparagraph_decoration_t"@

@
typedef struct 
{
  skparagraph_text_decoration_flags_t fType;
  skparagraph_text_decoration_mode_t fMode;
  sk_color_t fColor;
  skparagraph_text_decoration_style_t fStyle;
  sk_scalar_t fThicknessMultiplier;
} skparagraph_decoration_t
@
-}
data Skparagraph_decoration = Skparagraph_decoration
  { fType :: Skparagraph_text_decoration_flags -- ^ C field: @"skparagraph_text_decoration_flags_t fType"@
  , fMode :: Skparagraph_text_decoration_mode -- ^ C field: @"skparagraph_text_decoration_mode_t fMode"@
  , fColor :: Sk_color -- ^ C field: @"sk_color_t fColor"@
  , fStyle :: Skparagraph_text_decoration_style -- ^ C field: @"skparagraph_text_decoration_style_t fStyle"@
  , fThicknessMultiplier :: Sk_scalar -- ^ C field: @"sk_scalar_t fThicknessMultiplier"@
  }
instance Foreign.Storable.Offset.Offset "fType" Skparagraph_decoration where
  rawOffset = (#offset skparagraph_decoration_t, fType)
instance Foreign.Storable.Offset.Offset "fMode" Skparagraph_decoration where
  rawOffset = (#offset skparagraph_decoration_t, fMode)
instance Foreign.Storable.Offset.Offset "fColor" Skparagraph_decoration where
  rawOffset = (#offset skparagraph_decoration_t, fColor)
instance Foreign.Storable.Offset.Offset "fStyle" Skparagraph_decoration where
  rawOffset = (#offset skparagraph_decoration_t, fStyle)
instance Foreign.Storable.Offset.Offset "fThicknessMultiplier" Skparagraph_decoration where
  rawOffset = (#offset skparagraph_decoration_t, fThicknessMultiplier)
instance Foreign.Storable.Storable Skparagraph_decoration where
  sizeOf _ = (#size skparagraph_decoration_t)
  alignment _ = (#alignment skparagraph_decoration_t)
  peek p' = do
    fType <- (#peek skparagraph_decoration_t, fType) p'
    fMode <- (#peek skparagraph_decoration_t, fMode) p'
    fColor <- (#peek skparagraph_decoration_t, fColor) p'
    fStyle <- (#peek skparagraph_decoration_t, fStyle) p'
    fThicknessMultiplier <- (#peek skparagraph_decoration_t, fThicknessMultiplier) p'
    pure Skparagraph_decoration{..}
  poke p' Skparagraph_decoration{..} = do
    (#poke skparagraph_decoration_t, fType) p' fType
    (#poke skparagraph_decoration_t, fMode) p' fMode
    (#poke skparagraph_decoration_t, fColor) p' fColor
    (#poke skparagraph_decoration_t, fStyle) p' fStyle
    (#poke skparagraph_decoration_t, fThicknessMultiplier) p' fThicknessMultiplier

{- | C struct: @"skparagraph_font_feature_t"@

@
typedef struct 
{
  sk_string_t *fName;
  int *fValue;
} skparagraph_font_feature_t
@
-}
data Skparagraph_font_feature = Skparagraph_font_feature
  { fName :: Ptr (Sk_string) -- ^ C field: @"sk_string_t *fName"@
  , fValue :: Ptr (CInt) -- ^ C field: @"int *fValue"@
  }
instance Foreign.Storable.Offset.Offset "fName" Skparagraph_font_feature where
  rawOffset = (#offset skparagraph_font_feature_t, fName)
instance Foreign.Storable.Offset.Offset "fValue" Skparagraph_font_feature where
  rawOffset = (#offset skparagraph_font_feature_t, fValue)
instance Foreign.Storable.Storable Skparagraph_font_feature where
  sizeOf _ = (#size skparagraph_font_feature_t)
  alignment _ = (#alignment skparagraph_font_feature_t)
  peek p' = do
    fName <- (#peek skparagraph_font_feature_t, fName) p'
    fValue <- (#peek skparagraph_font_feature_t, fValue) p'
    pure Skparagraph_font_feature{..}
  poke p' Skparagraph_font_feature{..} = do
    (#poke skparagraph_font_feature_t, fName) p' fName
    (#poke skparagraph_font_feature_t, fValue) p' fValue

{- | C struct: @"skparagraph_block_t"@

@
typedef struct 
{
  size_t fRange_start;
  size_t fRange_end;
  skparagraph_text_style_t *fStyle;
} skparagraph_block_t
@
-}
data Skparagraph_block = Skparagraph_block
  { fRange_start :: CSize -- ^ C field: @"size_t fRange_start"@
  , fRange_end :: CSize -- ^ C field: @"size_t fRange_end"@
  , fStyle :: Ptr (Skparagraph_text_style) -- ^ C field: @"skparagraph_text_style_t *fStyle"@
  }
instance Foreign.Storable.Offset.Offset "fRange_start" Skparagraph_block where
  rawOffset = (#offset skparagraph_block_t, fRange_start)
instance Foreign.Storable.Offset.Offset "fRange_end" Skparagraph_block where
  rawOffset = (#offset skparagraph_block_t, fRange_end)
instance Foreign.Storable.Offset.Offset "fStyle" Skparagraph_block where
  rawOffset = (#offset skparagraph_block_t, fStyle)
instance Foreign.Storable.Storable Skparagraph_block where
  sizeOf _ = (#size skparagraph_block_t)
  alignment _ = (#alignment skparagraph_block_t)
  peek p' = do
    fRange_start <- (#peek skparagraph_block_t, fRange_start) p'
    fRange_end <- (#peek skparagraph_block_t, fRange_end) p'
    fStyle <- (#peek skparagraph_block_t, fStyle) p'
    pure Skparagraph_block{..}
  poke p' Skparagraph_block{..} = do
    (#poke skparagraph_block_t, fRange_start) p' fRange_start
    (#poke skparagraph_block_t, fRange_end) p' fRange_end
    (#poke skparagraph_block_t, fStyle) p' fStyle

{- | C struct: @"skparagraph_placeholder_t"@

@
typedef struct 
{
  size_t fRange_start;
  size_t fRange_end;
  skparagraph_placeholder_style_t fStyle;
  skparagraph_text_style_t *fTextStyle;
  size_t fBlocksBefore_start;
  size_t fBlocksBefore_end;
  size_t fTextBefore_start;
  size_t fTextBefore_end;
} skparagraph_placeholder_t
@
-}
data Skparagraph_placeholder = Skparagraph_placeholder
  { fRange_start :: CSize -- ^ C field: @"size_t fRange_start"@
  , fRange_end :: CSize -- ^ C field: @"size_t fRange_end"@
  , fStyle :: Skparagraph_placeholder_style -- ^ C field: @"skparagraph_placeholder_style_t fStyle"@
  , fTextStyle :: Ptr (Skparagraph_text_style) -- ^ C field: @"skparagraph_text_style_t *fTextStyle"@
  , fBlocksBefore_start :: CSize -- ^ C field: @"size_t fBlocksBefore_start"@
  , fBlocksBefore_end :: CSize -- ^ C field: @"size_t fBlocksBefore_end"@
  , fTextBefore_start :: CSize -- ^ C field: @"size_t fTextBefore_start"@
  , fTextBefore_end :: CSize -- ^ C field: @"size_t fTextBefore_end"@
  }
instance Foreign.Storable.Offset.Offset "fRange_start" Skparagraph_placeholder where
  rawOffset = (#offset skparagraph_placeholder_t, fRange_start)
instance Foreign.Storable.Offset.Offset "fRange_end" Skparagraph_placeholder where
  rawOffset = (#offset skparagraph_placeholder_t, fRange_end)
instance Foreign.Storable.Offset.Offset "fStyle" Skparagraph_placeholder where
  rawOffset = (#offset skparagraph_placeholder_t, fStyle)
instance Foreign.Storable.Offset.Offset "fTextStyle" Skparagraph_placeholder where
  rawOffset = (#offset skparagraph_placeholder_t, fTextStyle)
instance Foreign.Storable.Offset.Offset "fBlocksBefore_start" Skparagraph_placeholder where
  rawOffset = (#offset skparagraph_placeholder_t, fBlocksBefore_start)
instance Foreign.Storable.Offset.Offset "fBlocksBefore_end" Skparagraph_placeholder where
  rawOffset = (#offset skparagraph_placeholder_t, fBlocksBefore_end)
instance Foreign.Storable.Offset.Offset "fTextBefore_start" Skparagraph_placeholder where
  rawOffset = (#offset skparagraph_placeholder_t, fTextBefore_start)
instance Foreign.Storable.Offset.Offset "fTextBefore_end" Skparagraph_placeholder where
  rawOffset = (#offset skparagraph_placeholder_t, fTextBefore_end)
instance Foreign.Storable.Storable Skparagraph_placeholder where
  sizeOf _ = (#size skparagraph_placeholder_t)
  alignment _ = (#alignment skparagraph_placeholder_t)
  peek p' = do
    fRange_start <- (#peek skparagraph_placeholder_t, fRange_start) p'
    fRange_end <- (#peek skparagraph_placeholder_t, fRange_end) p'
    fStyle <- (#peek skparagraph_placeholder_t, fStyle) p'
    fTextStyle <- (#peek skparagraph_placeholder_t, fTextStyle) p'
    fBlocksBefore_start <- (#peek skparagraph_placeholder_t, fBlocksBefore_start) p'
    fBlocksBefore_end <- (#peek skparagraph_placeholder_t, fBlocksBefore_end) p'
    fTextBefore_start <- (#peek skparagraph_placeholder_t, fTextBefore_start) p'
    fTextBefore_end <- (#peek skparagraph_placeholder_t, fTextBefore_end) p'
    pure Skparagraph_placeholder{..}
  poke p' Skparagraph_placeholder{..} = do
    (#poke skparagraph_placeholder_t, fRange_start) p' fRange_start
    (#poke skparagraph_placeholder_t, fRange_end) p' fRange_end
    (#poke skparagraph_placeholder_t, fStyle) p' fStyle
    (#poke skparagraph_placeholder_t, fTextStyle) p' fTextStyle
    (#poke skparagraph_placeholder_t, fBlocksBefore_start) p' fBlocksBefore_start
    (#poke skparagraph_placeholder_t, fBlocksBefore_end) p' fBlocksBefore_end
    (#poke skparagraph_placeholder_t, fTextBefore_start) p' fTextBefore_start
    (#poke skparagraph_placeholder_t, fTextBefore_end) p' fTextBefore_end

{- | Opaque C struct: @"skparagraph_font_arguments_t"@
-}
data Skparagraph_font_arguments = Skparagraph_font_arguments

{- | Opaque C struct: @"skparagraph_font_collection_t"@
-}
data Skparagraph_font_collection = Skparagraph_font_collection

{- | C struct: @"skparagraph_style_metrics_t"@

@
typedef struct 
{
  skparagraph_text_style_t *text_style;
  sk_fontmetrics_t font_metrics;
} skparagraph_style_metrics_t
@
-}
data Skparagraph_style_metrics = Skparagraph_style_metrics
  { text_style :: Ptr (Skparagraph_text_style) -- ^ C field: @"skparagraph_text_style_t *text_style"@
  , font_metrics :: Sk_fontmetrics -- ^ C field: @"sk_fontmetrics_t font_metrics"@
  }
instance Foreign.Storable.Offset.Offset "text_style" Skparagraph_style_metrics where
  rawOffset = (#offset skparagraph_style_metrics_t, text_style)
instance Foreign.Storable.Offset.Offset "font_metrics" Skparagraph_style_metrics where
  rawOffset = (#offset skparagraph_style_metrics_t, font_metrics)
instance Foreign.Storable.Storable Skparagraph_style_metrics where
  sizeOf _ = (#size skparagraph_style_metrics_t)
  alignment _ = (#alignment skparagraph_style_metrics_t)
  peek p' = do
    text_style <- (#peek skparagraph_style_metrics_t, text_style) p'
    font_metrics <- (#peek skparagraph_style_metrics_t, font_metrics) p'
    pure Skparagraph_style_metrics{..}
  poke p' Skparagraph_style_metrics{..} = do
    (#poke skparagraph_style_metrics_t, text_style) p' text_style
    (#poke skparagraph_style_metrics_t, font_metrics) p' font_metrics

{- | C struct: @"skparagraph_line_metrics_t"@

@
typedef struct 
{
  size_t fStartIndex;
  size_t fEndIndex;
  size_t fEndExcludingWhitespaces;
  size_t fEndIncludingNewline;
  _Bool fHardBreak;
  double fAscent;
  double fDescent;
  double fUnscaledAscent;
  double fHeight;
  double fWidth;
  double fLeft;
  double fBaseline;
  size_t fLineNumber;
} skparagraph_line_metrics_t
@
-}
data Skparagraph_line_metrics = Skparagraph_line_metrics
  { fStartIndex :: CSize -- ^ C field: @"size_t fStartIndex"@
  , fEndIndex :: CSize -- ^ C field: @"size_t fEndIndex"@
  , fEndExcludingWhitespaces :: CSize -- ^ C field: @"size_t fEndExcludingWhitespaces"@
  , fEndIncludingNewline :: CSize -- ^ C field: @"size_t fEndIncludingNewline"@
  , fHardBreak :: CBool -- ^ C field: @"_Bool fHardBreak"@
  , fAscent :: CDouble -- ^ C field: @"double fAscent"@
  , fDescent :: CDouble -- ^ C field: @"double fDescent"@
  , fUnscaledAscent :: CDouble -- ^ C field: @"double fUnscaledAscent"@
  , fHeight :: CDouble -- ^ C field: @"double fHeight"@
  , fWidth :: CDouble -- ^ C field: @"double fWidth"@
  , fLeft :: CDouble -- ^ C field: @"double fLeft"@
  , fBaseline :: CDouble -- ^ C field: @"double fBaseline"@
  , fLineNumber :: CSize -- ^ C field: @"size_t fLineNumber"@
  }
instance Foreign.Storable.Offset.Offset "fStartIndex" Skparagraph_line_metrics where
  rawOffset = (#offset skparagraph_line_metrics_t, fStartIndex)
instance Foreign.Storable.Offset.Offset "fEndIndex" Skparagraph_line_metrics where
  rawOffset = (#offset skparagraph_line_metrics_t, fEndIndex)
instance Foreign.Storable.Offset.Offset "fEndExcludingWhitespaces" Skparagraph_line_metrics where
  rawOffset = (#offset skparagraph_line_metrics_t, fEndExcludingWhitespaces)
instance Foreign.Storable.Offset.Offset "fEndIncludingNewline" Skparagraph_line_metrics where
  rawOffset = (#offset skparagraph_line_metrics_t, fEndIncludingNewline)
instance Foreign.Storable.Offset.Offset "fHardBreak" Skparagraph_line_metrics where
  rawOffset = (#offset skparagraph_line_metrics_t, fHardBreak)
instance Foreign.Storable.Offset.Offset "fAscent" Skparagraph_line_metrics where
  rawOffset = (#offset skparagraph_line_metrics_t, fAscent)
instance Foreign.Storable.Offset.Offset "fDescent" Skparagraph_line_metrics where
  rawOffset = (#offset skparagraph_line_metrics_t, fDescent)
instance Foreign.Storable.Offset.Offset "fUnscaledAscent" Skparagraph_line_metrics where
  rawOffset = (#offset skparagraph_line_metrics_t, fUnscaledAscent)
instance Foreign.Storable.Offset.Offset "fHeight" Skparagraph_line_metrics where
  rawOffset = (#offset skparagraph_line_metrics_t, fHeight)
instance Foreign.Storable.Offset.Offset "fWidth" Skparagraph_line_metrics where
  rawOffset = (#offset skparagraph_line_metrics_t, fWidth)
instance Foreign.Storable.Offset.Offset "fLeft" Skparagraph_line_metrics where
  rawOffset = (#offset skparagraph_line_metrics_t, fLeft)
instance Foreign.Storable.Offset.Offset "fBaseline" Skparagraph_line_metrics where
  rawOffset = (#offset skparagraph_line_metrics_t, fBaseline)
instance Foreign.Storable.Offset.Offset "fLineNumber" Skparagraph_line_metrics where
  rawOffset = (#offset skparagraph_line_metrics_t, fLineNumber)
instance Foreign.Storable.Storable Skparagraph_line_metrics where
  sizeOf _ = (#size skparagraph_line_metrics_t)
  alignment _ = (#alignment skparagraph_line_metrics_t)
  peek p' = do
    fStartIndex <- (#peek skparagraph_line_metrics_t, fStartIndex) p'
    fEndIndex <- (#peek skparagraph_line_metrics_t, fEndIndex) p'
    fEndExcludingWhitespaces <- (#peek skparagraph_line_metrics_t, fEndExcludingWhitespaces) p'
    fEndIncludingNewline <- (#peek skparagraph_line_metrics_t, fEndIncludingNewline) p'
    fHardBreak <- (#peek skparagraph_line_metrics_t, fHardBreak) p'
    fAscent <- (#peek skparagraph_line_metrics_t, fAscent) p'
    fDescent <- (#peek skparagraph_line_metrics_t, fDescent) p'
    fUnscaledAscent <- (#peek skparagraph_line_metrics_t, fUnscaledAscent) p'
    fHeight <- (#peek skparagraph_line_metrics_t, fHeight) p'
    fWidth <- (#peek skparagraph_line_metrics_t, fWidth) p'
    fLeft <- (#peek skparagraph_line_metrics_t, fLeft) p'
    fBaseline <- (#peek skparagraph_line_metrics_t, fBaseline) p'
    fLineNumber <- (#peek skparagraph_line_metrics_t, fLineNumber) p'
    pure Skparagraph_line_metrics{..}
  poke p' Skparagraph_line_metrics{..} = do
    (#poke skparagraph_line_metrics_t, fStartIndex) p' fStartIndex
    (#poke skparagraph_line_metrics_t, fEndIndex) p' fEndIndex
    (#poke skparagraph_line_metrics_t, fEndExcludingWhitespaces) p' fEndExcludingWhitespaces
    (#poke skparagraph_line_metrics_t, fEndIncludingNewline) p' fEndIncludingNewline
    (#poke skparagraph_line_metrics_t, fHardBreak) p' fHardBreak
    (#poke skparagraph_line_metrics_t, fAscent) p' fAscent
    (#poke skparagraph_line_metrics_t, fDescent) p' fDescent
    (#poke skparagraph_line_metrics_t, fUnscaledAscent) p' fUnscaledAscent
    (#poke skparagraph_line_metrics_t, fHeight) p' fHeight
    (#poke skparagraph_line_metrics_t, fWidth) p' fWidth
    (#poke skparagraph_line_metrics_t, fLeft) p' fLeft
    (#poke skparagraph_line_metrics_t, fBaseline) p' fBaseline
    (#poke skparagraph_line_metrics_t, fLineNumber) p' fLineNumber

{- | C struct: @"skparagraph_text_shadow_t"@

@
typedef struct 
{
  sk_color_t fColor;
  sk_point_t fOffset;
  double fBlurSigma;
} skparagraph_text_shadow_t
@
-}
data Skparagraph_text_shadow = Skparagraph_text_shadow
  { fColor :: Sk_color -- ^ C field: @"sk_color_t fColor"@
  , fOffset :: Sk_point -- ^ C field: @"sk_point_t fOffset"@
  , fBlurSigma :: CDouble -- ^ C field: @"double fBlurSigma"@
  }
instance Foreign.Storable.Offset.Offset "fColor" Skparagraph_text_shadow where
  rawOffset = (#offset skparagraph_text_shadow_t, fColor)
instance Foreign.Storable.Offset.Offset "fOffset" Skparagraph_text_shadow where
  rawOffset = (#offset skparagraph_text_shadow_t, fOffset)
instance Foreign.Storable.Offset.Offset "fBlurSigma" Skparagraph_text_shadow where
  rawOffset = (#offset skparagraph_text_shadow_t, fBlurSigma)
instance Foreign.Storable.Storable Skparagraph_text_shadow where
  sizeOf _ = (#size skparagraph_text_shadow_t)
  alignment _ = (#alignment skparagraph_text_shadow_t)
  peek p' = do
    fColor <- (#peek skparagraph_text_shadow_t, fColor) p'
    fOffset <- (#peek skparagraph_text_shadow_t, fOffset) p'
    fBlurSigma <- (#peek skparagraph_text_shadow_t, fBlurSigma) p'
    pure Skparagraph_text_shadow{..}
  poke p' Skparagraph_text_shadow{..} = do
    (#poke skparagraph_text_shadow_t, fColor) p' fColor
    (#poke skparagraph_text_shadow_t, fOffset) p' fOffset
    (#poke skparagraph_text_shadow_t, fBlurSigma) p' fBlurSigma

{- | C type alias: @skparagraph_paint_id_t@

@
typedef int skparagraph_paint_id_t
@
-}
type Skparagraph_paint_id = CInt

{- | C struct: @"skparagraph_dash_path_effect_t"@

@
typedef struct 
{
  sk_scalar_t fOnLength;
  sk_scalar_t fOffLength;
} skparagraph_dash_path_effect_t
@
-}
data Skparagraph_dash_path_effect = Skparagraph_dash_path_effect
  { fOnLength :: Sk_scalar -- ^ C field: @"sk_scalar_t fOnLength"@
  , fOffLength :: Sk_scalar -- ^ C field: @"sk_scalar_t fOffLength"@
  }
instance Foreign.Storable.Offset.Offset "fOnLength" Skparagraph_dash_path_effect where
  rawOffset = (#offset skparagraph_dash_path_effect_t, fOnLength)
instance Foreign.Storable.Offset.Offset "fOffLength" Skparagraph_dash_path_effect where
  rawOffset = (#offset skparagraph_dash_path_effect_t, fOffLength)
instance Foreign.Storable.Storable Skparagraph_dash_path_effect where
  sizeOf _ = (#size skparagraph_dash_path_effect_t)
  alignment _ = (#alignment skparagraph_dash_path_effect_t)
  peek p' = do
    fOnLength <- (#peek skparagraph_dash_path_effect_t, fOnLength) p'
    fOffLength <- (#peek skparagraph_dash_path_effect_t, fOffLength) p'
    pure Skparagraph_dash_path_effect{..}
  poke p' Skparagraph_dash_path_effect{..} = do
    (#poke skparagraph_dash_path_effect_t, fOnLength) p' fOnLength
    (#poke skparagraph_dash_path_effect_t, fOffLength) p' fOffLength

{- | Opaque C struct: @"skparagraph_decoration_style_t"@
-}
data Skparagraph_decoration_style = Skparagraph_decoration_style

{- | Opaque C struct: @"skparagraph_paragraph_painter_t"@
-}
data Skparagraph_paragraph_painter = Skparagraph_paragraph_painter

{- | Opaque C struct: @"skparagraph_paragraph_t"@
-}
data Skparagraph_paragraph = Skparagraph_paragraph

{- | C struct: @"skparagraph_glyph_cluster_info_t"@

@
typedef struct 
{
  sk_rect_t fBounds;
  size_t fClusterTextRange_start;
  size_t fClusterTextRange_end;
  skparagraph_text_direction_t fGlyphClusterPosition;
} skparagraph_glyph_cluster_info_t
@
-}
data Skparagraph_glyph_cluster_info = Skparagraph_glyph_cluster_info
  { fBounds :: Sk_rect -- ^ C field: @"sk_rect_t fBounds"@
  , fClusterTextRange_start :: CSize -- ^ C field: @"size_t fClusterTextRange_start"@
  , fClusterTextRange_end :: CSize -- ^ C field: @"size_t fClusterTextRange_end"@
  , fGlyphClusterPosition :: Skparagraph_text_direction -- ^ C field: @"skparagraph_text_direction_t fGlyphClusterPosition"@
  }
instance Foreign.Storable.Offset.Offset "fBounds" Skparagraph_glyph_cluster_info where
  rawOffset = (#offset skparagraph_glyph_cluster_info_t, fBounds)
instance Foreign.Storable.Offset.Offset "fClusterTextRange_start" Skparagraph_glyph_cluster_info where
  rawOffset = (#offset skparagraph_glyph_cluster_info_t, fClusterTextRange_start)
instance Foreign.Storable.Offset.Offset "fClusterTextRange_end" Skparagraph_glyph_cluster_info where
  rawOffset = (#offset skparagraph_glyph_cluster_info_t, fClusterTextRange_end)
instance Foreign.Storable.Offset.Offset "fGlyphClusterPosition" Skparagraph_glyph_cluster_info where
  rawOffset = (#offset skparagraph_glyph_cluster_info_t, fGlyphClusterPosition)
instance Foreign.Storable.Storable Skparagraph_glyph_cluster_info where
  sizeOf _ = (#size skparagraph_glyph_cluster_info_t)
  alignment _ = (#alignment skparagraph_glyph_cluster_info_t)
  peek p' = do
    fBounds <- (#peek skparagraph_glyph_cluster_info_t, fBounds) p'
    fClusterTextRange_start <- (#peek skparagraph_glyph_cluster_info_t, fClusterTextRange_start) p'
    fClusterTextRange_end <- (#peek skparagraph_glyph_cluster_info_t, fClusterTextRange_end) p'
    fGlyphClusterPosition <- (#peek skparagraph_glyph_cluster_info_t, fGlyphClusterPosition) p'
    pure Skparagraph_glyph_cluster_info{..}
  poke p' Skparagraph_glyph_cluster_info{..} = do
    (#poke skparagraph_glyph_cluster_info_t, fBounds) p' fBounds
    (#poke skparagraph_glyph_cluster_info_t, fClusterTextRange_start) p' fClusterTextRange_start
    (#poke skparagraph_glyph_cluster_info_t, fClusterTextRange_end) p' fClusterTextRange_end
    (#poke skparagraph_glyph_cluster_info_t, fGlyphClusterPosition) p' fGlyphClusterPosition

{- | Opaque C struct: @"skparagraph_strut_style_t"@
-}
data Skparagraph_strut_style = Skparagraph_strut_style

{- | Opaque C struct: @"skparagraph_paragraph_style_t"@
-}
data Skparagraph_paragraph_style = Skparagraph_paragraph_style

{- | Opaque C struct: @"skparagraph_paragraph_builder_t"@
-}
data Skparagraph_paragraph_builder = Skparagraph_paragraph_builder

{- | Opaque C struct: @"skparagraph_paragraph_builder_impl_t"@
-}
data Skparagraph_paragraph_builder_impl = Skparagraph_paragraph_builder_impl

{- | Opaque C struct: @"skparagraph_paragraph_impl_t"@
-}
data Skparagraph_paragraph_impl = Skparagraph_paragraph_impl

{- | Opaque C struct: @"skparagraph_canvas_paragraph_painter_t"@
-}
data Skparagraph_canvas_paragraph_painter = Skparagraph_canvas_paragraph_painter

{- | Opaque C struct: @"skresources_image_asset_t"@
-}
data Skresources_image_asset = Skresources_image_asset

{- | Opaque C struct: @"skresources_multi_frame_image_asset_t"@
-}
data Skresources_multi_frame_image_asset = Skresources_multi_frame_image_asset

{- | Opaque C struct: @"skresources_external_track_asset_t"@
-}
data Skresources_external_track_asset = Skresources_external_track_asset

{- | Opaque C struct: @"skresources_resource_provider_t"@
-}
data Skresources_resource_provider = Skresources_resource_provider

{- | Opaque C struct: @"skunicode_skunicode_t"@
-}
data Skunicode_skunicode = Skunicode_skunicode

{- | Opaque C struct: @"sksvg_dom_t"@
-}
data Sksvg_dom = Sksvg_dom
