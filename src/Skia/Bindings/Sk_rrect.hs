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
module Skia.Bindings.Sk_rrect where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
sk_rrect_t *sk_rrect_new(void)
@
-}
foreign import ccall "sk_rrect_new" sk_rrect_new ::
  IO (Ptr (Sk_rrect)) -- ^ C return type: @"sk_rrect_t *"@

-- | Function pointer to 'sk_rrect_new'
foreign import ccall "&sk_rrect_new" p'sk_rrect_new ::
  FunPtr (IO (Ptr (Sk_rrect)))

{- | C function signature:

@
sk_rrect_t *sk_rrect_new_copy(const sk_rrect_t *rrect)
@
-}
foreign import ccall "sk_rrect_new_copy" sk_rrect_new_copy ::
  Ptr (Sk_rrect) -- ^ C argument @"const sk_rrect_t * rrect"@
  -> IO (Ptr (Sk_rrect)) -- ^ C return type: @"sk_rrect_t *"@

-- | Function pointer to 'sk_rrect_new_copy'
foreign import ccall "&sk_rrect_new_copy" p'sk_rrect_new_copy ::
  FunPtr (Ptr (Sk_rrect) -> IO (Ptr (Sk_rrect)))

{- | C function signature:

@
void sk_rrect_delete(const sk_rrect_t *rrect)
@
-}
foreign import ccall "sk_rrect_delete" sk_rrect_delete ::
  Ptr (Sk_rrect) -- ^ C argument @"const sk_rrect_t * rrect"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_rrect_delete'
foreign import ccall "&sk_rrect_delete" p'sk_rrect_delete ::
  FunPtr (Ptr (Sk_rrect) -> IO (()))

{- | C function signature:

@
sk_rrect_type_t sk_rrect_get_type(const sk_rrect_t *rrect)
@
-}
foreign import ccall "sk_rrect_get_type" sk_rrect_get_type ::
  Ptr (Sk_rrect) -- ^ C argument @"const sk_rrect_t * rrect"@
  -> IO (Sk_rrect_type) -- ^ C return type: @"sk_rrect_type_t"@

-- | Function pointer to 'sk_rrect_get_type'
foreign import ccall "&sk_rrect_get_type" p'sk_rrect_get_type ::
  FunPtr (Ptr (Sk_rrect) -> IO (Sk_rrect_type))

{- | C function signature:

@
void sk_rrect_get_rect(const sk_rrect_t *rrect, sk_rect_t *rect)
@
-}
foreign import ccall "sk_rrect_get_rect" sk_rrect_get_rect ::
  Ptr (Sk_rrect) -- ^ C argument @"const sk_rrect_t * rrect"@
  -> Ptr (Sk_rect) -- ^ C argument @"sk_rect_t * rect"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_rrect_get_rect'
foreign import ccall "&sk_rrect_get_rect" p'sk_rrect_get_rect ::
  FunPtr (Ptr (Sk_rrect) -> Ptr (Sk_rect) -> IO (()))

{- | C function signature:

@
void sk_rrect_get_radii(const sk_rrect_t *rrect, sk_rrect_corner_t corner, sk_vector_t *radii)
@
-}
foreign import ccall "sk_rrect_get_radii" sk_rrect_get_radii ::
  Ptr (Sk_rrect) -- ^ C argument @"const sk_rrect_t * rrect"@
  -> Sk_rrect_corner -- ^ C argument @"sk_rrect_corner_t corner"@
  -> Ptr (Sk_vector) -- ^ C argument @"sk_vector_t * radii"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_rrect_get_radii'
foreign import ccall "&sk_rrect_get_radii" p'sk_rrect_get_radii ::
  FunPtr (Ptr (Sk_rrect) -> Sk_rrect_corner -> Ptr (Sk_vector) -> IO (()))

{- | C function signature:

@
float sk_rrect_get_width(const sk_rrect_t *rrect)
@
-}
foreign import ccall "sk_rrect_get_width" sk_rrect_get_width ::
  Ptr (Sk_rrect) -- ^ C argument @"const sk_rrect_t * rrect"@
  -> IO (CFloat) -- ^ C return type: @"float"@

-- | Function pointer to 'sk_rrect_get_width'
foreign import ccall "&sk_rrect_get_width" p'sk_rrect_get_width ::
  FunPtr (Ptr (Sk_rrect) -> IO (CFloat))

{- | C function signature:

@
float sk_rrect_get_height(const sk_rrect_t *rrect)
@
-}
foreign import ccall "sk_rrect_get_height" sk_rrect_get_height ::
  Ptr (Sk_rrect) -- ^ C argument @"const sk_rrect_t * rrect"@
  -> IO (CFloat) -- ^ C return type: @"float"@

-- | Function pointer to 'sk_rrect_get_height'
foreign import ccall "&sk_rrect_get_height" p'sk_rrect_get_height ::
  FunPtr (Ptr (Sk_rrect) -> IO (CFloat))

{- | C function signature:

@
void sk_rrect_set_empty(sk_rrect_t *rrect)
@
-}
foreign import ccall "sk_rrect_set_empty" sk_rrect_set_empty ::
  Ptr (Sk_rrect) -- ^ C argument @"sk_rrect_t * rrect"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_rrect_set_empty'
foreign import ccall "&sk_rrect_set_empty" p'sk_rrect_set_empty ::
  FunPtr (Ptr (Sk_rrect) -> IO (()))

{- | C function signature:

@
void sk_rrect_set_rect(sk_rrect_t *rrect, const sk_rect_t *rect)
@
-}
foreign import ccall "sk_rrect_set_rect" sk_rrect_set_rect ::
  Ptr (Sk_rrect) -- ^ C argument @"sk_rrect_t * rrect"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * rect"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_rrect_set_rect'
foreign import ccall "&sk_rrect_set_rect" p'sk_rrect_set_rect ::
  FunPtr (Ptr (Sk_rrect) -> Ptr (Sk_rect) -> IO (()))

{- | C function signature:

@
void sk_rrect_set_oval(sk_rrect_t *rrect, const sk_rect_t *rect)
@
-}
foreign import ccall "sk_rrect_set_oval" sk_rrect_set_oval ::
  Ptr (Sk_rrect) -- ^ C argument @"sk_rrect_t * rrect"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * rect"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_rrect_set_oval'
foreign import ccall "&sk_rrect_set_oval" p'sk_rrect_set_oval ::
  FunPtr (Ptr (Sk_rrect) -> Ptr (Sk_rect) -> IO (()))

{- | C function signature:

@
void sk_rrect_set_rect_xy(sk_rrect_t *rrect, const sk_rect_t *rect, float xRad, float yRad)
@
-}
foreign import ccall "sk_rrect_set_rect_xy" sk_rrect_set_rect_xy ::
  Ptr (Sk_rrect) -- ^ C argument @"sk_rrect_t * rrect"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * rect"@
  -> CFloat -- ^ C argument @"float xRad"@
  -> CFloat -- ^ C argument @"float yRad"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_rrect_set_rect_xy'
foreign import ccall "&sk_rrect_set_rect_xy" p'sk_rrect_set_rect_xy ::
  FunPtr (Ptr (Sk_rrect) -> Ptr (Sk_rect) -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_rrect_set_nine_patch(sk_rrect_t *rrect, const sk_rect_t *rect, float leftRad, float topRad, float rightRad, float bottomRad)
@
-}
foreign import ccall "sk_rrect_set_nine_patch" sk_rrect_set_nine_patch ::
  Ptr (Sk_rrect) -- ^ C argument @"sk_rrect_t * rrect"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * rect"@
  -> CFloat -- ^ C argument @"float leftRad"@
  -> CFloat -- ^ C argument @"float topRad"@
  -> CFloat -- ^ C argument @"float rightRad"@
  -> CFloat -- ^ C argument @"float bottomRad"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_rrect_set_nine_patch'
foreign import ccall "&sk_rrect_set_nine_patch" p'sk_rrect_set_nine_patch ::
  FunPtr (Ptr (Sk_rrect) -> Ptr (Sk_rect) -> CFloat -> CFloat -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_rrect_set_rect_radii(sk_rrect_t *rrect, const sk_rect_t *rect, const sk_vector_t *radii)
@
-}
foreign import ccall "sk_rrect_set_rect_radii" sk_rrect_set_rect_radii ::
  Ptr (Sk_rrect) -- ^ C argument @"sk_rrect_t * rrect"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * rect"@
  -> Ptr (Sk_vector) -- ^ C argument @"const sk_vector_t * radii"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_rrect_set_rect_radii'
foreign import ccall "&sk_rrect_set_rect_radii" p'sk_rrect_set_rect_radii ::
  FunPtr (Ptr (Sk_rrect) -> Ptr (Sk_rect) -> Ptr (Sk_vector) -> IO (()))

{- | C function signature:

@
void sk_rrect_inset(sk_rrect_t *rrect, float dx, float dy)
@
-}
foreign import ccall "sk_rrect_inset" sk_rrect_inset ::
  Ptr (Sk_rrect) -- ^ C argument @"sk_rrect_t * rrect"@
  -> CFloat -- ^ C argument @"float dx"@
  -> CFloat -- ^ C argument @"float dy"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_rrect_inset'
foreign import ccall "&sk_rrect_inset" p'sk_rrect_inset ::
  FunPtr (Ptr (Sk_rrect) -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_rrect_outset(sk_rrect_t *rrect, float dx, float dy)
@
-}
foreign import ccall "sk_rrect_outset" sk_rrect_outset ::
  Ptr (Sk_rrect) -- ^ C argument @"sk_rrect_t * rrect"@
  -> CFloat -- ^ C argument @"float dx"@
  -> CFloat -- ^ C argument @"float dy"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_rrect_outset'
foreign import ccall "&sk_rrect_outset" p'sk_rrect_outset ::
  FunPtr (Ptr (Sk_rrect) -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_rrect_offset(sk_rrect_t *rrect, float dx, float dy)
@
-}
foreign import ccall "sk_rrect_offset" sk_rrect_offset ::
  Ptr (Sk_rrect) -- ^ C argument @"sk_rrect_t * rrect"@
  -> CFloat -- ^ C argument @"float dx"@
  -> CFloat -- ^ C argument @"float dy"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_rrect_offset'
foreign import ccall "&sk_rrect_offset" p'sk_rrect_offset ::
  FunPtr (Ptr (Sk_rrect) -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
_Bool sk_rrect_contains(const sk_rrect_t *rrect, const sk_rect_t *rect)
@
-}
foreign import ccall "sk_rrect_contains" sk_rrect_contains ::
  Ptr (Sk_rrect) -- ^ C argument @"const sk_rrect_t * rrect"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * rect"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_rrect_contains'
foreign import ccall "&sk_rrect_contains" p'sk_rrect_contains ::
  FunPtr (Ptr (Sk_rrect) -> Ptr (Sk_rect) -> IO (CBool))

{- | C function signature:

@
_Bool sk_rrect_is_valid(const sk_rrect_t *rrect)
@
-}
foreign import ccall "sk_rrect_is_valid" sk_rrect_is_valid ::
  Ptr (Sk_rrect) -- ^ C argument @"const sk_rrect_t * rrect"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_rrect_is_valid'
foreign import ccall "&sk_rrect_is_valid" p'sk_rrect_is_valid ::
  FunPtr (Ptr (Sk_rrect) -> IO (CBool))

{- | C function signature:

@
_Bool sk_rrect_transform(sk_rrect_t *rrect, const sk_matrix_t *matrix, sk_rrect_t *dest)
@
-}
foreign import ccall "sk_rrect_transform" sk_rrect_transform ::
  Ptr (Sk_rrect) -- ^ C argument @"sk_rrect_t * rrect"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * matrix"@
  -> Ptr (Sk_rrect) -- ^ C argument @"sk_rrect_t * dest"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_rrect_transform'
foreign import ccall "&sk_rrect_transform" p'sk_rrect_transform ::
  FunPtr (Ptr (Sk_rrect) -> Ptr (Sk_matrix) -> Ptr (Sk_rrect) -> IO (CBool))
