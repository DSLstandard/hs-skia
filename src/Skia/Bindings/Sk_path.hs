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
module Skia.Bindings.Sk_path where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
sk_path_t *sk_path_new(void)
@
-}
foreign import ccall "sk_path_new" sk_path_new ::
  IO (Ptr (Sk_path)) -- ^ C return type: @"sk_path_t *"@

-- | Function pointer to 'sk_path_new'
foreign import ccall "&sk_path_new" p'sk_path_new ::
  FunPtr (IO (Ptr (Sk_path)))

{- | C function signature:

@
void sk_path_delete(sk_path_t *)
@
-}
foreign import ccall "sk_path_delete" sk_path_delete ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_delete'
foreign import ccall "&sk_path_delete" p'sk_path_delete ::
  FunPtr (Ptr (Sk_path) -> IO (()))

{- | C function signature:

@
void sk_path_move_to(sk_path_t *, float x, float y)
@
-}
foreign import ccall "sk_path_move_to" sk_path_move_to ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> CFloat -- ^ C argument @"float x"@
  -> CFloat -- ^ C argument @"float y"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_move_to'
foreign import ccall "&sk_path_move_to" p'sk_path_move_to ::
  FunPtr (Ptr (Sk_path) -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_path_line_to(sk_path_t *, float x, float y)
@
-}
foreign import ccall "sk_path_line_to" sk_path_line_to ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> CFloat -- ^ C argument @"float x"@
  -> CFloat -- ^ C argument @"float y"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_line_to'
foreign import ccall "&sk_path_line_to" p'sk_path_line_to ::
  FunPtr (Ptr (Sk_path) -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_path_quad_to(sk_path_t *, float x0, float y0, float x1, float y1)
@
-}
foreign import ccall "sk_path_quad_to" sk_path_quad_to ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> CFloat -- ^ C argument @"float x0"@
  -> CFloat -- ^ C argument @"float y0"@
  -> CFloat -- ^ C argument @"float x1"@
  -> CFloat -- ^ C argument @"float y1"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_quad_to'
foreign import ccall "&sk_path_quad_to" p'sk_path_quad_to ::
  FunPtr (Ptr (Sk_path) -> CFloat -> CFloat -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_path_conic_to(sk_path_t *, float x0, float y0, float x1, float y1, float w)
@
-}
foreign import ccall "sk_path_conic_to" sk_path_conic_to ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> CFloat -- ^ C argument @"float x0"@
  -> CFloat -- ^ C argument @"float y0"@
  -> CFloat -- ^ C argument @"float x1"@
  -> CFloat -- ^ C argument @"float y1"@
  -> CFloat -- ^ C argument @"float w"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_conic_to'
foreign import ccall "&sk_path_conic_to" p'sk_path_conic_to ::
  FunPtr (Ptr (Sk_path) -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_path_cubic_to(sk_path_t *, float x0, float y0, float x1, float y1, float x2, float y2)
@
-}
foreign import ccall "sk_path_cubic_to" sk_path_cubic_to ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> CFloat -- ^ C argument @"float x0"@
  -> CFloat -- ^ C argument @"float y0"@
  -> CFloat -- ^ C argument @"float x1"@
  -> CFloat -- ^ C argument @"float y1"@
  -> CFloat -- ^ C argument @"float x2"@
  -> CFloat -- ^ C argument @"float y2"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_cubic_to'
foreign import ccall "&sk_path_cubic_to" p'sk_path_cubic_to ::
  FunPtr (Ptr (Sk_path) -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_path_arc_to(sk_path_t *, float rx, float ry, float xAxisRotate, sk_path_arc_size_t largeArc, sk_path_direction_t sweep, float x, float y)
@
-}
foreign import ccall "sk_path_arc_to" sk_path_arc_to ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> CFloat -- ^ C argument @"float rx"@
  -> CFloat -- ^ C argument @"float ry"@
  -> CFloat -- ^ C argument @"float xAxisRotate"@
  -> Sk_path_arc_size -- ^ C argument @"sk_path_arc_size_t largeArc"@
  -> Sk_path_direction -- ^ C argument @"sk_path_direction_t sweep"@
  -> CFloat -- ^ C argument @"float x"@
  -> CFloat -- ^ C argument @"float y"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_arc_to'
foreign import ccall "&sk_path_arc_to" p'sk_path_arc_to ::
  FunPtr (Ptr (Sk_path) -> CFloat -> CFloat -> CFloat -> Sk_path_arc_size -> Sk_path_direction -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_path_rarc_to(sk_path_t *, float rx, float ry, float xAxisRotate, sk_path_arc_size_t largeArc, sk_path_direction_t sweep, float x, float y)
@
-}
foreign import ccall "sk_path_rarc_to" sk_path_rarc_to ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> CFloat -- ^ C argument @"float rx"@
  -> CFloat -- ^ C argument @"float ry"@
  -> CFloat -- ^ C argument @"float xAxisRotate"@
  -> Sk_path_arc_size -- ^ C argument @"sk_path_arc_size_t largeArc"@
  -> Sk_path_direction -- ^ C argument @"sk_path_direction_t sweep"@
  -> CFloat -- ^ C argument @"float x"@
  -> CFloat -- ^ C argument @"float y"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_rarc_to'
foreign import ccall "&sk_path_rarc_to" p'sk_path_rarc_to ::
  FunPtr (Ptr (Sk_path) -> CFloat -> CFloat -> CFloat -> Sk_path_arc_size -> Sk_path_direction -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_path_arc_to_with_oval(sk_path_t *, const sk_rect_t *oval, float startAngle, float sweepAngle, _Bool forceMoveTo)
@
-}
foreign import ccall "sk_path_arc_to_with_oval" sk_path_arc_to_with_oval ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * oval"@
  -> CFloat -- ^ C argument @"float startAngle"@
  -> CFloat -- ^ C argument @"float sweepAngle"@
  -> CBool -- ^ C argument @"_Bool forceMoveTo"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_arc_to_with_oval'
foreign import ccall "&sk_path_arc_to_with_oval" p'sk_path_arc_to_with_oval ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_rect) -> CFloat -> CFloat -> CBool -> IO (()))

{- | C function signature:

@
void sk_path_arc_to_with_points(sk_path_t *, float x1, float y1, float x2, float y2, float radius)
@
-}
foreign import ccall "sk_path_arc_to_with_points" sk_path_arc_to_with_points ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> CFloat -- ^ C argument @"float x1"@
  -> CFloat -- ^ C argument @"float y1"@
  -> CFloat -- ^ C argument @"float x2"@
  -> CFloat -- ^ C argument @"float y2"@
  -> CFloat -- ^ C argument @"float radius"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_arc_to_with_points'
foreign import ccall "&sk_path_arc_to_with_points" p'sk_path_arc_to_with_points ::
  FunPtr (Ptr (Sk_path) -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_path_close(sk_path_t *)
@
-}
foreign import ccall "sk_path_close" sk_path_close ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_close'
foreign import ccall "&sk_path_close" p'sk_path_close ::
  FunPtr (Ptr (Sk_path) -> IO (()))

{- | C function signature:

@
void sk_path_add_rect(sk_path_t *, const sk_rect_t *, sk_path_direction_t)
@
-}
foreign import ccall "sk_path_add_rect" sk_path_add_rect ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> Ptr (Sk_rect) -- ^ C argument type: @"const sk_rect_t *"@
  -> Sk_path_direction -- ^ C argument type: @"sk_path_direction_t"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_add_rect'
foreign import ccall "&sk_path_add_rect" p'sk_path_add_rect ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_rect) -> Sk_path_direction -> IO (()))

{- | C function signature:

@
void sk_path_add_rrect(sk_path_t *, const sk_rrect_t *, sk_path_direction_t)
@
-}
foreign import ccall "sk_path_add_rrect" sk_path_add_rrect ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> Ptr (Sk_rrect) -- ^ C argument type: @"const sk_rrect_t *"@
  -> Sk_path_direction -- ^ C argument type: @"sk_path_direction_t"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_add_rrect'
foreign import ccall "&sk_path_add_rrect" p'sk_path_add_rrect ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_rrect) -> Sk_path_direction -> IO (()))

{- | C function signature:

@
void sk_path_add_rrect_start(sk_path_t *, const sk_rrect_t *, sk_path_direction_t, uint32_t)
@
-}
foreign import ccall "sk_path_add_rrect_start" sk_path_add_rrect_start ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> Ptr (Sk_rrect) -- ^ C argument type: @"const sk_rrect_t *"@
  -> Sk_path_direction -- ^ C argument type: @"sk_path_direction_t"@
  -> Word32 -- ^ C argument type: @"uint32_t"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_add_rrect_start'
foreign import ccall "&sk_path_add_rrect_start" p'sk_path_add_rrect_start ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_rrect) -> Sk_path_direction -> Word32 -> IO (()))

{- | C function signature:

@
void sk_path_add_rounded_rect(sk_path_t *, const sk_rect_t *, float, float, sk_path_direction_t)
@
-}
foreign import ccall "sk_path_add_rounded_rect" sk_path_add_rounded_rect ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> Ptr (Sk_rect) -- ^ C argument type: @"const sk_rect_t *"@
  -> CFloat -- ^ C argument type: @"float"@
  -> CFloat -- ^ C argument type: @"float"@
  -> Sk_path_direction -- ^ C argument type: @"sk_path_direction_t"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_add_rounded_rect'
foreign import ccall "&sk_path_add_rounded_rect" p'sk_path_add_rounded_rect ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_rect) -> CFloat -> CFloat -> Sk_path_direction -> IO (()))

{- | C function signature:

@
void sk_path_add_oval(sk_path_t *, const sk_rect_t *, sk_path_direction_t)
@
-}
foreign import ccall "sk_path_add_oval" sk_path_add_oval ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> Ptr (Sk_rect) -- ^ C argument type: @"const sk_rect_t *"@
  -> Sk_path_direction -- ^ C argument type: @"sk_path_direction_t"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_add_oval'
foreign import ccall "&sk_path_add_oval" p'sk_path_add_oval ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_rect) -> Sk_path_direction -> IO (()))

{- | C function signature:

@
void sk_path_add_circle(sk_path_t *, float x, float y, float radius, sk_path_direction_t dir)
@
-}
foreign import ccall "sk_path_add_circle" sk_path_add_circle ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> CFloat -- ^ C argument @"float x"@
  -> CFloat -- ^ C argument @"float y"@
  -> CFloat -- ^ C argument @"float radius"@
  -> Sk_path_direction -- ^ C argument @"sk_path_direction_t dir"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_add_circle'
foreign import ccall "&sk_path_add_circle" p'sk_path_add_circle ::
  FunPtr (Ptr (Sk_path) -> CFloat -> CFloat -> CFloat -> Sk_path_direction -> IO (()))

{- | C function signature:

@
void sk_path_get_bounds(const sk_path_t *, sk_rect_t *)
@
-}
foreign import ccall "sk_path_get_bounds" sk_path_get_bounds ::
  Ptr (Sk_path) -- ^ C argument type: @"const sk_path_t *"@
  -> Ptr (Sk_rect) -- ^ C argument type: @"sk_rect_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_get_bounds'
foreign import ccall "&sk_path_get_bounds" p'sk_path_get_bounds ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_rect) -> IO (()))

{- | C function signature:

@
void sk_path_compute_tight_bounds(const sk_path_t *, sk_rect_t *)
@
-}
foreign import ccall "sk_path_compute_tight_bounds" sk_path_compute_tight_bounds ::
  Ptr (Sk_path) -- ^ C argument type: @"const sk_path_t *"@
  -> Ptr (Sk_rect) -- ^ C argument type: @"sk_rect_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_compute_tight_bounds'
foreign import ccall "&sk_path_compute_tight_bounds" p'sk_path_compute_tight_bounds ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_rect) -> IO (()))

{- | C function signature:

@
void sk_path_rmove_to(sk_path_t *, float dx, float dy)
@
-}
foreign import ccall "sk_path_rmove_to" sk_path_rmove_to ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> CFloat -- ^ C argument @"float dx"@
  -> CFloat -- ^ C argument @"float dy"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_rmove_to'
foreign import ccall "&sk_path_rmove_to" p'sk_path_rmove_to ::
  FunPtr (Ptr (Sk_path) -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_path_rline_to(sk_path_t *, float dx, float yd)
@
-}
foreign import ccall "sk_path_rline_to" sk_path_rline_to ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> CFloat -- ^ C argument @"float dx"@
  -> CFloat -- ^ C argument @"float yd"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_rline_to'
foreign import ccall "&sk_path_rline_to" p'sk_path_rline_to ::
  FunPtr (Ptr (Sk_path) -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_path_rquad_to(sk_path_t *, float dx0, float dy0, float dx1, float dy1)
@
-}
foreign import ccall "sk_path_rquad_to" sk_path_rquad_to ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> CFloat -- ^ C argument @"float dx0"@
  -> CFloat -- ^ C argument @"float dy0"@
  -> CFloat -- ^ C argument @"float dx1"@
  -> CFloat -- ^ C argument @"float dy1"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_rquad_to'
foreign import ccall "&sk_path_rquad_to" p'sk_path_rquad_to ::
  FunPtr (Ptr (Sk_path) -> CFloat -> CFloat -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_path_rconic_to(sk_path_t *, float dx0, float dy0, float dx1, float dy1, float w)
@
-}
foreign import ccall "sk_path_rconic_to" sk_path_rconic_to ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> CFloat -- ^ C argument @"float dx0"@
  -> CFloat -- ^ C argument @"float dy0"@
  -> CFloat -- ^ C argument @"float dx1"@
  -> CFloat -- ^ C argument @"float dy1"@
  -> CFloat -- ^ C argument @"float w"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_rconic_to'
foreign import ccall "&sk_path_rconic_to" p'sk_path_rconic_to ::
  FunPtr (Ptr (Sk_path) -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_path_rcubic_to(sk_path_t *, float dx0, float dy0, float dx1, float dy1, float dx2, float dy2)
@
-}
foreign import ccall "sk_path_rcubic_to" sk_path_rcubic_to ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> CFloat -- ^ C argument @"float dx0"@
  -> CFloat -- ^ C argument @"float dy0"@
  -> CFloat -- ^ C argument @"float dx1"@
  -> CFloat -- ^ C argument @"float dy1"@
  -> CFloat -- ^ C argument @"float dx2"@
  -> CFloat -- ^ C argument @"float dy2"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_rcubic_to'
foreign import ccall "&sk_path_rcubic_to" p'sk_path_rcubic_to ::
  FunPtr (Ptr (Sk_path) -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
void sk_path_add_rect_start(sk_path_t *cpath, const sk_rect_t *crect, sk_path_direction_t cdir, uint32_t startIndex)
@
-}
foreign import ccall "sk_path_add_rect_start" sk_path_add_rect_start ::
  Ptr (Sk_path) -- ^ C argument @"sk_path_t * cpath"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * crect"@
  -> Sk_path_direction -- ^ C argument @"sk_path_direction_t cdir"@
  -> Word32 -- ^ C argument @"uint32_t startIndex"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_add_rect_start'
foreign import ccall "&sk_path_add_rect_start" p'sk_path_add_rect_start ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_rect) -> Sk_path_direction -> Word32 -> IO (()))

{- | C function signature:

@
void sk_path_add_arc(sk_path_t *cpath, const sk_rect_t *crect, float startAngle, float sweepAngle)
@
-}
foreign import ccall "sk_path_add_arc" sk_path_add_arc ::
  Ptr (Sk_path) -- ^ C argument @"sk_path_t * cpath"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * crect"@
  -> CFloat -- ^ C argument @"float startAngle"@
  -> CFloat -- ^ C argument @"float sweepAngle"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_add_arc'
foreign import ccall "&sk_path_add_arc" p'sk_path_add_arc ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_rect) -> CFloat -> CFloat -> IO (()))

{- | C function signature:

@
sk_path_filltype_t sk_path_get_filltype(sk_path_t *)
@
-}
foreign import ccall "sk_path_get_filltype" sk_path_get_filltype ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> IO (Sk_path_filltype) -- ^ C return type: @"sk_path_filltype_t"@

-- | Function pointer to 'sk_path_get_filltype'
foreign import ccall "&sk_path_get_filltype" p'sk_path_get_filltype ::
  FunPtr (Ptr (Sk_path) -> IO (Sk_path_filltype))

{- | C function signature:

@
void sk_path_set_filltype(sk_path_t *, sk_path_filltype_t)
@
-}
foreign import ccall "sk_path_set_filltype" sk_path_set_filltype ::
  Ptr (Sk_path) -- ^ C argument type: @"sk_path_t *"@
  -> Sk_path_filltype -- ^ C argument type: @"sk_path_filltype_t"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_set_filltype'
foreign import ccall "&sk_path_set_filltype" p'sk_path_set_filltype ::
  FunPtr (Ptr (Sk_path) -> Sk_path_filltype -> IO (()))

{- | C function signature:

@
void sk_path_transform(sk_path_t *cpath, const sk_matrix_t *cmatrix)
@
-}
foreign import ccall "sk_path_transform" sk_path_transform ::
  Ptr (Sk_path) -- ^ C argument @"sk_path_t * cpath"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * cmatrix"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_transform'
foreign import ccall "&sk_path_transform" p'sk_path_transform ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_matrix) -> IO (()))

{- | C function signature:

@
void sk_path_transform_to_dest(const sk_path_t *cpath, const sk_matrix_t *cmatrix, sk_path_t *destination)
@
-}
foreign import ccall "sk_path_transform_to_dest" sk_path_transform_to_dest ::
  Ptr (Sk_path) -- ^ C argument @"const sk_path_t * cpath"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * cmatrix"@
  -> Ptr (Sk_path) -- ^ C argument @"sk_path_t * destination"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_transform_to_dest'
foreign import ccall "&sk_path_transform_to_dest" p'sk_path_transform_to_dest ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_matrix) -> Ptr (Sk_path) -> IO (()))

{- | C function signature:

@
sk_path_t *sk_path_clone(const sk_path_t *cpath)
@
-}
foreign import ccall "sk_path_clone" sk_path_clone ::
  Ptr (Sk_path) -- ^ C argument @"const sk_path_t * cpath"@
  -> IO (Ptr (Sk_path)) -- ^ C return type: @"sk_path_t *"@

-- | Function pointer to 'sk_path_clone'
foreign import ccall "&sk_path_clone" p'sk_path_clone ::
  FunPtr (Ptr (Sk_path) -> IO (Ptr (Sk_path)))

{- | C function signature:

@
void sk_path_add_path_offset(sk_path_t *cpath, sk_path_t *other, float dx, float dy, sk_path_add_mode_t add_mode)
@
-}
foreign import ccall "sk_path_add_path_offset" sk_path_add_path_offset ::
  Ptr (Sk_path) -- ^ C argument @"sk_path_t * cpath"@
  -> Ptr (Sk_path) -- ^ C argument @"sk_path_t * other"@
  -> CFloat -- ^ C argument @"float dx"@
  -> CFloat -- ^ C argument @"float dy"@
  -> Sk_path_add_mode -- ^ C argument @"sk_path_add_mode_t add_mode"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_add_path_offset'
foreign import ccall "&sk_path_add_path_offset" p'sk_path_add_path_offset ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_path) -> CFloat -> CFloat -> Sk_path_add_mode -> IO (()))

{- | C function signature:

@
void sk_path_add_path_matrix(sk_path_t *cpath, sk_path_t *other, sk_matrix_t *matrix, sk_path_add_mode_t add_mode)
@
-}
foreign import ccall "sk_path_add_path_matrix" sk_path_add_path_matrix ::
  Ptr (Sk_path) -- ^ C argument @"sk_path_t * cpath"@
  -> Ptr (Sk_path) -- ^ C argument @"sk_path_t * other"@
  -> Ptr (Sk_matrix) -- ^ C argument @"sk_matrix_t * matrix"@
  -> Sk_path_add_mode -- ^ C argument @"sk_path_add_mode_t add_mode"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_add_path_matrix'
foreign import ccall "&sk_path_add_path_matrix" p'sk_path_add_path_matrix ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_path) -> Ptr (Sk_matrix) -> Sk_path_add_mode -> IO (()))

{- | C function signature:

@
void sk_path_add_path(sk_path_t *cpath, sk_path_t *other, sk_path_add_mode_t add_mode)
@
-}
foreign import ccall "sk_path_add_path" sk_path_add_path ::
  Ptr (Sk_path) -- ^ C argument @"sk_path_t * cpath"@
  -> Ptr (Sk_path) -- ^ C argument @"sk_path_t * other"@
  -> Sk_path_add_mode -- ^ C argument @"sk_path_add_mode_t add_mode"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_add_path'
foreign import ccall "&sk_path_add_path" p'sk_path_add_path ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_path) -> Sk_path_add_mode -> IO (()))

{- | C function signature:

@
void sk_path_add_path_reverse(sk_path_t *cpath, sk_path_t *other)
@
-}
foreign import ccall "sk_path_add_path_reverse" sk_path_add_path_reverse ::
  Ptr (Sk_path) -- ^ C argument @"sk_path_t * cpath"@
  -> Ptr (Sk_path) -- ^ C argument @"sk_path_t * other"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_add_path_reverse'
foreign import ccall "&sk_path_add_path_reverse" p'sk_path_add_path_reverse ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_path) -> IO (()))

{- | C function signature:

@
void sk_path_reset(sk_path_t *cpath)
@
-}
foreign import ccall "sk_path_reset" sk_path_reset ::
  Ptr (Sk_path) -- ^ C argument @"sk_path_t * cpath"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_reset'
foreign import ccall "&sk_path_reset" p'sk_path_reset ::
  FunPtr (Ptr (Sk_path) -> IO (()))

{- | C function signature:

@
void sk_path_rewind(sk_path_t *cpath)
@
-}
foreign import ccall "sk_path_rewind" sk_path_rewind ::
  Ptr (Sk_path) -- ^ C argument @"sk_path_t * cpath"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_rewind'
foreign import ccall "&sk_path_rewind" p'sk_path_rewind ::
  FunPtr (Ptr (Sk_path) -> IO (()))

{- | C function signature:

@
int sk_path_count_points(const sk_path_t *cpath)
@
-}
foreign import ccall "sk_path_count_points" sk_path_count_points ::
  Ptr (Sk_path) -- ^ C argument @"const sk_path_t * cpath"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_path_count_points'
foreign import ccall "&sk_path_count_points" p'sk_path_count_points ::
  FunPtr (Ptr (Sk_path) -> IO (CInt))

{- | C function signature:

@
int sk_path_count_verbs(const sk_path_t *cpath)
@
-}
foreign import ccall "sk_path_count_verbs" sk_path_count_verbs ::
  Ptr (Sk_path) -- ^ C argument @"const sk_path_t * cpath"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_path_count_verbs'
foreign import ccall "&sk_path_count_verbs" p'sk_path_count_verbs ::
  FunPtr (Ptr (Sk_path) -> IO (CInt))

{- | C function signature:

@
void sk_path_get_point(const sk_path_t *cpath, int index, sk_point_t *point)
@
-}
foreign import ccall "sk_path_get_point" sk_path_get_point ::
  Ptr (Sk_path) -- ^ C argument @"const sk_path_t * cpath"@
  -> CInt -- ^ C argument @"int index"@
  -> Ptr (Sk_point) -- ^ C argument @"sk_point_t * point"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_get_point'
foreign import ccall "&sk_path_get_point" p'sk_path_get_point ::
  FunPtr (Ptr (Sk_path) -> CInt -> Ptr (Sk_point) -> IO (()))

{- | C function signature:

@
int sk_path_get_points(const sk_path_t *cpath, sk_point_t *points, int max)
@
-}
foreign import ccall "sk_path_get_points" sk_path_get_points ::
  Ptr (Sk_path) -- ^ C argument @"const sk_path_t * cpath"@
  -> Ptr (Sk_point) -- ^ C argument @"sk_point_t * points"@
  -> CInt -- ^ C argument @"int max"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_path_get_points'
foreign import ccall "&sk_path_get_points" p'sk_path_get_points ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_point) -> CInt -> IO (CInt))

{- | C function signature:

@
_Bool sk_path_contains(const sk_path_t *cpath, float x, float y)
@
-}
foreign import ccall "sk_path_contains" sk_path_contains ::
  Ptr (Sk_path) -- ^ C argument @"const sk_path_t * cpath"@
  -> CFloat -- ^ C argument @"float x"@
  -> CFloat -- ^ C argument @"float y"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_path_contains'
foreign import ccall "&sk_path_contains" p'sk_path_contains ::
  FunPtr (Ptr (Sk_path) -> CFloat -> CFloat -> IO (CBool))

{- | C function signature:

@
_Bool sk_path_parse_svg_string(sk_path_t *cpath, const char *str)
@
-}
foreign import ccall "sk_path_parse_svg_string" sk_path_parse_svg_string ::
  Ptr (Sk_path) -- ^ C argument @"sk_path_t * cpath"@
  -> Ptr (CChar) -- ^ C argument @"const char * str"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_path_parse_svg_string'
foreign import ccall "&sk_path_parse_svg_string" p'sk_path_parse_svg_string ::
  FunPtr (Ptr (Sk_path) -> Ptr (CChar) -> IO (CBool))

{- | C function signature:

@
void sk_path_to_svg_string(const sk_path_t *cpath, sk_string_t *str)
@
-}
foreign import ccall "sk_path_to_svg_string" sk_path_to_svg_string ::
  Ptr (Sk_path) -- ^ C argument @"const sk_path_t * cpath"@
  -> Ptr (Sk_string) -- ^ C argument @"sk_string_t * str"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_to_svg_string'
foreign import ccall "&sk_path_to_svg_string" p'sk_path_to_svg_string ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_string) -> IO (()))

{- | C function signature:

@
_Bool sk_path_get_last_point(const sk_path_t *cpath, sk_point_t *point)
@
-}
foreign import ccall "sk_path_get_last_point" sk_path_get_last_point ::
  Ptr (Sk_path) -- ^ C argument @"const sk_path_t * cpath"@
  -> Ptr (Sk_point) -- ^ C argument @"sk_point_t * point"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_path_get_last_point'
foreign import ccall "&sk_path_get_last_point" p'sk_path_get_last_point ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_point) -> IO (CBool))

{- | C function signature:

@
int sk_path_convert_conic_to_quads(const sk_point_t *p0, const sk_point_t *p1, const sk_point_t *p2, float w, sk_point_t *pts, int pow2)
@
-}
foreign import ccall "sk_path_convert_conic_to_quads" sk_path_convert_conic_to_quads ::
  Ptr (Sk_point) -- ^ C argument @"const sk_point_t * p0"@
  -> Ptr (Sk_point) -- ^ C argument @"const sk_point_t * p1"@
  -> Ptr (Sk_point) -- ^ C argument @"const sk_point_t * p2"@
  -> CFloat -- ^ C argument @"float w"@
  -> Ptr (Sk_point) -- ^ C argument @"sk_point_t * pts"@
  -> CInt -- ^ C argument @"int pow2"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_path_convert_conic_to_quads'
foreign import ccall "&sk_path_convert_conic_to_quads" p'sk_path_convert_conic_to_quads ::
  FunPtr (Ptr (Sk_point) -> Ptr (Sk_point) -> Ptr (Sk_point) -> CFloat -> Ptr (Sk_point) -> CInt -> IO (CInt))

{- | C function signature:

@
void sk_path_add_poly(sk_path_t *cpath, const sk_point_t *points, int count, _Bool close)
@
-}
foreign import ccall "sk_path_add_poly" sk_path_add_poly ::
  Ptr (Sk_path) -- ^ C argument @"sk_path_t * cpath"@
  -> Ptr (Sk_point) -- ^ C argument @"const sk_point_t * points"@
  -> CInt -- ^ C argument @"int count"@
  -> CBool -- ^ C argument @"_Bool close"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_add_poly'
foreign import ccall "&sk_path_add_poly" p'sk_path_add_poly ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_point) -> CInt -> CBool -> IO (()))

{- | C function signature:

@
uint32_t sk_path_get_segment_masks(sk_path_t *cpath)
@
-}
foreign import ccall "sk_path_get_segment_masks" sk_path_get_segment_masks ::
  Ptr (Sk_path) -- ^ C argument @"sk_path_t * cpath"@
  -> IO (Word32) -- ^ C return type: @"uint32_t"@

-- | Function pointer to 'sk_path_get_segment_masks'
foreign import ccall "&sk_path_get_segment_masks" p'sk_path_get_segment_masks ::
  FunPtr (Ptr (Sk_path) -> IO (Word32))

{- | C function signature:

@
_Bool sk_path_is_oval(sk_path_t *cpath, sk_rect_t *bounds)
@
-}
foreign import ccall "sk_path_is_oval" sk_path_is_oval ::
  Ptr (Sk_path) -- ^ C argument @"sk_path_t * cpath"@
  -> Ptr (Sk_rect) -- ^ C argument @"sk_rect_t * bounds"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_path_is_oval'
foreign import ccall "&sk_path_is_oval" p'sk_path_is_oval ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_rect) -> IO (CBool))

{- | C function signature:

@
_Bool sk_path_is_rrect(sk_path_t *cpath, sk_rrect_t *bounds)
@
-}
foreign import ccall "sk_path_is_rrect" sk_path_is_rrect ::
  Ptr (Sk_path) -- ^ C argument @"sk_path_t * cpath"@
  -> Ptr (Sk_rrect) -- ^ C argument @"sk_rrect_t * bounds"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_path_is_rrect'
foreign import ccall "&sk_path_is_rrect" p'sk_path_is_rrect ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_rrect) -> IO (CBool))

{- | C function signature:

@
_Bool sk_path_is_line(sk_path_t *cpath, sk_point_t line[2])
@
-}
foreign import ccall "sk_path_is_line" sk_path_is_line ::
  Ptr (Sk_path) -- ^ C argument @"sk_path_t * cpath"@
  -> Ptr (Sk_point) -- ^ C argument @"sk_point_t [2] line"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_path_is_line'
foreign import ccall "&sk_path_is_line" p'sk_path_is_line ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_point) -> IO (CBool))

{- | C function signature:

@
_Bool sk_path_is_rect(sk_path_t *cpath, sk_rect_t *rect, _Bool *isClosed, sk_path_direction_t *direction)
@
-}
foreign import ccall "sk_path_is_rect" sk_path_is_rect ::
  Ptr (Sk_path) -- ^ C argument @"sk_path_t * cpath"@
  -> Ptr (Sk_rect) -- ^ C argument @"sk_rect_t * rect"@
  -> Ptr (CBool) -- ^ C argument @"_Bool * isClosed"@
  -> Ptr (Sk_path_direction) -- ^ C argument @"sk_path_direction_t * direction"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_path_is_rect'
foreign import ccall "&sk_path_is_rect" p'sk_path_is_rect ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_rect) -> Ptr (CBool) -> Ptr (Sk_path_direction) -> IO (CBool))

{- | C function signature:

@
_Bool sk_path_is_convex(const sk_path_t *cpath)
@
-}
foreign import ccall "sk_path_is_convex" sk_path_is_convex ::
  Ptr (Sk_path) -- ^ C argument @"const sk_path_t * cpath"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_path_is_convex'
foreign import ccall "&sk_path_is_convex" p'sk_path_is_convex ::
  FunPtr (Ptr (Sk_path) -> IO (CBool))

{- | C function signature:

@
sk_path_iterator_t *sk_path_create_iter(sk_path_t *cpath, int forceClose)
@
-}
foreign import ccall "sk_path_create_iter" sk_path_create_iter ::
  Ptr (Sk_path) -- ^ C argument @"sk_path_t * cpath"@
  -> CInt -- ^ C argument @"int forceClose"@
  -> IO (Ptr (Sk_path_iterator)) -- ^ C return type: @"sk_path_iterator_t *"@

-- | Function pointer to 'sk_path_create_iter'
foreign import ccall "&sk_path_create_iter" p'sk_path_create_iter ::
  FunPtr (Ptr (Sk_path) -> CInt -> IO (Ptr (Sk_path_iterator)))

{- | C function signature:

@
sk_path_verb_t sk_path_iter_next(sk_path_iterator_t *iterator, sk_point_t points[4])
@
-}
foreign import ccall "sk_path_iter_next" sk_path_iter_next ::
  Ptr (Sk_path_iterator) -- ^ C argument @"sk_path_iterator_t * iterator"@
  -> Ptr (Sk_point) -- ^ C argument @"sk_point_t [4] points"@
  -> IO (Sk_path_verb) -- ^ C return type: @"sk_path_verb_t"@

-- | Function pointer to 'sk_path_iter_next'
foreign import ccall "&sk_path_iter_next" p'sk_path_iter_next ::
  FunPtr (Ptr (Sk_path_iterator) -> Ptr (Sk_point) -> IO (Sk_path_verb))

{- | C function signature:

@
float sk_path_iter_conic_weight(sk_path_iterator_t *iterator)
@
-}
foreign import ccall "sk_path_iter_conic_weight" sk_path_iter_conic_weight ::
  Ptr (Sk_path_iterator) -- ^ C argument @"sk_path_iterator_t * iterator"@
  -> IO (CFloat) -- ^ C return type: @"float"@

-- | Function pointer to 'sk_path_iter_conic_weight'
foreign import ccall "&sk_path_iter_conic_weight" p'sk_path_iter_conic_weight ::
  FunPtr (Ptr (Sk_path_iterator) -> IO (CFloat))

{- | C function signature:

@
int sk_path_iter_is_close_line(sk_path_iterator_t *iterator)
@
-}
foreign import ccall "sk_path_iter_is_close_line" sk_path_iter_is_close_line ::
  Ptr (Sk_path_iterator) -- ^ C argument @"sk_path_iterator_t * iterator"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_path_iter_is_close_line'
foreign import ccall "&sk_path_iter_is_close_line" p'sk_path_iter_is_close_line ::
  FunPtr (Ptr (Sk_path_iterator) -> IO (CInt))

{- | C function signature:

@
int sk_path_iter_is_closed_contour(sk_path_iterator_t *iterator)
@
-}
foreign import ccall "sk_path_iter_is_closed_contour" sk_path_iter_is_closed_contour ::
  Ptr (Sk_path_iterator) -- ^ C argument @"sk_path_iterator_t * iterator"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_path_iter_is_closed_contour'
foreign import ccall "&sk_path_iter_is_closed_contour" p'sk_path_iter_is_closed_contour ::
  FunPtr (Ptr (Sk_path_iterator) -> IO (CInt))

{- | C function signature:

@
void sk_path_iter_destroy(sk_path_iterator_t *iterator)
@
-}
foreign import ccall "sk_path_iter_destroy" sk_path_iter_destroy ::
  Ptr (Sk_path_iterator) -- ^ C argument @"sk_path_iterator_t * iterator"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_iter_destroy'
foreign import ccall "&sk_path_iter_destroy" p'sk_path_iter_destroy ::
  FunPtr (Ptr (Sk_path_iterator) -> IO (()))

{- | C function signature:

@
sk_path_rawiterator_t *sk_path_create_rawiter(sk_path_t *cpath)
@
-}
foreign import ccall "sk_path_create_rawiter" sk_path_create_rawiter ::
  Ptr (Sk_path) -- ^ C argument @"sk_path_t * cpath"@
  -> IO (Ptr (Sk_path_rawiterator)) -- ^ C return type: @"sk_path_rawiterator_t *"@

-- | Function pointer to 'sk_path_create_rawiter'
foreign import ccall "&sk_path_create_rawiter" p'sk_path_create_rawiter ::
  FunPtr (Ptr (Sk_path) -> IO (Ptr (Sk_path_rawiterator)))

{- | C function signature:

@
sk_path_verb_t sk_path_rawiter_peek(sk_path_rawiterator_t *iterator)
@
-}
foreign import ccall "sk_path_rawiter_peek" sk_path_rawiter_peek ::
  Ptr (Sk_path_rawiterator) -- ^ C argument @"sk_path_rawiterator_t * iterator"@
  -> IO (Sk_path_verb) -- ^ C return type: @"sk_path_verb_t"@

-- | Function pointer to 'sk_path_rawiter_peek'
foreign import ccall "&sk_path_rawiter_peek" p'sk_path_rawiter_peek ::
  FunPtr (Ptr (Sk_path_rawiterator) -> IO (Sk_path_verb))

{- | C function signature:

@
sk_path_verb_t sk_path_rawiter_next(sk_path_rawiterator_t *iterator, sk_point_t points[4])
@
-}
foreign import ccall "sk_path_rawiter_next" sk_path_rawiter_next ::
  Ptr (Sk_path_rawiterator) -- ^ C argument @"sk_path_rawiterator_t * iterator"@
  -> Ptr (Sk_point) -- ^ C argument @"sk_point_t [4] points"@
  -> IO (Sk_path_verb) -- ^ C return type: @"sk_path_verb_t"@

-- | Function pointer to 'sk_path_rawiter_next'
foreign import ccall "&sk_path_rawiter_next" p'sk_path_rawiter_next ::
  FunPtr (Ptr (Sk_path_rawiterator) -> Ptr (Sk_point) -> IO (Sk_path_verb))

{- | C function signature:

@
float sk_path_rawiter_conic_weight(sk_path_rawiterator_t *iterator)
@
-}
foreign import ccall "sk_path_rawiter_conic_weight" sk_path_rawiter_conic_weight ::
  Ptr (Sk_path_rawiterator) -- ^ C argument @"sk_path_rawiterator_t * iterator"@
  -> IO (CFloat) -- ^ C return type: @"float"@

-- | Function pointer to 'sk_path_rawiter_conic_weight'
foreign import ccall "&sk_path_rawiter_conic_weight" p'sk_path_rawiter_conic_weight ::
  FunPtr (Ptr (Sk_path_rawiterator) -> IO (CFloat))

{- | C function signature:

@
void sk_path_rawiter_destroy(sk_path_rawiterator_t *iterator)
@
-}
foreign import ccall "sk_path_rawiter_destroy" sk_path_rawiter_destroy ::
  Ptr (Sk_path_rawiterator) -- ^ C argument @"sk_path_rawiterator_t * iterator"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_path_rawiter_destroy'
foreign import ccall "&sk_path_rawiter_destroy" p'sk_path_rawiter_destroy ::
  FunPtr (Ptr (Sk_path_rawiterator) -> IO (()))

{- | C function signature:

@
_Bool sk_pathop_op(const sk_path_t *one, const sk_path_t *two, sk_pathop_t op, sk_path_t *result)
@
-}
foreign import ccall "sk_pathop_op" sk_pathop_op ::
  Ptr (Sk_path) -- ^ C argument @"const sk_path_t * one"@
  -> Ptr (Sk_path) -- ^ C argument @"const sk_path_t * two"@
  -> Sk_pathop -- ^ C argument @"sk_pathop_t op"@
  -> Ptr (Sk_path) -- ^ C argument @"sk_path_t * result"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_pathop_op'
foreign import ccall "&sk_pathop_op" p'sk_pathop_op ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_path) -> Sk_pathop -> Ptr (Sk_path) -> IO (CBool))

{- | C function signature:

@
_Bool sk_pathop_simplify(const sk_path_t *path, sk_path_t *result)
@
-}
foreign import ccall "sk_pathop_simplify" sk_pathop_simplify ::
  Ptr (Sk_path) -- ^ C argument @"const sk_path_t * path"@
  -> Ptr (Sk_path) -- ^ C argument @"sk_path_t * result"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_pathop_simplify'
foreign import ccall "&sk_pathop_simplify" p'sk_pathop_simplify ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_path) -> IO (CBool))

{- | C function signature:

@
_Bool sk_pathop_tight_bounds(const sk_path_t *path, sk_rect_t *result)
@
-}
foreign import ccall "sk_pathop_tight_bounds" sk_pathop_tight_bounds ::
  Ptr (Sk_path) -- ^ C argument @"const sk_path_t * path"@
  -> Ptr (Sk_rect) -- ^ C argument @"sk_rect_t * result"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_pathop_tight_bounds'
foreign import ccall "&sk_pathop_tight_bounds" p'sk_pathop_tight_bounds ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_rect) -> IO (CBool))

{- | C function signature:

@
_Bool sk_pathop_as_winding(const sk_path_t *path, sk_path_t *result)
@
-}
foreign import ccall "sk_pathop_as_winding" sk_pathop_as_winding ::
  Ptr (Sk_path) -- ^ C argument @"const sk_path_t * path"@
  -> Ptr (Sk_path) -- ^ C argument @"sk_path_t * result"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_pathop_as_winding'
foreign import ccall "&sk_pathop_as_winding" p'sk_pathop_as_winding ::
  FunPtr (Ptr (Sk_path) -> Ptr (Sk_path) -> IO (CBool))

{- | C function signature:

@
sk_opbuilder_t *sk_opbuilder_new(void)
@
-}
foreign import ccall "sk_opbuilder_new" sk_opbuilder_new ::
  IO (Ptr (Sk_opbuilder)) -- ^ C return type: @"sk_opbuilder_t *"@

-- | Function pointer to 'sk_opbuilder_new'
foreign import ccall "&sk_opbuilder_new" p'sk_opbuilder_new ::
  FunPtr (IO (Ptr (Sk_opbuilder)))

{- | C function signature:

@
void sk_opbuilder_destroy(sk_opbuilder_t *builder)
@
-}
foreign import ccall "sk_opbuilder_destroy" sk_opbuilder_destroy ::
  Ptr (Sk_opbuilder) -- ^ C argument @"sk_opbuilder_t * builder"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_opbuilder_destroy'
foreign import ccall "&sk_opbuilder_destroy" p'sk_opbuilder_destroy ::
  FunPtr (Ptr (Sk_opbuilder) -> IO (()))

{- | C function signature:

@
void sk_opbuilder_add(sk_opbuilder_t *builder, const sk_path_t *path, sk_pathop_t op)
@
-}
foreign import ccall "sk_opbuilder_add" sk_opbuilder_add ::
  Ptr (Sk_opbuilder) -- ^ C argument @"sk_opbuilder_t * builder"@
  -> Ptr (Sk_path) -- ^ C argument @"const sk_path_t * path"@
  -> Sk_pathop -- ^ C argument @"sk_pathop_t op"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_opbuilder_add'
foreign import ccall "&sk_opbuilder_add" p'sk_opbuilder_add ::
  FunPtr (Ptr (Sk_opbuilder) -> Ptr (Sk_path) -> Sk_pathop -> IO (()))

{- | C function signature:

@
_Bool sk_opbuilder_resolve(sk_opbuilder_t *builder, sk_path_t *result)
@
-}
foreign import ccall "sk_opbuilder_resolve" sk_opbuilder_resolve ::
  Ptr (Sk_opbuilder) -- ^ C argument @"sk_opbuilder_t * builder"@
  -> Ptr (Sk_path) -- ^ C argument @"sk_path_t * result"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_opbuilder_resolve'
foreign import ccall "&sk_opbuilder_resolve" p'sk_opbuilder_resolve ::
  FunPtr (Ptr (Sk_opbuilder) -> Ptr (Sk_path) -> IO (CBool))

{- | C function signature:

@
sk_pathmeasure_t *sk_pathmeasure_new(void)
@
-}
foreign import ccall "sk_pathmeasure_new" sk_pathmeasure_new ::
  IO (Ptr (Sk_pathmeasure)) -- ^ C return type: @"sk_pathmeasure_t *"@

-- | Function pointer to 'sk_pathmeasure_new'
foreign import ccall "&sk_pathmeasure_new" p'sk_pathmeasure_new ::
  FunPtr (IO (Ptr (Sk_pathmeasure)))

{- | C function signature:

@
sk_pathmeasure_t *sk_pathmeasure_new_with_path(const sk_path_t *path, _Bool forceClosed, float resScale)
@
-}
foreign import ccall "sk_pathmeasure_new_with_path" sk_pathmeasure_new_with_path ::
  Ptr (Sk_path) -- ^ C argument @"const sk_path_t * path"@
  -> CBool -- ^ C argument @"_Bool forceClosed"@
  -> CFloat -- ^ C argument @"float resScale"@
  -> IO (Ptr (Sk_pathmeasure)) -- ^ C return type: @"sk_pathmeasure_t *"@

-- | Function pointer to 'sk_pathmeasure_new_with_path'
foreign import ccall "&sk_pathmeasure_new_with_path" p'sk_pathmeasure_new_with_path ::
  FunPtr (Ptr (Sk_path) -> CBool -> CFloat -> IO (Ptr (Sk_pathmeasure)))

{- | C function signature:

@
void sk_pathmeasure_destroy(sk_pathmeasure_t *pathMeasure)
@
-}
foreign import ccall "sk_pathmeasure_destroy" sk_pathmeasure_destroy ::
  Ptr (Sk_pathmeasure) -- ^ C argument @"sk_pathmeasure_t * pathMeasure"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_pathmeasure_destroy'
foreign import ccall "&sk_pathmeasure_destroy" p'sk_pathmeasure_destroy ::
  FunPtr (Ptr (Sk_pathmeasure) -> IO (()))

{- | C function signature:

@
void sk_pathmeasure_set_path(sk_pathmeasure_t *pathMeasure, const sk_path_t *path, _Bool forceClosed)
@
-}
foreign import ccall "sk_pathmeasure_set_path" sk_pathmeasure_set_path ::
  Ptr (Sk_pathmeasure) -- ^ C argument @"sk_pathmeasure_t * pathMeasure"@
  -> Ptr (Sk_path) -- ^ C argument @"const sk_path_t * path"@
  -> CBool -- ^ C argument @"_Bool forceClosed"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_pathmeasure_set_path'
foreign import ccall "&sk_pathmeasure_set_path" p'sk_pathmeasure_set_path ::
  FunPtr (Ptr (Sk_pathmeasure) -> Ptr (Sk_path) -> CBool -> IO (()))

{- | C function signature:

@
float sk_pathmeasure_get_length(sk_pathmeasure_t *pathMeasure)
@
-}
foreign import ccall "sk_pathmeasure_get_length" sk_pathmeasure_get_length ::
  Ptr (Sk_pathmeasure) -- ^ C argument @"sk_pathmeasure_t * pathMeasure"@
  -> IO (CFloat) -- ^ C return type: @"float"@

-- | Function pointer to 'sk_pathmeasure_get_length'
foreign import ccall "&sk_pathmeasure_get_length" p'sk_pathmeasure_get_length ::
  FunPtr (Ptr (Sk_pathmeasure) -> IO (CFloat))

{- | C function signature:

@
_Bool sk_pathmeasure_get_pos_tan(sk_pathmeasure_t *pathMeasure, float distance, sk_point_t *position, sk_vector_t *tangent)
@
-}
foreign import ccall "sk_pathmeasure_get_pos_tan" sk_pathmeasure_get_pos_tan ::
  Ptr (Sk_pathmeasure) -- ^ C argument @"sk_pathmeasure_t * pathMeasure"@
  -> CFloat -- ^ C argument @"float distance"@
  -> Ptr (Sk_point) -- ^ C argument @"sk_point_t * position"@
  -> Ptr (Sk_vector) -- ^ C argument @"sk_vector_t * tangent"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_pathmeasure_get_pos_tan'
foreign import ccall "&sk_pathmeasure_get_pos_tan" p'sk_pathmeasure_get_pos_tan ::
  FunPtr (Ptr (Sk_pathmeasure) -> CFloat -> Ptr (Sk_point) -> Ptr (Sk_vector) -> IO (CBool))

{- | C function signature:

@
_Bool sk_pathmeasure_get_matrix(sk_pathmeasure_t *pathMeasure, float distance, sk_matrix_t *matrix, sk_pathmeasure_matrixflags_t flags)
@
-}
foreign import ccall "sk_pathmeasure_get_matrix" sk_pathmeasure_get_matrix ::
  Ptr (Sk_pathmeasure) -- ^ C argument @"sk_pathmeasure_t * pathMeasure"@
  -> CFloat -- ^ C argument @"float distance"@
  -> Ptr (Sk_matrix) -- ^ C argument @"sk_matrix_t * matrix"@
  -> Sk_pathmeasure_matrixflags -- ^ C argument @"sk_pathmeasure_matrixflags_t flags"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_pathmeasure_get_matrix'
foreign import ccall "&sk_pathmeasure_get_matrix" p'sk_pathmeasure_get_matrix ::
  FunPtr (Ptr (Sk_pathmeasure) -> CFloat -> Ptr (Sk_matrix) -> Sk_pathmeasure_matrixflags -> IO (CBool))

{- | C function signature:

@
_Bool sk_pathmeasure_get_segment(sk_pathmeasure_t *pathMeasure, float start, float stop, sk_path_t *dst, _Bool startWithMoveTo)
@
-}
foreign import ccall "sk_pathmeasure_get_segment" sk_pathmeasure_get_segment ::
  Ptr (Sk_pathmeasure) -- ^ C argument @"sk_pathmeasure_t * pathMeasure"@
  -> CFloat -- ^ C argument @"float start"@
  -> CFloat -- ^ C argument @"float stop"@
  -> Ptr (Sk_path) -- ^ C argument @"sk_path_t * dst"@
  -> CBool -- ^ C argument @"_Bool startWithMoveTo"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_pathmeasure_get_segment'
foreign import ccall "&sk_pathmeasure_get_segment" p'sk_pathmeasure_get_segment ::
  FunPtr (Ptr (Sk_pathmeasure) -> CFloat -> CFloat -> Ptr (Sk_path) -> CBool -> IO (CBool))

{- | C function signature:

@
_Bool sk_pathmeasure_is_closed(sk_pathmeasure_t *pathMeasure)
@
-}
foreign import ccall "sk_pathmeasure_is_closed" sk_pathmeasure_is_closed ::
  Ptr (Sk_pathmeasure) -- ^ C argument @"sk_pathmeasure_t * pathMeasure"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_pathmeasure_is_closed'
foreign import ccall "&sk_pathmeasure_is_closed" p'sk_pathmeasure_is_closed ::
  FunPtr (Ptr (Sk_pathmeasure) -> IO (CBool))

{- | C function signature:

@
_Bool sk_pathmeasure_next_contour(sk_pathmeasure_t *pathMeasure)
@
-}
foreign import ccall "sk_pathmeasure_next_contour" sk_pathmeasure_next_contour ::
  Ptr (Sk_pathmeasure) -- ^ C argument @"sk_pathmeasure_t * pathMeasure"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_pathmeasure_next_contour'
foreign import ccall "&sk_pathmeasure_next_contour" p'sk_pathmeasure_next_contour ::
  FunPtr (Ptr (Sk_pathmeasure) -> IO (CBool))
