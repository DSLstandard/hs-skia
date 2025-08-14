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
module Skia.Bindings.Sk_region where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
sk_region_t *sk_region_new(void)
@
-}
foreign import ccall "sk_region_new" sk_region_new ::
  IO (Ptr (Sk_region)) -- ^ C return type: @"sk_region_t *"@

-- | Function pointer to 'sk_region_new'
foreign import ccall "&sk_region_new" p'sk_region_new ::
  FunPtr (IO (Ptr (Sk_region)))

{- | C function signature:

@
void sk_region_delete(sk_region_t *r)
@
-}
foreign import ccall "sk_region_delete" sk_region_delete ::
  Ptr (Sk_region) -- ^ C argument @"sk_region_t * r"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_region_delete'
foreign import ccall "&sk_region_delete" p'sk_region_delete ::
  FunPtr (Ptr (Sk_region) -> IO (()))

{- | C function signature:

@
_Bool sk_region_is_empty(const sk_region_t *r)
@
-}
foreign import ccall "sk_region_is_empty" sk_region_is_empty ::
  Ptr (Sk_region) -- ^ C argument @"const sk_region_t * r"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_is_empty'
foreign import ccall "&sk_region_is_empty" p'sk_region_is_empty ::
  FunPtr (Ptr (Sk_region) -> IO (CBool))

{- | C function signature:

@
_Bool sk_region_is_rect(const sk_region_t *r)
@
-}
foreign import ccall "sk_region_is_rect" sk_region_is_rect ::
  Ptr (Sk_region) -- ^ C argument @"const sk_region_t * r"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_is_rect'
foreign import ccall "&sk_region_is_rect" p'sk_region_is_rect ::
  FunPtr (Ptr (Sk_region) -> IO (CBool))

{- | C function signature:

@
_Bool sk_region_is_complex(const sk_region_t *r)
@
-}
foreign import ccall "sk_region_is_complex" sk_region_is_complex ::
  Ptr (Sk_region) -- ^ C argument @"const sk_region_t * r"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_is_complex'
foreign import ccall "&sk_region_is_complex" p'sk_region_is_complex ::
  FunPtr (Ptr (Sk_region) -> IO (CBool))

{- | C function signature:

@
void sk_region_get_bounds(const sk_region_t *r, sk_irect_t *rect)
@
-}
foreign import ccall "sk_region_get_bounds" sk_region_get_bounds ::
  Ptr (Sk_region) -- ^ C argument @"const sk_region_t * r"@
  -> Ptr (Sk_irect) -- ^ C argument @"sk_irect_t * rect"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_region_get_bounds'
foreign import ccall "&sk_region_get_bounds" p'sk_region_get_bounds ::
  FunPtr (Ptr (Sk_region) -> Ptr (Sk_irect) -> IO (()))

{- | C function signature:

@
_Bool sk_region_get_boundary_path(const sk_region_t *r, sk_path_t *path)
@
-}
foreign import ccall "sk_region_get_boundary_path" sk_region_get_boundary_path ::
  Ptr (Sk_region) -- ^ C argument @"const sk_region_t * r"@
  -> Ptr (Sk_path) -- ^ C argument @"sk_path_t * path"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_get_boundary_path'
foreign import ccall "&sk_region_get_boundary_path" p'sk_region_get_boundary_path ::
  FunPtr (Ptr (Sk_region) -> Ptr (Sk_path) -> IO (CBool))

{- | C function signature:

@
_Bool sk_region_set_empty(sk_region_t *r)
@
-}
foreign import ccall "sk_region_set_empty" sk_region_set_empty ::
  Ptr (Sk_region) -- ^ C argument @"sk_region_t * r"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_set_empty'
foreign import ccall "&sk_region_set_empty" p'sk_region_set_empty ::
  FunPtr (Ptr (Sk_region) -> IO (CBool))

{- | C function signature:

@
_Bool sk_region_set_rect(sk_region_t *r, const sk_irect_t *rect)
@
-}
foreign import ccall "sk_region_set_rect" sk_region_set_rect ::
  Ptr (Sk_region) -- ^ C argument @"sk_region_t * r"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * rect"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_set_rect'
foreign import ccall "&sk_region_set_rect" p'sk_region_set_rect ::
  FunPtr (Ptr (Sk_region) -> Ptr (Sk_irect) -> IO (CBool))

{- | C function signature:

@
_Bool sk_region_set_rects(sk_region_t *r, const sk_irect_t *rects, int count)
@
-}
foreign import ccall "sk_region_set_rects" sk_region_set_rects ::
  Ptr (Sk_region) -- ^ C argument @"sk_region_t * r"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * rects"@
  -> CInt -- ^ C argument @"int count"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_set_rects'
foreign import ccall "&sk_region_set_rects" p'sk_region_set_rects ::
  FunPtr (Ptr (Sk_region) -> Ptr (Sk_irect) -> CInt -> IO (CBool))

{- | C function signature:

@
_Bool sk_region_set_region(sk_region_t *r, const sk_region_t *region)
@
-}
foreign import ccall "sk_region_set_region" sk_region_set_region ::
  Ptr (Sk_region) -- ^ C argument @"sk_region_t * r"@
  -> Ptr (Sk_region) -- ^ C argument @"const sk_region_t * region"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_set_region'
foreign import ccall "&sk_region_set_region" p'sk_region_set_region ::
  FunPtr (Ptr (Sk_region) -> Ptr (Sk_region) -> IO (CBool))

{- | C function signature:

@
_Bool sk_region_set_path(sk_region_t *r, const sk_path_t *t, const sk_region_t *clip)
@
-}
foreign import ccall "sk_region_set_path" sk_region_set_path ::
  Ptr (Sk_region) -- ^ C argument @"sk_region_t * r"@
  -> Ptr (Sk_path) -- ^ C argument @"const sk_path_t * t"@
  -> Ptr (Sk_region) -- ^ C argument @"const sk_region_t * clip"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_set_path'
foreign import ccall "&sk_region_set_path" p'sk_region_set_path ::
  FunPtr (Ptr (Sk_region) -> Ptr (Sk_path) -> Ptr (Sk_region) -> IO (CBool))

{- | C function signature:

@
_Bool sk_region_intersects_rect(const sk_region_t *r, const sk_irect_t *rect)
@
-}
foreign import ccall "sk_region_intersects_rect" sk_region_intersects_rect ::
  Ptr (Sk_region) -- ^ C argument @"const sk_region_t * r"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * rect"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_intersects_rect'
foreign import ccall "&sk_region_intersects_rect" p'sk_region_intersects_rect ::
  FunPtr (Ptr (Sk_region) -> Ptr (Sk_irect) -> IO (CBool))

{- | C function signature:

@
_Bool sk_region_intersects(const sk_region_t *r, const sk_region_t *src)
@
-}
foreign import ccall "sk_region_intersects" sk_region_intersects ::
  Ptr (Sk_region) -- ^ C argument @"const sk_region_t * r"@
  -> Ptr (Sk_region) -- ^ C argument @"const sk_region_t * src"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_intersects'
foreign import ccall "&sk_region_intersects" p'sk_region_intersects ::
  FunPtr (Ptr (Sk_region) -> Ptr (Sk_region) -> IO (CBool))

{- | C function signature:

@
_Bool sk_region_contains_point(const sk_region_t *r, int x, int y)
@
-}
foreign import ccall "sk_region_contains_point" sk_region_contains_point ::
  Ptr (Sk_region) -- ^ C argument @"const sk_region_t * r"@
  -> CInt -- ^ C argument @"int x"@
  -> CInt -- ^ C argument @"int y"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_contains_point'
foreign import ccall "&sk_region_contains_point" p'sk_region_contains_point ::
  FunPtr (Ptr (Sk_region) -> CInt -> CInt -> IO (CBool))

{- | C function signature:

@
_Bool sk_region_contains_rect(const sk_region_t *r, const sk_irect_t *rect)
@
-}
foreign import ccall "sk_region_contains_rect" sk_region_contains_rect ::
  Ptr (Sk_region) -- ^ C argument @"const sk_region_t * r"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * rect"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_contains_rect'
foreign import ccall "&sk_region_contains_rect" p'sk_region_contains_rect ::
  FunPtr (Ptr (Sk_region) -> Ptr (Sk_irect) -> IO (CBool))

{- | C function signature:

@
_Bool sk_region_contains(const sk_region_t *r, const sk_region_t *region)
@
-}
foreign import ccall "sk_region_contains" sk_region_contains ::
  Ptr (Sk_region) -- ^ C argument @"const sk_region_t * r"@
  -> Ptr (Sk_region) -- ^ C argument @"const sk_region_t * region"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_contains'
foreign import ccall "&sk_region_contains" p'sk_region_contains ::
  FunPtr (Ptr (Sk_region) -> Ptr (Sk_region) -> IO (CBool))

{- | C function signature:

@
_Bool sk_region_quick_contains(const sk_region_t *r, const sk_irect_t *rect)
@
-}
foreign import ccall "sk_region_quick_contains" sk_region_quick_contains ::
  Ptr (Sk_region) -- ^ C argument @"const sk_region_t * r"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * rect"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_quick_contains'
foreign import ccall "&sk_region_quick_contains" p'sk_region_quick_contains ::
  FunPtr (Ptr (Sk_region) -> Ptr (Sk_irect) -> IO (CBool))

{- | C function signature:

@
_Bool sk_region_quick_reject_rect(const sk_region_t *r, const sk_irect_t *rect)
@
-}
foreign import ccall "sk_region_quick_reject_rect" sk_region_quick_reject_rect ::
  Ptr (Sk_region) -- ^ C argument @"const sk_region_t * r"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * rect"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_quick_reject_rect'
foreign import ccall "&sk_region_quick_reject_rect" p'sk_region_quick_reject_rect ::
  FunPtr (Ptr (Sk_region) -> Ptr (Sk_irect) -> IO (CBool))

{- | C function signature:

@
_Bool sk_region_quick_reject(const sk_region_t *r, const sk_region_t *region)
@
-}
foreign import ccall "sk_region_quick_reject" sk_region_quick_reject ::
  Ptr (Sk_region) -- ^ C argument @"const sk_region_t * r"@
  -> Ptr (Sk_region) -- ^ C argument @"const sk_region_t * region"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_quick_reject'
foreign import ccall "&sk_region_quick_reject" p'sk_region_quick_reject ::
  FunPtr (Ptr (Sk_region) -> Ptr (Sk_region) -> IO (CBool))

{- | C function signature:

@
void sk_region_translate(sk_region_t *r, int x, int y)
@
-}
foreign import ccall "sk_region_translate" sk_region_translate ::
  Ptr (Sk_region) -- ^ C argument @"sk_region_t * r"@
  -> CInt -- ^ C argument @"int x"@
  -> CInt -- ^ C argument @"int y"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_region_translate'
foreign import ccall "&sk_region_translate" p'sk_region_translate ::
  FunPtr (Ptr (Sk_region) -> CInt -> CInt -> IO (()))

{- | C function signature:

@
_Bool sk_region_op_rect(sk_region_t *r, const sk_irect_t *rect, sk_region_op_t op)
@
-}
foreign import ccall "sk_region_op_rect" sk_region_op_rect ::
  Ptr (Sk_region) -- ^ C argument @"sk_region_t * r"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * rect"@
  -> Sk_region_op -- ^ C argument @"sk_region_op_t op"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_op_rect'
foreign import ccall "&sk_region_op_rect" p'sk_region_op_rect ::
  FunPtr (Ptr (Sk_region) -> Ptr (Sk_irect) -> Sk_region_op -> IO (CBool))

{- | C function signature:

@
_Bool sk_region_op(sk_region_t *r, const sk_region_t *region, sk_region_op_t op)
@
-}
foreign import ccall "sk_region_op" sk_region_op ::
  Ptr (Sk_region) -- ^ C argument @"sk_region_t * r"@
  -> Ptr (Sk_region) -- ^ C argument @"const sk_region_t * region"@
  -> Sk_region_op -- ^ C argument @"sk_region_op_t op"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_op'
foreign import ccall "&sk_region_op" p'sk_region_op ::
  FunPtr (Ptr (Sk_region) -> Ptr (Sk_region) -> Sk_region_op -> IO (CBool))

{- | C function signature:

@
sk_region_iterator_t *sk_region_iterator_new(const sk_region_t *region)
@
-}
foreign import ccall "sk_region_iterator_new" sk_region_iterator_new ::
  Ptr (Sk_region) -- ^ C argument @"const sk_region_t * region"@
  -> IO (Ptr (Sk_region_iterator)) -- ^ C return type: @"sk_region_iterator_t *"@

-- | Function pointer to 'sk_region_iterator_new'
foreign import ccall "&sk_region_iterator_new" p'sk_region_iterator_new ::
  FunPtr (Ptr (Sk_region) -> IO (Ptr (Sk_region_iterator)))

{- | C function signature:

@
void sk_region_iterator_delete(sk_region_iterator_t *iter)
@
-}
foreign import ccall "sk_region_iterator_delete" sk_region_iterator_delete ::
  Ptr (Sk_region_iterator) -- ^ C argument @"sk_region_iterator_t * iter"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_region_iterator_delete'
foreign import ccall "&sk_region_iterator_delete" p'sk_region_iterator_delete ::
  FunPtr (Ptr (Sk_region_iterator) -> IO (()))

{- | C function signature:

@
_Bool sk_region_iterator_rewind(sk_region_iterator_t *iter)
@
-}
foreign import ccall "sk_region_iterator_rewind" sk_region_iterator_rewind ::
  Ptr (Sk_region_iterator) -- ^ C argument @"sk_region_iterator_t * iter"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_iterator_rewind'
foreign import ccall "&sk_region_iterator_rewind" p'sk_region_iterator_rewind ::
  FunPtr (Ptr (Sk_region_iterator) -> IO (CBool))

{- | C function signature:

@
_Bool sk_region_iterator_done(const sk_region_iterator_t *iter)
@
-}
foreign import ccall "sk_region_iterator_done" sk_region_iterator_done ::
  Ptr (Sk_region_iterator) -- ^ C argument @"const sk_region_iterator_t * iter"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_iterator_done'
foreign import ccall "&sk_region_iterator_done" p'sk_region_iterator_done ::
  FunPtr (Ptr (Sk_region_iterator) -> IO (CBool))

{- | C function signature:

@
void sk_region_iterator_next(sk_region_iterator_t *iter)
@
-}
foreign import ccall "sk_region_iterator_next" sk_region_iterator_next ::
  Ptr (Sk_region_iterator) -- ^ C argument @"sk_region_iterator_t * iter"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_region_iterator_next'
foreign import ccall "&sk_region_iterator_next" p'sk_region_iterator_next ::
  FunPtr (Ptr (Sk_region_iterator) -> IO (()))

{- | C function signature:

@
void sk_region_iterator_rect(const sk_region_iterator_t *iter, sk_irect_t *rect)
@
-}
foreign import ccall "sk_region_iterator_rect" sk_region_iterator_rect ::
  Ptr (Sk_region_iterator) -- ^ C argument @"const sk_region_iterator_t * iter"@
  -> Ptr (Sk_irect) -- ^ C argument @"sk_irect_t * rect"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_region_iterator_rect'
foreign import ccall "&sk_region_iterator_rect" p'sk_region_iterator_rect ::
  FunPtr (Ptr (Sk_region_iterator) -> Ptr (Sk_irect) -> IO (()))

{- | C function signature:

@
sk_region_cliperator_t *sk_region_cliperator_new(const sk_region_t *region, const sk_irect_t *clip)
@
-}
foreign import ccall "sk_region_cliperator_new" sk_region_cliperator_new ::
  Ptr (Sk_region) -- ^ C argument @"const sk_region_t * region"@
  -> Ptr (Sk_irect) -- ^ C argument @"const sk_irect_t * clip"@
  -> IO (Ptr (Sk_region_cliperator)) -- ^ C return type: @"sk_region_cliperator_t *"@

-- | Function pointer to 'sk_region_cliperator_new'
foreign import ccall "&sk_region_cliperator_new" p'sk_region_cliperator_new ::
  FunPtr (Ptr (Sk_region) -> Ptr (Sk_irect) -> IO (Ptr (Sk_region_cliperator)))

{- | C function signature:

@
void sk_region_cliperator_delete(sk_region_cliperator_t *iter)
@
-}
foreign import ccall "sk_region_cliperator_delete" sk_region_cliperator_delete ::
  Ptr (Sk_region_cliperator) -- ^ C argument @"sk_region_cliperator_t * iter"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_region_cliperator_delete'
foreign import ccall "&sk_region_cliperator_delete" p'sk_region_cliperator_delete ::
  FunPtr (Ptr (Sk_region_cliperator) -> IO (()))

{- | C function signature:

@
_Bool sk_region_cliperator_done(sk_region_cliperator_t *iter)
@
-}
foreign import ccall "sk_region_cliperator_done" sk_region_cliperator_done ::
  Ptr (Sk_region_cliperator) -- ^ C argument @"sk_region_cliperator_t * iter"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_cliperator_done'
foreign import ccall "&sk_region_cliperator_done" p'sk_region_cliperator_done ::
  FunPtr (Ptr (Sk_region_cliperator) -> IO (CBool))

{- | C function signature:

@
void sk_region_cliperator_next(sk_region_cliperator_t *iter)
@
-}
foreign import ccall "sk_region_cliperator_next" sk_region_cliperator_next ::
  Ptr (Sk_region_cliperator) -- ^ C argument @"sk_region_cliperator_t * iter"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_region_cliperator_next'
foreign import ccall "&sk_region_cliperator_next" p'sk_region_cliperator_next ::
  FunPtr (Ptr (Sk_region_cliperator) -> IO (()))

{- | C function signature:

@
void sk_region_cliperator_rect(const sk_region_cliperator_t *iter, sk_irect_t *rect)
@
-}
foreign import ccall "sk_region_cliperator_rect" sk_region_cliperator_rect ::
  Ptr (Sk_region_cliperator) -- ^ C argument @"const sk_region_cliperator_t * iter"@
  -> Ptr (Sk_irect) -- ^ C argument @"sk_irect_t * rect"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_region_cliperator_rect'
foreign import ccall "&sk_region_cliperator_rect" p'sk_region_cliperator_rect ::
  FunPtr (Ptr (Sk_region_cliperator) -> Ptr (Sk_irect) -> IO (()))

{- | C function signature:

@
sk_region_spanerator_t *sk_region_spanerator_new(const sk_region_t *region, int y, int left, int right)
@
-}
foreign import ccall "sk_region_spanerator_new" sk_region_spanerator_new ::
  Ptr (Sk_region) -- ^ C argument @"const sk_region_t * region"@
  -> CInt -- ^ C argument @"int y"@
  -> CInt -- ^ C argument @"int left"@
  -> CInt -- ^ C argument @"int right"@
  -> IO (Ptr (Sk_region_spanerator)) -- ^ C return type: @"sk_region_spanerator_t *"@

-- | Function pointer to 'sk_region_spanerator_new'
foreign import ccall "&sk_region_spanerator_new" p'sk_region_spanerator_new ::
  FunPtr (Ptr (Sk_region) -> CInt -> CInt -> CInt -> IO (Ptr (Sk_region_spanerator)))

{- | C function signature:

@
void sk_region_spanerator_delete(sk_region_spanerator_t *iter)
@
-}
foreign import ccall "sk_region_spanerator_delete" sk_region_spanerator_delete ::
  Ptr (Sk_region_spanerator) -- ^ C argument @"sk_region_spanerator_t * iter"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_region_spanerator_delete'
foreign import ccall "&sk_region_spanerator_delete" p'sk_region_spanerator_delete ::
  FunPtr (Ptr (Sk_region_spanerator) -> IO (()))

{- | C function signature:

@
_Bool sk_region_spanerator_next(sk_region_spanerator_t *iter, int *left, int *right)
@
-}
foreign import ccall "sk_region_spanerator_next" sk_region_spanerator_next ::
  Ptr (Sk_region_spanerator) -- ^ C argument @"sk_region_spanerator_t * iter"@
  -> Ptr (CInt) -- ^ C argument @"int * left"@
  -> Ptr (CInt) -- ^ C argument @"int * right"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'sk_region_spanerator_next'
foreign import ccall "&sk_region_spanerator_next" p'sk_region_spanerator_next ::
  FunPtr (Ptr (Sk_region_spanerator) -> Ptr (CInt) -> Ptr (CInt) -> IO (CBool))
