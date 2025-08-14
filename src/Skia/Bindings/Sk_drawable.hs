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
module Skia.Bindings.Sk_drawable where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void sk_drawable_unref(sk_drawable_t *)
@
-}
foreign import ccall "sk_drawable_unref" sk_drawable_unref ::
  Ptr (Sk_drawable) -- ^ C argument type: @"sk_drawable_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_drawable_unref'
foreign import ccall "&sk_drawable_unref" p'sk_drawable_unref ::
  FunPtr (Ptr (Sk_drawable) -> IO (()))

{- | C function signature:

@
uint32_t sk_drawable_get_generation_id(sk_drawable_t *)
@
-}
foreign import ccall "sk_drawable_get_generation_id" sk_drawable_get_generation_id ::
  Ptr (Sk_drawable) -- ^ C argument type: @"sk_drawable_t *"@
  -> IO (Word32) -- ^ C return type: @"uint32_t"@

-- | Function pointer to 'sk_drawable_get_generation_id'
foreign import ccall "&sk_drawable_get_generation_id" p'sk_drawable_get_generation_id ::
  FunPtr (Ptr (Sk_drawable) -> IO (Word32))

{- | C function signature:

@
void sk_drawable_get_bounds(sk_drawable_t *, sk_rect_t *)
@
-}
foreign import ccall "sk_drawable_get_bounds" sk_drawable_get_bounds ::
  Ptr (Sk_drawable) -- ^ C argument type: @"sk_drawable_t *"@
  -> Ptr (Sk_rect) -- ^ C argument type: @"sk_rect_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_drawable_get_bounds'
foreign import ccall "&sk_drawable_get_bounds" p'sk_drawable_get_bounds ::
  FunPtr (Ptr (Sk_drawable) -> Ptr (Sk_rect) -> IO (()))

{- | C function signature:

@
void sk_drawable_draw(sk_drawable_t *, sk_canvas_t *, const sk_matrix_t *)
@
-}
foreign import ccall "sk_drawable_draw" sk_drawable_draw ::
  Ptr (Sk_drawable) -- ^ C argument type: @"sk_drawable_t *"@
  -> Ptr (Sk_canvas) -- ^ C argument type: @"sk_canvas_t *"@
  -> Ptr (Sk_matrix) -- ^ C argument type: @"const sk_matrix_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_drawable_draw'
foreign import ccall "&sk_drawable_draw" p'sk_drawable_draw ::
  FunPtr (Ptr (Sk_drawable) -> Ptr (Sk_canvas) -> Ptr (Sk_matrix) -> IO (()))

{- | C function signature:

@
sk_picture_t *sk_drawable_new_picture_snapshot(sk_drawable_t *)
@
-}
foreign import ccall "sk_drawable_new_picture_snapshot" sk_drawable_new_picture_snapshot ::
  Ptr (Sk_drawable) -- ^ C argument type: @"sk_drawable_t *"@
  -> IO (Ptr (Sk_picture)) -- ^ C return type: @"sk_picture_t *"@

-- | Function pointer to 'sk_drawable_new_picture_snapshot'
foreign import ccall "&sk_drawable_new_picture_snapshot" p'sk_drawable_new_picture_snapshot ::
  FunPtr (Ptr (Sk_drawable) -> IO (Ptr (Sk_picture)))

{- | C function signature:

@
void sk_drawable_notify_drawing_changed(sk_drawable_t *)
@
-}
foreign import ccall "sk_drawable_notify_drawing_changed" sk_drawable_notify_drawing_changed ::
  Ptr (Sk_drawable) -- ^ C argument type: @"sk_drawable_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_drawable_notify_drawing_changed'
foreign import ccall "&sk_drawable_notify_drawing_changed" p'sk_drawable_notify_drawing_changed ::
  FunPtr (Ptr (Sk_drawable) -> IO (()))

{- | C function signature:

@
size_t sk_drawable_approximate_bytes_used(sk_drawable_t *)
@
-}
foreign import ccall "sk_drawable_approximate_bytes_used" sk_drawable_approximate_bytes_used ::
  Ptr (Sk_drawable) -- ^ C argument type: @"sk_drawable_t *"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_drawable_approximate_bytes_used'
foreign import ccall "&sk_drawable_approximate_bytes_used" p'sk_drawable_approximate_bytes_used ::
  FunPtr (Ptr (Sk_drawable) -> IO (CSize))
