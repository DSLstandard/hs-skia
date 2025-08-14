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
module Skia.Bindings.Sk_textblob where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void sk_textblob_ref(const sk_textblob_t *blob)
@
-}
foreign import ccall "sk_textblob_ref" sk_textblob_ref ::
  Ptr (Sk_textblob) -- ^ C argument @"const sk_textblob_t * blob"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_textblob_ref'
foreign import ccall "&sk_textblob_ref" p'sk_textblob_ref ::
  FunPtr (Ptr (Sk_textblob) -> IO (()))

{- | C function signature:

@
void sk_textblob_unref(const sk_textblob_t *blob)
@
-}
foreign import ccall "sk_textblob_unref" sk_textblob_unref ::
  Ptr (Sk_textblob) -- ^ C argument @"const sk_textblob_t * blob"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_textblob_unref'
foreign import ccall "&sk_textblob_unref" p'sk_textblob_unref ::
  FunPtr (Ptr (Sk_textblob) -> IO (()))

{- | C function signature:

@
uint32_t sk_textblob_get_unique_id(const sk_textblob_t *blob)
@
-}
foreign import ccall "sk_textblob_get_unique_id" sk_textblob_get_unique_id ::
  Ptr (Sk_textblob) -- ^ C argument @"const sk_textblob_t * blob"@
  -> IO (Word32) -- ^ C return type: @"uint32_t"@

-- | Function pointer to 'sk_textblob_get_unique_id'
foreign import ccall "&sk_textblob_get_unique_id" p'sk_textblob_get_unique_id ::
  FunPtr (Ptr (Sk_textblob) -> IO (Word32))

{- | C function signature:

@
void sk_textblob_get_bounds(const sk_textblob_t *blob, sk_rect_t *bounds)
@
-}
foreign import ccall "sk_textblob_get_bounds" sk_textblob_get_bounds ::
  Ptr (Sk_textblob) -- ^ C argument @"const sk_textblob_t * blob"@
  -> Ptr (Sk_rect) -- ^ C argument @"sk_rect_t * bounds"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_textblob_get_bounds'
foreign import ccall "&sk_textblob_get_bounds" p'sk_textblob_get_bounds ::
  FunPtr (Ptr (Sk_textblob) -> Ptr (Sk_rect) -> IO (()))

{- | C function signature:

@
int sk_textblob_get_intercepts(const sk_textblob_t *blob, const float bounds[2], float intervals[], const sk_paint_t *paint)
@
-}
foreign import ccall "sk_textblob_get_intercepts" sk_textblob_get_intercepts ::
  Ptr (Sk_textblob) -- ^ C argument @"const sk_textblob_t * blob"@
  -> Ptr (CFloat) -- ^ C argument @"const float [2] bounds"@
  -> Ptr (CFloat) -- ^ C argument @"float [] intervals"@
  -> Ptr (Sk_paint) -- ^ C argument @"const sk_paint_t * paint"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_textblob_get_intercepts'
foreign import ccall "&sk_textblob_get_intercepts" p'sk_textblob_get_intercepts ::
  FunPtr (Ptr (Sk_textblob) -> Ptr (CFloat) -> Ptr (CFloat) -> Ptr (Sk_paint) -> IO (CInt))

{- | C function signature:

@
sk_textblob_builder_t *sk_textblob_builder_new(void)
@
-}
foreign import ccall "sk_textblob_builder_new" sk_textblob_builder_new ::
  IO (Ptr (Sk_textblob_builder)) -- ^ C return type: @"sk_textblob_builder_t *"@

-- | Function pointer to 'sk_textblob_builder_new'
foreign import ccall "&sk_textblob_builder_new" p'sk_textblob_builder_new ::
  FunPtr (IO (Ptr (Sk_textblob_builder)))

{- | C function signature:

@
void sk_textblob_builder_delete(sk_textblob_builder_t *builder)
@
-}
foreign import ccall "sk_textblob_builder_delete" sk_textblob_builder_delete ::
  Ptr (Sk_textblob_builder) -- ^ C argument @"sk_textblob_builder_t * builder"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_textblob_builder_delete'
foreign import ccall "&sk_textblob_builder_delete" p'sk_textblob_builder_delete ::
  FunPtr (Ptr (Sk_textblob_builder) -> IO (()))

{- | C function signature:

@
sk_textblob_t *sk_textblob_builder_make(sk_textblob_builder_t *builder)
@
-}
foreign import ccall "sk_textblob_builder_make" sk_textblob_builder_make ::
  Ptr (Sk_textblob_builder) -- ^ C argument @"sk_textblob_builder_t * builder"@
  -> IO (Ptr (Sk_textblob)) -- ^ C return type: @"sk_textblob_t *"@

-- | Function pointer to 'sk_textblob_builder_make'
foreign import ccall "&sk_textblob_builder_make" p'sk_textblob_builder_make ::
  FunPtr (Ptr (Sk_textblob_builder) -> IO (Ptr (Sk_textblob)))

{- | C function signature:

@
void sk_textblob_builder_alloc_run(sk_textblob_builder_t *builder, const sk_font_t *font, int count, float x, float y, const sk_rect_t *bounds, sk_textblob_builder_runbuffer_t *runbuffer)
@
-}
foreign import ccall "sk_textblob_builder_alloc_run" sk_textblob_builder_alloc_run ::
  Ptr (Sk_textblob_builder) -- ^ C argument @"sk_textblob_builder_t * builder"@
  -> Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> CInt -- ^ C argument @"int count"@
  -> CFloat -- ^ C argument @"float x"@
  -> CFloat -- ^ C argument @"float y"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * bounds"@
  -> Ptr (Sk_textblob_builder_runbuffer) -- ^ C argument @"sk_textblob_builder_runbuffer_t * runbuffer"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_textblob_builder_alloc_run'
foreign import ccall "&sk_textblob_builder_alloc_run" p'sk_textblob_builder_alloc_run ::
  FunPtr (Ptr (Sk_textblob_builder) -> Ptr (Sk_font) -> CInt -> CFloat -> CFloat -> Ptr (Sk_rect) -> Ptr (Sk_textblob_builder_runbuffer) -> IO (()))

{- | C function signature:

@
void sk_textblob_builder_alloc_run_pos_h(sk_textblob_builder_t *builder, const sk_font_t *font, int count, float y, const sk_rect_t *bounds, sk_textblob_builder_runbuffer_t *runbuffer)
@
-}
foreign import ccall "sk_textblob_builder_alloc_run_pos_h" sk_textblob_builder_alloc_run_pos_h ::
  Ptr (Sk_textblob_builder) -- ^ C argument @"sk_textblob_builder_t * builder"@
  -> Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> CInt -- ^ C argument @"int count"@
  -> CFloat -- ^ C argument @"float y"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * bounds"@
  -> Ptr (Sk_textblob_builder_runbuffer) -- ^ C argument @"sk_textblob_builder_runbuffer_t * runbuffer"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_textblob_builder_alloc_run_pos_h'
foreign import ccall "&sk_textblob_builder_alloc_run_pos_h" p'sk_textblob_builder_alloc_run_pos_h ::
  FunPtr (Ptr (Sk_textblob_builder) -> Ptr (Sk_font) -> CInt -> CFloat -> Ptr (Sk_rect) -> Ptr (Sk_textblob_builder_runbuffer) -> IO (()))

{- | C function signature:

@
void sk_textblob_builder_alloc_run_pos(sk_textblob_builder_t *builder, const sk_font_t *font, int count, const sk_rect_t *bounds, sk_textblob_builder_runbuffer_t *runbuffer)
@
-}
foreign import ccall "sk_textblob_builder_alloc_run_pos" sk_textblob_builder_alloc_run_pos ::
  Ptr (Sk_textblob_builder) -- ^ C argument @"sk_textblob_builder_t * builder"@
  -> Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> CInt -- ^ C argument @"int count"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * bounds"@
  -> Ptr (Sk_textblob_builder_runbuffer) -- ^ C argument @"sk_textblob_builder_runbuffer_t * runbuffer"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_textblob_builder_alloc_run_pos'
foreign import ccall "&sk_textblob_builder_alloc_run_pos" p'sk_textblob_builder_alloc_run_pos ::
  FunPtr (Ptr (Sk_textblob_builder) -> Ptr (Sk_font) -> CInt -> Ptr (Sk_rect) -> Ptr (Sk_textblob_builder_runbuffer) -> IO (()))

{- | C function signature:

@
void sk_textblob_builder_alloc_run_rsxform(sk_textblob_builder_t *builder, const sk_font_t *font, int count, sk_textblob_builder_runbuffer_t *runbuffer)
@
-}
foreign import ccall "sk_textblob_builder_alloc_run_rsxform" sk_textblob_builder_alloc_run_rsxform ::
  Ptr (Sk_textblob_builder) -- ^ C argument @"sk_textblob_builder_t * builder"@
  -> Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> CInt -- ^ C argument @"int count"@
  -> Ptr (Sk_textblob_builder_runbuffer) -- ^ C argument @"sk_textblob_builder_runbuffer_t * runbuffer"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_textblob_builder_alloc_run_rsxform'
foreign import ccall "&sk_textblob_builder_alloc_run_rsxform" p'sk_textblob_builder_alloc_run_rsxform ::
  FunPtr (Ptr (Sk_textblob_builder) -> Ptr (Sk_font) -> CInt -> Ptr (Sk_textblob_builder_runbuffer) -> IO (()))

{- | C function signature:

@
void sk_textblob_builder_alloc_run_text(sk_textblob_builder_t *builder, const sk_font_t *font, int count, float x, float y, int textByteCount, const sk_rect_t *bounds, sk_textblob_builder_runbuffer_t *runbuffer)
@
-}
foreign import ccall "sk_textblob_builder_alloc_run_text" sk_textblob_builder_alloc_run_text ::
  Ptr (Sk_textblob_builder) -- ^ C argument @"sk_textblob_builder_t * builder"@
  -> Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> CInt -- ^ C argument @"int count"@
  -> CFloat -- ^ C argument @"float x"@
  -> CFloat -- ^ C argument @"float y"@
  -> CInt -- ^ C argument @"int textByteCount"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * bounds"@
  -> Ptr (Sk_textblob_builder_runbuffer) -- ^ C argument @"sk_textblob_builder_runbuffer_t * runbuffer"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_textblob_builder_alloc_run_text'
foreign import ccall "&sk_textblob_builder_alloc_run_text" p'sk_textblob_builder_alloc_run_text ::
  FunPtr (Ptr (Sk_textblob_builder) -> Ptr (Sk_font) -> CInt -> CFloat -> CFloat -> CInt -> Ptr (Sk_rect) -> Ptr (Sk_textblob_builder_runbuffer) -> IO (()))

{- | C function signature:

@
void sk_textblob_builder_alloc_run_text_pos_h(sk_textblob_builder_t *builder, const sk_font_t *font, int count, float y, int textByteCount, const sk_rect_t *bounds, sk_textblob_builder_runbuffer_t *runbuffer)
@
-}
foreign import ccall "sk_textblob_builder_alloc_run_text_pos_h" sk_textblob_builder_alloc_run_text_pos_h ::
  Ptr (Sk_textblob_builder) -- ^ C argument @"sk_textblob_builder_t * builder"@
  -> Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> CInt -- ^ C argument @"int count"@
  -> CFloat -- ^ C argument @"float y"@
  -> CInt -- ^ C argument @"int textByteCount"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * bounds"@
  -> Ptr (Sk_textblob_builder_runbuffer) -- ^ C argument @"sk_textblob_builder_runbuffer_t * runbuffer"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_textblob_builder_alloc_run_text_pos_h'
foreign import ccall "&sk_textblob_builder_alloc_run_text_pos_h" p'sk_textblob_builder_alloc_run_text_pos_h ::
  FunPtr (Ptr (Sk_textblob_builder) -> Ptr (Sk_font) -> CInt -> CFloat -> CInt -> Ptr (Sk_rect) -> Ptr (Sk_textblob_builder_runbuffer) -> IO (()))

{- | C function signature:

@
void sk_textblob_builder_alloc_run_text_pos(sk_textblob_builder_t *builder, const sk_font_t *font, int count, int textByteCount, const sk_rect_t *bounds, sk_textblob_builder_runbuffer_t *runbuffer)
@
-}
foreign import ccall "sk_textblob_builder_alloc_run_text_pos" sk_textblob_builder_alloc_run_text_pos ::
  Ptr (Sk_textblob_builder) -- ^ C argument @"sk_textblob_builder_t * builder"@
  -> Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> CInt -- ^ C argument @"int count"@
  -> CInt -- ^ C argument @"int textByteCount"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * bounds"@
  -> Ptr (Sk_textblob_builder_runbuffer) -- ^ C argument @"sk_textblob_builder_runbuffer_t * runbuffer"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_textblob_builder_alloc_run_text_pos'
foreign import ccall "&sk_textblob_builder_alloc_run_text_pos" p'sk_textblob_builder_alloc_run_text_pos ::
  FunPtr (Ptr (Sk_textblob_builder) -> Ptr (Sk_font) -> CInt -> CInt -> Ptr (Sk_rect) -> Ptr (Sk_textblob_builder_runbuffer) -> IO (()))

{- | C function signature:

@
void sk_textblob_builder_alloc_run_text_rsxform(sk_textblob_builder_t *builder, const sk_font_t *font, int count, int textByteCount, const sk_rect_t *bounds, sk_textblob_builder_runbuffer_t *runbuffer)
@
-}
foreign import ccall "sk_textblob_builder_alloc_run_text_rsxform" sk_textblob_builder_alloc_run_text_rsxform ::
  Ptr (Sk_textblob_builder) -- ^ C argument @"sk_textblob_builder_t * builder"@
  -> Ptr (Sk_font) -- ^ C argument @"const sk_font_t * font"@
  -> CInt -- ^ C argument @"int count"@
  -> CInt -- ^ C argument @"int textByteCount"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * bounds"@
  -> Ptr (Sk_textblob_builder_runbuffer) -- ^ C argument @"sk_textblob_builder_runbuffer_t * runbuffer"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_textblob_builder_alloc_run_text_rsxform'
foreign import ccall "&sk_textblob_builder_alloc_run_text_rsxform" p'sk_textblob_builder_alloc_run_text_rsxform ::
  FunPtr (Ptr (Sk_textblob_builder) -> Ptr (Sk_font) -> CInt -> CInt -> Ptr (Sk_rect) -> Ptr (Sk_textblob_builder_runbuffer) -> IO (()))
