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
module Skia.Bindings.Sk_picture where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
sk_picture_recorder_t *sk_picture_recorder_new(void)
@
-}
foreign import ccall "sk_picture_recorder_new" sk_picture_recorder_new ::
  IO (Ptr (Sk_picture_recorder)) -- ^ C return type: @"sk_picture_recorder_t *"@

-- | Function pointer to 'sk_picture_recorder_new'
foreign import ccall "&sk_picture_recorder_new" p'sk_picture_recorder_new ::
  FunPtr (IO (Ptr (Sk_picture_recorder)))

{- | C function signature:

@
void sk_picture_recorder_delete(sk_picture_recorder_t *)
@
-}
foreign import ccall "sk_picture_recorder_delete" sk_picture_recorder_delete ::
  Ptr (Sk_picture_recorder) -- ^ C argument type: @"sk_picture_recorder_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_picture_recorder_delete'
foreign import ccall "&sk_picture_recorder_delete" p'sk_picture_recorder_delete ::
  FunPtr (Ptr (Sk_picture_recorder) -> IO (()))

{- | C function signature:

@
sk_canvas_t *sk_picture_recorder_begin_recording(sk_picture_recorder_t *, const sk_rect_t *)
@
-}
foreign import ccall "sk_picture_recorder_begin_recording" sk_picture_recorder_begin_recording ::
  Ptr (Sk_picture_recorder) -- ^ C argument type: @"sk_picture_recorder_t *"@
  -> Ptr (Sk_rect) -- ^ C argument type: @"const sk_rect_t *"@
  -> IO (Ptr (Sk_canvas)) -- ^ C return type: @"sk_canvas_t *"@

-- | Function pointer to 'sk_picture_recorder_begin_recording'
foreign import ccall "&sk_picture_recorder_begin_recording" p'sk_picture_recorder_begin_recording ::
  FunPtr (Ptr (Sk_picture_recorder) -> Ptr (Sk_rect) -> IO (Ptr (Sk_canvas)))

{- | C function signature:

@
sk_canvas_t *sk_picture_recorder_begin_recording_with_bbh_factory(sk_picture_recorder_t *, const sk_rect_t *, sk_bbh_factory_t *)
@
-}
foreign import ccall "sk_picture_recorder_begin_recording_with_bbh_factory" sk_picture_recorder_begin_recording_with_bbh_factory ::
  Ptr (Sk_picture_recorder) -- ^ C argument type: @"sk_picture_recorder_t *"@
  -> Ptr (Sk_rect) -- ^ C argument type: @"const sk_rect_t *"@
  -> Ptr (Sk_bbh_factory) -- ^ C argument type: @"sk_bbh_factory_t *"@
  -> IO (Ptr (Sk_canvas)) -- ^ C return type: @"sk_canvas_t *"@

-- | Function pointer to 'sk_picture_recorder_begin_recording_with_bbh_factory'
foreign import ccall "&sk_picture_recorder_begin_recording_with_bbh_factory" p'sk_picture_recorder_begin_recording_with_bbh_factory ::
  FunPtr (Ptr (Sk_picture_recorder) -> Ptr (Sk_rect) -> Ptr (Sk_bbh_factory) -> IO (Ptr (Sk_canvas)))

{- | C function signature:

@
sk_picture_t *sk_picture_recorder_end_recording(sk_picture_recorder_t *)
@
-}
foreign import ccall "sk_picture_recorder_end_recording" sk_picture_recorder_end_recording ::
  Ptr (Sk_picture_recorder) -- ^ C argument type: @"sk_picture_recorder_t *"@
  -> IO (Ptr (Sk_picture)) -- ^ C return type: @"sk_picture_t *"@

-- | Function pointer to 'sk_picture_recorder_end_recording'
foreign import ccall "&sk_picture_recorder_end_recording" p'sk_picture_recorder_end_recording ::
  FunPtr (Ptr (Sk_picture_recorder) -> IO (Ptr (Sk_picture)))

{- | C function signature:

@
sk_drawable_t *sk_picture_recorder_end_recording_as_drawable(sk_picture_recorder_t *)
@
-}
foreign import ccall "sk_picture_recorder_end_recording_as_drawable" sk_picture_recorder_end_recording_as_drawable ::
  Ptr (Sk_picture_recorder) -- ^ C argument type: @"sk_picture_recorder_t *"@
  -> IO (Ptr (Sk_drawable)) -- ^ C return type: @"sk_drawable_t *"@

-- | Function pointer to 'sk_picture_recorder_end_recording_as_drawable'
foreign import ccall "&sk_picture_recorder_end_recording_as_drawable" p'sk_picture_recorder_end_recording_as_drawable ::
  FunPtr (Ptr (Sk_picture_recorder) -> IO (Ptr (Sk_drawable)))

{- | C function signature:

@
sk_canvas_t *sk_picture_get_recording_canvas(sk_picture_recorder_t *crec)
@
-}
foreign import ccall "sk_picture_get_recording_canvas" sk_picture_get_recording_canvas ::
  Ptr (Sk_picture_recorder) -- ^ C argument @"sk_picture_recorder_t * crec"@
  -> IO (Ptr (Sk_canvas)) -- ^ C return type: @"sk_canvas_t *"@

-- | Function pointer to 'sk_picture_get_recording_canvas'
foreign import ccall "&sk_picture_get_recording_canvas" p'sk_picture_get_recording_canvas ::
  FunPtr (Ptr (Sk_picture_recorder) -> IO (Ptr (Sk_canvas)))

{- | C function signature:

@
void sk_picture_ref(sk_picture_t *)
@
-}
foreign import ccall "sk_picture_ref" sk_picture_ref ::
  Ptr (Sk_picture) -- ^ C argument type: @"sk_picture_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_picture_ref'
foreign import ccall "&sk_picture_ref" p'sk_picture_ref ::
  FunPtr (Ptr (Sk_picture) -> IO (()))

{- | C function signature:

@
void sk_picture_unref(sk_picture_t *)
@
-}
foreign import ccall "sk_picture_unref" sk_picture_unref ::
  Ptr (Sk_picture) -- ^ C argument type: @"sk_picture_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_picture_unref'
foreign import ccall "&sk_picture_unref" p'sk_picture_unref ::
  FunPtr (Ptr (Sk_picture) -> IO (()))

{- | C function signature:

@
uint32_t sk_picture_get_unique_id(sk_picture_t *)
@
-}
foreign import ccall "sk_picture_get_unique_id" sk_picture_get_unique_id ::
  Ptr (Sk_picture) -- ^ C argument type: @"sk_picture_t *"@
  -> IO (Word32) -- ^ C return type: @"uint32_t"@

-- | Function pointer to 'sk_picture_get_unique_id'
foreign import ccall "&sk_picture_get_unique_id" p'sk_picture_get_unique_id ::
  FunPtr (Ptr (Sk_picture) -> IO (Word32))

{- | C function signature:

@
void sk_picture_get_cull_rect(sk_picture_t *, sk_rect_t *)
@
-}
foreign import ccall "sk_picture_get_cull_rect" sk_picture_get_cull_rect ::
  Ptr (Sk_picture) -- ^ C argument type: @"sk_picture_t *"@
  -> Ptr (Sk_rect) -- ^ C argument type: @"sk_rect_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_picture_get_cull_rect'
foreign import ccall "&sk_picture_get_cull_rect" p'sk_picture_get_cull_rect ::
  FunPtr (Ptr (Sk_picture) -> Ptr (Sk_rect) -> IO (()))

{- | C function signature:

@
sk_shader_t *sk_picture_make_shader(sk_picture_t *src, sk_shader_tilemode_t tmx, sk_shader_tilemode_t tmy, sk_filter_mode_t mode, const sk_matrix_t *localMatrix, const sk_rect_t *tile)
@
-}
foreign import ccall "sk_picture_make_shader" sk_picture_make_shader ::
  Ptr (Sk_picture) -- ^ C argument @"sk_picture_t * src"@
  -> Sk_shader_tilemode -- ^ C argument @"sk_shader_tilemode_t tmx"@
  -> Sk_shader_tilemode -- ^ C argument @"sk_shader_tilemode_t tmy"@
  -> Sk_filter_mode -- ^ C argument @"sk_filter_mode_t mode"@
  -> Ptr (Sk_matrix) -- ^ C argument @"const sk_matrix_t * localMatrix"@
  -> Ptr (Sk_rect) -- ^ C argument @"const sk_rect_t * tile"@
  -> IO (Ptr (Sk_shader)) -- ^ C return type: @"sk_shader_t *"@

-- | Function pointer to 'sk_picture_make_shader'
foreign import ccall "&sk_picture_make_shader" p'sk_picture_make_shader ::
  FunPtr (Ptr (Sk_picture) -> Sk_shader_tilemode -> Sk_shader_tilemode -> Sk_filter_mode -> Ptr (Sk_matrix) -> Ptr (Sk_rect) -> IO (Ptr (Sk_shader)))

{- | C function signature:

@
sk_data_t *sk_picture_serialize_to_data(const sk_picture_t *picture)
@
-}
foreign import ccall "sk_picture_serialize_to_data" sk_picture_serialize_to_data ::
  Ptr (Sk_picture) -- ^ C argument @"const sk_picture_t * picture"@
  -> IO (Ptr (Sk_data)) -- ^ C return type: @"sk_data_t *"@

-- | Function pointer to 'sk_picture_serialize_to_data'
foreign import ccall "&sk_picture_serialize_to_data" p'sk_picture_serialize_to_data ::
  FunPtr (Ptr (Sk_picture) -> IO (Ptr (Sk_data)))

{- | C function signature:

@
void sk_picture_serialize_to_stream(const sk_picture_t *picture, sk_wstream_t *stream)
@
-}
foreign import ccall "sk_picture_serialize_to_stream" sk_picture_serialize_to_stream ::
  Ptr (Sk_picture) -- ^ C argument @"const sk_picture_t * picture"@
  -> Ptr (Sk_wstream) -- ^ C argument @"sk_wstream_t * stream"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_picture_serialize_to_stream'
foreign import ccall "&sk_picture_serialize_to_stream" p'sk_picture_serialize_to_stream ::
  FunPtr (Ptr (Sk_picture) -> Ptr (Sk_wstream) -> IO (()))

{- | C function signature:

@
sk_picture_t *sk_picture_deserialize_from_stream(sk_stream_t *stream)
@
-}
foreign import ccall "sk_picture_deserialize_from_stream" sk_picture_deserialize_from_stream ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * stream"@
  -> IO (Ptr (Sk_picture)) -- ^ C return type: @"sk_picture_t *"@

-- | Function pointer to 'sk_picture_deserialize_from_stream'
foreign import ccall "&sk_picture_deserialize_from_stream" p'sk_picture_deserialize_from_stream ::
  FunPtr (Ptr (Sk_stream) -> IO (Ptr (Sk_picture)))

{- | C function signature:

@
sk_picture_t *sk_picture_deserialize_from_data(sk_data_t *data)
@
-}
foreign import ccall "sk_picture_deserialize_from_data" sk_picture_deserialize_from_data ::
  Ptr (Sk_data) -- ^ C argument @"sk_data_t * data"@
  -> IO (Ptr (Sk_picture)) -- ^ C return type: @"sk_picture_t *"@

-- | Function pointer to 'sk_picture_deserialize_from_data'
foreign import ccall "&sk_picture_deserialize_from_data" p'sk_picture_deserialize_from_data ::
  FunPtr (Ptr (Sk_data) -> IO (Ptr (Sk_picture)))

{- | C function signature:

@
sk_picture_t *sk_picture_deserialize_from_memory(void *buffer, size_t length)
@
-}
foreign import ccall "sk_picture_deserialize_from_memory" sk_picture_deserialize_from_memory ::
  Ptr (()) -- ^ C argument @"void * buffer"@
  -> CSize -- ^ C argument @"size_t length"@
  -> IO (Ptr (Sk_picture)) -- ^ C return type: @"sk_picture_t *"@

-- | Function pointer to 'sk_picture_deserialize_from_memory'
foreign import ccall "&sk_picture_deserialize_from_memory" p'sk_picture_deserialize_from_memory ::
  FunPtr (Ptr (()) -> CSize -> IO (Ptr (Sk_picture)))

{- | C function signature:

@
void sk_picture_playback(const sk_picture_t *picture, sk_canvas_t *canvas)
@
-}
foreign import ccall "sk_picture_playback" sk_picture_playback ::
  Ptr (Sk_picture) -- ^ C argument @"const sk_picture_t * picture"@
  -> Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * canvas"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_picture_playback'
foreign import ccall "&sk_picture_playback" p'sk_picture_playback ::
  FunPtr (Ptr (Sk_picture) -> Ptr (Sk_canvas) -> IO (()))

{- | C function signature:

@
int sk_picture_approximate_op_count(const sk_picture_t *picture, _Bool nested)
@
-}
foreign import ccall "sk_picture_approximate_op_count" sk_picture_approximate_op_count ::
  Ptr (Sk_picture) -- ^ C argument @"const sk_picture_t * picture"@
  -> CBool -- ^ C argument @"_Bool nested"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'sk_picture_approximate_op_count'
foreign import ccall "&sk_picture_approximate_op_count" p'sk_picture_approximate_op_count ::
  FunPtr (Ptr (Sk_picture) -> CBool -> IO (CInt))

{- | C function signature:

@
size_t sk_picture_approximate_bytes_used(const sk_picture_t *picture)
@
-}
foreign import ccall "sk_picture_approximate_bytes_used" sk_picture_approximate_bytes_used ::
  Ptr (Sk_picture) -- ^ C argument @"const sk_picture_t * picture"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'sk_picture_approximate_bytes_used'
foreign import ccall "&sk_picture_approximate_bytes_used" p'sk_picture_approximate_bytes_used ::
  FunPtr (Ptr (Sk_picture) -> IO (CSize))

{- | C function signature:

@
sk_rtree_factory_t *sk_rtree_factory_new(void)
@
-}
foreign import ccall "sk_rtree_factory_new" sk_rtree_factory_new ::
  IO (Ptr (Sk_rtree_factory)) -- ^ C return type: @"sk_rtree_factory_t *"@

-- | Function pointer to 'sk_rtree_factory_new'
foreign import ccall "&sk_rtree_factory_new" p'sk_rtree_factory_new ::
  FunPtr (IO (Ptr (Sk_rtree_factory)))

{- | C function signature:

@
void sk_rtree_factory_delete(sk_rtree_factory_t *)
@
-}
foreign import ccall "sk_rtree_factory_delete" sk_rtree_factory_delete ::
  Ptr (Sk_rtree_factory) -- ^ C argument type: @"sk_rtree_factory_t *"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'sk_rtree_factory_delete'
foreign import ccall "&sk_rtree_factory_delete" p'sk_rtree_factory_delete ::
  FunPtr (Ptr (Sk_rtree_factory) -> IO (()))
