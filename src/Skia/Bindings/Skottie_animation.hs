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
module Skia.Bindings.Skottie_animation where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
skottie_animation_t *skottie_animation_make_from_string(const char *data, size_t length)
@
-}
foreign import ccall "skottie_animation_make_from_string" skottie_animation_make_from_string ::
  Ptr (CChar) -- ^ C argument @"const char * data"@
  -> CSize -- ^ C argument @"size_t length"@
  -> IO (Ptr (Skottie_animation)) -- ^ C return type: @"skottie_animation_t *"@

-- | Function pointer to 'skottie_animation_make_from_string'
foreign import ccall "&skottie_animation_make_from_string" p'skottie_animation_make_from_string ::
  FunPtr (Ptr (CChar) -> CSize -> IO (Ptr (Skottie_animation)))

{- | C function signature:

@
skottie_animation_t *skottie_animation_make_from_data(const char *data, size_t length)
@
-}
foreign import ccall "skottie_animation_make_from_data" skottie_animation_make_from_data ::
  Ptr (CChar) -- ^ C argument @"const char * data"@
  -> CSize -- ^ C argument @"size_t length"@
  -> IO (Ptr (Skottie_animation)) -- ^ C return type: @"skottie_animation_t *"@

-- | Function pointer to 'skottie_animation_make_from_data'
foreign import ccall "&skottie_animation_make_from_data" p'skottie_animation_make_from_data ::
  FunPtr (Ptr (CChar) -> CSize -> IO (Ptr (Skottie_animation)))

{- | C function signature:

@
skottie_animation_t *skottie_animation_make_from_stream(sk_stream_t *stream)
@
-}
foreign import ccall "skottie_animation_make_from_stream" skottie_animation_make_from_stream ::
  Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * stream"@
  -> IO (Ptr (Skottie_animation)) -- ^ C return type: @"skottie_animation_t *"@

-- | Function pointer to 'skottie_animation_make_from_stream'
foreign import ccall "&skottie_animation_make_from_stream" p'skottie_animation_make_from_stream ::
  FunPtr (Ptr (Sk_stream) -> IO (Ptr (Skottie_animation)))

{- | C function signature:

@
skottie_animation_t *skottie_animation_make_from_file(const char *path)
@
-}
foreign import ccall "skottie_animation_make_from_file" skottie_animation_make_from_file ::
  Ptr (CChar) -- ^ C argument @"const char * path"@
  -> IO (Ptr (Skottie_animation)) -- ^ C return type: @"skottie_animation_t *"@

-- | Function pointer to 'skottie_animation_make_from_file'
foreign import ccall "&skottie_animation_make_from_file" p'skottie_animation_make_from_file ::
  FunPtr (Ptr (CChar) -> IO (Ptr (Skottie_animation)))

{- | C function signature:

@
void skottie_animation_ref(skottie_animation_t *instance)
@
-}
foreign import ccall "skottie_animation_ref" skottie_animation_ref ::
  Ptr (Skottie_animation) -- ^ C argument @"skottie_animation_t * instance"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skottie_animation_ref'
foreign import ccall "&skottie_animation_ref" p'skottie_animation_ref ::
  FunPtr (Ptr (Skottie_animation) -> IO (()))

{- | C function signature:

@
void skottie_animation_unref(skottie_animation_t *instance)
@
-}
foreign import ccall "skottie_animation_unref" skottie_animation_unref ::
  Ptr (Skottie_animation) -- ^ C argument @"skottie_animation_t * instance"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skottie_animation_unref'
foreign import ccall "&skottie_animation_unref" p'skottie_animation_unref ::
  FunPtr (Ptr (Skottie_animation) -> IO (()))

{- | C function signature:

@
void skottie_animation_delete(skottie_animation_t *instance)
@
-}
foreign import ccall "skottie_animation_delete" skottie_animation_delete ::
  Ptr (Skottie_animation) -- ^ C argument @"skottie_animation_t * instance"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skottie_animation_delete'
foreign import ccall "&skottie_animation_delete" p'skottie_animation_delete ::
  FunPtr (Ptr (Skottie_animation) -> IO (()))

{- | C function signature:

@
void skottie_animation_render(skottie_animation_t *instance, sk_canvas_t *canvas, sk_rect_t *dst)
@
-}
foreign import ccall "skottie_animation_render" skottie_animation_render ::
  Ptr (Skottie_animation) -- ^ C argument @"skottie_animation_t * instance"@
  -> Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * canvas"@
  -> Ptr (Sk_rect) -- ^ C argument @"sk_rect_t * dst"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skottie_animation_render'
foreign import ccall "&skottie_animation_render" p'skottie_animation_render ::
  FunPtr (Ptr (Skottie_animation) -> Ptr (Sk_canvas) -> Ptr (Sk_rect) -> IO (()))

{- | C function signature:

@
void skottie_animation_render_with_flags(skottie_animation_t *instance, sk_canvas_t *canvas, sk_rect_t *dst, skottie_animation_renderflags_t flags)
@
-}
foreign import ccall "skottie_animation_render_with_flags" skottie_animation_render_with_flags ::
  Ptr (Skottie_animation) -- ^ C argument @"skottie_animation_t * instance"@
  -> Ptr (Sk_canvas) -- ^ C argument @"sk_canvas_t * canvas"@
  -> Ptr (Sk_rect) -- ^ C argument @"sk_rect_t * dst"@
  -> Skottie_animation_renderflags -- ^ C argument @"skottie_animation_renderflags_t flags"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skottie_animation_render_with_flags'
foreign import ccall "&skottie_animation_render_with_flags" p'skottie_animation_render_with_flags ::
  FunPtr (Ptr (Skottie_animation) -> Ptr (Sk_canvas) -> Ptr (Sk_rect) -> Skottie_animation_renderflags -> IO (()))

{- | C function signature:

@
void skottie_animation_seek(skottie_animation_t *instance, float t, sksg_invalidation_controller_t *ic)
@
-}
foreign import ccall "skottie_animation_seek" skottie_animation_seek ::
  Ptr (Skottie_animation) -- ^ C argument @"skottie_animation_t * instance"@
  -> CFloat -- ^ C argument @"float t"@
  -> Ptr (Sksg_invalidation_controller) -- ^ C argument @"sksg_invalidation_controller_t * ic"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skottie_animation_seek'
foreign import ccall "&skottie_animation_seek" p'skottie_animation_seek ::
  FunPtr (Ptr (Skottie_animation) -> CFloat -> Ptr (Sksg_invalidation_controller) -> IO (()))

{- | C function signature:

@
void skottie_animation_seek_frame(skottie_animation_t *instance, float t, sksg_invalidation_controller_t *ic)
@
-}
foreign import ccall "skottie_animation_seek_frame" skottie_animation_seek_frame ::
  Ptr (Skottie_animation) -- ^ C argument @"skottie_animation_t * instance"@
  -> CFloat -- ^ C argument @"float t"@
  -> Ptr (Sksg_invalidation_controller) -- ^ C argument @"sksg_invalidation_controller_t * ic"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skottie_animation_seek_frame'
foreign import ccall "&skottie_animation_seek_frame" p'skottie_animation_seek_frame ::
  FunPtr (Ptr (Skottie_animation) -> CFloat -> Ptr (Sksg_invalidation_controller) -> IO (()))

{- | C function signature:

@
void skottie_animation_seek_frame_time(skottie_animation_t *instance, float t, sksg_invalidation_controller_t *ic)
@
-}
foreign import ccall "skottie_animation_seek_frame_time" skottie_animation_seek_frame_time ::
  Ptr (Skottie_animation) -- ^ C argument @"skottie_animation_t * instance"@
  -> CFloat -- ^ C argument @"float t"@
  -> Ptr (Sksg_invalidation_controller) -- ^ C argument @"sksg_invalidation_controller_t * ic"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skottie_animation_seek_frame_time'
foreign import ccall "&skottie_animation_seek_frame_time" p'skottie_animation_seek_frame_time ::
  FunPtr (Ptr (Skottie_animation) -> CFloat -> Ptr (Sksg_invalidation_controller) -> IO (()))

{- | C function signature:

@
double skottie_animation_get_duration(skottie_animation_t *instance)
@
-}
foreign import ccall "skottie_animation_get_duration" skottie_animation_get_duration ::
  Ptr (Skottie_animation) -- ^ C argument @"skottie_animation_t * instance"@
  -> IO (CDouble) -- ^ C return type: @"double"@

-- | Function pointer to 'skottie_animation_get_duration'
foreign import ccall "&skottie_animation_get_duration" p'skottie_animation_get_duration ::
  FunPtr (Ptr (Skottie_animation) -> IO (CDouble))

{- | C function signature:

@
double skottie_animation_get_fps(skottie_animation_t *instance)
@
-}
foreign import ccall "skottie_animation_get_fps" skottie_animation_get_fps ::
  Ptr (Skottie_animation) -- ^ C argument @"skottie_animation_t * instance"@
  -> IO (CDouble) -- ^ C return type: @"double"@

-- | Function pointer to 'skottie_animation_get_fps'
foreign import ccall "&skottie_animation_get_fps" p'skottie_animation_get_fps ::
  FunPtr (Ptr (Skottie_animation) -> IO (CDouble))

{- | C function signature:

@
double skottie_animation_get_in_point(skottie_animation_t *instance)
@
-}
foreign import ccall "skottie_animation_get_in_point" skottie_animation_get_in_point ::
  Ptr (Skottie_animation) -- ^ C argument @"skottie_animation_t * instance"@
  -> IO (CDouble) -- ^ C return type: @"double"@

-- | Function pointer to 'skottie_animation_get_in_point'
foreign import ccall "&skottie_animation_get_in_point" p'skottie_animation_get_in_point ::
  FunPtr (Ptr (Skottie_animation) -> IO (CDouble))

{- | C function signature:

@
double skottie_animation_get_out_point(skottie_animation_t *instance)
@
-}
foreign import ccall "skottie_animation_get_out_point" skottie_animation_get_out_point ::
  Ptr (Skottie_animation) -- ^ C argument @"skottie_animation_t * instance"@
  -> IO (CDouble) -- ^ C return type: @"double"@

-- | Function pointer to 'skottie_animation_get_out_point'
foreign import ccall "&skottie_animation_get_out_point" p'skottie_animation_get_out_point ::
  FunPtr (Ptr (Skottie_animation) -> IO (CDouble))

{- | C function signature:

@
void skottie_animation_get_version(skottie_animation_t *instance, sk_string_t *version)
@
-}
foreign import ccall "skottie_animation_get_version" skottie_animation_get_version ::
  Ptr (Skottie_animation) -- ^ C argument @"skottie_animation_t * instance"@
  -> Ptr (Sk_string) -- ^ C argument @"sk_string_t * version"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skottie_animation_get_version'
foreign import ccall "&skottie_animation_get_version" p'skottie_animation_get_version ::
  FunPtr (Ptr (Skottie_animation) -> Ptr (Sk_string) -> IO (()))

{- | C function signature:

@
void skottie_animation_get_size(skottie_animation_t *instance, sk_size_t *size)
@
-}
foreign import ccall "skottie_animation_get_size" skottie_animation_get_size ::
  Ptr (Skottie_animation) -- ^ C argument @"skottie_animation_t * instance"@
  -> Ptr (Sk_size) -- ^ C argument @"sk_size_t * size"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skottie_animation_get_size'
foreign import ccall "&skottie_animation_get_size" p'skottie_animation_get_size ::
  FunPtr (Ptr (Skottie_animation) -> Ptr (Sk_size) -> IO (()))

{- | C function signature:

@
skottie_animation_builder_t *skottie_animation_builder_new(skottie_animation_builder_flags_t flags)
@
-}
foreign import ccall "skottie_animation_builder_new" skottie_animation_builder_new ::
  Skottie_animation_builder_flags -- ^ C argument @"skottie_animation_builder_flags_t flags"@
  -> IO (Ptr (Skottie_animation_builder)) -- ^ C return type: @"skottie_animation_builder_t *"@

-- | Function pointer to 'skottie_animation_builder_new'
foreign import ccall "&skottie_animation_builder_new" p'skottie_animation_builder_new ::
  FunPtr (Skottie_animation_builder_flags -> IO (Ptr (Skottie_animation_builder)))

{- | C function signature:

@
void skottie_animation_builder_delete(skottie_animation_builder_t *instance)
@
-}
foreign import ccall "skottie_animation_builder_delete" skottie_animation_builder_delete ::
  Ptr (Skottie_animation_builder) -- ^ C argument @"skottie_animation_builder_t * instance"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skottie_animation_builder_delete'
foreign import ccall "&skottie_animation_builder_delete" p'skottie_animation_builder_delete ::
  FunPtr (Ptr (Skottie_animation_builder) -> IO (()))

{- | C function signature:

@
void skottie_animation_builder_get_stats(skottie_animation_builder_t *instance, skottie_animation_builder_stats_t *stats)
@
-}
foreign import ccall "skottie_animation_builder_get_stats" skottie_animation_builder_get_stats ::
  Ptr (Skottie_animation_builder) -- ^ C argument @"skottie_animation_builder_t * instance"@
  -> Ptr (Skottie_animation_builder_stats) -- ^ C argument @"skottie_animation_builder_stats_t * stats"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skottie_animation_builder_get_stats'
foreign import ccall "&skottie_animation_builder_get_stats" p'skottie_animation_builder_get_stats ::
  FunPtr (Ptr (Skottie_animation_builder) -> Ptr (Skottie_animation_builder_stats) -> IO (()))

{- | C function signature:

@
void skottie_animation_builder_set_resource_provider(skottie_animation_builder_t *instance, skresources_resource_provider_t *resourceProvider)
@
-}
foreign import ccall "skottie_animation_builder_set_resource_provider" skottie_animation_builder_set_resource_provider ::
  Ptr (Skottie_animation_builder) -- ^ C argument @"skottie_animation_builder_t * instance"@
  -> Ptr (Skresources_resource_provider) -- ^ C argument @"skresources_resource_provider_t * resourceProvider"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skottie_animation_builder_set_resource_provider'
foreign import ccall "&skottie_animation_builder_set_resource_provider" p'skottie_animation_builder_set_resource_provider ::
  FunPtr (Ptr (Skottie_animation_builder) -> Ptr (Skresources_resource_provider) -> IO (()))

{- | C function signature:

@
void skottie_animation_builder_set_font_manager(skottie_animation_builder_t *instance, sk_fontmgr_t *fontManager)
@
-}
foreign import ccall "skottie_animation_builder_set_font_manager" skottie_animation_builder_set_font_manager ::
  Ptr (Skottie_animation_builder) -- ^ C argument @"skottie_animation_builder_t * instance"@
  -> Ptr (Sk_fontmgr) -- ^ C argument @"sk_fontmgr_t * fontManager"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'skottie_animation_builder_set_font_manager'
foreign import ccall "&skottie_animation_builder_set_font_manager" p'skottie_animation_builder_set_font_manager ::
  FunPtr (Ptr (Skottie_animation_builder) -> Ptr (Sk_fontmgr) -> IO (()))

{- | C function signature:

@
skottie_animation_t *skottie_animation_builder_make_from_stream(skottie_animation_builder_t *instance, sk_stream_t *stream)
@
-}
foreign import ccall "skottie_animation_builder_make_from_stream" skottie_animation_builder_make_from_stream ::
  Ptr (Skottie_animation_builder) -- ^ C argument @"skottie_animation_builder_t * instance"@
  -> Ptr (Sk_stream) -- ^ C argument @"sk_stream_t * stream"@
  -> IO (Ptr (Skottie_animation)) -- ^ C return type: @"skottie_animation_t *"@

-- | Function pointer to 'skottie_animation_builder_make_from_stream'
foreign import ccall "&skottie_animation_builder_make_from_stream" p'skottie_animation_builder_make_from_stream ::
  FunPtr (Ptr (Skottie_animation_builder) -> Ptr (Sk_stream) -> IO (Ptr (Skottie_animation)))

{- | C function signature:

@
skottie_animation_t *skottie_animation_builder_make_from_file(skottie_animation_builder_t *instance, const char *path)
@
-}
foreign import ccall "skottie_animation_builder_make_from_file" skottie_animation_builder_make_from_file ::
  Ptr (Skottie_animation_builder) -- ^ C argument @"skottie_animation_builder_t * instance"@
  -> Ptr (CChar) -- ^ C argument @"const char * path"@
  -> IO (Ptr (Skottie_animation)) -- ^ C return type: @"skottie_animation_t *"@

-- | Function pointer to 'skottie_animation_builder_make_from_file'
foreign import ccall "&skottie_animation_builder_make_from_file" p'skottie_animation_builder_make_from_file ::
  FunPtr (Ptr (Skottie_animation_builder) -> Ptr (CChar) -> IO (Ptr (Skottie_animation)))

{- | C function signature:

@
skottie_animation_t *skottie_animation_builder_make_from_string(skottie_animation_builder_t *instance, const char *data, size_t length)
@
-}
foreign import ccall "skottie_animation_builder_make_from_string" skottie_animation_builder_make_from_string ::
  Ptr (Skottie_animation_builder) -- ^ C argument @"skottie_animation_builder_t * instance"@
  -> Ptr (CChar) -- ^ C argument @"const char * data"@
  -> CSize -- ^ C argument @"size_t length"@
  -> IO (Ptr (Skottie_animation)) -- ^ C return type: @"skottie_animation_t *"@

-- | Function pointer to 'skottie_animation_builder_make_from_string'
foreign import ccall "&skottie_animation_builder_make_from_string" p'skottie_animation_builder_make_from_string ::
  FunPtr (Ptr (Skottie_animation_builder) -> Ptr (CChar) -> CSize -> IO (Ptr (Skottie_animation)))

{- | C function signature:

@
skottie_animation_t *skottie_animation_builder_make_from_data(skottie_animation_builder_t *instance, const char *data, size_t length)
@
-}
foreign import ccall "skottie_animation_builder_make_from_data" skottie_animation_builder_make_from_data ::
  Ptr (Skottie_animation_builder) -- ^ C argument @"skottie_animation_builder_t * instance"@
  -> Ptr (CChar) -- ^ C argument @"const char * data"@
  -> CSize -- ^ C argument @"size_t length"@
  -> IO (Ptr (Skottie_animation)) -- ^ C return type: @"skottie_animation_t *"@

-- | Function pointer to 'skottie_animation_builder_make_from_data'
foreign import ccall "&skottie_animation_builder_make_from_data" p'skottie_animation_builder_make_from_data ::
  FunPtr (Ptr (Skottie_animation_builder) -> Ptr (CChar) -> CSize -> IO (Ptr (Skottie_animation)))
