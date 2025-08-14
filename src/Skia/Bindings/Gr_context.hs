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
module Skia.Bindings.Gr_context where

import Foreign
import Foreign.C
import Foreign.Storable
import Foreign.Storable.Offset

import Skia.Bindings.Types


{- | C function signature:

@
void gr_recording_context_unref(gr_recording_context_t *context)
@
-}
foreign import ccall "gr_recording_context_unref" gr_recording_context_unref ::
  Ptr (Gr_recording_context) -- ^ C argument @"gr_recording_context_t * context"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_recording_context_unref'
foreign import ccall "&gr_recording_context_unref" p'gr_recording_context_unref ::
  FunPtr (Ptr (Gr_recording_context) -> IO (()))

{- | C function signature:

@
int gr_recording_context_get_max_surface_sample_count_for_color_type(gr_recording_context_t *context, sk_colortype_t colorType)
@
-}
foreign import ccall "gr_recording_context_get_max_surface_sample_count_for_color_type" gr_recording_context_get_max_surface_sample_count_for_color_type ::
  Ptr (Gr_recording_context) -- ^ C argument @"gr_recording_context_t * context"@
  -> Sk_colortype -- ^ C argument @"sk_colortype_t colorType"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'gr_recording_context_get_max_surface_sample_count_for_color_type'
foreign import ccall "&gr_recording_context_get_max_surface_sample_count_for_color_type" p'gr_recording_context_get_max_surface_sample_count_for_color_type ::
  FunPtr (Ptr (Gr_recording_context) -> Sk_colortype -> IO (CInt))

{- | C function signature:

@
gr_backend_t gr_recording_context_get_backend(gr_recording_context_t *context)
@
-}
foreign import ccall "gr_recording_context_get_backend" gr_recording_context_get_backend ::
  Ptr (Gr_recording_context) -- ^ C argument @"gr_recording_context_t * context"@
  -> IO (Gr_backend) -- ^ C return type: @"gr_backend_t"@

-- | Function pointer to 'gr_recording_context_get_backend'
foreign import ccall "&gr_recording_context_get_backend" p'gr_recording_context_get_backend ::
  FunPtr (Ptr (Gr_recording_context) -> IO (Gr_backend))

{- | C function signature:

@
_Bool gr_recording_context_is_abandoned(gr_recording_context_t *context)
@
-}
foreign import ccall "gr_recording_context_is_abandoned" gr_recording_context_is_abandoned ::
  Ptr (Gr_recording_context) -- ^ C argument @"gr_recording_context_t * context"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'gr_recording_context_is_abandoned'
foreign import ccall "&gr_recording_context_is_abandoned" p'gr_recording_context_is_abandoned ::
  FunPtr (Ptr (Gr_recording_context) -> IO (CBool))

{- | C function signature:

@
int gr_recording_context_max_texture_size(gr_recording_context_t *context)
@
-}
foreign import ccall "gr_recording_context_max_texture_size" gr_recording_context_max_texture_size ::
  Ptr (Gr_recording_context) -- ^ C argument @"gr_recording_context_t * context"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'gr_recording_context_max_texture_size'
foreign import ccall "&gr_recording_context_max_texture_size" p'gr_recording_context_max_texture_size ::
  FunPtr (Ptr (Gr_recording_context) -> IO (CInt))

{- | C function signature:

@
int gr_recording_context_max_render_target_size(gr_recording_context_t *context)
@
-}
foreign import ccall "gr_recording_context_max_render_target_size" gr_recording_context_max_render_target_size ::
  Ptr (Gr_recording_context) -- ^ C argument @"gr_recording_context_t * context"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'gr_recording_context_max_render_target_size'
foreign import ccall "&gr_recording_context_max_render_target_size" p'gr_recording_context_max_render_target_size ::
  FunPtr (Ptr (Gr_recording_context) -> IO (CInt))

{- | C function signature:

@
gr_direct_context_t *gr_recording_context_get_direct_context(gr_recording_context_t *context)
@
-}
foreign import ccall "gr_recording_context_get_direct_context" gr_recording_context_get_direct_context ::
  Ptr (Gr_recording_context) -- ^ C argument @"gr_recording_context_t * context"@
  -> IO (Ptr (Gr_direct_context)) -- ^ C return type: @"gr_direct_context_t *"@

-- | Function pointer to 'gr_recording_context_get_direct_context'
foreign import ccall "&gr_recording_context_get_direct_context" p'gr_recording_context_get_direct_context ::
  FunPtr (Ptr (Gr_recording_context) -> IO (Ptr (Gr_direct_context)))

{- | C function signature:

@
gr_direct_context_t *gr_direct_context_make_gl(const gr_glinterface_t *glInterface)
@
-}
foreign import ccall "gr_direct_context_make_gl" gr_direct_context_make_gl ::
  Ptr (Gr_glinterface) -- ^ C argument @"const gr_glinterface_t * glInterface"@
  -> IO (Ptr (Gr_direct_context)) -- ^ C return type: @"gr_direct_context_t *"@

-- | Function pointer to 'gr_direct_context_make_gl'
foreign import ccall "&gr_direct_context_make_gl" p'gr_direct_context_make_gl ::
  FunPtr (Ptr (Gr_glinterface) -> IO (Ptr (Gr_direct_context)))

{- | C function signature:

@
gr_direct_context_t *gr_direct_context_make_gl_with_options(const gr_glinterface_t *glInterface, const gr_context_options_t *options)
@
-}
foreign import ccall "gr_direct_context_make_gl_with_options" gr_direct_context_make_gl_with_options ::
  Ptr (Gr_glinterface) -- ^ C argument @"const gr_glinterface_t * glInterface"@
  -> Ptr (Gr_context_options) -- ^ C argument @"const gr_context_options_t * options"@
  -> IO (Ptr (Gr_direct_context)) -- ^ C return type: @"gr_direct_context_t *"@

-- | Function pointer to 'gr_direct_context_make_gl_with_options'
foreign import ccall "&gr_direct_context_make_gl_with_options" p'gr_direct_context_make_gl_with_options ::
  FunPtr (Ptr (Gr_glinterface) -> Ptr (Gr_context_options) -> IO (Ptr (Gr_direct_context)))

{- | C function signature:

@
gr_direct_context_t *gr_direct_context_make_vulkan(const gr_vk_backendcontext_t *vkBackendContext)
@
-}
foreign import ccall "gr_direct_context_make_vulkan" gr_direct_context_make_vulkan ::
  Ptr (Gr_vk_backendcontext) -- ^ C argument @"const gr_vk_backendcontext_t * vkBackendContext"@
  -> IO (Ptr (Gr_direct_context)) -- ^ C return type: @"gr_direct_context_t *"@

-- | Function pointer to 'gr_direct_context_make_vulkan'
foreign import ccall "&gr_direct_context_make_vulkan" p'gr_direct_context_make_vulkan ::
  FunPtr (Ptr (Gr_vk_backendcontext) -> IO (Ptr (Gr_direct_context)))

{- | C function signature:

@
gr_direct_context_t *gr_direct_context_make_vulkan_with_options(const gr_vk_backendcontext_t *vkBackendContext, const gr_context_options_t *options)
@
-}
foreign import ccall "gr_direct_context_make_vulkan_with_options" gr_direct_context_make_vulkan_with_options ::
  Ptr (Gr_vk_backendcontext) -- ^ C argument @"const gr_vk_backendcontext_t * vkBackendContext"@
  -> Ptr (Gr_context_options) -- ^ C argument @"const gr_context_options_t * options"@
  -> IO (Ptr (Gr_direct_context)) -- ^ C return type: @"gr_direct_context_t *"@

-- | Function pointer to 'gr_direct_context_make_vulkan_with_options'
foreign import ccall "&gr_direct_context_make_vulkan_with_options" p'gr_direct_context_make_vulkan_with_options ::
  FunPtr (Ptr (Gr_vk_backendcontext) -> Ptr (Gr_context_options) -> IO (Ptr (Gr_direct_context)))

{- | C function signature:

@
gr_direct_context_t *gr_direct_context_make_metal(void *device, void *queue)
@
-}
foreign import ccall "gr_direct_context_make_metal" gr_direct_context_make_metal ::
  Ptr (()) -- ^ C argument @"void * device"@
  -> Ptr (()) -- ^ C argument @"void * queue"@
  -> IO (Ptr (Gr_direct_context)) -- ^ C return type: @"gr_direct_context_t *"@

-- | Function pointer to 'gr_direct_context_make_metal'
foreign import ccall "&gr_direct_context_make_metal" p'gr_direct_context_make_metal ::
  FunPtr (Ptr (()) -> Ptr (()) -> IO (Ptr (Gr_direct_context)))

{- | C function signature:

@
gr_direct_context_t *gr_direct_context_make_metal_with_options(void *device, void *queue, const gr_context_options_t *options)
@
-}
foreign import ccall "gr_direct_context_make_metal_with_options" gr_direct_context_make_metal_with_options ::
  Ptr (()) -- ^ C argument @"void * device"@
  -> Ptr (()) -- ^ C argument @"void * queue"@
  -> Ptr (Gr_context_options) -- ^ C argument @"const gr_context_options_t * options"@
  -> IO (Ptr (Gr_direct_context)) -- ^ C return type: @"gr_direct_context_t *"@

-- | Function pointer to 'gr_direct_context_make_metal_with_options'
foreign import ccall "&gr_direct_context_make_metal_with_options" p'gr_direct_context_make_metal_with_options ::
  FunPtr (Ptr (()) -> Ptr (()) -> Ptr (Gr_context_options) -> IO (Ptr (Gr_direct_context)))

{- | C function signature:

@
gr_direct_context_t *gr_direct_context_make_direct3d(const gr_d3d_backendcontext_t *d3dBackendContext)
@
-}
foreign import ccall "gr_direct_context_make_direct3d" gr_direct_context_make_direct3d ::
  Ptr (Gr_d3d_backendcontext) -- ^ C argument @"const gr_d3d_backendcontext_t * d3dBackendContext"@
  -> IO (Ptr (Gr_direct_context)) -- ^ C return type: @"gr_direct_context_t *"@

-- | Function pointer to 'gr_direct_context_make_direct3d'
foreign import ccall "&gr_direct_context_make_direct3d" p'gr_direct_context_make_direct3d ::
  FunPtr (Ptr (Gr_d3d_backendcontext) -> IO (Ptr (Gr_direct_context)))

{- | C function signature:

@
gr_direct_context_t *gr_direct_context_make_direct3d_with_options(const gr_d3d_backendcontext_t *d3dBackendContext, const gr_context_options_t *options)
@
-}
foreign import ccall "gr_direct_context_make_direct3d_with_options" gr_direct_context_make_direct3d_with_options ::
  Ptr (Gr_d3d_backendcontext) -- ^ C argument @"const gr_d3d_backendcontext_t * d3dBackendContext"@
  -> Ptr (Gr_context_options) -- ^ C argument @"const gr_context_options_t * options"@
  -> IO (Ptr (Gr_direct_context)) -- ^ C return type: @"gr_direct_context_t *"@

-- | Function pointer to 'gr_direct_context_make_direct3d_with_options'
foreign import ccall "&gr_direct_context_make_direct3d_with_options" p'gr_direct_context_make_direct3d_with_options ::
  FunPtr (Ptr (Gr_d3d_backendcontext) -> Ptr (Gr_context_options) -> IO (Ptr (Gr_direct_context)))

{- | C function signature:

@
_Bool gr_direct_context_is_abandoned(gr_direct_context_t *context)
@
-}
foreign import ccall "gr_direct_context_is_abandoned" gr_direct_context_is_abandoned ::
  Ptr (Gr_direct_context) -- ^ C argument @"gr_direct_context_t * context"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'gr_direct_context_is_abandoned'
foreign import ccall "&gr_direct_context_is_abandoned" p'gr_direct_context_is_abandoned ::
  FunPtr (Ptr (Gr_direct_context) -> IO (CBool))

{- | C function signature:

@
void gr_direct_context_abandon_context(gr_direct_context_t *context)
@
-}
foreign import ccall "gr_direct_context_abandon_context" gr_direct_context_abandon_context ::
  Ptr (Gr_direct_context) -- ^ C argument @"gr_direct_context_t * context"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_direct_context_abandon_context'
foreign import ccall "&gr_direct_context_abandon_context" p'gr_direct_context_abandon_context ::
  FunPtr (Ptr (Gr_direct_context) -> IO (()))

{- | C function signature:

@
void gr_direct_context_release_resources_and_abandon_context(gr_direct_context_t *context)
@
-}
foreign import ccall "gr_direct_context_release_resources_and_abandon_context" gr_direct_context_release_resources_and_abandon_context ::
  Ptr (Gr_direct_context) -- ^ C argument @"gr_direct_context_t * context"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_direct_context_release_resources_and_abandon_context'
foreign import ccall "&gr_direct_context_release_resources_and_abandon_context" p'gr_direct_context_release_resources_and_abandon_context ::
  FunPtr (Ptr (Gr_direct_context) -> IO (()))

{- | C function signature:

@
size_t gr_direct_context_get_resource_cache_limit(gr_direct_context_t *context)
@
-}
foreign import ccall "gr_direct_context_get_resource_cache_limit" gr_direct_context_get_resource_cache_limit ::
  Ptr (Gr_direct_context) -- ^ C argument @"gr_direct_context_t * context"@
  -> IO (CSize) -- ^ C return type: @"size_t"@

-- | Function pointer to 'gr_direct_context_get_resource_cache_limit'
foreign import ccall "&gr_direct_context_get_resource_cache_limit" p'gr_direct_context_get_resource_cache_limit ::
  FunPtr (Ptr (Gr_direct_context) -> IO (CSize))

{- | C function signature:

@
void gr_direct_context_set_resource_cache_limit(gr_direct_context_t *context, size_t maxResourceBytes)
@
-}
foreign import ccall "gr_direct_context_set_resource_cache_limit" gr_direct_context_set_resource_cache_limit ::
  Ptr (Gr_direct_context) -- ^ C argument @"gr_direct_context_t * context"@
  -> CSize -- ^ C argument @"size_t maxResourceBytes"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_direct_context_set_resource_cache_limit'
foreign import ccall "&gr_direct_context_set_resource_cache_limit" p'gr_direct_context_set_resource_cache_limit ::
  FunPtr (Ptr (Gr_direct_context) -> CSize -> IO (()))

{- | C function signature:

@
void gr_direct_context_get_resource_cache_usage(gr_direct_context_t *context, int *maxResources, size_t *maxResourceBytes)
@
-}
foreign import ccall "gr_direct_context_get_resource_cache_usage" gr_direct_context_get_resource_cache_usage ::
  Ptr (Gr_direct_context) -- ^ C argument @"gr_direct_context_t * context"@
  -> Ptr (CInt) -- ^ C argument @"int * maxResources"@
  -> Ptr (CSize) -- ^ C argument @"size_t * maxResourceBytes"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_direct_context_get_resource_cache_usage'
foreign import ccall "&gr_direct_context_get_resource_cache_usage" p'gr_direct_context_get_resource_cache_usage ::
  FunPtr (Ptr (Gr_direct_context) -> Ptr (CInt) -> Ptr (CSize) -> IO (()))

{- | C function signature:

@
void gr_direct_context_flush(gr_direct_context_t *context)
@
-}
foreign import ccall "gr_direct_context_flush" gr_direct_context_flush ::
  Ptr (Gr_direct_context) -- ^ C argument @"gr_direct_context_t * context"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_direct_context_flush'
foreign import ccall "&gr_direct_context_flush" p'gr_direct_context_flush ::
  FunPtr (Ptr (Gr_direct_context) -> IO (()))

{- | C function signature:

@
_Bool gr_direct_context_submit(gr_direct_context_t *context, _Bool syncCpu)
@
-}
foreign import ccall "gr_direct_context_submit" gr_direct_context_submit ::
  Ptr (Gr_direct_context) -- ^ C argument @"gr_direct_context_t * context"@
  -> CBool -- ^ C argument @"_Bool syncCpu"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'gr_direct_context_submit'
foreign import ccall "&gr_direct_context_submit" p'gr_direct_context_submit ::
  FunPtr (Ptr (Gr_direct_context) -> CBool -> IO (CBool))

{- | C function signature:

@
void gr_direct_context_flush_and_submit(gr_direct_context_t *context, _Bool syncCpu)
@
-}
foreign import ccall "gr_direct_context_flush_and_submit" gr_direct_context_flush_and_submit ::
  Ptr (Gr_direct_context) -- ^ C argument @"gr_direct_context_t * context"@
  -> CBool -- ^ C argument @"_Bool syncCpu"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_direct_context_flush_and_submit'
foreign import ccall "&gr_direct_context_flush_and_submit" p'gr_direct_context_flush_and_submit ::
  FunPtr (Ptr (Gr_direct_context) -> CBool -> IO (()))

{- | C function signature:

@
void gr_direct_context_flush_image(gr_direct_context_t *context, const sk_image_t *image)
@
-}
foreign import ccall "gr_direct_context_flush_image" gr_direct_context_flush_image ::
  Ptr (Gr_direct_context) -- ^ C argument @"gr_direct_context_t * context"@
  -> Ptr (Sk_image) -- ^ C argument @"const sk_image_t * image"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_direct_context_flush_image'
foreign import ccall "&gr_direct_context_flush_image" p'gr_direct_context_flush_image ::
  FunPtr (Ptr (Gr_direct_context) -> Ptr (Sk_image) -> IO (()))

{- | C function signature:

@
void gr_direct_context_flush_surface(gr_direct_context_t *context, sk_surface_t *surface)
@
-}
foreign import ccall "gr_direct_context_flush_surface" gr_direct_context_flush_surface ::
  Ptr (Gr_direct_context) -- ^ C argument @"gr_direct_context_t * context"@
  -> Ptr (Sk_surface) -- ^ C argument @"sk_surface_t * surface"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_direct_context_flush_surface'
foreign import ccall "&gr_direct_context_flush_surface" p'gr_direct_context_flush_surface ::
  FunPtr (Ptr (Gr_direct_context) -> Ptr (Sk_surface) -> IO (()))

{- | C function signature:

@
void gr_direct_context_reset_context(gr_direct_context_t *context, uint32_t state)
@
-}
foreign import ccall "gr_direct_context_reset_context" gr_direct_context_reset_context ::
  Ptr (Gr_direct_context) -- ^ C argument @"gr_direct_context_t * context"@
  -> Word32 -- ^ C argument @"uint32_t state"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_direct_context_reset_context'
foreign import ccall "&gr_direct_context_reset_context" p'gr_direct_context_reset_context ::
  FunPtr (Ptr (Gr_direct_context) -> Word32 -> IO (()))

{- | C function signature:

@
void gr_direct_context_dump_memory_statistics(const gr_direct_context_t *context, sk_tracememorydump_t *dump)
@
-}
foreign import ccall "gr_direct_context_dump_memory_statistics" gr_direct_context_dump_memory_statistics ::
  Ptr (Gr_direct_context) -- ^ C argument @"const gr_direct_context_t * context"@
  -> Ptr (Sk_tracememorydump) -- ^ C argument @"sk_tracememorydump_t * dump"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_direct_context_dump_memory_statistics'
foreign import ccall "&gr_direct_context_dump_memory_statistics" p'gr_direct_context_dump_memory_statistics ::
  FunPtr (Ptr (Gr_direct_context) -> Ptr (Sk_tracememorydump) -> IO (()))

{- | C function signature:

@
void gr_direct_context_free_gpu_resources(gr_direct_context_t *context)
@
-}
foreign import ccall "gr_direct_context_free_gpu_resources" gr_direct_context_free_gpu_resources ::
  Ptr (Gr_direct_context) -- ^ C argument @"gr_direct_context_t * context"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_direct_context_free_gpu_resources'
foreign import ccall "&gr_direct_context_free_gpu_resources" p'gr_direct_context_free_gpu_resources ::
  FunPtr (Ptr (Gr_direct_context) -> IO (()))

{- | C function signature:

@
void gr_direct_context_perform_deferred_cleanup(gr_direct_context_t *context, long long ms)
@
-}
foreign import ccall "gr_direct_context_perform_deferred_cleanup" gr_direct_context_perform_deferred_cleanup ::
  Ptr (Gr_direct_context) -- ^ C argument @"gr_direct_context_t * context"@
  -> CLLong -- ^ C argument @"long long ms"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_direct_context_perform_deferred_cleanup'
foreign import ccall "&gr_direct_context_perform_deferred_cleanup" p'gr_direct_context_perform_deferred_cleanup ::
  FunPtr (Ptr (Gr_direct_context) -> CLLong -> IO (()))

{- | C function signature:

@
void gr_direct_context_purge_unlocked_resources_bytes(gr_direct_context_t *context, size_t bytesToPurge, _Bool preferScratchResources)
@
-}
foreign import ccall "gr_direct_context_purge_unlocked_resources_bytes" gr_direct_context_purge_unlocked_resources_bytes ::
  Ptr (Gr_direct_context) -- ^ C argument @"gr_direct_context_t * context"@
  -> CSize -- ^ C argument @"size_t bytesToPurge"@
  -> CBool -- ^ C argument @"_Bool preferScratchResources"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_direct_context_purge_unlocked_resources_bytes'
foreign import ccall "&gr_direct_context_purge_unlocked_resources_bytes" p'gr_direct_context_purge_unlocked_resources_bytes ::
  FunPtr (Ptr (Gr_direct_context) -> CSize -> CBool -> IO (()))

{- | C function signature:

@
void gr_direct_context_purge_unlocked_resources(gr_direct_context_t *context, _Bool scratchResourcesOnly)
@
-}
foreign import ccall "gr_direct_context_purge_unlocked_resources" gr_direct_context_purge_unlocked_resources ::
  Ptr (Gr_direct_context) -- ^ C argument @"gr_direct_context_t * context"@
  -> CBool -- ^ C argument @"_Bool scratchResourcesOnly"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_direct_context_purge_unlocked_resources'
foreign import ccall "&gr_direct_context_purge_unlocked_resources" p'gr_direct_context_purge_unlocked_resources ::
  FunPtr (Ptr (Gr_direct_context) -> CBool -> IO (()))

{- | C function signature:

@
const gr_glinterface_t *gr_glinterface_create_native_interface(void)
@
-}
foreign import ccall "gr_glinterface_create_native_interface" gr_glinterface_create_native_interface ::
  IO (Ptr (Gr_glinterface)) -- ^ C return type: @"const gr_glinterface_t *"@

-- | Function pointer to 'gr_glinterface_create_native_interface'
foreign import ccall "&gr_glinterface_create_native_interface" p'gr_glinterface_create_native_interface ::
  FunPtr (IO (Ptr (Gr_glinterface)))

{- | C function signature:

@
const gr_glinterface_t *gr_glinterface_assemble_interface(void *ctx, gr_gl_get_proc get)
@
-}
foreign import ccall "gr_glinterface_assemble_interface" gr_glinterface_assemble_interface ::
  Ptr (()) -- ^ C argument @"void * ctx"@
  -> FunPtr Gr_gl_get_proc -- ^ C argument @"gr_gl_get_proc get"@
  -> IO (Ptr (Gr_glinterface)) -- ^ C return type: @"const gr_glinterface_t *"@

-- | Function pointer to 'gr_glinterface_assemble_interface'
foreign import ccall "&gr_glinterface_assemble_interface" p'gr_glinterface_assemble_interface ::
  FunPtr (Ptr (()) -> FunPtr Gr_gl_get_proc -> IO (Ptr (Gr_glinterface)))

{- | C function signature:

@
const gr_glinterface_t *gr_glinterface_assemble_gl_interface(void *ctx, gr_gl_get_proc get)
@
-}
foreign import ccall "gr_glinterface_assemble_gl_interface" gr_glinterface_assemble_gl_interface ::
  Ptr (()) -- ^ C argument @"void * ctx"@
  -> FunPtr Gr_gl_get_proc -- ^ C argument @"gr_gl_get_proc get"@
  -> IO (Ptr (Gr_glinterface)) -- ^ C return type: @"const gr_glinterface_t *"@

-- | Function pointer to 'gr_glinterface_assemble_gl_interface'
foreign import ccall "&gr_glinterface_assemble_gl_interface" p'gr_glinterface_assemble_gl_interface ::
  FunPtr (Ptr (()) -> FunPtr Gr_gl_get_proc -> IO (Ptr (Gr_glinterface)))

{- | C function signature:

@
const gr_glinterface_t *gr_glinterface_assemble_gles_interface(void *ctx, gr_gl_get_proc get)
@
-}
foreign import ccall "gr_glinterface_assemble_gles_interface" gr_glinterface_assemble_gles_interface ::
  Ptr (()) -- ^ C argument @"void * ctx"@
  -> FunPtr Gr_gl_get_proc -- ^ C argument @"gr_gl_get_proc get"@
  -> IO (Ptr (Gr_glinterface)) -- ^ C return type: @"const gr_glinterface_t *"@

-- | Function pointer to 'gr_glinterface_assemble_gles_interface'
foreign import ccall "&gr_glinterface_assemble_gles_interface" p'gr_glinterface_assemble_gles_interface ::
  FunPtr (Ptr (()) -> FunPtr Gr_gl_get_proc -> IO (Ptr (Gr_glinterface)))

{- | C function signature:

@
const gr_glinterface_t *gr_glinterface_assemble_webgl_interface(void *ctx, gr_gl_get_proc get)
@
-}
foreign import ccall "gr_glinterface_assemble_webgl_interface" gr_glinterface_assemble_webgl_interface ::
  Ptr (()) -- ^ C argument @"void * ctx"@
  -> FunPtr Gr_gl_get_proc -- ^ C argument @"gr_gl_get_proc get"@
  -> IO (Ptr (Gr_glinterface)) -- ^ C return type: @"const gr_glinterface_t *"@

-- | Function pointer to 'gr_glinterface_assemble_webgl_interface'
foreign import ccall "&gr_glinterface_assemble_webgl_interface" p'gr_glinterface_assemble_webgl_interface ::
  FunPtr (Ptr (()) -> FunPtr Gr_gl_get_proc -> IO (Ptr (Gr_glinterface)))

{- | C function signature:

@
void gr_glinterface_unref(const gr_glinterface_t *glInterface)
@
-}
foreign import ccall "gr_glinterface_unref" gr_glinterface_unref ::
  Ptr (Gr_glinterface) -- ^ C argument @"const gr_glinterface_t * glInterface"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_glinterface_unref'
foreign import ccall "&gr_glinterface_unref" p'gr_glinterface_unref ::
  FunPtr (Ptr (Gr_glinterface) -> IO (()))

{- | C function signature:

@
_Bool gr_glinterface_validate(const gr_glinterface_t *glInterface)
@
-}
foreign import ccall "gr_glinterface_validate" gr_glinterface_validate ::
  Ptr (Gr_glinterface) -- ^ C argument @"const gr_glinterface_t * glInterface"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'gr_glinterface_validate'
foreign import ccall "&gr_glinterface_validate" p'gr_glinterface_validate ::
  FunPtr (Ptr (Gr_glinterface) -> IO (CBool))

{- | C function signature:

@
_Bool gr_glinterface_has_extension(const gr_glinterface_t *glInterface, const char *extension)
@
-}
foreign import ccall "gr_glinterface_has_extension" gr_glinterface_has_extension ::
  Ptr (Gr_glinterface) -- ^ C argument @"const gr_glinterface_t * glInterface"@
  -> Ptr (CChar) -- ^ C argument @"const char * extension"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'gr_glinterface_has_extension'
foreign import ccall "&gr_glinterface_has_extension" p'gr_glinterface_has_extension ::
  FunPtr (Ptr (Gr_glinterface) -> Ptr (CChar) -> IO (CBool))

{- | C function signature:

@
gr_vk_extensions_t *gr_vk_extensions_new(void)
@
-}
foreign import ccall "gr_vk_extensions_new" gr_vk_extensions_new ::
  IO (Ptr (Gr_vk_extensions)) -- ^ C return type: @"gr_vk_extensions_t *"@

-- | Function pointer to 'gr_vk_extensions_new'
foreign import ccall "&gr_vk_extensions_new" p'gr_vk_extensions_new ::
  FunPtr (IO (Ptr (Gr_vk_extensions)))

{- | C function signature:

@
void gr_vk_extensions_delete(gr_vk_extensions_t *extensions)
@
-}
foreign import ccall "gr_vk_extensions_delete" gr_vk_extensions_delete ::
  Ptr (Gr_vk_extensions) -- ^ C argument @"gr_vk_extensions_t * extensions"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_vk_extensions_delete'
foreign import ccall "&gr_vk_extensions_delete" p'gr_vk_extensions_delete ::
  FunPtr (Ptr (Gr_vk_extensions) -> IO (()))

{- | C function signature:

@
void gr_vk_extensions_init(gr_vk_extensions_t *extensions, gr_vk_get_proc getProc, void *userData, vk_instance_t *instance, vk_physical_device_t *physDev, uint32_t instanceExtensionCount, const char **instanceExtensions, uint32_t deviceExtensionCount, const char **deviceExtensions)
@
-}
foreign import ccall "gr_vk_extensions_init" gr_vk_extensions_init ::
  Ptr (Gr_vk_extensions) -- ^ C argument @"gr_vk_extensions_t * extensions"@
  -> FunPtr Gr_vk_get_proc -- ^ C argument @"gr_vk_get_proc getProc"@
  -> Ptr (()) -- ^ C argument @"void * userData"@
  -> Ptr (Vk_instance) -- ^ C argument @"vk_instance_t * instance"@
  -> Ptr (Vk_physical_device) -- ^ C argument @"vk_physical_device_t * physDev"@
  -> Word32 -- ^ C argument @"uint32_t instanceExtensionCount"@
  -> Ptr (Ptr (CChar)) -- ^ C argument @"const char ** instanceExtensions"@
  -> Word32 -- ^ C argument @"uint32_t deviceExtensionCount"@
  -> Ptr (Ptr (CChar)) -- ^ C argument @"const char ** deviceExtensions"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_vk_extensions_init'
foreign import ccall "&gr_vk_extensions_init" p'gr_vk_extensions_init ::
  FunPtr (Ptr (Gr_vk_extensions) -> FunPtr Gr_vk_get_proc -> Ptr (()) -> Ptr (Vk_instance) -> Ptr (Vk_physical_device) -> Word32 -> Ptr (Ptr (CChar)) -> Word32 -> Ptr (Ptr (CChar)) -> IO (()))

{- | C function signature:

@
_Bool gr_vk_extensions_has_extension(gr_vk_extensions_t *extensions, const char *ext, uint32_t minVersion)
@
-}
foreign import ccall "gr_vk_extensions_has_extension" gr_vk_extensions_has_extension ::
  Ptr (Gr_vk_extensions) -- ^ C argument @"gr_vk_extensions_t * extensions"@
  -> Ptr (CChar) -- ^ C argument @"const char * ext"@
  -> Word32 -- ^ C argument @"uint32_t minVersion"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'gr_vk_extensions_has_extension'
foreign import ccall "&gr_vk_extensions_has_extension" p'gr_vk_extensions_has_extension ::
  FunPtr (Ptr (Gr_vk_extensions) -> Ptr (CChar) -> Word32 -> IO (CBool))

{- | C function signature:

@
gr_backendtexture_t *gr_backendtexture_new_gl(int width, int height, _Bool mipmapped, const gr_gl_textureinfo_t *glInfo)
@
-}
foreign import ccall "gr_backendtexture_new_gl" gr_backendtexture_new_gl ::
  CInt -- ^ C argument @"int width"@
  -> CInt -- ^ C argument @"int height"@
  -> CBool -- ^ C argument @"_Bool mipmapped"@
  -> Ptr (Gr_gl_textureinfo) -- ^ C argument @"const gr_gl_textureinfo_t * glInfo"@
  -> IO (Ptr (Gr_backendtexture)) -- ^ C return type: @"gr_backendtexture_t *"@

-- | Function pointer to 'gr_backendtexture_new_gl'
foreign import ccall "&gr_backendtexture_new_gl" p'gr_backendtexture_new_gl ::
  FunPtr (CInt -> CInt -> CBool -> Ptr (Gr_gl_textureinfo) -> IO (Ptr (Gr_backendtexture)))

{- | C function signature:

@
gr_backendtexture_t *gr_backendtexture_new_vulkan(int width, int height, const gr_vk_imageinfo_t *vkInfo)
@
-}
foreign import ccall "gr_backendtexture_new_vulkan" gr_backendtexture_new_vulkan ::
  CInt -- ^ C argument @"int width"@
  -> CInt -- ^ C argument @"int height"@
  -> Ptr (Gr_vk_imageinfo) -- ^ C argument @"const gr_vk_imageinfo_t * vkInfo"@
  -> IO (Ptr (Gr_backendtexture)) -- ^ C return type: @"gr_backendtexture_t *"@

-- | Function pointer to 'gr_backendtexture_new_vulkan'
foreign import ccall "&gr_backendtexture_new_vulkan" p'gr_backendtexture_new_vulkan ::
  FunPtr (CInt -> CInt -> Ptr (Gr_vk_imageinfo) -> IO (Ptr (Gr_backendtexture)))

{- | C function signature:

@
gr_backendtexture_t *gr_backendtexture_new_metal(int width, int height, _Bool mipmapped, const gr_mtl_textureinfo_t *mtlInfo)
@
-}
foreign import ccall "gr_backendtexture_new_metal" gr_backendtexture_new_metal ::
  CInt -- ^ C argument @"int width"@
  -> CInt -- ^ C argument @"int height"@
  -> CBool -- ^ C argument @"_Bool mipmapped"@
  -> Ptr (Gr_mtl_textureinfo) -- ^ C argument @"const gr_mtl_textureinfo_t * mtlInfo"@
  -> IO (Ptr (Gr_backendtexture)) -- ^ C return type: @"gr_backendtexture_t *"@

-- | Function pointer to 'gr_backendtexture_new_metal'
foreign import ccall "&gr_backendtexture_new_metal" p'gr_backendtexture_new_metal ::
  FunPtr (CInt -> CInt -> CBool -> Ptr (Gr_mtl_textureinfo) -> IO (Ptr (Gr_backendtexture)))

{- | C function signature:

@
gr_backendtexture_t *gr_backendtexture_new_direct3d(int width, int height, const gr_d3d_textureresourceinfo_t *d3dInfo)
@
-}
foreign import ccall "gr_backendtexture_new_direct3d" gr_backendtexture_new_direct3d ::
  CInt -- ^ C argument @"int width"@
  -> CInt -- ^ C argument @"int height"@
  -> Ptr (Gr_d3d_textureresourceinfo) -- ^ C argument @"const gr_d3d_textureresourceinfo_t * d3dInfo"@
  -> IO (Ptr (Gr_backendtexture)) -- ^ C return type: @"gr_backendtexture_t *"@

-- | Function pointer to 'gr_backendtexture_new_direct3d'
foreign import ccall "&gr_backendtexture_new_direct3d" p'gr_backendtexture_new_direct3d ::
  FunPtr (CInt -> CInt -> Ptr (Gr_d3d_textureresourceinfo) -> IO (Ptr (Gr_backendtexture)))

{- | C function signature:

@
void gr_backendtexture_delete(gr_backendtexture_t *texture)
@
-}
foreign import ccall "gr_backendtexture_delete" gr_backendtexture_delete ::
  Ptr (Gr_backendtexture) -- ^ C argument @"gr_backendtexture_t * texture"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_backendtexture_delete'
foreign import ccall "&gr_backendtexture_delete" p'gr_backendtexture_delete ::
  FunPtr (Ptr (Gr_backendtexture) -> IO (()))

{- | C function signature:

@
_Bool gr_backendtexture_is_valid(const gr_backendtexture_t *texture)
@
-}
foreign import ccall "gr_backendtexture_is_valid" gr_backendtexture_is_valid ::
  Ptr (Gr_backendtexture) -- ^ C argument @"const gr_backendtexture_t * texture"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'gr_backendtexture_is_valid'
foreign import ccall "&gr_backendtexture_is_valid" p'gr_backendtexture_is_valid ::
  FunPtr (Ptr (Gr_backendtexture) -> IO (CBool))

{- | C function signature:

@
int gr_backendtexture_get_width(const gr_backendtexture_t *texture)
@
-}
foreign import ccall "gr_backendtexture_get_width" gr_backendtexture_get_width ::
  Ptr (Gr_backendtexture) -- ^ C argument @"const gr_backendtexture_t * texture"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'gr_backendtexture_get_width'
foreign import ccall "&gr_backendtexture_get_width" p'gr_backendtexture_get_width ::
  FunPtr (Ptr (Gr_backendtexture) -> IO (CInt))

{- | C function signature:

@
int gr_backendtexture_get_height(const gr_backendtexture_t *texture)
@
-}
foreign import ccall "gr_backendtexture_get_height" gr_backendtexture_get_height ::
  Ptr (Gr_backendtexture) -- ^ C argument @"const gr_backendtexture_t * texture"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'gr_backendtexture_get_height'
foreign import ccall "&gr_backendtexture_get_height" p'gr_backendtexture_get_height ::
  FunPtr (Ptr (Gr_backendtexture) -> IO (CInt))

{- | C function signature:

@
_Bool gr_backendtexture_has_mipmaps(const gr_backendtexture_t *texture)
@
-}
foreign import ccall "gr_backendtexture_has_mipmaps" gr_backendtexture_has_mipmaps ::
  Ptr (Gr_backendtexture) -- ^ C argument @"const gr_backendtexture_t * texture"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'gr_backendtexture_has_mipmaps'
foreign import ccall "&gr_backendtexture_has_mipmaps" p'gr_backendtexture_has_mipmaps ::
  FunPtr (Ptr (Gr_backendtexture) -> IO (CBool))

{- | C function signature:

@
gr_backend_t gr_backendtexture_get_backend(const gr_backendtexture_t *texture)
@
-}
foreign import ccall "gr_backendtexture_get_backend" gr_backendtexture_get_backend ::
  Ptr (Gr_backendtexture) -- ^ C argument @"const gr_backendtexture_t * texture"@
  -> IO (Gr_backend) -- ^ C return type: @"gr_backend_t"@

-- | Function pointer to 'gr_backendtexture_get_backend'
foreign import ccall "&gr_backendtexture_get_backend" p'gr_backendtexture_get_backend ::
  FunPtr (Ptr (Gr_backendtexture) -> IO (Gr_backend))

{- | C function signature:

@
_Bool gr_backendtexture_get_gl_textureinfo(const gr_backendtexture_t *texture, gr_gl_textureinfo_t *glInfo)
@
-}
foreign import ccall "gr_backendtexture_get_gl_textureinfo" gr_backendtexture_get_gl_textureinfo ::
  Ptr (Gr_backendtexture) -- ^ C argument @"const gr_backendtexture_t * texture"@
  -> Ptr (Gr_gl_textureinfo) -- ^ C argument @"gr_gl_textureinfo_t * glInfo"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'gr_backendtexture_get_gl_textureinfo'
foreign import ccall "&gr_backendtexture_get_gl_textureinfo" p'gr_backendtexture_get_gl_textureinfo ::
  FunPtr (Ptr (Gr_backendtexture) -> Ptr (Gr_gl_textureinfo) -> IO (CBool))

{- | C function signature:

@
gr_backendrendertarget_t *gr_backendrendertarget_new_gl(int width, int height, int samples, int stencils, const gr_gl_framebufferinfo_t *glInfo)
@
-}
foreign import ccall "gr_backendrendertarget_new_gl" gr_backendrendertarget_new_gl ::
  CInt -- ^ C argument @"int width"@
  -> CInt -- ^ C argument @"int height"@
  -> CInt -- ^ C argument @"int samples"@
  -> CInt -- ^ C argument @"int stencils"@
  -> Ptr (Gr_gl_framebufferinfo) -- ^ C argument @"const gr_gl_framebufferinfo_t * glInfo"@
  -> IO (Ptr (Gr_backendrendertarget)) -- ^ C return type: @"gr_backendrendertarget_t *"@

-- | Function pointer to 'gr_backendrendertarget_new_gl'
foreign import ccall "&gr_backendrendertarget_new_gl" p'gr_backendrendertarget_new_gl ::
  FunPtr (CInt -> CInt -> CInt -> CInt -> Ptr (Gr_gl_framebufferinfo) -> IO (Ptr (Gr_backendrendertarget)))

{- | C function signature:

@
gr_backendrendertarget_t *gr_backendrendertarget_new_vulkan(int width, int height, const gr_vk_imageinfo_t *vkImageInfo)
@
-}
foreign import ccall "gr_backendrendertarget_new_vulkan" gr_backendrendertarget_new_vulkan ::
  CInt -- ^ C argument @"int width"@
  -> CInt -- ^ C argument @"int height"@
  -> Ptr (Gr_vk_imageinfo) -- ^ C argument @"const gr_vk_imageinfo_t * vkImageInfo"@
  -> IO (Ptr (Gr_backendrendertarget)) -- ^ C return type: @"gr_backendrendertarget_t *"@

-- | Function pointer to 'gr_backendrendertarget_new_vulkan'
foreign import ccall "&gr_backendrendertarget_new_vulkan" p'gr_backendrendertarget_new_vulkan ::
  FunPtr (CInt -> CInt -> Ptr (Gr_vk_imageinfo) -> IO (Ptr (Gr_backendrendertarget)))

{- | C function signature:

@
gr_backendrendertarget_t *gr_backendrendertarget_new_metal(int width, int height, const gr_mtl_textureinfo_t *mtlInfo)
@
-}
foreign import ccall "gr_backendrendertarget_new_metal" gr_backendrendertarget_new_metal ::
  CInt -- ^ C argument @"int width"@
  -> CInt -- ^ C argument @"int height"@
  -> Ptr (Gr_mtl_textureinfo) -- ^ C argument @"const gr_mtl_textureinfo_t * mtlInfo"@
  -> IO (Ptr (Gr_backendrendertarget)) -- ^ C return type: @"gr_backendrendertarget_t *"@

-- | Function pointer to 'gr_backendrendertarget_new_metal'
foreign import ccall "&gr_backendrendertarget_new_metal" p'gr_backendrendertarget_new_metal ::
  FunPtr (CInt -> CInt -> Ptr (Gr_mtl_textureinfo) -> IO (Ptr (Gr_backendrendertarget)))

{- | C function signature:

@
gr_backendrendertarget_t *gr_backendrendertarget_new_direct3d(int width, int height, const gr_d3d_textureresourceinfo_t *d3dInfo)
@
-}
foreign import ccall "gr_backendrendertarget_new_direct3d" gr_backendrendertarget_new_direct3d ::
  CInt -- ^ C argument @"int width"@
  -> CInt -- ^ C argument @"int height"@
  -> Ptr (Gr_d3d_textureresourceinfo) -- ^ C argument @"const gr_d3d_textureresourceinfo_t * d3dInfo"@
  -> IO (Ptr (Gr_backendrendertarget)) -- ^ C return type: @"gr_backendrendertarget_t *"@

-- | Function pointer to 'gr_backendrendertarget_new_direct3d'
foreign import ccall "&gr_backendrendertarget_new_direct3d" p'gr_backendrendertarget_new_direct3d ::
  FunPtr (CInt -> CInt -> Ptr (Gr_d3d_textureresourceinfo) -> IO (Ptr (Gr_backendrendertarget)))

{- | C function signature:

@
void gr_backendrendertarget_delete(gr_backendrendertarget_t *rendertarget)
@
-}
foreign import ccall "gr_backendrendertarget_delete" gr_backendrendertarget_delete ::
  Ptr (Gr_backendrendertarget) -- ^ C argument @"gr_backendrendertarget_t * rendertarget"@
  -> IO (()) -- ^ C return type: @"void"@

-- | Function pointer to 'gr_backendrendertarget_delete'
foreign import ccall "&gr_backendrendertarget_delete" p'gr_backendrendertarget_delete ::
  FunPtr (Ptr (Gr_backendrendertarget) -> IO (()))

{- | C function signature:

@
_Bool gr_backendrendertarget_is_valid(const gr_backendrendertarget_t *rendertarget)
@
-}
foreign import ccall "gr_backendrendertarget_is_valid" gr_backendrendertarget_is_valid ::
  Ptr (Gr_backendrendertarget) -- ^ C argument @"const gr_backendrendertarget_t * rendertarget"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'gr_backendrendertarget_is_valid'
foreign import ccall "&gr_backendrendertarget_is_valid" p'gr_backendrendertarget_is_valid ::
  FunPtr (Ptr (Gr_backendrendertarget) -> IO (CBool))

{- | C function signature:

@
int gr_backendrendertarget_get_width(const gr_backendrendertarget_t *rendertarget)
@
-}
foreign import ccall "gr_backendrendertarget_get_width" gr_backendrendertarget_get_width ::
  Ptr (Gr_backendrendertarget) -- ^ C argument @"const gr_backendrendertarget_t * rendertarget"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'gr_backendrendertarget_get_width'
foreign import ccall "&gr_backendrendertarget_get_width" p'gr_backendrendertarget_get_width ::
  FunPtr (Ptr (Gr_backendrendertarget) -> IO (CInt))

{- | C function signature:

@
int gr_backendrendertarget_get_height(const gr_backendrendertarget_t *rendertarget)
@
-}
foreign import ccall "gr_backendrendertarget_get_height" gr_backendrendertarget_get_height ::
  Ptr (Gr_backendrendertarget) -- ^ C argument @"const gr_backendrendertarget_t * rendertarget"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'gr_backendrendertarget_get_height'
foreign import ccall "&gr_backendrendertarget_get_height" p'gr_backendrendertarget_get_height ::
  FunPtr (Ptr (Gr_backendrendertarget) -> IO (CInt))

{- | C function signature:

@
int gr_backendrendertarget_get_samples(const gr_backendrendertarget_t *rendertarget)
@
-}
foreign import ccall "gr_backendrendertarget_get_samples" gr_backendrendertarget_get_samples ::
  Ptr (Gr_backendrendertarget) -- ^ C argument @"const gr_backendrendertarget_t * rendertarget"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'gr_backendrendertarget_get_samples'
foreign import ccall "&gr_backendrendertarget_get_samples" p'gr_backendrendertarget_get_samples ::
  FunPtr (Ptr (Gr_backendrendertarget) -> IO (CInt))

{- | C function signature:

@
int gr_backendrendertarget_get_stencils(const gr_backendrendertarget_t *rendertarget)
@
-}
foreign import ccall "gr_backendrendertarget_get_stencils" gr_backendrendertarget_get_stencils ::
  Ptr (Gr_backendrendertarget) -- ^ C argument @"const gr_backendrendertarget_t * rendertarget"@
  -> IO (CInt) -- ^ C return type: @"int"@

-- | Function pointer to 'gr_backendrendertarget_get_stencils'
foreign import ccall "&gr_backendrendertarget_get_stencils" p'gr_backendrendertarget_get_stencils ::
  FunPtr (Ptr (Gr_backendrendertarget) -> IO (CInt))

{- | C function signature:

@
gr_backend_t gr_backendrendertarget_get_backend(const gr_backendrendertarget_t *rendertarget)
@
-}
foreign import ccall "gr_backendrendertarget_get_backend" gr_backendrendertarget_get_backend ::
  Ptr (Gr_backendrendertarget) -- ^ C argument @"const gr_backendrendertarget_t * rendertarget"@
  -> IO (Gr_backend) -- ^ C return type: @"gr_backend_t"@

-- | Function pointer to 'gr_backendrendertarget_get_backend'
foreign import ccall "&gr_backendrendertarget_get_backend" p'gr_backendrendertarget_get_backend ::
  FunPtr (Ptr (Gr_backendrendertarget) -> IO (Gr_backend))

{- | C function signature:

@
_Bool gr_backendrendertarget_get_gl_framebufferinfo(const gr_backendrendertarget_t *rendertarget, gr_gl_framebufferinfo_t *glInfo)
@
-}
foreign import ccall "gr_backendrendertarget_get_gl_framebufferinfo" gr_backendrendertarget_get_gl_framebufferinfo ::
  Ptr (Gr_backendrendertarget) -- ^ C argument @"const gr_backendrendertarget_t * rendertarget"@
  -> Ptr (Gr_gl_framebufferinfo) -- ^ C argument @"gr_gl_framebufferinfo_t * glInfo"@
  -> IO (CBool) -- ^ C return type: @"_Bool"@

-- | Function pointer to 'gr_backendrendertarget_get_gl_framebufferinfo'
foreign import ccall "&gr_backendrendertarget_get_gl_framebufferinfo" p'gr_backendrendertarget_get_gl_framebufferinfo ::
  FunPtr (Ptr (Gr_backendrendertarget) -> Ptr (Gr_gl_framebufferinfo) -> IO (CBool))
