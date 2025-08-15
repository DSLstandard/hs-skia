#pragma once

#include "sk_types.h"

SK_C_PLUS_PLUS_BEGIN_GUARD

SK_C_API const sksvg_dom_t *sksvg_dom_create(sk_stream_t *stream);
SK_C_API void sksvg_dom_get_container_size(
    sksvg_dom_t* dom, 
    int32_t viewportWidth, 
    int32_t viewportHeight, 
    sk_scalar_t dpi,
    int32_t* dstWidth, 
    int32_t* dstHeight
);
SK_C_API void sksvg_dom_render(sksvg_dom_t *dom, sk_canvas_t *canvas);

SK_C_PLUS_PLUS_END_GUARD
