#include "skia_capi/sksvg.h"

#include "common_include_pch.hpp"

const sksvg_dom_t* sksvg_dom_create(sk_stream_t *stream) {
    auto dom = SkSVGDOM::Builder()
        .make(*AsStream(stream))
        .release();
    return ToSkSVGDOM(dom);
}

void sksvg_dom_get_container_size(
    sksvg_dom_t* dom, 
    int32_t viewportWidth, 
    int32_t viewportHeight, 
    sk_scalar_t dpi,
    int32_t* dstWidth, 
    int32_t* dstHeight
) {
    SkSize viewportSize = SkSize::Make(viewportWidth, viewportHeight);
    SkSVGLengthContext lenctx(viewportSize, dpi);
    auto size = AsSkSVGDOM(dom)->getRoot()->intrinsicSize(lenctx);
    *dstWidth = size.fWidth;
    *dstHeight = size.fHeight;
}

void sksvg_dom_render(sksvg_dom_t* dom, sk_canvas_t* canvas) {
    AsSkSVGDOM(dom)->render(AsCanvas(canvas));
}
