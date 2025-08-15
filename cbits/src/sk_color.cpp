// color type

#include "skia_capi/sk_color.h"

#include "common_include_pch.hpp"

sk_colortype_t sk_colortype_get_default_8888(void) {
    return (sk_colortype_t)SkColorType::kN32_SkColorType;
}