#pragma once

#include "sk_types.h"

SK_C_PLUS_PLUS_BEGIN_GUARD

SK_C_API bool skunicode_unicode_is_supported_icu(void);
SK_C_API bool skunicode_unicode_is_supported_icu4x(void);
SK_C_API bool skunicode_unicode_is_supported_libgrapheme(void);

SK_C_API const skunicode_skunicode_t *skunicode_unicode_new_icu(void);
SK_C_API const skunicode_skunicode_t *skunicode_unicode_new_icu4x(void);
SK_C_API const skunicode_skunicode_t *skunicode_unicode_new_libgrapheme(void);

SK_C_PLUS_PLUS_END_GUARD
