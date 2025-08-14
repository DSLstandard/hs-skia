#pragma once

#include "sk_types.h"

SK_C_PLUS_PLUS_BEGIN_GUARD

bool skfontmgr_is_supported_fontconfig(void);
sk_fontmgr_t* skfontmgr_new_fontconfig(void);

bool skfontmgr_is_supported_gdi(void);
sk_fontmgr_t* skfontmgr_new_gdi(void);

SK_C_PLUS_PLUS_END_GUARD