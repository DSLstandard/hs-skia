#include "skia_capi/sk_fontmgr.h"

#include "common_include_pch.hpp"

// NOTE: Using weak symbols as a trick to allow this .cpp to be compiled with
// unsolved ::SkFontMgr_New_[XYZ]() references if Skia is not compiled to
// support the corresponding SkFontMgr backend.

// ** include/ports/SkFontMgr_fontconfig.h

// Copied from
// https://github.com/servo/libfontconfig/blob/4e97940af72c90191594a688b21c381105aadca4/fontconfig/fontconfig.h#L289C1-L289C38
typedef struct _FcConfig FcConfig;

/** Create a font manager around a FontConfig instance.
 *  If 'fc' is NULL, will use a new default config.
 *  Takes ownership of 'fc' and will call FcConfigDestroy on it.
 */
__attribute__((weak)) sk_sp<SkFontMgr> SkFontMgr_New_FontConfig(FcConfig* fc);

bool skfontmgr_is_supported_fontconfig(void) {
    return SkFontMgr_New_FontConfig != nullptr;
}

sk_fontmgr_t* skfontmgr_new_fontconfig(void) {
    // TODO: Allow user to pass in FcConfig
    return skfontmgr_is_supported_fontconfig()
               ? ToFontMgr(SkFontMgr_New_FontConfig(nullptr).release())
               : nullptr;
}

// ** include/ports/SkTypeface_win.h

__attribute__((weak)) sk_sp<SkFontMgr> SkFontMgr_New_GDI();

bool skfontmgr_is_supported_gdi(void) { return SkFontMgr_New_GDI != nullptr; }

sk_fontmgr_t* skfontmgr_new_gdi(void) {
    return skfontmgr_is_supported_gdi()
               ? ToFontMgr(SkFontMgr_New_GDI().release())
               : nullptr;
}
