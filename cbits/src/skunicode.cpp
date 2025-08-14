#include "skia_capi/skunicode.h"

#include "common_include_pch.hpp"

// Using weak symbols as a trick to allow skunicode.cpp to be compiled with
// unsolved ::Make() references if Skia is not compiled to support the
// corresponding SkUnicode backend.

__attribute__((weak)) sk_sp<SkUnicode> SkUnicodes::ICU::Make();
__attribute__((weak)) sk_sp<SkUnicode> SkUnicodes::ICU4X::Make();
__attribute__((weak)) sk_sp<SkUnicode> SkUnicodes::Libgrapheme::Make();

bool skunicode_unicode_is_supported_icu(void) {
    return SkUnicodes::ICU::Make != nullptr;
}

bool skunicode_unicode_is_supported_icu4x(void) {
    return SkUnicodes::ICU4X::Make != nullptr;
}

bool skunicode_unicode_is_supported_libgrapheme(void) {
    return SkUnicodes::Libgrapheme::Make != nullptr;
}

const skunicode_skunicode_t *skunicode_unicode_new_icu(void) {
    return skunicode_unicode_is_supported_icu()
               ? ToSkUnicode(SkUnicodes::ICU::Make().release())
               : nullptr;
}

const skunicode_skunicode_t *skunicode_unicode_new_icu4x(void) {
    return skunicode_unicode_is_supported_icu4x()
               ? ToSkUnicode(SkUnicodes::ICU4X::Make().release())
               : nullptr;
}

const skunicode_skunicode_t *skunicode_unicode_new_libgrapheme(void) {
    return skunicode_unicode_is_supported_libgrapheme()
               ? ToSkUnicode(SkUnicodes::Libgrapheme::Make().release())
               : nullptr;
}