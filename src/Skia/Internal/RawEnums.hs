module Skia.Internal.RawEnums where

import Foreign.C

-- | Skia constant @SkPaint::Style::kFill_Style@
pattern SKPAINT__STYLE__KFILL_STYLE :: CInt
pattern SKPAINT__STYLE__KFILL_STYLE = 0

-- | Skia constant @SkPaint::Style::kStroke_Style@
pattern SKPAINT__STYLE__KSTROKE_STYLE :: CInt
pattern SKPAINT__STYLE__KSTROKE_STYLE = 1

-- | Skia constant @SkPaint::Style::kStrokeAndFill_Style@
pattern SKPAINT__STYLE__KSTROKEANDFILL_STYLE :: CInt
pattern SKPAINT__STYLE__KSTROKEANDFILL_STYLE = 2

-- | Skia constant @SkPaint::Cap::kButt_Cap@
pattern SKPAINT__CAP__KBUTT_CAP :: CInt
pattern SKPAINT__CAP__KBUTT_CAP = 0

-- | Skia constant @SkPaint::Cap::kRound_Cap@
pattern SKPAINT__CAP__KROUND_CAP :: CInt
pattern SKPAINT__CAP__KROUND_CAP = 1

-- | Skia constant @SkPaint::Cap::kSquare_Cap@
pattern SKPAINT__CAP__KSQUARE_CAP :: CInt
pattern SKPAINT__CAP__KSQUARE_CAP = 2

-- | Skia constant @SkPaint::Join::kMiter_Join@
pattern SKPAINT__JOIN__KMITER_JOIN :: CInt
pattern SKPAINT__JOIN__KMITER_JOIN = 0

-- | Skia constant @SkPaint::Join::kRound_Join@
pattern SKPAINT__JOIN__KROUND_JOIN :: CInt
pattern SKPAINT__JOIN__KROUND_JOIN = 1

-- | Skia constant @SkPaint::Join::kBevel_Join@
pattern SKPAINT__JOIN__KBEVEL_JOIN :: CInt
pattern SKPAINT__JOIN__KBEVEL_JOIN = 2

-- | Skia constant @SkBlendMode::kClear@
pattern SKBLENDMODE__KCLEAR :: CInt
pattern SKBLENDMODE__KCLEAR = 0

-- | Skia constant @SkBlendMode::kSrc@
pattern SKBLENDMODE__KSRC :: CInt
pattern SKBLENDMODE__KSRC = 1

-- | Skia constant @SkBlendMode::kDst@
pattern SKBLENDMODE__KDST :: CInt
pattern SKBLENDMODE__KDST = 2

-- | Skia constant @SkBlendMode::kSrcOver@
pattern SKBLENDMODE__KSRCOVER :: CInt
pattern SKBLENDMODE__KSRCOVER = 3

-- | Skia constant @SkBlendMode::kDstOver@
pattern SKBLENDMODE__KDSTOVER :: CInt
pattern SKBLENDMODE__KDSTOVER = 4

-- | Skia constant @SkBlendMode::kSrcIn@
pattern SKBLENDMODE__KSRCIN :: CInt
pattern SKBLENDMODE__KSRCIN = 5

-- | Skia constant @SkBlendMode::kDstIn@
pattern SKBLENDMODE__KDSTIN :: CInt
pattern SKBLENDMODE__KDSTIN = 6

-- | Skia constant @SkBlendMode::kSrcOut@
pattern SKBLENDMODE__KSRCOUT :: CInt
pattern SKBLENDMODE__KSRCOUT = 7

-- | Skia constant @SkBlendMode::kDstOut@
pattern SKBLENDMODE__KDSTOUT :: CInt
pattern SKBLENDMODE__KDSTOUT = 8

-- | Skia constant @SkBlendMode::kSrcATop@
pattern SKBLENDMODE__KSRCATOP :: CInt
pattern SKBLENDMODE__KSRCATOP = 9

-- | Skia constant @SkBlendMode::kDstATop@
pattern SKBLENDMODE__KDSTATOP :: CInt
pattern SKBLENDMODE__KDSTATOP = 10

-- | Skia constant @SkBlendMode::kXor@
pattern SKBLENDMODE__KXOR :: CInt
pattern SKBLENDMODE__KXOR = 11

-- | Skia constant @SkBlendMode::kPlus@
pattern SKBLENDMODE__KPLUS :: CInt
pattern SKBLENDMODE__KPLUS = 12

-- | Skia constant @SkBlendMode::kModulate@
pattern SKBLENDMODE__KMODULATE :: CInt
pattern SKBLENDMODE__KMODULATE = 13

-- | Skia constant @SkBlendMode::kScreen@
pattern SKBLENDMODE__KSCREEN :: CInt
pattern SKBLENDMODE__KSCREEN = 14

-- | Skia constant @SkBlendMode::kOverlay@
pattern SKBLENDMODE__KOVERLAY :: CInt
pattern SKBLENDMODE__KOVERLAY = 15

-- | Skia constant @SkBlendMode::kDarken@
pattern SKBLENDMODE__KDARKEN :: CInt
pattern SKBLENDMODE__KDARKEN = 16

-- | Skia constant @SkBlendMode::kLighten@
pattern SKBLENDMODE__KLIGHTEN :: CInt
pattern SKBLENDMODE__KLIGHTEN = 17

-- | Skia constant @SkBlendMode::kColorDodge@
pattern SKBLENDMODE__KCOLORDODGE :: CInt
pattern SKBLENDMODE__KCOLORDODGE = 18

-- | Skia constant @SkBlendMode::kColorBurn@
pattern SKBLENDMODE__KCOLORBURN :: CInt
pattern SKBLENDMODE__KCOLORBURN = 19

-- | Skia constant @SkBlendMode::kHardLight@
pattern SKBLENDMODE__KHARDLIGHT :: CInt
pattern SKBLENDMODE__KHARDLIGHT = 20

-- | Skia constant @SkBlendMode::kSoftLight@
pattern SKBLENDMODE__KSOFTLIGHT :: CInt
pattern SKBLENDMODE__KSOFTLIGHT = 21

-- | Skia constant @SkBlendMode::kDifference@
pattern SKBLENDMODE__KDIFFERENCE :: CInt
pattern SKBLENDMODE__KDIFFERENCE = 22

-- | Skia constant @SkBlendMode::kExclusion@
pattern SKBLENDMODE__KEXCLUSION :: CInt
pattern SKBLENDMODE__KEXCLUSION = 23

-- | Skia constant @SkBlendMode::kMultiply@
pattern SKBLENDMODE__KMULTIPLY :: CInt
pattern SKBLENDMODE__KMULTIPLY = 24

-- | Skia constant @SkBlendMode::kHue@
pattern SKBLENDMODE__KHUE :: CInt
pattern SKBLENDMODE__KHUE = 25

-- | Skia constant @SkBlendMode::kSaturation@
pattern SKBLENDMODE__KSATURATION :: CInt
pattern SKBLENDMODE__KSATURATION = 26

-- | Skia constant @SkBlendMode::kColor@
pattern SKBLENDMODE__KCOLOR :: CInt
pattern SKBLENDMODE__KCOLOR = 27

-- | Skia constant @SkBlendMode::kLuminosity@
pattern SKBLENDMODE__KLUMINOSITY :: CInt
pattern SKBLENDMODE__KLUMINOSITY = 28

-- | Skia constant @SkPixelGeometry::kUnknown_SkPixelGeometry@
pattern SKPIXELGEOMETRY__KUNKNOWN_SKPIXELGEOMETRY :: CInt
pattern SKPIXELGEOMETRY__KUNKNOWN_SKPIXELGEOMETRY = 0

-- | Skia constant @SkPixelGeometry::kRGB_H_SkPixelGeometry@
pattern SKPIXELGEOMETRY__KRGB_H_SKPIXELGEOMETRY :: CInt
pattern SKPIXELGEOMETRY__KRGB_H_SKPIXELGEOMETRY = 1

-- | Skia constant @SkPixelGeometry::kBGR_H_SkPixelGeometry@
pattern SKPIXELGEOMETRY__KBGR_H_SKPIXELGEOMETRY :: CInt
pattern SKPIXELGEOMETRY__KBGR_H_SKPIXELGEOMETRY = 2

-- | Skia constant @SkPixelGeometry::kRGB_V_SkPixelGeometry@
pattern SKPIXELGEOMETRY__KRGB_V_SKPIXELGEOMETRY :: CInt
pattern SKPIXELGEOMETRY__KRGB_V_SKPIXELGEOMETRY = 3

-- | Skia constant @SkPixelGeometry::kBGR_V_SkPixelGeometry@
pattern SKPIXELGEOMETRY__KBGR_V_SKPIXELGEOMETRY :: CInt
pattern SKPIXELGEOMETRY__KBGR_V_SKPIXELGEOMETRY = 4

-- | Skia constant @SkAlphaType::kUnknown_SkAlphaType@
pattern SKALPHATYPE__KUNKNOWN_SKALPHATYPE :: CInt
pattern SKALPHATYPE__KUNKNOWN_SKALPHATYPE = 0

-- | Skia constant @SkAlphaType::kOpaque_SkAlphaType@
pattern SKALPHATYPE__KOPAQUE_SKALPHATYPE :: CInt
pattern SKALPHATYPE__KOPAQUE_SKALPHATYPE = 1

-- | Skia constant @SkAlphaType::kPremul_SkAlphaType@
pattern SKALPHATYPE__KPREMUL_SKALPHATYPE :: CInt
pattern SKALPHATYPE__KPREMUL_SKALPHATYPE = 2

-- | Skia constant @SkAlphaType::kUnpremul_SkAlphaType@
pattern SKALPHATYPE__KUNPREMUL_SKALPHATYPE :: CInt
pattern SKALPHATYPE__KUNPREMUL_SKALPHATYPE = 3

-- | Skia constant @SkColorType::kUnknown_SkColorType@
pattern SKCOLORTYPE__KUNKNOWN_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KUNKNOWN_SKCOLORTYPE = 0

-- | Skia constant @SkColorType::kAlpha_8_SkColorType@
pattern SKCOLORTYPE__KALPHA_8_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KALPHA_8_SKCOLORTYPE = 1

-- | Skia constant @SkColorType::kRGB_565_SkColorType@
pattern SKCOLORTYPE__KRGB_565_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KRGB_565_SKCOLORTYPE = 2

-- | Skia constant @SkColorType::kARGB_4444_SkColorType@
pattern SKCOLORTYPE__KARGB_4444_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KARGB_4444_SKCOLORTYPE = 3

-- | Skia constant @SkColorType::kRGBA_8888_SkColorType@
pattern SKCOLORTYPE__KRGBA_8888_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KRGBA_8888_SKCOLORTYPE = 4

-- | Skia constant @SkColorType::kRGB_888x_SkColorType@
pattern SKCOLORTYPE__KRGB_888X_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KRGB_888X_SKCOLORTYPE = 5

-- | Skia constant @SkColorType::kBGRA_8888_SkColorType@
pattern SKCOLORTYPE__KBGRA_8888_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KBGRA_8888_SKCOLORTYPE = 6

-- | Skia constant @SkColorType::kRGBA_1010102_SkColorType@
pattern SKCOLORTYPE__KRGBA_1010102_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KRGBA_1010102_SKCOLORTYPE = 7

-- | Skia constant @SkColorType::kBGRA_1010102_SkColorType@
pattern SKCOLORTYPE__KBGRA_1010102_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KBGRA_1010102_SKCOLORTYPE = 8

-- | Skia constant @SkColorType::kRGB_101010x_SkColorType@
pattern SKCOLORTYPE__KRGB_101010X_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KRGB_101010X_SKCOLORTYPE = 9

-- | Skia constant @SkColorType::kBGR_101010x_SkColorType@
pattern SKCOLORTYPE__KBGR_101010X_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KBGR_101010X_SKCOLORTYPE = 10

-- | Skia constant @SkColorType::kBGR_101010x_XR_SkColorType@
pattern SKCOLORTYPE__KBGR_101010X_XR_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KBGR_101010X_XR_SKCOLORTYPE = 11

-- | Skia constant @SkColorType::kBGRA_10101010_XR_SkColorType@
pattern SKCOLORTYPE__KBGRA_10101010_XR_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KBGRA_10101010_XR_SKCOLORTYPE = 12

-- | Skia constant @SkColorType::kRGBA_10x6_SkColorType@
pattern SKCOLORTYPE__KRGBA_10X6_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KRGBA_10X6_SKCOLORTYPE = 13

-- | Skia constant @SkColorType::kGray_8_SkColorType@
pattern SKCOLORTYPE__KGRAY_8_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KGRAY_8_SKCOLORTYPE = 14

-- | Skia constant @SkColorType::kRGBA_F16Norm_SkColorType@
pattern SKCOLORTYPE__KRGBA_F16NORM_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KRGBA_F16NORM_SKCOLORTYPE = 15

-- | Skia constant @SkColorType::kRGBA_F16_SkColorType@
pattern SKCOLORTYPE__KRGBA_F16_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KRGBA_F16_SKCOLORTYPE = 16

-- | Skia constant @SkColorType::kRGB_F16F16F16x_SkColorType@
pattern SKCOLORTYPE__KRGB_F16F16F16X_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KRGB_F16F16F16X_SKCOLORTYPE = 17

-- | Skia constant @SkColorType::kRGBA_F32_SkColorType@
pattern SKCOLORTYPE__KRGBA_F32_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KRGBA_F32_SKCOLORTYPE = 18

-- | Skia constant @SkColorType::kR8G8_unorm_SkColorType@
pattern SKCOLORTYPE__KR8G8_UNORM_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KR8G8_UNORM_SKCOLORTYPE = 19

-- | Skia constant @SkColorType::kA16_float_SkColorType@
pattern SKCOLORTYPE__KA16_FLOAT_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KA16_FLOAT_SKCOLORTYPE = 20

-- | Skia constant @SkColorType::kR16G16_float_SkColorType@
pattern SKCOLORTYPE__KR16G16_FLOAT_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KR16G16_FLOAT_SKCOLORTYPE = 21

-- | Skia constant @SkColorType::kA16_unorm_SkColorType@
pattern SKCOLORTYPE__KA16_UNORM_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KA16_UNORM_SKCOLORTYPE = 22

-- | Skia constant @SkColorType::kR16G16_unorm_SkColorType@
pattern SKCOLORTYPE__KR16G16_UNORM_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KR16G16_UNORM_SKCOLORTYPE = 23

-- | Skia constant @SkColorType::kR16G16B16A16_unorm_SkColorType@
pattern SKCOLORTYPE__KR16G16B16A16_UNORM_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KR16G16B16A16_UNORM_SKCOLORTYPE = 24

-- | Skia constant @SkColorType::kSRGBA_8888_SkColorType@
pattern SKCOLORTYPE__KSRGBA_8888_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KSRGBA_8888_SKCOLORTYPE = 25

-- | Skia constant @SkColorType::kR8_unorm_SkColorType@
pattern SKCOLORTYPE__KR8_UNORM_SKCOLORTYPE :: CInt
pattern SKCOLORTYPE__KR8_UNORM_SKCOLORTYPE = 26

-- | Skia constant @SkClipOp::kDifference@
pattern SKCLIPOP__KDIFFERENCE :: CInt
pattern SKCLIPOP__KDIFFERENCE = 0

-- | Skia constant @SkClipOp::kIntersect@
pattern SKCLIPOP__KINTERSECT :: CInt
pattern SKCLIPOP__KINTERSECT = 1

-- | Skia constant @SkClipOp::kMax_EnumValue@
pattern SKCLIPOP__KMAX_ENUMVALUE :: CInt
pattern SKCLIPOP__KMAX_ENUMVALUE = 1

-- | Skia constant @SkFilterMode::kNearest@
pattern SKFILTERMODE__KNEAREST :: CInt
pattern SKFILTERMODE__KNEAREST = 0

-- | Skia constant @SkFilterMode::kLinear@
pattern SKFILTERMODE__KLINEAR :: CInt
pattern SKFILTERMODE__KLINEAR = 1

-- | Skia constant @SkMipmapMode::kNone@
pattern SKMIPMAPMODE__KNONE :: CInt
pattern SKMIPMAPMODE__KNONE = 0

-- | Skia constant @SkMipmapMode::kNearest@
pattern SKMIPMAPMODE__KNEAREST :: CInt
pattern SKMIPMAPMODE__KNEAREST = 1

-- | Skia constant @SkMipmapMode::kLinear@
pattern SKMIPMAPMODE__KLINEAR :: CInt
pattern SKMIPMAPMODE__KLINEAR = 2

-- | Skia constant @GrSurfaceOrigin::kTopLeft_GrSurfaceOrigin@
pattern GRSURFACEORIGIN__KTOPLEFT_GRSURFACEORIGIN :: CInt
pattern GRSURFACEORIGIN__KTOPLEFT_GRSURFACEORIGIN = 0

-- | Skia constant @GrSurfaceOrigin::kBottomLeft_GrSurfaceOrigin@
pattern GRSURFACEORIGIN__KBOTTOMLEFT_GRSURFACEORIGIN :: CInt
pattern GRSURFACEORIGIN__KBOTTOMLEFT_GRSURFACEORIGIN = 1
