module Skia.SkVersion where

import Skia.Internal.Prelude
import Language.C.Inline qualified as C

C.context $ C.baseCtx <> cppSkiaObjectTypes

C.include "core/SkMilestone.h"

-- | Skia milestone
milestone :: Int
milestone = fromIntegral [C.pure| int { SK_MILESTONE } |]
