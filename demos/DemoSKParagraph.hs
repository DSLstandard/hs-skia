module Main where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Text qualified as T
import Graphics.UI.GLFW qualified as GLFW
import Linear
import NeatInterpolation
import SharedUtils.MakeDemoApp
import Skia.Color
import Skia.Rect
import Skia.Enums
import Skia.SKCanvas qualified as SKCanvas
import Skia.SKFontManager qualified as SKFontManager
import Skia.SKPaint qualified as SKPaint
import Skia.SKParagraph.Enums as SKParagraph
import Skia.SKParagraph.FontCollection qualified as FontCollection
import Skia.SKParagraph.Objects
import Skia.SKParagraph.Paragraph qualified as Paragraph
import Skia.SKParagraph.ParagraphBuilder qualified as ParagraphBuilder
import Skia.SKParagraph.ParagraphStyle qualified as ParagraphStyle
import Skia.SKParagraph.TextStyle qualified as TextStyle
import Skia.SKUnicode qualified as SKUnicode

{-
#########################################################
NOTE: The code here somewhat follows the code in the following links/files:
- https://skia.googlesource.com/skia/+/7a1bf999357aa755768f7b72265b91eea5c2758c/samplecode/SampleParagraph.cpp
- Google Skia m129's example/external_client/src/shape_text.cpp
#########################################################
-}

-- Generated with https://lorem-ipsum-multilingual.ahmedtokyo.com/ and https://lipsum.sugutsukaeru.jp/index.cgi.
multilingualLoremIpsum :: T.Text
multilingualLoremIpsum =
  [trimming|
    Sed nulla deserunt fugiat elit sunt commodo. Id qui do nulla ut id id. Et dolore enim ad aliqua voluptate aute et cupidatat cupidatat.

    Adipiscing id sint irure laborum veniam deserunt ea eiusmod pariatur dolor eiusmod qui. Nulla est nulla nostrud ullamco. Minim sed incididunt aliqua eu ad proident. Id consequat sunt id officia exercitation sint tempor consectetur quis ad.

    او زیرا چه درباره هم هیچ مگر. قبل شما چرا اندک نزدیک همه مانند او. بعد ثانیه آسان دیروز همیشه فردا ما. هفته دیگر ثانیه بلند بلکه کوچک نزدیک امروز چه کسی خود.  سریع دقیقه ما گاهی پیش هرگز آن بزرگ مانند به ندرت یک آن امروز بد.  البته آن نزدیک هرگز خود بلند یک روز چه اکنون ساعت. کدام خوب هر من کرد بدون شد.  خود سال هیچ اندک آسان یک.

    彼らもすべてできるだけこの約束らに従ってののために載せたない。よほど多数をお話しらもよほどそうした独立ませうなどにあるているにも尊重吹き込んんますが、あいにくにしか現われですたですた。

    がたでありましのはまるで多年をけっしてたありなり。けっして岡田さんを周旋事いろいろ創設を云っでしょ便所この人我々か懊悩がというお発展たないたないて、その今はそれか他人学校にして、嘉納君ののを習慣のあなたにむしろご存在と知れてみんな主義がご危くに読むようにたしかご意味にするなうと、すでに大分尊敬にきめあるていでものをなるないまし。
  |]

main :: IO ()
main = runDemoCanvasWindowApp "SKParagraph demo" (V2 800 600) \window obtainCanvas -> do
  -- Setup colors
  (_, bgPaint) <- SKPaint.create
  SKPaint.setColorRGBA bgPaint (RGBA 1.0 1.0 0.0 1.0) Nothing

  (_, fgPaint) <- SKPaint.create
  SKPaint.setColorRGBA fgPaint (RGBA 0.0 0.0 0.0 1.0) Nothing

  (_, rectPaint) <- SKPaint.create
  SKPaint.setColorRGBA fgPaint (RGBA 1.0 0.0 0.0 1.0) Nothing
  SKPaint.setStrokeWidth rectPaint 1
  SKPaint.setStyle rectPaint SKPaintStyle'Stroke

  (_, unicode) <- SKUnicode.createAutoDetect

  -- Setup paragraph
  (_, defaultStyle) <- TextStyle.createEmpty
  TextStyle.setBackgroundPaint defaultStyle bgPaint
  TextStyle.setForegroundPaint defaultStyle fgPaint
  TextStyle.setFontFamilies defaultStyle ["sans-serif"]
  TextStyle.setFontSize defaultStyle 16

  (_, paraStyle) <- ParagraphStyle.createEmpty
  (_, fontmgr) <- SKFontManager.createByFontconfig
  ParagraphStyle.setTextStyle paraStyle defaultStyle
  ParagraphStyle.setTextAlign paraStyle SKParagraph.TextAlign'Justify

  (_, fontcol) <- FontCollection.createEmpty
  FontCollection.setDefaultFontManager fontcol fontmgr []

  (_, builder) <- ParagraphBuilder.create paraStyle fontcol unicode

  ParagraphBuilder.addText builder multilingualLoremIpsum

  (_, para) <- ParagraphBuilder.build builder

  -- Main loop
  fix \loop -> do
    liftIO $ GLFW.pollEvents

    (canvas, flushCanvas) <- obtainCanvas

    (winW, winH) <- liftIO $ GLFW.getWindowSize window
    (mouseX, mouseY) <- liftIO $ GLFW.getCursorPos window

    runResourceT do
      SKCanvas.clearRGBA canvas (RGBA 1.0 1.0 1.0 1.0)

      Paragraph.layout para (fromIntegral winW)
      Just glyphInfo <- Paragraph.getClosestGlyphClusterAt para (V2 (realToFrac mouseX) (realToFrac mouseY))

      Paragraph.paintWithCanvas para canvas (V2 0 0)
      SKCanvas.drawRect canvas glyphInfo.bounds rectPaint

    liftIO $ flushCanvas
    liftIO $ GLFW.swapBuffers window

    shouldClose <- liftIO $ GLFW.windowShouldClose window
    unless shouldClose loop
