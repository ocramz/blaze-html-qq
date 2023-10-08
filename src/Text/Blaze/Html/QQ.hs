{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
module Text.Blaze.Html.QQ (blaze) where
import Text.Blaze.Html.QQ.Internal -- (blaze, documentQQ)

import qualified Data.Text.Lazy as TL (Text)

import qualified Text.Blaze.Html.Renderer.Text as BT (renderHtml)

a :: TL.Text
a = BT.renderHtml [blaze|
<div></div>|]

b :: TL.Text
b = BT.renderHtml [blaze|
<div>
  <h1 class="widget" style="potato">
  Hello!
  </h1>
</div>
|]

-- x = drawHTML "<div><h1 class=\"widget\">Hello World</h1></div>"
