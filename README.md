# blaze-html-qq

[![CI](https://github.com/ocramz/blaze-html-qq/actions/workflows/ci.yml/badge.svg)](https://github.com/ocramz/blaze-html-qq/actions/workflows/ci.yml)

QuasiQuoter for `blaze-html`. Parse inline HTML snippets at compile time and produce `MarkdownM` values, e.g. 

```haskell
{-# language QuasiQuotes #-}
import qualified Data.Text.Lazy as TL (Text)
import qualified Text.Blaze.Html.Renderer.Text as BT (renderHtml)

import Text.Blaze.HTML.QQ (blaze)

test :: TL.Text
test = BT.renderHtml [blaze|
<div>
  <h1 class="widget" style="potato">
  Hello!
  </h1>
</div>
|]
```

```
> test
"<div><h1 class=\"widget\" style=\"potato\">Hello!</h1></div>"
```
