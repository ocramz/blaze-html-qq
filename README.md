# blaze-html-qq

[![CI](https://github.com/ocramz/blaze-html-qq/actions/workflows/ci.yml/badge.svg)](https://github.com/ocramz/blaze-html-qq/actions/workflows/ci.yml)

QuasiQuoter for [`blaze-html`](https://hackage.haskell.org/package/blaze-html). Parse inline HTML snippets at compile time and produce `MarkupM` values, e.g. 

```haskell
{-# language QuasiQuotes #-}
-- blaze-html
import qualified Text.Blaze.Html.Renderer.Text as BT (renderHtml)
-- text
import qualified Data.Text.Lazy as TL (Text)


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

This can play well alongside server-side web frameworks such as [HTMX](https://htmx.org/) and [`scotty`](https://hackage.haskell.org/package/scotty).
