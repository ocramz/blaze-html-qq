{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# options_ghc -Wno-unused-imports #-}
module Text.Blaze.HTML.QQ where

import Data.String (IsString(..))

-- html-conduit
import Text.HTML.DOM (parseLT)
import Text.XML (Document(..), Element(..), Name(..), Node(..))
-- template-haskell
import Language.Haskell.TH.Quote (QuasiQuoter(..))
-- text
import qualified Data.Text.Lazy as LT (Text)



h0 :: LT.Text
h0 = "<div class=\"col-12 uppercase\"><div class=\"uppercase\">Col-12</div></div>"


{-
<div class="row">
    <div class="col-12 uppercase">
        <div class="uppercase">Col-12</div>
    </div>
</div>
<div class="row">
    <div class="col-11 uppercase">
        <div class="uppercase">Col-11</div>
    </div>
    <div class="col-1 uppercase">
        <div class="uppercase">Col-1</div>
    </div>
</div>
<div class="row">
    <div class="col-10 uppercase">
        <div class="uppercase">Col-10</div>
    </div>
    <div class="col-2 uppercase">
        <div class="uppercase">Col-2</div>
    </div>
</div>
<div class="row">
    <div class="col-9 uppercase">
        <div class="uppercase">Col-9</div>
    </div>
    <div class="col-3 uppercase">
        <div class="uppercase">Col-3</div>
    </div>
</div>
<div class="row">
    <div class="col-8 uppercase">
        <div class="uppercase">Col-8</div>
    </div>
    <div class="col-4 uppercase">
        <div class="uppercase">Col-4</div>
    </div>
</div>
<div class="row">
    <div class="col-7 uppercase">
        <div class="uppercase">Col-7</div>
    </div>
    <div class="col-5 uppercase">
        <div class="uppercase">Col-5</div>
    </div>
</div>
<div class="row">
    <div class="col-6 uppercase">
        <div class="uppercase">Col-6</div>
    </div>
    <div class="col-6 uppercase">
        <div class="uppercase">Col-6</div>
    </div>
</div>
<div class="row">
    <div class="col-5 uppercase">
        <div class="uppercase">Col-5</div>
    </div>
    <div class="col-7 uppercase">
        <div class="uppercase">Col-7</div>
    </div>
</div>
<div class="row">
    <div class="col-4 uppercase">
        <div class="uppercase">Col-4</div>
    </div>
    <div class="col-8 uppercase">
        <div class="uppercase">Col-8</div>
    </div>
</div>
<div class="row">
    <div class="col-3 uppercase">
        <div class="uppercase">Col-3</div>
    </div>
    <div class="col-9 uppercase">
        <div class="uppercase">Col-9</div>
    </div>
</div>
<div class="row">
    <div class="col-2 uppercase">
        <div class="uppercase">Col-2</div>
    </div>
    <div class="col-10 uppercase">
        <div class="uppercase">Col-10</div>
    </div>
</div>
<div class="row">
    <div class="col-1 uppercase">
        <div class="uppercase">Col-1</div>
    </div>
    <div class="col-11 uppercase">
        <div class="uppercase">Col-11</div>
    </div>
</div>
-}
