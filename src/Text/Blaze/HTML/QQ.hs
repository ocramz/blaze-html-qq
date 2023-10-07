{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# options_ghc -Wno-unused-imports #-}
module Text.Blaze.HTML.QQ where

import Data.String (IsString(..))

-- blaze-markup
import Text.Blaze.Internal (MarkupM(..), Attribute, AttributeValue, ChoiceString(..), Attributable(..), attribute, Tag(..))
-- containers
import qualified Data.Map as M (Map, foldrWithKey)
-- html-conduit
import Text.HTML.DOM (parseLT)
import Text.XML (Document(..), Element(..), Name(..), Node(..))
-- template-haskell
import Language.Haskell.TH.Quote (QuasiQuoter(..))
-- text
import qualified Data.Text as T (Text, unpack)
import qualified Data.Text.Lazy as TL (Text)

toMarkup :: Node -> MarkupM ()
toMarkup = \case
  NodeComment c -> Comment (Text c) ()

attributes :: M.Map Name T.Text -> Attribute
attributes = M.foldrWithKey ins mempty
  where
    ins n v acc = attribute (fromString attrName) (fromString nameKeyStr) (fromString $ T.unpack v) <> acc
      where
        attrName = T.unpack $ nameLocalName n
        nameKeyStr = attrName <> "=\""

h0 :: TL.Text
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
