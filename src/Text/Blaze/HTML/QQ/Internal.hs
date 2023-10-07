{-# language DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
{-# options_ghc -Wno-unused-imports #-}
{-# options_ghc -Wno-orphans #-}
module Text.Blaze.Html.QQ.Internal (documentQQ, fromDocument_) where

import Data.Maybe (listToMaybe)
import Data.String (IsString(..))

-- blaze-html
import qualified Text.Blaze.Html.Renderer.Pretty as BP (renderHtml)
-- blaze-markup
import Text.Blaze.Internal (MarkupM(..), Attribute, AttributeValue, ChoiceString(..), StaticString(..), Attributable(..), attribute, Tag)
-- conduit
import Conduit ((.|), runConduit)
import qualified Data.Conduit.List as CL (sourceList)
-- containers
import Data.Tree (Tree(..), Forest)
import qualified Data.Map as M (Map, foldrWithKey)
-- html-conduit
import Text.HTML.DOM (sinkDocText)
import Text.XML (Document(..), Element(..), Name(..), Node(..))
-- html-parse
import qualified Text.HTML.Parser as HP (parseTokens, Token(..))
import qualified Text.HTML.Tree as HP (tokensToForest, ParseTokenForestError(..))
-- template-haskell
import Language.Haskell.TH (runQ)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Lift(..))
-- text
import qualified Data.Text as T (Text, unpack)
import qualified Data.Text.Lazy as TL (Text, toChunks, pack)
-- th-lift-instances
import Instances.TH.Lift
-- xml-types
import Data.XML.Types (Miscellaneous(..), Prologue(..), Instruction(..), Doctype(..), ExternalID(..))


-- using 'html-parse' instead of conduit-xml https://hackage.haskell.org/package/html-parse-0.2.0.2
-- hp :: T.Text -> Either HP.ParseTokenForestError (Forest HP.Token)
hp = fmap listToMaybe . HP.tokensToForest . HP.parseTokens
s :: T.Text
s = "<div><h1 class=widget>Hello World</h1></div>"




-- | A quasiquoter for 'Document's
documentQQ :: QuasiQuoter
documentQQ = QuasiQuoter eParser p1 p2 p3
  where
    eParser str = case parseLT (TL.pack str) of
      Left e -> error e
      Right x -> [| x |]
    p1 _ = error "blazeQQ cannot be used for patterns"
    p2 _ = error "blazeQQ cannot be used for types"
    p3 _ = error "blazeQQ cannot be used for declarations"

deriving instance Lift Miscellaneous
deriving instance Lift Document
deriving instance Lift Element
deriving instance Lift Name
deriving instance Lift Node
deriving instance Lift Instruction
deriving instance Lift Doctype
deriving instance Lift ExternalID
deriving instance Lift Prologue



parseLT :: TL.Text -> Either String Document
parseLT tl =
  case runConduit $ CL.sourceList (TL.toChunks tl) .| sinkDocText of
    Left e -> Left ("parseLT: " ++ show e)
    Right x -> Right x

-- roundtrip :: TL.Text -> String
-- roundtrip = BP.renderHtml . parseMarkup

-- parseMarkup :: TL.Text -> MarkupM ()
-- parseMarkup = fromDocument . parseLT

-- fromDocumentExpQ x = [| fromDocument x |]



fromDocument_ :: Document -> MarkupM ()
fromDocument_ (Document _ el _) = fromElement el

toMarkup :: Node -> MarkupM ()
toMarkup = \case
  NodeComment c -> Comment (Text c) ()
  NodeElement e -> fromElement e
  _ -> mempty

fromElement :: Element -> MarkupM ()
fromElement (Element n as ns) = do
  parent n ! (attributes as)
  foldMap toMarkup ns

parent :: Name -> MarkupM ()
parent (Name tn _ _) = Parent (fromString s) (fromString so) (fromString sel) mempty
  where
    s = T.unpack tn
    so = "<" <> s
    sel = "</" <> s <> ">"

attributes :: M.Map Name T.Text -> Attribute
attributes = M.foldrWithKey ins mempty
  where
    ins n v acc = attribute (fromString attrName) (fromString nameKeyStr) (fromString $ T.unpack v) <> acc
      where
        attrName = T.unpack $ nameLocalName n
        nameKeyStr = attrName <> "=\""

-- h0 :: TL.Text
-- h0 = "<div class=\"col-12 uppercase\"><div class=\"uppercase\">Col-12</div></div>"


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
