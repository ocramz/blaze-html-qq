{-# language DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
{-# options_ghc -Wno-unused-imports #-}
{-# options_ghc -Wno-unused-top-binds #-}
{-# options_ghc -Wno-orphans #-}
module Text.Blaze.Html.QQ.Internal (documentQQ) where

import Control.Monad (foldM)
import Data.Maybe (listToMaybe)
import Data.String (IsString(..))

-- blaze-html
import Text.Blaze.Html5 (docType)
import qualified Text.Blaze.Html.Renderer.Pretty as BP (renderHtml)
-- blaze-markup
import Text.Blaze.Internal (MarkupM(..), Attribute, AttributeValue, ChoiceString(..), StaticString(..), Attributable(..), attribute, Tag, preEscapedText)
-- conduit
import Conduit ((.|), runConduit)
import qualified Data.Conduit.List as CL (sourceList)
-- containers
import Data.Tree (Tree(..), Forest)
import qualified Data.Map as M (Map, foldrWithKey)
-- html-conduit
import Text.HTML.DOM (sinkDocText)
import qualified Text.XML as X (Document(..), Element(..), Name(..), Node(..))
-- html-parse
import qualified Text.HTML.Parser as HP (parseTokens, Token(..), TagName, Attr(..), AttrName, AttrValue)
import qualified Text.HTML.Tree as HP (tokensToForest, ParseTokenForestError(..))
-- template-haskell
import Language.Haskell.TH (runQ)
import Language.Haskell.TH.Ppr (Ppr(..))
import Language.Haskell.TH.PprLib (Doc)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Code, Quasi(..), Q, Quote(..), Lift(..), Exp(..), Name(..), OccName(..), NameSpace(..), NameFlavour(..), ModName(..), PkgName(..))
-- text
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.Text.Internal.Builder as TB (Builder, toLazyText)
import qualified Data.Text.Lazy as TL (Text, toChunks, pack, unpack, toStrict)
-- th-lift-instances
import Instances.TH.Lift
-- xml-types
import qualified Data.XML.Types as X (Miscellaneous(..), Prologue(..), Instruction(..), Doctype(..), ExternalID(..))


data A = A Int Char
deriving instance Lift A

-- | === html-parse

-- using 'html-parse' instead of conduit-xml https://hackage.haskell.org/package/html-parse-0.2.0.2
-- hp :: T.Text -> Either HP.ParseTokenForestError (Forest HP.Token)
hp :: T.Text -> Either HP.ParseTokenForestError (Maybe (Tree HP.Token))
hp = fmap listToMaybe . HP.tokensToForest . HP.parseTokens
s0 :: T.Text
s0 = "<div><h1 class=widget>Hello World</h1></div>"

deriving instance Lift HP.Attr
-- deriving instance Lift HP.Token

parseHTML :: T.Text -> Either String (MarkupM ())
parseHTML t = case HP.tokensToForest (HP.parseTokens t) of
  Left e -> Left $ unwords ["mismatched brackets:", show e]
  Right xl -> case listToMaybe xl of
    Just tt -> Right $ fromTree tt
    Nothing -> Left $ unwords ["there should be exactly one top node"]

fromTree :: Tree HP.Token -> MarkupM ()
fromTree (Node nod nods) =
  case nod of
    HP.Comment tn -> Comment (mkCS $ TL.unpack $ TB.toLazyText tn) ()
    HP.Doctype _ -> Content (PreEscaped $ Text "<!DOCTYPE HTML>") ()
    HP.ContentText tn -> Content (mkCS $ T.unpack tn) ()
    HP.TagSelfClose tn attrs ->
      let
        s = T.unpack tn
        so = "<" <> s
      in
        addAttributes (Leaf (mkSS s) (mkSS so) (mkSS ">") () ) attrs
    HP.TagOpen tn attrs ->
        let
          s = T.unpack tn
          so = "<" <> s
          sel = "</" <> s <> ">"
        in
          addAttributes (Parent (mkSS s) (mkSS so) (mkSS sel) (foldMap fromTree nods) ) attrs
    _ -> Empty ()

addAttributes :: MarkupM a -> [HP.Attr] -> MarkupM a
addAttributes x = foldr ins x
  where
    ins (HP.Attr n v) acc = AddAttribute (mkSS attrName) (mkSS nameKeyStr) (mkCS $ T.unpack v) acc
      where
        attrName = T.unpack n
        nameKeyStr = attrName <> "=\""

-- | build a ChoiceString
mkCS :: String -> ChoiceString
mkCS = String
-- | build a StaticString -- XXX breaks the 'Text.Blaze.Renderer.String' backend
mkSS :: String -> StaticString
mkSS s = let t = T.pack s in StaticString id (T.encodeUtf8 t) t

fromTreeQ :: Tree HP.Token -> Q Exp
fromTreeQ (Node nod nods) =
  case nod of
    HP.Comment tn -> let
      tl = TB.toLazyText tn in [| Comment (mkCS $ TL.unpack tl) () |]
    HP.Doctype _ -> [|Content (PreEscaped $ Text "<!DOCTYPE HTML>") ()|]
    HP.ContentText tn -> [|Content (mkCS $ T.unpack tn) ()|]
    HP.TagSelfClose tn attrs ->
      let
        s = T.unpack tn
        so = "<" <> s
      in
        [|addAttributes (Leaf (mkSS s) (mkSS so) (mkSS ">") () ) attrs|]
    
    HP.TagOpen tn attrs ->
        let
          s = T.unpack tn
          so = "<" <> s
          sel = "</" <> s <> ">"
        in
          undefined -- TODO recursion
          -- [|addAttributes (Parent (mkSS s) (mkSS so) (mkSS sel) _ attrs|]
    --       [|addAttributes (Parent (mkSS s) (mkSS so) (mkSS sel) (foldM (\e x -> fromTreeQ x) nods) ) attrs|]
    _ -> [| Empty () |]

bla :: Code Q StaticString
bla = [|| mkSS "42"||]

recurs tn attrs =
  let
    s = T.unpack tn
    so = "<" <> s
    sel = "</" <> s <> ">"
  in
    [||addAttributes (Parent (mkSS s) (mkSS so) (mkSS sel) undefined) attrs||]


appEs :: Foldable t => Name -> t Exp -> Exp
appEs cn = foldr (flip AppE) (ConE cn)

q :: QuasiQuoter
q = QuasiQuoter f u u u
  where
    f str = do
      mpn <- pkgName <$> [|Empty ()|] :: Q (Maybe PkgName)
      case mpn of
        Nothing -> error "cannot find PkgName of a MarkupM value. Is blaze-markup installed?"
        Just pn -> do
          let
            mkName = mkName0 pn -- all the names constructed with this will be under the right PkgName
          undefined
    u = error "undefined"

-- | Extremely unsafe. Don't try this at home kids
mkName0 :: PkgName
        -> String -- ^ name of the constructor, e.g. "Content"
        -> Name
mkName0 pn conName = Name (OccName conName) (NameG DataName pn (ModName "Text.Blaze.Internal"))


pkgName :: Exp -> Maybe PkgName
pkgName = \case
  ConE (Name _ nf) -> case nf of
    NameG _ pn _ -> Just pn
    _ -> Nothing
  AppE e _ -> pkgName e
  _ -> Nothing



-- con :: Name -> String
-- con (Name on nf) = unwords [show on, ty]
--   where
--     ty = case nf of
--       NameS -> "S"
--       NameQ md -> unwords ["Q", show md]
--       NameU u -> unwords ["U", show u]
--       NameL l -> unwords ["L", show l]
--       NameG ns pn mn -> unwords ["G", show ns, show pn, show mn]

-- instance Lift StaticString where
--   liftTyped (StaticString _ bs t) = [||StaticString id bs t||] 
-- deriving instance Lift ChoiceString

-- toMarkup :: Node -> MarkupM ()
-- toMarkup = \case
--   NodeComment c -> Comment (Text c) ()
--   NodeElement e -> fromElement e
--   _ -> mempty

-- fromElement :: Element -> MarkupM ()
-- fromElement (Element n as ns) = do
--   parent n ! (attributes as)
--   foldMap toMarkup ns

-- parent :: Name -> MarkupM ()
-- parent (Name tn _ _) = Parent (fromString s) (fromString so) (fromString sel) mempty
--   where
--     s = T.unpack tn
--     so = "<" <> s
--     sel = "</" <> s <> ">"

-- attributes :: M.Map Name T.Text -> Attribute
-- attributes = M.foldrWithKey ins mempty
--   where
--     ins n v acc = attribute (fromString attrName) (fromString nameKeyStr) (fromString $ T.unpack v) <> acc
--       where
--         attrName = T.unpack $ nameLocalName n
--         nameKeyStr = attrName <> "=\""







-- | === xml-conduit


-- | A quasiquoter for 'X.Document's
documentQQ :: QuasiQuoter
documentQQ = QuasiQuoter eParser p1 p2 p3
  where
    eParser str = case parseLT (TL.pack str) of
      Left e -> error e
      Right x -> [| x |]
    p1 _ = error "blazeQQ cannot be used for patterns"
    p2 _ = error "blazeQQ cannot be used for types"
    p3 _ = error "blazeQQ cannot be used for declarations"

deriving instance Lift X.Miscellaneous
deriving instance Lift X.Document
deriving instance Lift X.Element
deriving instance Lift X.Name
deriving instance Lift X.Node
deriving instance Lift X.Instruction
deriving instance Lift X.Doctype
deriving instance Lift X.ExternalID
deriving instance Lift X.Prologue



parseLT :: TL.Text -> Either String X.Document
parseLT tl =
  case runConduit $ CL.sourceList (TL.toChunks tl) .| sinkDocText of
    Left e -> Left ("parseLT: " ++ show e)
    Right x -> Right x

-- roundtrip :: TL.Text -> String
-- roundtrip = BP.renderHtml . parseMarkup

-- parseMarkup :: TL.Text -> MarkupM ()
-- parseMarkup = fromDocument_ . parseLT


fromDocument_ :: X.Document -> MarkupM ()
fromDocument_ (X.Document _ el _) = fromElement el

toMarkup :: X.Node -> MarkupM ()
toMarkup = \case
  X.NodeComment c -> Comment (Text c) ()
  X.NodeElement e -> fromElement e
  _ -> mempty

fromElement :: X.Element -> MarkupM ()
fromElement (X.Element n as ns) = do
  parent n ! (attributes as)
  foldMap toMarkup ns

parent :: X.Name -> MarkupM ()
parent (X.Name tn _ _) = Parent (fromString s) (fromString so) (fromString sel) mempty
  where
    s = T.unpack tn
    so = "<" <> s
    sel = "</" <> s <> ">"

attributes :: M.Map X.Name T.Text -> Attribute
attributes = M.foldrWithKey ins mempty
  where
    ins n v acc = attribute (fromString attrName) (fromString nameKeyStr) (fromString $ T.unpack v) <> acc
      where
        attrName = T.unpack $ X.nameLocalName n
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
