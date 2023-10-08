{-# language DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}

{-# options_ghc -Wno-unused-top-binds #-}
{-# options_ghc -Wno-orphans #-}
{-# options_ghc -Wno-missing-methods #-}
module Text.Blaze.Html.QQ.Internal (
  blaze
  , drawHTML

  ) where

import Control.Monad (foldM)
import Data.Maybe (listToMaybe)

-- blaze-html
-- blaze-markup
import Text.Blaze.Internal (MarkupM(..), ChoiceString(..), StaticString(..))

-- containers
import Data.Tree (Tree(..), drawTree)
-- html-parse
import qualified Text.HTML.Parser as HP (parseTokens, Token(..), Attr(..))
import qualified Text.HTML.Tree as HP (tokensToForest, ParseTokenForestError(..))
-- template-haskell
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Q, Lift(..), Exp(..), Name(..), OccName(..), NameSpace(..), NameFlavour(..), ModName(..), PkgName(..))
-- text
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.Text.Internal.Builder as TB (toLazyText)
import qualified Data.Text.Lazy as TL (unpack)
-- th-lift-instances
import Instances.TH.Lift ()


-- | Produces a @blaze@ 'MarkupM' term from an HTML string.
--
-- Compilation will fail if the HTML is malformed (e.g. tags are mismatched, or there is no single top-level tag).
blaze :: QuasiQuoter
blaze = QuasiQuoter f u u u
  where
    f str = do
      let
        t = T.pack $ noNewlines str
      case HP.tokensToForest (HP.parseTokens t) of
        Left e -> fail $ unwords ["mismatched brackets:", show e]
        Right xl -> case listToMaybe xl of
          Just tt -> do
            -- reportWarning $ drawTree (show <$> tt)
            fromTreeQ tt
          Nothing -> fail $ unwords ["there should be exactly one top node"]
    u = error "The 'blaze' quasiquoter can only be used to produce expressions"


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
          do
            mpn <- pkgName <$> [|Empty ()|] :: Q (Maybe PkgName)
            case mpn of
              Nothing -> error "cannot find PkgName of a MarkupM value. Is blaze-markup installed?"
              Just pn -> do
                let
                  mkName :: String -> Name
                  mkName = mkName0 pn "Text.Blaze.Internal" -- all the names constructed with this will be under the right PkgName
                  appendE = mkName "Append"
                [| addAttributes (Parent (mkSS s) (mkSS so) (mkSS sel) $(
                                     (appConE2M appendE [|Empty ()|] =<< traverse fromTreeQ nods) :: Q Exp
                                                                        ) ) attrs |]
    _ -> [| Empty () |]

deriving instance Lift HP.Attr

instance Lift HP.Token where
  liftTyped _ = error "unimplemented"
  lift = \case
    HP.Comment tb -> let t = TB.toLazyText tb in [| TB.toLazyText t |]
    HP.TagOpen tn attrs -> [| HP.TagOpen tn attrs |]
    HP.TagSelfClose tn attrs -> [| HP.TagSelfClose tn attrs |]
    HP.TagClose tn -> [| HP.TagClose tn |]
    HP.Doctype t -> [| HP.Doctype t|]
    HP.ContentText t -> [| HP.ContentText t|]
    HP.ContentChar c -> [| HP.ContentChar c |]


appConE2M :: (Monad m, Foldable t) => Name -> m Exp -> t Exp -> m Exp
appConE2M cn eq0 es = eq0 >>= \e0 ->
  foldM (\acc e-> pure (AppE (AppE (ConE cn) e) acc)
        ) e0 es

-- | Extremely unsafe. Don't try this at home kids
mkName0 :: PkgName
        -> String -- ^ module name, e.g. "Text.Blaze.Internal"
        -> String -- ^ name of the constructor, e.g. "Content"
        -> Name
mkName0 pn modName conName = Name (OccName conName) (NameG DataName pn (ModName modName))


pkgName :: Exp -> Maybe PkgName
pkgName = \case
  ConE (Name _ nf) -> case nf of
    NameG _ pn _ -> Just pn
    _ -> Nothing
  AppE e _ -> pkgName e
  _ -> Nothing

addAttributes :: MarkupM a -> [HP.Attr] -> MarkupM a
addAttributes x = foldr ins x
  where
    ins (HP.Attr n v) acc = AddAttribute (mkSS attrName) (mkSS nameKeyStr) (mkCS $ T.unpack v) acc
      where
        attrName = T.unpack n
        nameKeyStr = attrName <> "=\""

noNewlines :: String -> String
noNewlines = skipChar (== '\n')

-- λ> skipChar (== 'o') "potato"
-- "ptat"
skipChar :: (Char -> Bool) -> String -> String
skipChar f = foldr ins []
  where
    ins c acc
      | f c = acc
      | otherwise = c : acc

-- | build a ChoiceString
mkCS :: String -> ChoiceString
mkCS = String
-- | build a StaticString -- XXX breaks the 'Text.Blaze.Renderer.String' backend
mkSS :: String -> StaticString
mkSS s = let t = T.pack s in StaticString id (T.encodeUtf8 t) t
{-# INLINE mkSS #-}











-- using 'html-parse' instead of conduit-xml https://hackage.haskell.org/package/html-parse-0.2.0.2
-- hp :: T.Text -> Either HP.ParseTokenForestError (Forest HP.Token)
hp :: T.Text -> Either HP.ParseTokenForestError (Maybe (Tree HP.Token))
hp = fmap listToMaybe . HP.tokensToForest . HP.parseTokens
s0 :: T.Text
s0 = "<div><h1 class=widget>Hello World</h1></div>"



parseHTML :: T.Text -> Either String (MarkupM ())
parseHTML t = case HP.tokensToForest (HP.parseTokens t) of
  Left e -> Left $ unwords ["mismatched brackets:", show e]
  Right xl -> case listToMaybe xl of
    Just tt -> Right $ fromTree tt
    Nothing -> Left $ unwords ["there should be exactly one top node"]

drawHTML :: T.Text -> IO ()
drawHTML t = case HP.tokensToForest (HP.parseTokens t) of
  Left e -> error $ unwords ["mismatched brackets:", show e]
  Right xl -> case listToMaybe xl of
    Just tt -> putStrLn $ drawTree (show <$> tt)
    Nothing -> error  $ unwords ["there should be exactly one top node"]

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



-- === XXX delenda :



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









-- roundtrip :: TL.Text -> String
-- roundtrip = BP.renderHtml . parseMarkup






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
