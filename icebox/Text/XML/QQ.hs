{-# LANGUAGE LambdaCase #-}
{-# options_ghc -Wno-unused-imports #-}
module Text.XML.QQ (
  -- * xml-conduit
  documentQQ
  ) where

import Data.String (IsString(..))

-- blaze-html
import qualified Text.Blaze.Html.Renderer.Pretty as BP (renderHtml)
-- blaze-markup
import Text.Blaze.Internal (MarkupM(..), Attribute, AttributeValue, ChoiceString(..), StaticString(..), Attributable(..), attribute, Tag, preEscapedText)
-- conduit
import Conduit ((.|), runConduit)
import qualified Data.Conduit.List as CL (sourceList)
-- containers
import qualified Data.Map as M (Map, foldrWithKey)
-- html-conduit
import Text.HTML.DOM (sinkDocText)
import qualified Text.XML as X (Document(..), Element(..), Name(..), Node(..))
-- template-haskell
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Code, Quasi(..), Q, Quote(..), Lift(..))
-- text
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.Lazy as TL (Text, toChunks, pack, unpack, toStrict)
-- th-lift-instances
import Instances.TH.Lift ()
-- xml-types
import qualified Data.XML.Types as X (Miscellaneous(..), Prologue(..), Instruction(..), Doctype(..), ExternalID(..))

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
