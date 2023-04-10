{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
module Text.Blaze.HTML.QQ where

import Data.String (IsString(..))

-- html-conduit
import Text.HTML.DOM (parseLT)
import Text.XML (Document(..), Element(..), Name(..), Node(..))
-- template-haskell
import Language.Haskell.TH.Quote (QuasiQuoter(..))
-- text
import qualified Data.Text.Lazy as LT (Text)


