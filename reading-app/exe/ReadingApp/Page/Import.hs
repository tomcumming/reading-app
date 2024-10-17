module ReadingApp.Page.Import (API, server) where

import GHC.Generics (Generic)
import ReadingApp.Page.Wrapper (wrapper)
import Servant qualified as Sv
import Servant.HTML.Blaze qualified as B
import Text.Blaze.Html5 qualified as B
import Text.Blaze.Html5.Attributes qualified as Attr

data Routes mode = Routes
  { rtImportDict ::
      mode
        Sv.:- "dictionary" Sv.:> Sv.Post '[B.HTML] B.Html,
    rtRoot :: mode Sv.:- Sv.Get '[B.HTML] B.Html
  }
  deriving (Generic)

type API = Sv.NamedRoutes Routes

server :: Sv.Server API
server =
  Routes
    { rtImportDict = pure $ B.p "clicked!",
      rtRoot = pure $ wrapper $ do
        B.h1 "Import Data"
        B.button "Import dictionary"
          B.! Attr.id "import-dictionary"
          B.! B.customAttribute "hx-trigger" "click"
          B.! B.customAttribute "hx-post" "/import/dictionary"
          B.! B.customAttribute "hx-target" "#import-dictionary"
          B.! B.customAttribute "hx-swap" "outerHTML"
    }
