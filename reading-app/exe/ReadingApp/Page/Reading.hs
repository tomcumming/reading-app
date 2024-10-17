module ReadingApp.Page.Reading (API, server) where

import Data.Text qualified as T
import GHC.Generics (Generic)
import ReadingApp.Page.Wrapper (wrapper)
import Servant.API qualified as Sv
import Servant.HTML.Blaze qualified as B (HTML)
import Servant.Server qualified as Sv
import Text.Blaze.Html qualified as B
import Text.Blaze.Html5 qualified as B
import Text.Blaze.Html5.Attributes qualified as Attr

data Routes mode = Routes
  { rtSearch ::
      mode
        Sv.:- "search"
          Sv.:> Sv.QueryParam "s" T.Text
          Sv.:> Sv.Get '[B.HTML] B.Html,
    rtRoot :: mode Sv.:- Sv.Get '[B.HTML] B.Html
  }
  deriving (Generic)

type API = Sv.NamedRoutes Routes

server :: Sv.Server API
server =
  Routes
    { rtSearch = \s' -> do
        s <- maybe (fail "No search string provided") pure s'
        pure $ B.p $ "Searched: " <> B.text s,
      rtRoot = pure $ wrapper $ do
        B.h1 "Search test"
        B.input
          B.! Attr.type_ "search"
          B.! Attr.placeholder "Word or phrase..."
          B.! Attr.name "s"
          B.! B.customAttribute "hx-trigger" "input delay:300ms"
          B.! B.customAttribute "hx-get" "/reading/search"
          B.! B.customAttribute "hx-target" "#search-results"
        B.div "Search results..."
          B.! Attr.id "search-results"
    }
