module ReadingApp.Page.Search (API, server) where

import Data.Text qualified as T
import GHC.Generics (Generic)
import ReadingApp.Page.Wrapper (wrapper)
import ReadingApp.RAM (RAM)
import Servant qualified as Sv
import Servant.HTML.Blaze qualified as B
import Text.Blaze.Html5 qualified as B
import Text.Blaze.Html5.Attributes qualified as Attr

data Routes mode = Routes
  { rtLookup ::
      mode
        Sv.:- "lookup"
          Sv.:> Sv.QueryParam "s" T.Text
          Sv.:> Sv.Get '[B.HTML] B.Html,
    rtRoot :: mode Sv.:- Sv.Get '[B.HTML] B.Html
  }
  deriving (Generic)

type API = Sv.NamedRoutes Routes

server :: Sv.ServerT API RAM
server =
  Routes
    { rtLookup = \s' -> do
        _s <- maybe (fail "No search string provided") pure s'
        fail "TODO",
      -- pure $ B.ul $ flip foldMap (M.toList ds) $ \(d, es) ->
      --   B.li $ do
      --     B.p (B.text $ unDictId d)
      --     B.ul $ flip foldMap es $ \e -> B.li (B.text $ head $ toList $ defTrans e),
      rtRoot = pure $ wrapper $ do
        B.h1 "Search test"
        B.input
          B.! Attr.type_ "search"
          B.! Attr.placeholder "Word or phrase..."
          B.! Attr.name "s"
          B.! B.customAttribute "hx-trigger" "input delay:300ms"
          B.! B.customAttribute "hx-get" "/search/lookup"
          B.! B.customAttribute "hx-target" "#search-results"
        B.div "Search results..."
          B.! Attr.id "search-results"
    }
