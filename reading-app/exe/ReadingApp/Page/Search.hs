module ReadingApp.Page.Search (API, server) where

import Control.Category ((>>>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.IORef (readIORef)
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import DiskData qualified as DD
import GHC.Generics (Generic)
import ReadingApp.Db (Definition, DictId (DictId), defTrans, unDictId)
import ReadingApp.Page.Wrapper (wrapper)
import ReadingApp.PhraseIndex (PhraseIndex, phraseIndexLookup)
import ReadingApp.RAM (Env (envPhraseIndex), RAM)
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
        s <- maybe (fail "No search string provided") pure s'
        ds <- testLookup s
        pure $ B.ul $ flip foldMap (M.toList ds) $ \(d, es) ->
          B.li $ do
            B.p (B.text $ unDictId d)
            B.ul $ flip foldMap es $ \e -> B.li (B.text $ head $ toList $ defTrans e),
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

testLookup :: T.Text -> RAM (M.Map DictId [Definition])
testLookup s = do
  pIdx <- asks envPhraseIndex >>= (readIORef >>> liftIO)
  M.traverseWithKey (testLookupDict s) pIdx

suffixes :: T.Text -> [T.Text]
suffixes s = case T.uncons s of
  Nothing -> []
  Just (_, post) -> s : suffixes post

testLookupDict :: T.Text -> DictId -> PhraseIndex -> RAM [Definition]
testLookupDict s (DictId dictName) pIdx = do
  let ms =
        suffixes s
          & fmap (phraseIndexLookup pIdx)
          & M.unionsWith (<>)

  let parentDir = "data/dicts"
  let dd = DD.diskData $ parentDir <> "/" <> T.unpack dictName <> ".dict"
  fetched <- liftIO $ traverse (DD.fetchSet dd) ms
  pure $ concatMap M.elems fetched
