module ReadingApp.Page.NextWord (API, server) where

import Control.Category ((>>>))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Data.Aeson qualified as Aeson
import Data.Foldable (fold)
import Data.Function ((&))
import Data.IORef (readIORef)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Sq
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Generics (Generic)
import ReadingApp.BestPath (Token (..), bestPathFrom, bestPaths)
import ReadingApp.Dict (WordId)
import ReadingApp.Page.Wrapper (wrapper)
import ReadingApp.RAM (RAM, envDictIndex)
import ReadingApp.Tokenize (tokenize)
import Servant.API qualified as Sv
import Servant.HTML.Blaze qualified as B (HTML)
import Servant.Server qualified as Sv
import Text.Blaze.Html qualified as B
import Text.Blaze.Html5 qualified as B
import Text.Blaze.Html5.Attributes qualified as Attr

data NextWordState = NextWordState
  { tsReadThroughId :: Word64,
    tsInitial :: T.Text
  }
  deriving (Generic)

instance Aeson.ToJSON NextWordState

instance Aeson.FromJSON NextWordState

data Routes mode = Routes
  { rtRootGet ::
      mode
        Sv.:- Sv.Capture "readThroughId" Word64
          Sv.:> Sv.Get '[B.HTML] B.Html,
    rtSearch ::
      mode
        Sv.:- Sv.Capture "readThroughId" Word64
          Sv.:> "tokenize"
          Sv.:> Sv.QueryParam "txt" T.Text
          Sv.:> Sv.Get '[B.HTML] B.Html
  }
  deriving (Generic)

type API = Sv.NamedRoutes Routes

server :: Sv.ServerT API RAM
server =
  Routes
    { rtRootGet = \readThroughId -> do
        pure $ wrapper $ do
          B.p "this will be the previous phrases"
          B.input
            B.! Attr.type_ "text"
            B.! Attr.name "txt"
            B.! Attr.placeholder "Next 漢字..."
            B.! B.customAttribute "hx-trigger" "input delay:300ms"
            B.! B.customAttribute
              "hx-get"
              (B.preEscapedToValue $ lookupUrl readThroughId)
            B.! B.customAttribute "hx-target" "#lookup-results"
          B.p "This will be word len choices"
            B.! Attr.id "lookup-results",
      rtSearch = \_readThroughId txt' -> do
        txt <- maybe (fail "No tokenize text provided") pure txt'
        choices <- makeChoices txt
        pure $ wrapper $ do
          B.p ("Searched: " <> B.text txt)
          B.ul $ mapM_ renderChoice choices
    }

renderChoice :: Choice -> B.Html
renderChoice Choice {..} = B.li $ do
  B.span (choTok & tokenText & B.text)
  B.span $ forM_ choPath $ \Token {..} -> B.span (B.text tokenText)

data Choice = Choice
  { choTok :: Token,
    choPath :: Sq.Seq Token
  }

makeChoices :: T.Text -> RAM (Sq.Seq Choice)
makeChoices txt = do
  dIdx <- asks envDictIndex >>= (readIORef >>> liftIO)
  let ts = tokenize dIdx txt
  let bps = bestPaths ts
  let firstTokens = fromMaybe mempty $ ts M.!? 0
  M.mapWithKey (makeChoice bps) firstTokens
    & M.elems
    & Sq.fromList
    & pure
  where
    makeChoice :: M.Map Int Token -> T.Text -> S.Set WordId -> Choice
    makeChoice bps tokenText tokenWords =
      Choice
        { choTok = Token {tokenText, tokenWords},
          choPath = bestPathFrom bps (T.length tokenText)
        }

lookupUrl :: Word64 -> String
lookupUrl readThroughId =
  fold
    [ "/next-word/",
      show readThroughId,
      "/tokenize"
    ]
