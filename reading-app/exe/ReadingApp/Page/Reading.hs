module ReadingApp.Page.Reading (API, server) where

import Control.Category ((>>>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Data.Aeson qualified as Aeson
import Data.IORef (readIORef)
import Data.Map qualified as M
import Data.Sequence qualified as Sq
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Word (Word64)
import GHC.Generics (Generic)
import ReadingApp.Db (DictId (DictId))
import ReadingApp.Page.Wrapper (wrapper)
import ReadingApp.RAM (RAM, envPhraseIndex)
import ReadingApp.Tokenize (Paths (..), tokenize)
import Servant.API qualified as Sv
import Servant.HTML.Blaze qualified as B (HTML)
import Servant.Server qualified as Sv
import Text.Blaze.Html qualified as B
import Text.Blaze.Html5 qualified as B
import Text.Blaze.Html5.Attributes qualified as Attr
import Web.FormUrlEncoded qualified as Form

newtype NextPhrasePost = NextPhrasePost T.Text

instance Form.FromForm NextPhrasePost where
  fromForm fm =
    NextPhrasePost
      <$> Form.parseUnique "phrase" fm

data TokenizeState = TokenizeState
  { tsSelected :: Sq.Seq T.Text,
    tsPending :: T.Text
  }
  deriving (Generic)

instance Aeson.ToJSON TokenizeState

instance Aeson.FromJSON TokenizeState

initialTokenizeState :: T.Text -> TokenizeState
initialTokenizeState tsPending =
  TokenizeState {tsSelected = mempty, tsPending}

data Routes mode = Routes
  { rtNextPhrase ::
      mode
        Sv.:- "next-phrase"
          Sv.:> Sv.Get '[B.HTML] B.Html,
    rtNextPhrasePost ::
      mode
        Sv.:- "next-phrase"
          Sv.:> Sv.ReqBody '[Sv.FormUrlEncoded] NextPhrasePost
          Sv.:> Sv.Post '[B.HTML] B.Html,
    rtRoot :: mode Sv.:- Sv.Get '[Sv.PlainText] Sv.NoContent
  }
  deriving (Generic)

type API = Sv.NamedRoutes Routes

server :: Word64 -> Sv.ServerT API RAM
server readThroughId =
  Routes
    { rtNextPhrase = do
        pure $ wrapper $ do
          B.h1 "Enter phrase characters"
          B.form
            B.! Attr.method "post"
            $ do
              B.input
                B.! Attr.name "phrase"
                B.! Attr.type_ "text"
              B.input B.! Attr.type_ "submit",
      rtNextPhrasePost = \(NextPhrasePost phrase) ->
        tokenizePage (initialTokenizeState phrase),
      rtRoot = do
        -- TODO safe link
        let url =
              "/reading/"
                <> T.pack (show readThroughId)
                <> "/next-phrase"
        throwError $ Sv.err302 {Sv.errHeaders = [("Location", T.encodeUtf8 url)]}
    }

tokenizePage :: TokenizeState -> RAM B.Html
tokenizePage TokenizeState {..} = do
  dicts <- asks envPhraseIndex >>= (readIORef >>> liftIO)
  pIdx <-
    maybe
      (fail "Could not find dictionary")
      pure
      $ dicts M.!? DictId "cc-cedict"
  let paths = tokenize pIdx tsPending
  liftIO $ mapM_ putStrLn (printTree paths)
  pure $ wrapper $ B.h1 "TODO"

printTree :: Paths -> [String]
printTree (Paths paths) =
  M.foldMapWithKey
    (\k v -> T.unpack k : (("  " <>) <$> printTree v))
    paths
