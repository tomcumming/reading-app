module ReadingApp.Page.Reading (API, server) where

import Control.Monad.Error.Class (throwError)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Word (Word64)
import GHC.Generics (Generic)
import ReadingApp.Page.Wrapper (wrapper)
import ReadingApp.RAM (RAM)
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
      rtNextPhrasePost = \(NextPhrasePost phrase) -> do
        pure $ wrapper $ B.h1 (B.text phrase),
      rtRoot = do
        -- TODO safe link
        let url =
              "/reading/"
                <> T.pack (show readThroughId)
                <> "/next-phrase"
        throwError $ Sv.err302 {Sv.errHeaders = [("Location", T.encodeUtf8 url)]}
    }
