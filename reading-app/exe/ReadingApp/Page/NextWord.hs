module ReadingApp.Page.NextWord (API, server) where

import Control.Category ((>>>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Data.Aeson qualified as Aeson
import Data.Foldable (fold)
import Data.IORef (readIORef)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Generics (Generic)
import ReadingApp.Page.Wrapper (wrapper)
import ReadingApp.RAM (RAM, envDictIndex)
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
          Sv.:> Sv.Get '[B.HTML] B.Html
  }
  deriving (Generic)

type API = Sv.NamedRoutes Routes

server :: Sv.ServerT API RAM
server =
  Routes
    { rtRootGet = \readThroughId -> do
        _pIdx <- asks envDictIndex >>= (readIORef >>> liftIO)
        pure $ wrapper $ do
          B.p "this will be the previous phrases"
          B.input
            B.! Attr.type_ "text"
            B.! Attr.name "Next characters"
            B.! Attr.placeholder "Next 漢字..."
            B.! B.customAttribute "hx-trigger" "input delay:300ms"
            B.! B.customAttribute
              "hx-get"
              (B.preEscapedToValue $ lookupUrl readThroughId)
            B.! B.customAttribute "hx-target" "#lookup-results"
          B.p "This will be word len choices"
            B.! Attr.id "lookup-results"
    }

lookupUrl :: Word64 -> String
lookupUrl readThroughId =
  fold
    [ "/next-word/",
      show readThroughId,
      "/lookup"
    ]

{- TODO
- Best path?
- We need 1,2,3,4 character words and their best path
- path should include if word is in dict (which)
  - Maybe that can be annotated after?

-}
