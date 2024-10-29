module Main (main) where

import Control.Category ((>>>))
import Control.Monad.Reader (runReaderT)
import Data.IORef (newIORef)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import ReadingApp.Db.Index (indexDicts)
import ReadingApp.Page.Import qualified as Import
import ReadingApp.Page.Reading qualified as Reading
import ReadingApp.Page.Search qualified as Search
import ReadingApp.RAM (Env (..), RAM)
import Servant qualified as Sv

data Routes mode = Routes
  { rtStatic :: mode Sv.:- "static" Sv.:> Sv.Raw,
    rtImport :: mode Sv.:- "import" Sv.:> Import.API,
    rtReading ::
      mode
        Sv.:- "reading"
          Sv.:> Sv.Capture "readThroughId" Word64
          Sv.:> Reading.API,
    rtSearch :: mode Sv.:- "search" Sv.:> Search.API
  }
  deriving (Generic)

type API = Sv.NamedRoutes Routes

server :: Sv.ServerT API RAM
server =
  Routes
    { rtStatic = Sv.serveDirectoryWebApp "reading-app/static",
      rtImport = Import.server,
      rtReading = Reading.server,
      rtSearch = Search.server
    }

app :: Env -> Sv.Application
app env =
  Sv.serveWithContextT
    (Sv.Proxy @API)
    Sv.EmptyContext
    ((`runReaderT` env) >>> Sv.Handler)
    server

main :: IO ()
main = do
  envPhraseIndex <- indexDicts >>= newIORef
  let env = Env {envPhraseIndex}
  run 8080 (app env)
