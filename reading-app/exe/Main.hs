module Main (main) where

import Control.Category ((>>>))
import Control.Monad.Reader (runReaderT)
import Data.IORef (newIORef)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import ReadingApp.Dict qualified as Dicts
import ReadingApp.Page.Import qualified as Import
import ReadingApp.Page.NextWord qualified as NextWord
import ReadingApp.Page.Search qualified as Search
import ReadingApp.RAM (Env (..), RAM)
import Servant qualified as Sv

data Routes mode = Routes
  { rtStatic :: mode Sv.:- "static" Sv.:> Sv.Raw,
    rtImport :: mode Sv.:- "import" Sv.:> Import.API,
    rtNextWord ::
      mode
        Sv.:- "next-word" Sv.:> NextWord.API,
    rtSearch :: mode Sv.:- "search" Sv.:> Search.API
  }
  deriving (Generic)

type API = Sv.NamedRoutes Routes

server :: Sv.ServerT API RAM
server =
  Routes
    { rtStatic = Sv.serveDirectoryWebApp "reading-app/static",
      rtImport = Import.server,
      rtNextWord = NextWord.server,
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
  envDictIndex <- Dicts.loadDictIndex >>= newIORef
  let env = Env {envDictIndex}
  run 8080 (app env)
