module Main (main) where

import Control.Category ((>>>))
import Control.Monad.Reader (runReaderT)
import Data.IORef (newIORef)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import ReadingApp.API.ReadThrough qualified as ReadThrough
import ReadingApp.Dict qualified as Dicts
import ReadingApp.RAM (Env (..), RAM)
import Servant qualified as Sv

data Routes mode = Routes
  { rtReadthough :: mode Sv.:- "readthrough" Sv.:> ReadThrough.API,
    rtRoot :: mode Sv.:- Sv.Raw
  }
  deriving (Generic)

type API = Sv.NamedRoutes Routes

server :: Sv.ServerT API RAM
server =
  Routes
    { rtReadthough = ReadThrough.server,
      rtRoot = Sv.serveDirectoryFileServer "reading-app/www"
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
