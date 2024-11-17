module Main (main, API) where

import Control.Category ((>>>))
import Control.Monad.Reader (runReaderT)
import Data.IORef (newIORef)
import Network.Wai.Handler.Warp (run)
import ReadingApp.API (API, Routes (..))
import ReadingApp.Dict qualified as Dicts
import ReadingApp.Pages.ReadThrough qualified as ReadThrough
import ReadingApp.Pages.ReadThroughs qualified as ReadThroughsPage
import ReadingApp.RAM (Env (..), RAM)
import Servant qualified as Sv

server :: Sv.ServerT API RAM
server =
  Routes
    { rtReadThrough = ReadThrough.server,
      rtRoot = ReadThroughsPage.server,
      rtStatic = Sv.serveDirectoryFileServer "reading-app/www"
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
