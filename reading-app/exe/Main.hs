module Main (main, API) where

import Control.Category ((>>>))
import Control.Monad.Reader (runReaderT)
import Data.IORef (newIORef)
import Network.Wai.Handler.Warp (run)
import ReadingApp.API (API, Routes (..))
import ReadingApp.Config (cfgStrokeData, loadConfig)
import ReadingApp.Dict qualified as Dicts
import ReadingApp.Pages.ReadThrough qualified as ReadThrough
import ReadingApp.Pages.ReadThroughs qualified as ReadThroughsPage
import ReadingApp.RAM (Env (..), RAM)
import Servant qualified as Sv

server :: Env -> Sv.ServerT API RAM
server Env {..} =
  Routes
    { rtReadThrough = ReadThrough.server,
      rtHanziWriterData = Sv.serveDirectoryFileServer (cfgStrokeData envConfig),
      rtRoot = ReadThroughsPage.server,
      rtStatic = Sv.serveDirectoryFileServer "reading-app/www"
    }

app :: Env -> Sv.Application
app env =
  Sv.serveWithContextT
    (Sv.Proxy @API)
    Sv.EmptyContext
    ((`runReaderT` env) >>> Sv.Handler)
    (server env)

main :: IO ()
main = do
  envConfig <- loadConfig
  envDictIndex <- Dicts.loadDictIndex >>= newIORef
  let env = Env {..}
  run 8080 (app env)
