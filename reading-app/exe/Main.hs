module Main (main) where

import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import ReadingApp.Page.Import qualified as Import
import ReadingApp.Page.Reading qualified as Reading
import Servant qualified as Sv

data Routes mode = Routes
  { rtStatic :: mode Sv.:- "static" Sv.:> Sv.Raw,
    rtImport :: mode Sv.:- "import" Sv.:> Import.API,
    rtReading :: mode Sv.:- "reading" Sv.:> Reading.API
  }
  deriving (Generic)

type API = Sv.NamedRoutes Routes

server :: Sv.Server API
server =
  Routes
    { rtStatic = Sv.serveDirectoryWebApp "reading-app/static",
      rtImport = Import.server,
      rtReading = Reading.server
    }

app :: Sv.Application
app = Sv.serve (Sv.Proxy @API) server

main :: IO ()
main = do
  run 8080 app
