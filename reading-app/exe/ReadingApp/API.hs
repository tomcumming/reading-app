module ReadingApp.API where

import Data.Function ((&))
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import ReadingApp.API.ReadThrough qualified as ReadThrough
import ReadingApp.API.ReadThroughs qualified as ReadThroughs
import Servant qualified as Sv
import Servant.HTML.Blaze qualified as B
import Text.Blaze.Html5 qualified as B

data Routes mode = Routes
  { rtReadThrough :: mode Sv.:- "readthrough" Sv.:> ReadThrough.API,
    rtHanziWriterData :: mode Sv.:- "hanzi-writer" Sv.:> Sv.Raw,
    rtRoot :: mode Sv.:- ReadThroughs.API,
    rtStatic :: mode Sv.:- Sv.Raw
  }
  deriving (Generic)

type API = Sv.NamedRoutes Routes

type X =
  "readthrough"
    Sv.:> Sv.Capture "rtId" ReadThrough.ReadThId
    Sv.:> Sv.Get '[B.HTML] B.Html

type ReadThroughReadLink =
  "readthrough"
    Sv.:> Sv.Capture "rtId" ReadThrough.ReadThId
    Sv.:> Sv.Get '[B.HTML] B.Html

allLinks :: Routes (Sv.AsLink Sv.Link)
allLinks = Sv.allLinks (Proxy @API)

readThroughLink :: ReadThrough.ReadThId -> Sv.Link
readThroughLink = rtReadThrough allLinks & ReadThrough.rtRead
