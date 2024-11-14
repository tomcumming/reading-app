module ReadingApp.API.Readthrough
  ( API,
    server,
  )
where

import Control.Category ((>>>))
import Control.Monad (when, (>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as Aeson
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Word (Word32)
import GHC.Generics (Generic)
import ReadingApp.RAM (RAM)
import Servant qualified as Sv
import System.Directory (copyFile, doesFileExist, listDirectory)
import System.FilePath (dropExtension, takeExtension)
import Text.Read (readMaybe)

data Routes mode = Routes
  { rtCreate ::
      mode
        Sv.:- "create"
          Sv.:> Sv.ReqBody '[Sv.JSON] T.Text
          Sv.:> Sv.Post '[Sv.JSON] ReadThId,
    rtRoot :: mode Sv.:- Sv.Get '[Sv.JSON] (M.Map ReadThId ReadTh)
  }
  deriving (Generic)

type API = Sv.NamedRoutes Routes

newtype ReadThId = ReadThId {unReadThId :: Word32}
  deriving (Eq, Ord, Bounded, Enum, Show) via Word32

instance Aeson.ToJSON ReadThId where
  toJSON = Aeson.toJSON . unReadThId

instance Aeson.ToJSONKey ReadThId

data ReadTh = ReadTh
  { rthName :: T.Text,
    rthLastView :: UTCTime
  }
  deriving (Generic)

instance Aeson.ToJSON ReadTh

instance Aeson.FromJSON ReadTh

server :: Sv.ServerT API RAM
server =
  Routes
    { rtCreate = \rthName -> liftIO $ do
        rthLastView <- liftIO getCurrentTime
        let rth =
              ReadTh
                { rthName,
                  rthLastView
                }
        rtId <- nextReadThId
        writeReadTh rtId rth
        pure rtId,
      rtRoot = liftIO allReadThs
    }

readThDir :: FilePath
readThDir = "data/readthroughs"

readThrPath :: ReadThId -> FilePath
readThrPath rhId = fold [readThDir, "/", show rhId, ".json"]

readThFiles :: IO (S.Set ReadThId)
readThFiles = do
  files <- listDirectory readThDir
  files
    & filter (takeExtension >>> (== ".json"))
    & traverse (readMaybe . dropExtension)
    & maybe (fail "Could not parse readthrough file name") pure
    & fmap (fmap ReadThId >>> S.fromList)

allReadThs :: IO (M.Map ReadThId ReadTh)
allReadThs =
  readThFiles
    >>= ( S.toList
            >>> fmap (\rtId -> (rtId, readThrPath rtId))
            >>> M.fromList
            >>> (traverse Aeson.decodeFileStrict >=> M.traverseWithKey flagErr)
        )
  where
    flagErr :: ReadThId -> Maybe ReadTh -> IO ReadTh
    flagErr rtId =
      maybe
        (fail $ "Failed to decode readthrough " <> show rtId)
        pure

nextReadThId :: IO ReadThId
nextReadThId =
  readThFiles
    >>= (S.lookupMax >>> fromMaybe minBound >>> succ >>> pure)

writeReadTh :: ReadThId -> ReadTh -> IO ()
writeReadTh rtId rth = do
  let path = readThrPath rtId
  exists <- doesFileExist path
  when exists $ copyFile path (path <> ".old")
  Aeson.encodeFile path rth
