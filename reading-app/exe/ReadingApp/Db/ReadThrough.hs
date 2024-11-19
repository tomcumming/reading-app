module ReadingApp.Db.ReadThrough
  ( nextReadThId,
    loadReadTh,
    allReadThs,
    writeReadTh,
    strokeDataPath,
  )
where

import Control.Category ((>>>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Data.Aeson qualified as Aeson
import Data.Bool (bool)
import Data.Foldable (fold)
import Data.Function ((&))
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import ReadingApp.API.ReadThrough (ReadTh, ReadThId (..))
import ReadingApp.Config (cfgStrokeData)
import ReadingApp.RAM (RAM, envConfig)
import System.Directory (copyFile, doesFileExist, listDirectory)
import System.FilePath (dropExtension, takeExtension, (<.>), (</>))
import Text.Read (readMaybe)

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

loadReadTh :: ReadThId -> IO ReadTh
loadReadTh rtId =
  Aeson.decodeFileStrict (readThrPath rtId)
    >>= maybe
      (fail $ "Failed to decode readthrough " <> show rtId)
      pure

allReadThs :: IO (M.Map ReadThId ReadTh)
allReadThs =
  readThFiles
    >>= (S.toList >>> traverse (\rtId -> (rtId,) <$> loadReadTh rtId))
    >>= (M.fromList >>> pure)

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

strokeDataPath :: T.Text -> RAM (Maybe FilePath)
strokeDataPath char = do
  dir <- asks (envConfig >>> cfgStrokeData)
  let p = dir </> T.unpack char <.> "json"
  doesFileExist p
    & liftIO
    & fmap (bool Nothing (Just p))
