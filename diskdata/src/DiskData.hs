module DiskData
  ( DiskData,
    ItemIdx (..),
    FileOffset,
    start,
    diskData,
    indexOffset,
    appendData,
    streamFrom,
    fetchSet,
  )
where

import Control.Category ((>>>))
import Control.Monad (unless)
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Aeson.Decoding (decodeStrictText)
import Data.Aeson.Text (encodeToLazyText)
import Data.Foldable (forM_)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text.IO ()
import Data.Text.IO qualified as Txt (hGetLine)
import Data.Text.Lazy.IO qualified as Txt (hPutStrLn)
import Data.Traversable (forM)
import Data.Vector qualified as V
import Data.Word (Word32, Word64)
import Foreign qualified as IO
import Streaming qualified as Sm
import Streaming.Prelude qualified as Sm
import System.FilePath qualified as Fp
import System.IO qualified as IO

data DiskData = DiskData
  { ddIndex :: FilePath,
    ddData :: FilePath
  }
  deriving (Show)

newtype ItemIdx = ItemIdx {unItemIdx :: Word32}
  deriving (Eq, Ord, Enum)
  deriving (Show) via Word32

newtype FileOffset = FileOffset {unFileOffset :: Word64}
  deriving (Eq, Ord)
  deriving (Show, IO.Storable) via Word64

start :: ItemIdx
start = ItemIdx 0

diskData :: FilePath -> DiskData
diskData ddData =
  DiskData
    { ddIndex = Fp.addExtension (Fp.dropExtension ddData) "idx",
      ddData
    }

int64Size :: Int
int64Size = 8

indexOffset :: IO.Handle -> ItemIdx -> IO FileOffset
indexOffset hdl itemIdx = do
  let fileOff = fromIntegral int64Size * fromIntegral (unItemIdx itemIdx)
  IO.hSeek hdl IO.AbsoluteSeek fileOff
  isEof <- IO.hIsEOF hdl
  if isEof
    then pure (FileOffset (fromIntegral fileOff))
    else do
      buf <- IO.mallocBytes int64Size
      _bytesRead <- IO.hGetBuf hdl buf int64Size
      IO.peek buf

appendOffsets :: IO.Handle -> V.Vector FileOffset -> IO ()
appendOffsets hdl offs = do
  buf <- IO.mallocBytes int64Size
  forM_ offs $ \off -> do
    IO.poke buf off
    IO.hPutBuf hdl buf int64Size

appendItems ::
  forall a.
  (ToJSON a) =>
  IO.Handle ->
  Sm.Stream (Sm.Of a) IO () ->
  IO (V.Vector FileOffset)
appendItems hdl items = do
  Sm.mapM writeItem items
    & Sm.fold V.snoc mempty id
    & fmap Sm.fst'
  where
    writeItem :: (ToJSON a) => a -> IO FileOffset
    writeItem =
      encodeToLazyText >>> \txt -> do
        off <- IO.hTell hdl
        Txt.hPutStrLn hdl txt
        pure (FileOffset (fromIntegral off))

data ForWriting = ForWriting
  { fwIndex :: IO.Handle,
    fwData :: IO.Handle,
    fwItemOffset :: ItemIdx
  }

sizeToLength :: Integer -> ItemIdx
sizeToLength = (`div` fromIntegral int64Size) >>> fromIntegral >>> ItemIdx

-- | Open and test for currupt files
openForWriting :: DiskData -> (ForWriting -> IO a) -> IO a
openForWriting DiskData {..} f = IO.withFile ddIndex IO.ReadWriteMode $ \fwIndex ->
  IO.withFile ddData IO.ReadWriteMode $
    \fwData ->
      ( do
          IO.hSeek fwIndex IO.SeekFromEnd 0
          IO.hSeek fwData IO.SeekFromEnd 0
          fwItemOffset <- sizeToLength <$> IO.hTell fwIndex
          dataPos <- IO.hTell fwData
          if unItemIdx fwItemOffset == 0
            then do
              unless (dataPos == 0) $ fail $ ddData <> " was expected to be empty"
            else do
              -- Ensure previous element exists
              prevOff <- indexOffset fwIndex (pred fwItemOffset)
              IO.hSeek fwData IO.AbsoluteSeek (fromIntegral (unFileOffset prevOff))
              let prevOffCorr = unwords ["Can't read prev item", show ddData]
              prevItemTxt <- Txt.hGetLine fwData
              decodeStrictText @Value prevItemTxt
                & maybe (fail prevOffCorr) (const (pure ()))
              print =<< IO.hTell fwData
              print =<< IO.hIsEOF fwData
              IO.hIsEOF fwData
                >>= flip
                  unless
                  (fail $ unwords ["Expected EOF after last item in", show ddData])
          f ForWriting {..}
      )

appendData :: (ToJSON a) => DiskData -> Sm.Stream (Sm.Of a) IO () -> IO ItemIdx
appendData dd items = openForWriting dd $ \ForWriting {..} -> do
  fileOffs <- appendItems fwData items
  IO.hFlush fwData
  appendOffsets fwIndex fileOffs
  pure fwItemOffset

streamFrom ::
  forall a b.
  (FromJSON a) =>
  DiskData ->
  ItemIdx ->
  (Sm.Stream (Sm.Of (ItemIdx, a)) IO () -> IO b) ->
  IO b
streamFrom dd fromIndexOff f = do
  maybeFileOff <- IO.withFile (ddIndex dd) IO.ReadMode $
    \hdl -> do
      IO.hSeek hdl IO.SeekFromEnd 0
      fileEnd <- IO.hTell hdl
      if sizeToLength fileEnd <= fromIndexOff
        then pure Nothing
        else Just <$> indexOffset hdl fromIndexOff
  case maybeFileOff of
    Nothing -> f mempty
    Just fileOff -> IO.withFile (ddData dd) IO.ReadMode $ \hdl -> do
      IO.hSeek hdl IO.AbsoluteSeek (fromIntegral (unFileOffset fileOff))
      f (jsonLines hdl fromIndexOff)
  where
    jsonLines :: IO.Handle -> ItemIdx -> Sm.Stream (Sm.Of (ItemIdx, a)) IO ()
    jsonLines hdl itemOff = do
      isEof <- lift $ IO.hIsEOF hdl
      if isEof
        then pure ()
        else do
          dataLine <- lift $ Txt.hGetLine hdl
          case decodeStrictText dataLine of
            Nothing -> fail $ unwords ["Failed to decode item in", show (ddData dd), "at line", show itemOff]
            Just item -> Sm.yield (itemOff, item) >> jsonLines hdl (succ itemOff)

fetchSet :: forall a. (FromJSON a) => DiskData -> Set.Set ItemIdx -> IO (Map.Map ItemIdx a)
fetchSet dd idxs = IO.withFile (ddIndex dd) IO.ReadMode $ \hi ->
  IO.withFile (ddData dd) IO.ReadMode $ \hd ->
    Map.fromList
      <$> forM (Set.toList idxs) (goItem hi hd)
  where
    goItem :: IO.Handle -> IO.Handle -> ItemIdx -> IO (ItemIdx, a)
    goItem hi hd idx = do
      off <- indexOffset hi idx
      IO.hSeek hd IO.AbsoluteSeek (fromIntegral (unFileOffset off))
      let itemError = unwords ["Can't read item at", show idx, show (ddData dd)]
      item <-
        Txt.hGetLine hd
          >>= (decodeStrictText >>> maybe (fail itemError) pure)
      pure (idx, item)
