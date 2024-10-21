module ReadingApp.Db.Index (indexDicts) where

import Control.Category ((>>>))
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Word (Word32)
import DiskData qualified as DD
import ReadingApp.Db (DictId (DictId), defMatch)
import ReadingApp.RAM (PhraseIndex)
import Streaming.Prelude qualified as Sm
import System.Directory (listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))

indexDicts ::
  IO (M.Map DictId PhraseIndex)
indexDicts = do
  dictNames <-
    listDirectory "data/dicts"
      <&> ( filter (takeExtension >>> (== ".dict"))
              >>> fmap (readName "data/dicts")
              >>> M.fromList
          )
  traverse indexDict dictNames

readName :: FilePath -> FilePath -> (DictId, FilePath)
readName parnt fp = (takeBaseName fp & T.pack & DictId, parnt </> fp)

indexDict :: FilePath -> IO PhraseIndex
indexDict fp = do
  let dd = DD.diskData fp
  DD.streamFrom dd 0 $
    Sm.map defMatch
      >>> Sm.zip (Sm.each [0 ..])
      >>> Sm.fold_ go mempty id
  where
    go :: PhraseIndex -> (Word32, Set.Set T.Text) -> PhraseIndex
    go pIdx (idx, ps) = foldl' (go2 idx) pIdx ps

    go2 :: Word32 -> PhraseIndex -> T.Text -> PhraseIndex
    go2 idx pIdx m = M.insertWith (<>) m (Set.singleton idx) pIdx
