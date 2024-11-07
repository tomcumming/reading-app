module ReadingApp.Dict
  ( DictId (..),
    DictIndex (..),
    WordId (..),
    Entry (..),
    loadDictIndex,
    refreshDictIndex,
    addEntries,
  )
where

import CCCEdict.Pinyin (Pinyin)
import Control.Category ((>>>))
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (second)
import Data.Map qualified as M
import Data.Sequence qualified as Sq
import Data.Set qualified as Set
import Data.Text qualified as T
import DiskData qualified as DD
import GHC.Generics (Generic)
import ReadingApp.PhraseIndex (PhraseIndex, phraseIndexSingleton)
import Streaming.Prelude qualified as Sm
import System.Directory (doesFileExist)

data DictId
  = BuiltInDict
  | UserDict
  deriving (Eq, Ord, Show)

newtype DictIndex = DictIndex
  {unDictIndex :: M.Map DictId PhraseIndex}

data WordId = WordId
  { wiDict :: !DictId,
    wiIdx :: !DD.ItemIdx
  }
  deriving (Eq, Ord, Show)

data Entry = Entry
  { entSimp :: !(Maybe T.Text),
    entTrad :: !(Maybe T.Text),
    entPinyin :: !(Sq.Seq Pinyin),
    entDefs :: !(Sq.Seq T.Text)
  }
  deriving (Generic)

instance ToJSON Entry

instance FromJSON Entry

loadDictIndex :: IO DictIndex
loadDictIndex =
  DictIndex
    <$> foldMap loadSingle [BuiltInDict, UserDict]
  where
    loadSingle :: DictId -> IO (M.Map DictId PhraseIndex)
    loadSingle = refreshDictIndex (DictIndex mempty) >>> fmap unDictIndex

dictFileName :: DictId -> FilePath
dictFileName =
  (<> ".jsonl") . \case
    BuiltInDict -> "built-in"
    UserDict -> "user"

dictPath :: DictId -> FilePath
dictPath = dictFileName >>> ("data/dicts/" <>)

refreshDictIndex :: DictIndex -> DictId -> IO DictIndex
refreshDictIndex (DictIndex dIdx) dId = do
  let dp = dictPath dId
  exists <- doesFileExist dp
  if not exists
    then pure $ DictIndex mempty
    else do
      let dd = DD.diskData (dictPath dId)

      phrIdx <- DD.streamFrom dd DD.start (Sm.map entryPhrases >>> Sm.fold_ go mempty id)
      pure $ DictIndex $ M.insert dId phrIdx dIdx
  where
    go :: PhraseIndex -> (DD.ItemIdx, Set.Set T.Text) -> PhraseIndex
    go pIdx (idx, ps) = pIdx <> foldMap (`phraseIndexSingleton` idx) ps

    entryPhrases :: (DD.ItemIdx, Entry) -> (DD.ItemIdx, Set.Set T.Text)
    entryPhrases = second $ \Entry {entSimp, entTrad} ->
      foldMap (foldMap Set.singleton) [entSimp, entTrad]

addEntries :: DictId -> Sm.Stream (Sm.Of Entry) IO () -> IO ()
addEntries dId defsStrm = do
  let dd = DD.diskData (dictPath dId)
  void $ DD.appendData dd defsStrm
