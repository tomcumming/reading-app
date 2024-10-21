module ReadingApp.Db
  ( DictId (..),
    Definition (..),
    addDefinitions,
  )
where

import CCCEdict.Pinyin (Pinyin)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Sequence qualified as Sq
import Data.Set qualified as Set
import Data.Text qualified as T
import DiskData qualified as DD
import GHC.Generics (Generic)
import Streaming.Prelude qualified as S

newtype DictId = DictId {unDictId :: T.Text}
  deriving (Eq, Ord, Show) via T.Text

data Definition = Definition
  { defMatch :: Set.Set T.Text,
    defPinyin :: Sq.Seq Pinyin,
    defTrans :: Sq.Seq T.Text
  }
  deriving (Generic)

instance ToJSON Definition

instance FromJSON Definition

addDefinitions ::
  DictId ->
  S.Stream (S.Of Definition) IO () ->
  IO ()
addDefinitions (DictId dictName) defsStrm = do
  let parentDir = "data/dicts"
  let dd = DD.diskData $ parentDir <> "/" <> T.unpack dictName <> ".dict"
  void $ DD.appendData dd defsStrm
