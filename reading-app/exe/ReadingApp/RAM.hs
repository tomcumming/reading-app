module ReadingApp.RAM
  ( RAM,
    PhraseIndex,
    Env (..),
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Data.IORef (IORef)
import Data.Map.Strict qualified as M
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Word (Word32)
import ReadingApp.Db (DictId)
import Servant qualified as Sv

type RAM = ReaderT Env (ExceptT Sv.ServerError IO)

type PhraseIndex = M.Map T.Text (Set.Set Word32)

data Env = Env
  {envPhraseIndex :: IORef (M.Map DictId PhraseIndex)}
