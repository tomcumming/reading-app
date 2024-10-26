module ReadingApp.RAM
  ( RAM,
    Env (..),
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Data.IORef (IORef)
import Data.Map.Strict qualified as M
import ReadingApp.Db (DictId)
import ReadingApp.PhraseIndex (PhraseIndex)
import Servant qualified as Sv

type RAM = ReaderT Env (ExceptT Sv.ServerError IO)

data Env = Env
  {envPhraseIndex :: IORef (M.Map DictId PhraseIndex)}
