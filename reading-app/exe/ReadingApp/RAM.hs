module ReadingApp.RAM
  ( RAM,
    Env (..),
  )
where

import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (ReaderT)
import Data.IORef (IORef)
import ReadingApp.Dict qualified as Dicts
import Servant qualified as Sv

type RAM = ReaderT Env (ExceptT Sv.ServerError IO)

data Env = Env
  {envDictIndex :: IORef Dicts.DictIndex}
