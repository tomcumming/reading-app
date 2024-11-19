module ReadingApp.Config
  ( Config (..),
    loadConfig,
  )
where

import Data.Aeson qualified as Aeson
import GHC.Generics (Generic)

data Config = Config
  {cfgStrokeData :: FilePath}
  deriving (Generic)

instance Aeson.ToJSON Config

instance Aeson.FromJSON Config

loadConfig :: IO Config
loadConfig =
  Aeson.decodeFileStrict "config.json"
    >>= maybe (fail "Could not load config file") pure
