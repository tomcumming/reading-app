module ReadingApp.API.ReadThrough where

import Control.Category ((>>>))
import Data.Aeson qualified as Aeson
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Void (Void)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Servant qualified as Sv
import Servant.HTML.Blaze qualified as B
import Text.Blaze.Html5 qualified as B
import Web.FormUrlEncoded (FromForm)

data Routes mode = Routes
  { rtCreate ::
      mode
        Sv.:- "create"
          Sv.:> Sv.Capture "name" T.Text
          Sv.:> Sv.Post '[Sv.JSON] ReadThId,
    rtRead ::
      mode
        Sv.:- Sv.Capture "rtId" ReadThId
          Sv.:> Sv.Get '[B.HTML] B.Html,
    rtTokenizeChoices ::
      mode
        Sv.:- Sv.Capture "rtId" ReadThId
          Sv.:> "tokenize-choices"
          Sv.:> Sv.QueryParam "search" T.Text
          Sv.:> Sv.Get '[B.HTML] B.Html,
    rtChoose ::
      mode
        Sv.:- Sv.Capture "rtId" ReadThId
          Sv.:> "choose"
          Sv.:> Sv.ReqBody '[Sv.FormUrlEncoded] UserChosenToken
          Sv.:> Sv.Post '[Sv.JSON] Void
  }
  deriving (Generic)

type API = Sv.NamedRoutes Routes

newtype ReadThId = ReadThId {unReadThId :: Word32}
  deriving (Eq, Ord, Bounded, Enum, Show) via Word32

instance Sv.FromHttpApiData ReadThId where
  parseUrlPiece = Sv.parseUrlPiece >>> fmap ReadThId

instance Sv.ToHttpApiData ReadThId where
  toUrlPiece = unReadThId >>> Sv.toUrlPiece

instance Aeson.ToJSON ReadThId where
  toJSON = Aeson.toJSON . unReadThId

instance Aeson.ToJSONKey ReadThId

newtype CharIdx = CharIdx {unCharIdx :: Int}
  deriving (Eq, Ord, Aeson.ToJSON, Aeson.FromJSON) via Int

data ReadThLastLearn
  = LastStrokeOrder CharIdx
  | LastPronounce CharIdx
  | LastTranslate
  deriving (Generic)

instance Aeson.ToJSON ReadThLastLearn

instance Aeson.FromJSON ReadThLastLearn

data Practice = Practice
  { praPhrase :: Seq.Seq T.Text,
    praLast :: Maybe ReadThLastLearn
  }
  deriving (Generic)

instance Aeson.ToJSON Practice

instance Aeson.FromJSON Practice

data ReadTh = ReadTh
  { rthName :: T.Text,
    rthLastView :: UTCTime,
    rthCurrentPhrase :: Seq.Seq Chosen,
    rthUnTokenized :: T.Text,
    rthPractice :: Maybe Practice
  }
  deriving (Generic)

instance Aeson.ToJSON ReadTh

instance Aeson.FromJSON ReadTh

data Choice = Choice
  { choText :: T.Text,
    choRest :: Seq.Seq T.Text
  }
  deriving (Generic)

instance Aeson.ToJSON Choice

data Chosen = Chosen
  { csnText :: T.Text,
    csnSkipped :: Bool
  }
  deriving (Generic)

instance Aeson.ToJSON Chosen

instance Aeson.FromJSON Chosen

data UserChosenToken = UserChosenToken
  { chtToken :: T.Text,
    chtSkipped :: Bool,
    chtRest :: T.Text
  }
  deriving (Generic)

instance FromForm UserChosenToken
