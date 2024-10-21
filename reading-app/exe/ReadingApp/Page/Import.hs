module ReadingApp.Page.Import (API, server) where

import CCCEdict qualified
import Control.Category ((>>>))
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Set qualified as Set
import Data.Text qualified as T
import GHC.Generics (Generic)
import ReadingApp.Db (DictId (DictId))
import ReadingApp.Db qualified as Db
import ReadingApp.Dict.CCCEdict (hanWord, zipCharPinyin)
import ReadingApp.Page.Wrapper (wrapper)
import ReadingApp.RAM (RAM)
import Servant qualified as Sv
import Servant.HTML.Blaze qualified as B
import Streaming.Prelude qualified as S
import System.Directory (getCurrentDirectory)
import Text.Blaze.Html5 qualified as B
import Text.Blaze.Html5.Attributes qualified as Attr

data Routes mode = Routes
  { rtImportCCCE ::
      mode
        Sv.:- "cc-cedict" Sv.:> Sv.Post '[B.HTML] B.Html,
    rtRoot :: mode Sv.:- Sv.Get '[B.HTML] B.Html
  }
  deriving (Generic)

type API = Sv.NamedRoutes Routes

server :: Sv.ServerT API RAM
server =
  Routes
    { rtImportCCCE = do
        liftIO importCCCedict

        pure $
          B.button "Dictionary imported"
            B.! Attr.disabled "",
      rtRoot = pure $ wrapper $ do
        B.h1 "Import Data"
        B.button "Import CC-CEdict"
          B.! Attr.id "import-cc-cedict"
          B.! B.customAttribute "hx-trigger" "click"
          B.! B.customAttribute "hx-post" "/import/cc-cedict"
          B.! B.customAttribute "hx-target" "#import-cc-cedict"
          B.! B.customAttribute "hx-swap" "outerHTML"
    }

importCCCedict :: IO ()
importCCCedict = do
  print =<< getCurrentDirectory
  S.readFile "data/cedict_1_0_ts_utf-8_mdbg.txt" $ \rawLines -> do
    let defsStrm =
          S.map T.pack rawLines
            & CCCEdict.parseLines
            & S.mapM parseCCCEdictEntry
            & flip S.for S.each
    Db.addDefinitions (DictId "cc-cedict") defsStrm

parseCCCEdictEntry :: (MonadFail m) => CCCEdict.Definition -> m (Maybe Db.Definition)
parseCCCEdictEntry CCCEdict.Definition {..}
  | hanWord defSimp && hanWord defTrad = do
      maybePinyin <-
        zipCharPinyin defSimp defPinyin
          & either (T.unpack >>> fail) (fmap (fmap snd) >>> pure)
      pure $ do
        py <- maybePinyin
        pure
          Db.Definition
            { defMatch = Set.fromList [defSimp, defTrad],
              defPinyin = py,
              defTrans
            }
  | otherwise = pure Nothing
