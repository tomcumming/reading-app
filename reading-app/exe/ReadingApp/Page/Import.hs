module ReadingApp.Page.Import (API, server) where

import CCCEdict qualified
import CCCEdict.Pinyin (Pinyin, parsePinyin)
import Control.Category ((>>>))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Generics (Generic)
import ReadingApp.Db qualified as Db
import ReadingApp.Page.Wrapper (wrapper)
import Servant qualified as Sv
import Servant.HTML.Blaze qualified as B
import Streaming.Prelude qualified as S
import System.Directory (getCurrentDirectory)
import Text.Blaze.Html5 qualified as B
import Text.Blaze.Html5.Attributes qualified as Attr
import Unicode.Char.General.Scripts (script)
import Unicode.Internal.Char.Scripts (Script (Han))

data Routes mode = Routes
  { rtImportCCCE ::
      mode
        Sv.:- "cc-cedict" Sv.:> Sv.Post '[B.HTML] B.Html,
    rtRoot :: mode Sv.:- Sv.Get '[B.HTML] B.Html
  }
  deriving (Generic)

type API = Sv.NamedRoutes Routes

server :: Sv.Server API
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
            & S.mapM parseEntry
            & flip S.for S.each
    Db.addDefinitions "cc-cedict" defsStrm

parseEntry :: (MonadFail m) => CCCEdict.Definition -> m (Maybe Db.Definition)
parseEntry CCCEdict.Definition {..}
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

-- TODO order cccedict translations by usefulness
-- TODO where does this live

zipCharPinyin ::
  T.Text ->
  V.Vector T.Text ->
  Either
    T.Text
    (Maybe (V.Vector (T.Text, Pinyin)))
zipCharPinyin word pyts
  | any (`Set.member` ignoredPinyin) pyts = pure Nothing
  | Set.member word ignoredWords = pure Nothing
  | "-" `elem` pyts = pure Nothing
  | otherwise = do
      pys <- traverse parsePinyin' pyts
      let cs = V.fromList (T.singleton <$> T.unpack word)

      unless (V.length pys == V.length cs) $
        Left "Character count doesn't match Pinyin"
      pure $ Just $ V.zip cs pys
  where
    ignoredPinyin :: Set.Set T.Text
    ignoredPinyin =
      Set.fromList
        [ "xx5",
          "m1",
          "m2",
          "m4",
          "hng5",
          "hm5"
        ]

    ignoredWords :: Set.Set T.Text
    ignoredWords =
      Set.fromList
        [ "兙",
          "兛",
          "兝",
          "兞",
          "兡",
          "兣",
          "瓩",
          "瓰",
          "瓱",
          "瓸",
          "瓼",
          "粨"
        ]

hanWord :: T.Text -> Bool
hanWord = T.all (script >>> (== Han))

parsePinyin' :: T.Text -> Either T.Text Pinyin
parsePinyin' txt =
  maybe
    (Left $ "Parsing pinyin: " <> T.pack (show txt))
    pure
    $ parsePinyin txt
