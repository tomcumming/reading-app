module CCCEdict.Pinyin
  ( Pinyin (..),
    parsePinyin,
    renderPinyin,
  )
where

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Control.Monad (guard, (>=>))
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Text qualified as T
import GHC.Generics (Generic)

data Tone
  = Tone1
  | Tone2
  | Tone3
  | Tone4
  | Tone5

data Pinyin = Pinyin
  { pinInit :: !(Maybe Initial),
    pinFinal :: !Final,
    pinTone :: !Tone
  }
  deriving (Generic)

instance ToJSON Pinyin where
  toJSON = renderPinyin >>> toJSON

instance FromJSON Pinyin where
  parseJSON =
    parseJSON >=> \txt ->
      maybe
        (fail $ "Could not parse pinyin: " <> show txt)
        pure
        (parsePinyin txt)

data Initial
  = Initb
  | Initc
  | Initch
  | Initd
  | Initf
  | Initg
  | Inith
  | Initj
  | Initk
  | Initl
  | Initm
  | Initn
  | Initp
  | Initq
  | Initr
  | Inits
  | Initsh
  | Initt
  | Initw
  | Initx
  | Inity
  | Initz
  | Initzh

data Final
  = Fina
  | Finai
  | Finan
  | Finang
  | Finao
  | Fine
  | Finei
  | Finen
  | Fineng
  | Finer
  | Fini
  | Finia
  | Finian
  | Finiang
  | Finiao
  | Finie
  | Finin
  | Fining
  | Finiong
  | Finiu
  | Fino
  | Finong
  | Finou
  | Finu
  | Finua
  | Finuai
  | Finuan
  | Finuang
  | Finue
  | Finui
  | Finun
  | Finuo
  | Finv
  | Finve

parsePinyin :: T.Text -> Maybe Pinyin
parsePinyin txtIn = inclInitial <|> finalAndTone txt
  where
    txt = T.toLower txtIn

    inclInitial = do
      (ini, txt') <- takeInitial txt
      py <- finalAndTone txt'
      pure $ py {pinInit = Just ini}

    finalAndTone t1 = do
      let pinInit = Nothing
      (pinFinal, t2) <- takeFinal t1
      (pinTone, t3) <- takeTone t2
      guard (T.null t3)
      pure Pinyin {..}

takeInitial :: T.Text -> Maybe (Initial, T.Text)
takeInitial txt
  | Just txt' <- T.stripPrefix "zh" txt = Just (Initzh, txt')
  | Just txt' <- T.stripPrefix "z" txt = Just (Initz, txt')
  | Just txt' <- T.stripPrefix "y" txt = Just (Inity, txt')
  | Just txt' <- T.stripPrefix "x" txt = Just (Initx, txt')
  | Just txt' <- T.stripPrefix "w" txt = Just (Initw, txt')
  | Just txt' <- T.stripPrefix "t" txt = Just (Initt, txt')
  | Just txt' <- T.stripPrefix "sh" txt = Just (Initsh, txt')
  | Just txt' <- T.stripPrefix "s" txt = Just (Inits, txt')
  | Just txt' <- T.stripPrefix "r" txt = Just (Initr, txt')
  | Just txt' <- T.stripPrefix "q" txt = Just (Initq, txt')
  | Just txt' <- T.stripPrefix "p" txt = Just (Initp, txt')
  | Just txt' <- T.stripPrefix "n" txt = Just (Initn, txt')
  | Just txt' <- T.stripPrefix "m" txt = Just (Initm, txt')
  | Just txt' <- T.stripPrefix "l" txt = Just (Initl, txt')
  | Just txt' <- T.stripPrefix "k" txt = Just (Initk, txt')
  | Just txt' <- T.stripPrefix "j" txt = Just (Initj, txt')
  | Just txt' <- T.stripPrefix "h" txt = Just (Inith, txt')
  | Just txt' <- T.stripPrefix "g" txt = Just (Initg, txt')
  | Just txt' <- T.stripPrefix "f" txt = Just (Initf, txt')
  | Just txt' <- T.stripPrefix "d" txt = Just (Initd, txt')
  | Just txt' <- T.stripPrefix "ch" txt = Just (Initch, txt')
  | Just txt' <- T.stripPrefix "c" txt = Just (Initc, txt')
  | Just txt' <- T.stripPrefix "b" txt = Just (Initb, txt')
  | otherwise = Nothing

takeFinal :: T.Text -> Maybe (Final, T.Text)
takeFinal txt
  | Just txt' <- T.stripPrefix "ve" txt = Just (Finve, txt')
  | Just txt' <- T.stripPrefix "v" txt = Just (Finv, txt')
  | Just txt' <- T.stripPrefix "uo" txt = Just (Finuo, txt')
  | Just txt' <- T.stripPrefix "un" txt = Just (Finun, txt')
  | Just txt' <- T.stripPrefix "ui" txt = Just (Finui, txt')
  | Just txt' <- T.stripPrefix "ue" txt = Just (Finue, txt')
  | Just txt' <- T.stripPrefix "uang" txt = Just (Finuang, txt')
  | Just txt' <- T.stripPrefix "uan" txt = Just (Finuan, txt')
  | Just txt' <- T.stripPrefix "uai" txt = Just (Finuai, txt')
  | Just txt' <- T.stripPrefix "ua" txt = Just (Finua, txt')
  | Just txt' <- T.stripPrefix "u:e" txt = Just (Finve, txt')
  | Just txt' <- T.stripPrefix "u:" txt = Just (Finv, txt')
  | Just txt' <- T.stripPrefix "u" txt = Just (Finu, txt')
  | Just txt' <- T.stripPrefix "r" txt = Just (Finer, txt')
  | Just txt' <- T.stripPrefix "ou" txt = Just (Finou, txt')
  | Just txt' <- T.stripPrefix "ong" txt = Just (Finong, txt')
  | Just txt' <- T.stripPrefix "o" txt = Just (Fino, txt')
  | Just txt' <- T.stripPrefix "iu" txt = Just (Finiu, txt')
  | Just txt' <- T.stripPrefix "iong" txt = Just (Finiong, txt')
  | Just txt' <- T.stripPrefix "ing" txt = Just (Fining, txt')
  | Just txt' <- T.stripPrefix "in" txt = Just (Finin, txt')
  | Just txt' <- T.stripPrefix "ie" txt = Just (Finie, txt')
  | Just txt' <- T.stripPrefix "iao" txt = Just (Finiao, txt')
  | Just txt' <- T.stripPrefix "iang" txt = Just (Finiang, txt')
  | Just txt' <- T.stripPrefix "ian" txt = Just (Finian, txt')
  | Just txt' <- T.stripPrefix "ia" txt = Just (Finia, txt')
  | Just txt' <- T.stripPrefix "i" txt = Just (Fini, txt')
  | Just txt' <- T.stripPrefix "er" txt = Just (Finer, txt')
  | Just txt' <- T.stripPrefix "eng" txt = Just (Fineng, txt')
  | Just txt' <- T.stripPrefix "en" txt = Just (Finen, txt')
  | Just txt' <- T.stripPrefix "ei" txt = Just (Finei, txt')
  | Just txt' <- T.stripPrefix "e" txt = Just (Fine, txt')
  | Just txt' <- T.stripPrefix "ao" txt = Just (Finao, txt')
  | Just txt' <- T.stripPrefix "ang" txt = Just (Finang, txt')
  | Just txt' <- T.stripPrefix "an" txt = Just (Finan, txt')
  | Just txt' <- T.stripPrefix "ai" txt = Just (Finai, txt')
  | Just txt' <- T.stripPrefix "a" txt = Just (Fina, txt')
  | otherwise = Nothing

takeTone :: T.Text -> Maybe (Tone, T.Text)
takeTone txt
  | Just txt' <- T.stripPrefix "1" txt = Just (Tone1, txt')
  | Just txt' <- T.stripPrefix "2" txt = Just (Tone2, txt')
  | Just txt' <- T.stripPrefix "3" txt = Just (Tone3, txt')
  | Just txt' <- T.stripPrefix "4" txt = Just (Tone4, txt')
  | Just txt' <- T.stripPrefix "5" txt = Just (Tone5, txt')
  | otherwise = Nothing

renderPinyin :: Pinyin -> T.Text
renderPinyin Pinyin {..} =
  maybe "" renderInitial pinInit
    <> renderFinal pinFinal
    <> renderTone pinTone

renderInitial :: Initial -> T.Text
renderInitial = \case
  Initb -> "b"
  Initc -> "c"
  Initch -> "ch"
  Initd -> "d"
  Initf -> "f"
  Initg -> "g"
  Inith -> "h"
  Initj -> "j"
  Initk -> "k"
  Initl -> "l"
  Initm -> "m"
  Initn -> "n"
  Initp -> "p"
  Initq -> "q"
  Initr -> "r"
  Inits -> "s"
  Initsh -> "sh"
  Initt -> "t"
  Initw -> "w"
  Initx -> "x"
  Inity -> "y"
  Initz -> "z"
  Initzh -> "zh"

renderFinal :: Final -> T.Text
renderFinal = \case
  Fina -> "a"
  Finai -> "ai"
  Finan -> "an"
  Finang -> "ang"
  Finao -> "ao"
  Fine -> "e"
  Finei -> "ei"
  Finen -> "en"
  Fineng -> "eng"
  Finer -> "er"
  Fini -> "i"
  Finia -> "ia"
  Finian -> "ian"
  Finiang -> "iang"
  Finiao -> "iao"
  Finie -> "ie"
  Finin -> "in"
  Fining -> "ing"
  Finiong -> "iong"
  Finiu -> "iu"
  Fino -> "o"
  Finong -> "ong"
  Finou -> "ou"
  Finu -> "u"
  Finua -> "ua"
  Finuai -> "uai"
  Finuan -> "uan"
  Finuang -> "uang"
  Finue -> "ue"
  Finui -> "ui"
  Finun -> "un"
  Finuo -> "uo"
  Finv -> "v"
  Finve -> "ve"

renderTone :: Tone -> T.Text
renderTone = \case
  Tone1 -> "1"
  Tone2 -> "2"
  Tone3 -> "3"
  Tone4 -> "4"
  Tone5 -> "5"
