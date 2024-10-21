module ReadingApp.Dict.CCCEdict
  ( zipCharPinyin,
    hanWord,
  )
where

import CCCEdict.Pinyin (Pinyin, parsePinyin)
import Control.Category ((>>>))
import Control.Monad (unless)
import Data.Sequence qualified as Sq
import Data.Set qualified as Set
import Data.Text qualified as T
import Unicode.Char.General.Scripts (Script (Han), script)

-- TODO order cccedict translations by usefulness

zipCharPinyin ::
  T.Text ->
  Sq.Seq T.Text ->
  Either
    T.Text
    (Maybe (Sq.Seq (T.Text, Pinyin)))
zipCharPinyin word pyts
  | any (`Set.member` ignoredPinyin) pyts = pure Nothing
  | Set.member word ignoredWords = pure Nothing
  | "-" `elem` pyts = pure Nothing
  | otherwise = do
      pys <- traverse parsePinyin' pyts
      let cs = Sq.fromList (T.singleton <$> T.unpack word)

      unless (Sq.length pys == Sq.length cs) $
        Left "Character count doesn't match Pinyin"
      pure $ Just $ Sq.zip cs pys
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
