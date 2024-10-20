module CCCEdict
  ( Definition (..),
    parseEntry,
    parseLines,
    hanWord,
    zipCharPinyin,
  )
where

import CCCEdict.Pinyin (Pinyin, parsePinyin)
import Control.Category ((>>>))
import Control.Monad (unless, when)
import Control.Monad.State.Class (MonadState (get), gets, put, state)
import Control.Monad.State.Strict (StateT, evalStateT)
import Control.Monad.Trans (lift)
import Data.Char (isAscii, isNumber, isSpace)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector qualified as V
import Streaming.Prelude qualified as S
import Unicode.Char.General.Scripts (Script (Han), script)

data Definition = Definition
  { defSimp :: !T.Text,
    defTrad :: !T.Text,
    defPinyin :: !(V.Vector T.Text),
    defTrans :: !(V.Vector T.Text)
  }
  deriving (Show)

dropCR :: T.Text -> T.Text
dropCR txt = fromMaybe txt $ T.stripSuffix "\r" txt

parseLines :: (MonadFail m) => S.Stream (S.Of T.Text) m () -> S.Stream (S.Of Definition) m ()
parseLines =
  S.map dropCR
    >>> S.zip (S.each [1 :: Int ..])
    >>> S.dropWhile (snd >>> T.isPrefixOf "#")
    >>> S.filter (snd >>> isMultiLingual >>> not)
    >>> S.mapM (uncurry parseAndReport)

isMultiLingual :: T.Text -> Bool
isMultiLingual =
  T.takeWhile (isSpace >>> not)
    >>> T.any (\c -> isAscii c || isNumber c)

parseAndReport :: (MonadFail m) => Int -> T.Text -> m Definition
parseAndReport l =
  parseEntry
    >>> either
      (T.unpack >>> (("On line " <> show l <> ": ") <>) >>> fail)
      pure

parseEntry :: T.Text -> Either T.Text Definition
parseEntry = evalStateT $ do
  defSimp <- state (T.span (isSpace >>> not))
  when (T.null defSimp) $ lift $ Left "Could not parse simplified"
  oneSpace

  defTrad <- state (T.span (isSpace >>> not))
  when (T.null defSimp) $ lift $ Left "Could not parse traditional"
  oneSpace

  defPinyin <- parsePinyins
  oneSpace

  defTrans <- parseTrans
  rest <- get
  unless (T.null rest) $ lift $ Left "Expected end of line"

  pure Definition {..}

expect :: Char -> StateT T.Text (Either T.Text) ()
expect c =
  gets T.uncons >>= \case
    Just (c', rest) | c == c' -> put rest
    r ->
      lift $
        Left $
          ("Expected " <> T.pack (show c) <> ", got: ")
            <> maybe "<EOS>" (fst >>> show >>> T.pack) r

oneSpace :: StateT T.Text (Either T.Text) ()
oneSpace = expect ' '

parsePinyins :: StateT T.Text (Either T.Text) (V.Vector T.Text)
parsePinyins = do
  expect '['
  pinyinStr <- state (T.span (/= ']'))
  expect ']'
  pure $ V.fromList $ T.splitOn " " pinyinStr

parseTrans :: StateT T.Text (Either T.Text) (V.Vector T.Text)
parseTrans = expect '/' >> go >>= (V.fromList >>> pure)
  where
    go =
      gets T.null >>= \case
        True -> pure mempty
        False -> do
          def <- state (T.span (/= '/'))
          expect '/'
          (def :) <$> go

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
