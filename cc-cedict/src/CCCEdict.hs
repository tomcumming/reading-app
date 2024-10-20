module CCCEdict
  ( Definition (..),
    parseEntry,
    parseLines,
  )
where

import Control.Category ((>>>))
import Control.Monad (unless, when)
import Control.Monad.State.Class (MonadState (get), gets, put, state)
import Control.Monad.State.Strict (StateT, evalStateT)
import Control.Monad.Trans (lift)
import Data.Char (isAscii, isNumber, isSpace)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Vector qualified as V
import Streaming.Prelude qualified as S

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
