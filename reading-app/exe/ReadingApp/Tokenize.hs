module ReadingApp.Tokenize (Tokens, tokenize) where

import Data.Bifunctor (second)
import Data.Function ((&))
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import ReadingApp.Dict (DictId, DictIndex (..), WordId (WordId))
import ReadingApp.PhraseIndex (PhraseIndex, phraseIndexLookup)

type Tokens = M.Map Int (M.Map T.Text (S.Set WordId))

tokenize :: DictIndex -> T.Text -> Tokens
tokenize dIdx = tokenize' dIdx 0

tokenize' :: DictIndex -> Int -> T.Text -> M.Map Int (M.Map T.Text (S.Set WordId))
tokenize' dIdx i txt = case T.uncons txt of
  Nothing -> mempty
  Just (c, txt') ->
    M.insert
      i
      ( M.insertWith (<>) (T.singleton c) mempty $
          dictIndexLookup dIdx txt
      )
      $ tokenize' dIdx (succ i) txt'

dictIndexLookup :: DictIndex -> T.Text -> M.Map T.Text (S.Set WordId)
dictIndexLookup (DictIndex dIdxs) txt =
  M.toList dIdxs
    >>= uncurry go
    & M.fromListWith (<>)
  where
    go :: DictId -> PhraseIndex -> [(T.Text, S.Set WordId)]
    go dIdx pIdx =
      phraseIndexLookup pIdx txt
        & M.toList
        & fmap (second (S.mapMonotonic (WordId dIdx)))
