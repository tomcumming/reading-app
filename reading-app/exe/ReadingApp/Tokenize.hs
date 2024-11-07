module ReadingApp.Tokenize (Paths (..), Step (..), tokenize) where

import Control.Monad.State.Strict (State, evalState, gets, modify)
import Data.Bifunctor (second)
import Data.Function ((&))
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace (traceShowM)
import ReadingApp.Dict (DictId, DictIndex (..), WordId (WordId))
import ReadingApp.PhraseIndex (PhraseIndex, phraseIndexLookup)

-- TODO use DictIndex, store phraseId or whatever
newtype Paths = Paths {unPaths :: M.Map T.Text Step}

data Step = Step
  { stepWords :: !(S.Set WordId),
    stepPaths :: !Paths
  }

tokenize :: DictIndex -> T.Text -> Paths
tokenize dIdx txt = evalState (tokenizeMemo dIdx 0 txt) mempty

-- TODO graphemes rather than Char?

type TokM = State (M.Map Int Paths)

tokenizeMemo :: DictIndex -> Int -> T.Text -> TokM Paths
tokenizeMemo dIdx i txt =
  gets (M.!? i) >>= \case
    Just ps -> pure ps
    Nothing -> case T.uncons txt of
      Nothing -> pure $ Paths mempty
      Just (c, _) -> do
        traceShowM (i, txt)
        let foundWords = dictIndexLookup dIdx txt
        let allWords = M.insertWith (<>) (T.singleton c) mempty foundWords
        ps <- Paths <$> M.traverseWithKey go allWords
        modify (M.insert i ps)
        pure ps
  where
    go :: T.Text -> S.Set WordId -> TokM Step
    go p stepWords = do
      let l = T.length p
      stepPaths <- tokenizeMemo dIdx (i + l) (T.drop l txt)
      pure Step {stepWords, stepPaths}

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
