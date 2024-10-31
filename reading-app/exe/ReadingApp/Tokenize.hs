module ReadingApp.Tokenize (Paths (..), tokenize) where

import Control.Monad.State.Strict (State, evalState, gets, modify)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Word (Word32)
import ReadingApp.PhraseIndex (PhraseIndex, phraseIndexLookup)

newtype Paths = Paths {unPaths :: M.Map T.Text Paths}

tokenize :: PhraseIndex -> T.Text -> Paths
tokenize pIdx txt = evalState (tokenizeMemo pIdx txt) mempty

-- TODO graphemes rather than Char?

tokenizeMemo :: PhraseIndex -> T.Text -> State (M.Map T.Text Paths) Paths
tokenizeMemo pIdx txt =
  gets (M.!? txt) >>= \case
    Just ps -> pure ps
    Nothing -> case T.uncons txt of
      Nothing -> pure $ Paths mempty
      Just (c, _) -> do
        fromIndex <- M.traverseWithKey go $ phraseIndexLookup pIdx txt
        default_ <- M.traverseWithKey go $ M.singleton (T.singleton c) mempty
        let paths = Paths $ fromIndex `M.union` default_
        modify (M.insert txt paths)
        pure paths
  where
    go :: T.Text -> S.Set Word32 -> State (M.Map T.Text Paths) Paths
    go p _ = tokenizeMemo pIdx (T.drop (T.length p) txt)
