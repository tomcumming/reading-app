module ReadingApp.PhraseIndex
  ( PhraseIndex,
    phraseIndexSingleton,
    phraseIndexLookup,
    phraseIndexFind,
  )
where

import Control.Category ((>>>))
import Data.Map qualified as Map
import Data.Set qualified as S
import Data.Set qualified as Set
import Data.Text qualified as T
import DiskData (ItemIdx)

data PhraseIndex = PhraseIndex
  { piPhrases :: Set.Set ItemIdx,
    piChildren :: Map.Map Char PhraseIndex
  }

instance Semigroup PhraseIndex where
  pi1 <> pi2 =
    PhraseIndex
      { piPhrases = piPhrases pi1 <> piPhrases pi2,
        piChildren = Map.unionWith (<>) (piChildren pi1) (piChildren pi2)
      }

instance Monoid PhraseIndex where
  mempty = PhraseIndex mempty mempty

phraseIndexSingleton :: T.Text -> ItemIdx -> PhraseIndex
phraseIndexSingleton p i = case T.uncons p of
  Nothing ->
    PhraseIndex
      { piPhrases = Set.singleton i,
        piChildren = mempty
      }
  Just (c, p') ->
    PhraseIndex
      { piPhrases = mempty,
        piChildren = Map.singleton c (phraseIndexSingleton p' i)
      }

phraseIndexLookup :: PhraseIndex -> T.Text -> Map.Map T.Text (Set.Set ItemIdx)
phraseIndexLookup = go ""
  where
    go :: T.Text -> PhraseIndex -> T.Text -> Map.Map T.Text (Set.Set ItemIdx)
    go t pIdx p
      | Just (c, p') <- T.uncons p,
        Just pIdx' <- piChildren pIdx Map.!? c =
          here
            <> go (T.snoc t c) pIdx' p'
      | otherwise = here
      where
        here
          | Set.null (piPhrases pIdx) = mempty
          | otherwise = Map.singleton t (piPhrases pIdx)

-- | Find exact match
phraseIndexFind :: PhraseIndex -> T.Text -> S.Set ItemIdx
phraseIndexFind pIdx =
  T.uncons >>> \case
    Nothing -> piPhrases pIdx
    Just (c, rest) -> case piChildren pIdx Map.!? c of
      Nothing -> mempty
      Just pIdx' -> phraseIndexFind pIdx' rest
