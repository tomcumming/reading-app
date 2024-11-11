module ReadingApp.BestPath (Token (..), bestPaths, bestPathFrom) where

import Control.Category ((>>>))
import Data.Foldable1 (minimumBy)
import Data.Function ((&))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Sequence qualified as Sq
import Data.Set qualified as S
import Data.Text qualified as T
import ReadingApp.Dict (WordId)
import ReadingApp.Tokenize (Tokens)

data Token = Token
  { tokenText :: T.Text,
    tokenWords :: S.Set WordId
  }

instance Show Token where
  show Token {tokenText} = T.unpack tokenText

newtype Score = Score {scoreLen :: Int} deriving (Eq, Show)

instance Ord Score where
  Score l1 <= Score l2 = l1 <= l2

zeroScore :: Score
zeroScore = Score 0

-- TODO higher score for words in user/read-through dictionary etc
addScore :: S.Set WordId -> Score -> Score
addScore _ Score {scoreLen} = Score (succ scoreLen)

bestPathFrom :: M.Map Int Token -> Int -> Sq.Seq Token
bestPathFrom solved start
  | Just token <- solved M.!? start =
      token Sq.:<| bestPathFrom solved (start + T.length (tokenText token))
  | otherwise = mempty

bestPaths :: Tokens -> M.Map Int Token
bestPaths = flip bestPaths' mempty >>> fmap snd

bestPaths' :: Tokens -> M.Map Int (Score, Token) -> M.Map Int (Score, Token)
bestPaths' ts solved = case M.maxViewWithKey ts of
  Nothing -> solved
  Just ((i, ws), ts') ->
    let choices =
          M.mapWithKey
            ( \txt tokenWords ->
                ( addScore tokenWords $
                    maybe zeroScore (fst >>> addScore tokenWords) $
                      M.lookup (i + T.length txt) solved,
                  Token
                    { tokenText = txt,
                      tokenWords
                    }
                )
            )
            ws
        best =
          M.elems choices
            & NE.nonEmpty
            & fromMaybe (error $ "No choices at " <> show i)
            & minimumBy (comparing fst)
     in bestPaths' ts' (M.insert i best solved)
