module Main (main) where

import CCCEdict (defPinyin, parseLines)
import CCCEdict.Pinyin (renderPinyin)
import Control.Category ((>>>))
import Data.Function ((&))
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text qualified as T
import Streaming.Prelude qualified as S

main :: IO ()
main =
  S.stdinLn
    & S.map T.pack
    & parseLines
    & S.map defPinyin
    & S.concat
    & S.map renderPinyin
    & S.fold_ (\m p -> Map.insertWith (+) p (1 :: Int) m) mempty id
    & fmap (Map.toList >>> List.sortOn snd)
    >>= mapM_ print
