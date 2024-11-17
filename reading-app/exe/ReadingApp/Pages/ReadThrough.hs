module ReadingApp.Pages.ReadThrough (API, server) where

import Control.Category ((>>>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Data.Char (isSpace)
import Data.Foldable (fold, forM_)
import Data.Function ((&))
import Data.IORef (readIORef)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import ReadingApp.API.ReadThrough (API, Choice (..), ReadTh (..), ReadThId, Routes (..))
import ReadingApp.BestPath (bestPathFrom, bestPaths, tokenText)
import ReadingApp.Db.ReadThrough (nextReadThId, writeReadTh)
import ReadingApp.RAM (RAM, envDictIndex)
import ReadingApp.Tokenize (Tokens, tokenize)
import ReadingApp.Views.Wrapper (wrapperMarkup)
import Servant qualified as Sv
import Text.Blaze.Html5 qualified as B
import Text.Blaze.Html5.Attributes qualified as At

server :: Sv.ServerT API RAM
server =
  Routes
    { rtCreate = \rthName -> liftIO $ do
        rtId <- nextReadThId
        rthLastView <- getCurrentTime
        let rt = ReadTh {rthName, rthLastView}
        writeReadTh rtId rt
        pure rtId,
      rtRead = \rtId -> testReadThroughPage rtId & pure,
      rtTokenizeChoices = \rtId maybeSearch -> do
        choices <- handleTokenize rtId maybeSearch
        renderChoices choices & pure
    }

testReadThroughPage :: ReadThId -> B.Html
testReadThroughPage rtId = wrapperMarkup head_ $ do
  B.input
    B.! At.name "tokenize"
    B.! B.dataAttribute "rtid" (show rtId & B.stringValue)
    B.! At.placeholder "Enter phrase..."
  B.div ""
    B.! At.class_ "buttons tokenize-choices"
  where
    head_ =
      B.script ""
        B.! At.type_ "module"
        B.! At.src "/js/pages/readthrough.js"

renderChoices :: [Choice] -> B.Html
renderChoices = \case
  [] -> B.p "Enter phrase..."
  choices@(firstChoice : _) ->
    makeChoice True firstChoice
      <> forM_ choices (makeChoice False)
  where
    makeChoice isSkip Choice {..} = B.button
      B.! At.class_ class_
      B.! B.dataAttribute "token" (B.textValue choText)
      B.! B.dataAttribute "rest" (fold choRest & B.textValue)
      $ do
        B.span (B.text choText)
        forM_ choRest $ B.text >>> B.span
      where
        class_ = (if isSkip then "skip " else "") <> "choice"

handleTokenize :: ReadThId -> Maybe T.Text -> RAM [Choice]
handleTokenize _rtId maybeSearch = do
  let maybeSearch' = fmap processSearch maybeSearch
  search <- maybe (fail "No search provided") pure maybeSearch'

  dictIdx <- asks envDictIndex >>= (readIORef >>> liftIO)
  let tokens = tokenize dictIdx search
  makeChoices tokens & pure

processSearch :: T.Text -> T.Text
processSearch = T.filter (isSpace >>> not)

makeChoices :: Tokens -> [Choice]
makeChoices tokens = do
  let paths = bestPaths tokens
  (choText, _words) <- (tokens M.!? 0) & maybe [] M.toList
  let rest = bestPathFrom paths $ T.length choText
  pure Choice {choText, choRest = tokenText <$> rest}
