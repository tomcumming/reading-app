module ReadingApp.Pages.ReadThrough (API, server) where

import Control.Category ((>>>))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Data.Char (isSpace)
import Data.Foldable (fold, forM_)
import Data.Function ((&))
import Data.IORef (readIORef)
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.Void (Void)
import ReadingApp.API (readThroughLink)
import ReadingApp.API.ReadThrough (API, Choice (..), Chosen (..), ReadTh (..), ReadThId, Routes (..), UserChosenToken (..))
import ReadingApp.BestPath (bestPathFrom, bestPaths, tokenText)
import ReadingApp.Db.ReadThrough (loadReadTh, nextReadThId, writeReadTh)
import ReadingApp.RAM (RAM, envDictIndex)
import ReadingApp.Tokenize (Tokens, tokenize)
import ReadingApp.Views.Wrapper (wrapperMarkup)
import Servant qualified as Sv
import Text.Blaze.Html5 qualified as B
import Text.Blaze.Html5.Attributes qualified as At

server :: Sv.ServerT API RAM
server =
  Routes
    { rtCreate = createNew >>> liftIO,
      rtRead = \rtId -> do
        rth <- loadReadTh rtId & liftIO
        choices <- handleTokenize rtId (rthUnTokenized rth)
        testReadThroughPage rtId rth (renderChoices choices) & pure,
      rtTokenizeChoices = \rtId maybeSearch -> do
        search <- maybe (fail "No search provided") pure maybeSearch
        choices <- handleTokenize rtId search
        renderChoices choices & pure,
      rtChoose = chooseNextToken
    }

createNew :: T.Text -> IO ReadThId
createNew rthName = do
  rtId <- nextReadThId
  rthLastView <- getCurrentTime
  let rthCurrentPhrase = mempty
  let rt = ReadTh {rthName, rthLastView, rthCurrentPhrase, rthUnTokenized = ""}
  writeReadTh rtId rt
  pure rtId

testReadThroughPage :: ReadThId -> ReadTh -> B.Html -> B.Html
testReadThroughPage rtId ReadTh {..} renderedChoices = wrapperMarkup head_ $ do
  unless (Seq.null rthCurrentPhrase)
    $ B.p
      B.! At.class_ "current-phrase"
    $ forM_ rthCurrentPhrase
    $ \Chosen {..} ->
      B.span B.! At.class_ (if csnSkipped then "" else "skipped") $ B.text csnText
  B.input
    B.! At.name "tokenize"
    B.! B.dataAttribute "rtid" (show rtId & B.stringValue)
    B.! At.placeholder "Enter phrase..."
    B.! At.value (B.textValue rthUnTokenized)
  B.div B.! At.class_ "buttons tokenize-choices" $ renderedChoices
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
    makeChoice isSkip Choice {..} = B.form
      B.! At.method "post"
      B.! At.action "/readthrough/1/choose" -- TODO
      $ do
        B.input
          B.! At.type_ "hidden"
          B.! At.name "chtToken"
          B.! At.value (B.textValue choText)
        B.input
          B.! At.type_ "hidden"
          B.! At.name "chtRest"
          B.! At.value (fold choRest & B.textValue)
        B.input
          B.! At.type_ "hidden"
          B.! At.name "chtSkipped"
          B.! At.value (show isSkip & B.stringValue)
        B.button
          B.! At.class_ class_
          B.! B.dataAttribute "token" (B.textValue choText)
          B.! B.dataAttribute "rest" (fold choRest & B.textValue)
          $ do
            B.span (B.text choText)
            forM_ choRest $ B.text >>> B.span
      where
        class_ = (if isSkip then "skip " else "") <> "choice"

handleTokenize :: ReadThId -> T.Text -> RAM [Choice]
handleTokenize _rtId search = do
  dictIdx <- asks envDictIndex >>= (readIORef >>> liftIO)
  let search' = processSearch search
  let tokens = tokenize dictIdx search'
  makeChoices tokens & pure

processSearch :: T.Text -> T.Text
processSearch = T.filter (isSpace >>> not)

makeChoices :: Tokens -> [Choice]
makeChoices tokens = do
  let paths = bestPaths tokens
  (choText, _words) <- (tokens M.!? 0) & maybe [] M.toList
  let rest = bestPathFrom paths $ T.length choText
  pure Choice {choText, choRest = tokenText <$> rest}

chooseNextToken :: ReadThId -> UserChosenToken -> RAM Void
chooseNextToken rtId UserChosenToken {..} = do
  ReadTh {..} <- loadReadTh rtId & liftIO
  let chosen = Chosen {csnText = chtToken, csnSkipped = chtSkipped}
  rthLastView' <- liftIO getCurrentTime
  let rth =
        ReadTh
          { rthName,
            rthCurrentPhrase = rthCurrentPhrase Seq.|> chosen,
            rthLastView = rthLastView',
            rthUnTokenized = chtRest
          }
  writeReadTh rtId rth & liftIO
  let uri = readThroughLink rtId
  Sv.throwError $ Sv.err301 {Sv.errHeaders = [("Location", "/" <> Sv.toHeader uri)]}
