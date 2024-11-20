module ReadingApp.Pages.ReadThrough (RTAPI.API, server) where

import CCCEdict.Pinyin (Pinyin)
import Control.Category ((>>>))
import Control.Monad (unless, (>=>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (asks)
import Data.Char (isSpace)
import Data.Foldable (find, fold, forM_, toList)
import Data.Function ((&))
import Data.IORef (readIORef)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.Void (Void)
import ReadingApp.API (readThroughLink)
import ReadingApp.API.ReadThrough qualified as RTAPI
import ReadingApp.BestPath (bestPathFrom, bestPaths, tokenText)
import ReadingApp.Db.ReadThrough (loadReadTh, nextReadThId, strokeDataPath, writeReadTh)
import ReadingApp.Dict qualified as Dict
import ReadingApp.RAM (RAM, envDictIndex)
import ReadingApp.Tokenize (Tokens, tokenize)
import ReadingApp.Views.Wrapper (wrapperMarkup)
import Servant qualified as Sv
import Text.Blaze.Html5 qualified as B
import Text.Blaze.Html5.Attributes qualified as At

data Learning
  = LearnStrokeOrder RTAPI.CharIdx FilePath
  | LearnPronunciation RTAPI.CharIdx (NE.NonEmpty Pinyin)
  | LearnTranslation (M.Map Dict.WordId Dict.Entry)

server :: Sv.ServerT RTAPI.API RAM
server =
  RTAPI.Routes
    { rtCreate = createNew >>> liftIO,
      rtRead = readThroughPage,
      rtTokenizeChoices = \rtId maybeSearch -> do
        search <- maybe (fail "No search provided") pure maybeSearch
        choices <- handleTokenize rtId search
        renderChoices choices & pure,
      rtChoose = chooseNextToken
    }

createNew :: T.Text -> IO RTAPI.ReadThId
createNew rthName = do
  rtId <- nextReadThId
  rthLastView <- getCurrentTime
  let rthCurrentPhrase = mempty
  let rthPractice = Nothing
  let rt = RTAPI.ReadTh {rthUnTokenized = "", ..}
  writeReadTh rtId rt
  pure rtId

readThroughPage :: RTAPI.ReadThId -> RAM B.Html
readThroughPage rtId = do
  rth <- loadReadTh rtId & liftIO
  case RTAPI.rthPractice rth of
    Nothing -> do
      choices <- renderChoices <$> handleTokenize rtId (RTAPI.rthUnTokenized rth)
      renderTokenize rtId rth choices & pure
    Just practice -> do
      nl <- nextLearnable practice
      case nl of
        Nothing -> do
          liftIO $ writeReadTh rtId (rth {RTAPI.rthPractice = Nothing})
          readThroughPage rtId
        Just learning -> learningPage rtId (RTAPI.praPhrase practice) learning

learningPage :: RTAPI.ReadThId -> Seq.Seq T.Text -> Learning -> RAM B.Html
learningPage _rtId chars l = do
  body_ <- case l of
    LearnStrokeOrder i _ -> do
      c <-
        chars Seq.!? RTAPI.unCharIdx i
          & maybe (fail "Invalid char idx") pure
      pure $ do
        charInCtx i
        B.div ""
          B.! At.class_ "hanzi-writer-target"
          B.! B.dataAttribute "char" (B.textValue c)
    LearnPronunciation i _ -> charInCtx i & pure
    LearnTranslation {} -> undefined

  let head_ = do
        B.script "" B.! At.type_ "text/javascript" B.! At.src "/js/hanzi-writer/hanzi-writer.js"
        B.script "" B.! At.type_ "module" B.! At.src "/js/pages/readthrough/learning.js"

  wrapperMarkup head_ body_ & pure
  where
    charInCtx ci = B.p B.! At.class_ "char-in-ctx" $
      fold $
        flip Seq.mapWithIndex chars $
          \i c ->
            let class_ = if i == RTAPI.unCharIdx ci then "current" else ""
             in B.span (B.text c) B.! At.class_ class_

renderTokenize :: RTAPI.ReadThId -> RTAPI.ReadTh -> B.Html -> B.Html
renderTokenize rtId RTAPI.ReadTh {..} renderedChoices = wrapperMarkup head_ $ do
  unless (Seq.null rthCurrentPhrase)
    $ B.p
      B.! At.class_ "current-phrase"
    $ forM_ rthCurrentPhrase
    $ \RTAPI.Chosen {..} ->
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
        B.! At.src "/js/pages/readthrough/tokenize.js"

renderChoices :: [RTAPI.Choice] -> B.Html
renderChoices = \case
  [] -> B.p "Enter phrase..."
  choices@(firstChoice : _) ->
    makeChoice True firstChoice
      <> forM_ choices (makeChoice False)
  where
    makeChoice isSkip RTAPI.Choice {..} = B.form
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

handleTokenize :: RTAPI.ReadThId -> T.Text -> RAM [RTAPI.Choice]
handleTokenize _rtId search = do
  dictIdx <- asks envDictIndex >>= (readIORef >>> liftIO)
  let search' = processSearch search
  let tokens = tokenize dictIdx search'
  makeChoices tokens & pure

processSearch :: T.Text -> T.Text
processSearch = T.filter (isSpace >>> not)

makeChoices :: Tokens -> [RTAPI.Choice]
makeChoices tokens = do
  let paths = bestPaths tokens
  (choText, _words) <- (tokens M.!? 0) & maybe [] M.toList
  let rest = bestPathFrom paths $ T.length choText
  pure RTAPI.Choice {choText, choRest = tokenText <$> rest}

strokePaths :: Seq.Seq T.Text -> RAM (Seq.Seq (RTAPI.CharIdx, FilePath))
strokePaths =
  Seq.traverseWithIndex (\i c -> fmap (RTAPI.CharIdx i,) <$> strokeDataPath c)
    >=> (toList >>> catMaybes >>> Seq.fromList >>> pure)

pronunciations :: Seq.Seq T.Text -> RAM (Maybe (Seq.Seq (NE.NonEmpty Pinyin)))
pronunciations chars = do
  let phrase = fold chars
  dictIdx <- asks envDictIndex >>= (readIORef >>> liftIO)
  wordEntries <- Dict.lookupPhrase dictIdx phrase & liftIO
  let pinyins =
        wordEntries
          & fmap (Dict.entPinyin >>> fmap NE.singleton)
          & M.mapMaybe (toList >>> NE.nonEmpty)
          & foldl1 (NE.zipWith (<>))
          & toList
          & Seq.fromList
  (if Seq.null pinyins then Nothing else Just pinyins) & pure

translations :: Seq.Seq T.Text -> RAM (Maybe (M.Map Dict.WordId Dict.Entry))
translations chars = do
  let phrase = fold chars
  dictIdx <- asks envDictIndex >>= (readIORef >>> liftIO)
  wordEntries <- Dict.lookupPhrase dictIdx phrase & liftIO
  (if M.null wordEntries then Nothing else Just wordEntries) & pure

nextLearnable :: RTAPI.Practice -> RAM (Maybe Learning)
nextLearnable RTAPI.Practice {..} = case praLast of
  Nothing -> nextStroke (RTAPI.CharIdx 0)
  Just (RTAPI.LastStrokeOrder (RTAPI.CharIdx i)) ->
    nextStroke (succ i & RTAPI.CharIdx)
  Just (RTAPI.LastPronounce (RTAPI.CharIdx i)) ->
    nextPronunciation (succ i & RTAPI.CharIdx)
  Just RTAPI.LastTranslate -> pure Nothing
  where
    nextStroke :: RTAPI.CharIdx -> RAM (Maybe Learning)
    nextStroke i =
      strokePaths praPhrase
        >>= ( find (fst >>> (>= i))
                >>> fmap (uncurry LearnStrokeOrder)
                >>> maybe (nextPronunciation (RTAPI.CharIdx 0)) (Just >>> pure)
            )
    nextPronunciation :: RTAPI.CharIdx -> RAM (Maybe Learning)
    nextPronunciation i =
      pronunciations praPhrase
        >>= ( (>>= (Seq.!? RTAPI.unCharIdx i))
                >>> maybe
                  (translations praPhrase & fmap (fmap LearnTranslation))
                  (LearnPronunciation i >>> Just >>> pure)
            )

chooseNextToken :: RTAPI.ReadThId -> RTAPI.UserChosenToken -> RAM Void
chooseNextToken rtId RTAPI.UserChosenToken {..} = do
  RTAPI.ReadTh {..} <- loadReadTh rtId & liftIO
  let chosen = RTAPI.Chosen {csnText = chtToken, csnSkipped = chtSkipped}

  let chars = T.unpack chtToken & fmap T.singleton & Seq.fromList
  let rthPractice' = RTAPI.Practice chars Nothing

  rthLastView' <- liftIO getCurrentTime
  let rth =
        RTAPI.ReadTh
          { rthName,
            rthCurrentPhrase = rthCurrentPhrase Seq.|> chosen,
            rthLastView = rthLastView',
            rthUnTokenized = chtRest,
            rthPractice = Just rthPractice'
          }
  writeReadTh rtId rth & liftIO
  let uri = readThroughLink rtId
  Sv.throwError $ Sv.err301 {Sv.errHeaders = [("Location", "/" <> Sv.toHeader uri)]}
