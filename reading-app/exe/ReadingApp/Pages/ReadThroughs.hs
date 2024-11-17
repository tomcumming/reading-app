module ReadingApp.Pages.ReadThroughs (API, server) where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Map qualified as M
import ReadingApp.API (readThroughLink)
import ReadingApp.API.ReadThrough (ReadTh (..), ReadThId)
import ReadingApp.API.ReadThroughs (API, Routes (..))
import ReadingApp.Db.ReadThrough (allReadThs)
import ReadingApp.RAM (RAM)
import ReadingApp.Views.Wrapper (wrapperMarkup)
import Servant qualified as Sv
import Text.Blaze.Html5 qualified as B
import Text.Blaze.Html5.Attributes qualified as At

server :: Sv.ServerT API RAM
server =
  Routes
    { rtRoot = do
        rths <- liftIO allReadThs
        rootView rths & pure
    }

rootView :: M.Map ReadThId ReadTh -> B.Html
rootView rths = wrapperMarkup head_ body
  where
    head_ = B.script "" B.! At.type_ "module" B.! At.src "/js/pages/readthroughs.js"
    body = do
      B.h1 "Read-Throughs"
      B.div B.! At.class_ "buttons" $ do
        B.button "+ Create New" B.! At.class_ "create-new-readthrough right"
        flip foldMap (M.toList rths) $ \(rtId, ReadTh {..}) ->
          B.a
            B.! At.href (B.preEscapedStringValue $ show $ Sv.linkURI $ readThroughLink rtId)
            B.! At.class_ "button"
            $ B.text rthName
