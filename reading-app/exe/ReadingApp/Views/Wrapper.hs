module ReadingApp.Views.Wrapper (wrapperMarkup, wrapperMarkup') where

import Text.Blaze.Html5 qualified as B
import Text.Blaze.Html5.Attributes qualified as At

wrapperMarkup :: B.Html -> B.Html -> B.Html
wrapperMarkup head_ body = B.html B.! At.lang "en-UK" $ do
  B.head $ do
    B.title "Reading App"
    B.link B.! At.rel "stylesheet" B.! At.type_ "text/css" B.! At.href "/css/simple.css"
    B.link B.! At.rel "stylesheet" B.! At.type_ "text/css" B.! At.href "/css/reading-app.css"
    B.meta B.! At.charset "UTF-8"
    B.meta B.! At.name "viewport" B.! At.content "width=device-width"
    head_
  B.body body

wrapperMarkup' :: B.Html -> B.Html
wrapperMarkup' = wrapperMarkup mempty
