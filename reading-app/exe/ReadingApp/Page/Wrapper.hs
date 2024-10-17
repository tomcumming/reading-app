module ReadingApp.Page.Wrapper (wrapper) where

import Text.Blaze.Html5 qualified as B
import Text.Blaze.Html5.Attributes qualified as Attr

wrapper :: B.Html -> B.Html
wrapper inner =
  B.docTypeHtml $ do
    B.head $ do
      B.title "Reading App"
      B.link
        B.! Attr.rel "stylesheet"
        B.! Attr.type_ "text/css"
        B.! Attr.href "/static/simple.css"
      B.script mempty B.! Attr.src "/static/htmx.js"
    B.body inner
