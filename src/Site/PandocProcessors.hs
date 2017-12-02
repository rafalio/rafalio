{-# LANGUAGE OverloadedStrings #-}

module Site.PandocProcessors where

import           Text.Pandoc
import           Text.Pandoc.Shared
import           Text.Pandoc.Walk
import           Text.Pandoc.Writers.HTML

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html (preEscapedToHtml, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Highlighting.Kate (highlightAs, defaultFormatOpts)
import           Text.Highlighting.Kate.Format.HTML (formatHtmlBlock)

import           Hakyll

-- Put code blocks into a figure environment. 
-- Pass extenstion to figure out what language it is
processCodeBlocks :: String -> Pandoc -> Pandoc
processCodeBlocks ext = walk (processCodeBlock ext)

processCodeBlock :: String -> Block -> Block
processCodeBlock ext b@(CodeBlock (_, classes, pairs) code) =
  let lang     = getLang classes ext
      caption  = lookup "caption" pairs
      codeHtml = formatHtmlBlock defaultFormatOpts (highlightAs lang code)
      captStr  = maybe "" (renderHtml . H.figcaption . H.span . H.toHtml) caption
      composed = renderHtml $ H.figure ! A.class_ "code" $ do
        preEscapedToHtml $ (renderHtml codeHtml) ++ captStr
  in
    RawBlock "html" composed

processCodeBlock _ x = x

-- Pandoc options with Math Mode (no TOC)
pandocWriterOptions :: WriterOptions
pandocWriterOptions = defaultHakyllWriterOptions {
  writerHTMLMathMethod = MathJax ""
}

-- For pages where I want a TOC
pandocWriterOptionsTOC :: WriterOptions
pandocWriterOptionsTOC = defaultHakyllWriterOptions {
  writerHTMLMathMethod = MathJax "",
  writerTOCDepth = defaultTOCDepth,
  writerTableOfContents = True,
  writerTemplate = Just "<div class=\"toc\"><h3>Table of Contents</h3>\n$toc$\n</div>$body$",
  writerNumberSections = True
}
  where
    defaultTOCDepth = 4

getLang _ ".lhs" = "haskell"
getLang [] _ = "text"
getLang xs _ = head xs
