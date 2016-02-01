{-# LANGUAGE OverloadedStrings #-}

module Site.Util where

import           Text.Pandoc
import           Text.Pandoc.Shared
import           Text.Pandoc.Walk
import           Text.Pandoc.Writers.HTML
import Hakyll
import qualified Data.Map as M
import Data.Maybe

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html (preEscapedToHtml, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Highlighting.Kate (highlightAs, defaultFormatOpts)
import           Text.Highlighting.Kate.Format.HTML (formatHtmlBlock)

import           Data.Time.Format
import           Data.Time.Clock

import Data.List
import Control.Applicative

import Site.Contexts


import           System.FilePath    (takeBaseName, takeDirectory, takeFileName)

getPostBodies :: [Item String] -> Compiler String
getPostBodies = return . concat . intersperse "<hr />" . map itemBody

postList sortFilter pattern = do
    posts   <- sortFilter =<< loadAll pattern
    itemTpl <- loadBody "templates/post-item.html"
    applyTemplateList itemTpl postCtx posts

-- Sort by recency
postListRecent pat = postList recentFirst pat

-- This gets rid of the date string in my .md post, and adds the "posts/" prefix
-- This is unsafe
preparePostString :: String -> String
preparePostString path =
    let fn = takeFileName path
        parsedTime = parseTimeM True defaultTimeLocale "%Y-%m-%d" (take 10 fn) :: Maybe UTCTime
    in
      ((++) "posts/") $ case parsedTime of
          Nothing -> fn         -- parse failed, no date available, keep filename
          Just _  -> drop 11 fn -- get rid of the timestamp

-- Put code blocks into a figure environment

processCodeBlocks :: Pandoc -> Pandoc
processCodeBlocks = walk processCodeBlock

processCodeBlock :: Block -> Block
processCodeBlock b@(CodeBlock (_, classes, pairs) code) =
  let lang     = getLang classes
      caption  = lookup "caption" pairs
      codeHtml = formatHtmlBlock defaultFormatOpts (highlightAs lang code)
      captStr  = maybe "" (renderHtml . H.figcaption . H.span . H.toHtml) caption
      composed = renderHtml $ H.figure ! A.class_ "code" $ do
        preEscapedToHtml $ (renderHtml codeHtml) ++ captStr
  in
    RawBlock "html" composed

processCodeBlock x = x

getLang [] = "text"
getLang xs = head xs

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
  writerStandalone = True,
  writerTemplate = "<div class=\"toc\"><h3>Table of Contents</h3>\n$toc$\n</div>$body$",
  writerNumberSections = True
}
  where
    defaultTOCDepth = 4


selectCustomPandocCompiler :: Item String -> Compiler (Item String)
selectCustomPandocCompiler item = do
    metadata <- getMetadata $ itemIdentifier item
    let hasToc   = M.member "toc" metadata
    let tocVal   = (M.lookup "toc" metadata >>= fmap fst . listToMaybe . reads) <|> (Just 4)

    let wOptions = if hasToc then (pandocWriterOptionsTOC {writerTOCDepth = fromJust tocVal}) else pandocWriterOptions

    let sections = M.member "notocsections" metadata
    let finalOptions = wOptions {writerNumberSections = not sections}

    pandocCompilerWithTransform defaultHakyllReaderOptions finalOptions processCodeBlocks

