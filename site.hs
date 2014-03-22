--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend,mconcat)
import           Hakyll
import           Data.List (intersperse, sort, sortBy)
import           Data.Ord (comparing)
import           Algorithms.NaturalSort as NS
import           Text.Pandoc
import           Text.Pandoc.Shared
import           Text.Pandoc.Walk
import           Text.Pandoc.Writers.HTML
import           Data.List.Split (splitOn)
import           Hakyll.Core.Routes
import           Control.Monad
import qualified Data.Map as M
import           Data.Maybe
import           System.FilePath    (takeBaseName, takeDirectory, takeFileName)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html (preEscapedToHtml, (!))
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Highlighting.Kate (highlightAs, defaultFormatOpts)
import           Text.Highlighting.Kate.Format.HTML (formatHtmlBlock)


getCategory :: MonadMetadata m => Identifier -> m [String]
getCategory = return . return . takeBaseName . takeDirectory . toFilePath
--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do

    match "static/**" $ route idRoute >> compile copyFileCompiler
    match "css/*.css" $ route idRoute >> compile compressCssCompiler
    match "css/*.scss"  $ do 
      route $ setExtension "css"
      compile $ getResourceString >>=
                withItemBody (unixFilter "scss" ["--trace"]) >>=
                return . fmap compressCss

    match "templates/*" $ compile templateCompiler
    match "*.csl" $ compile cslCompiler
    match "*.bib" $ compile biblioCompiler 

    match "posts/img/**" $ do
        route $ gsubRoute "posts/" (const "")
        compile copyFileCompiler

    postTags <- buildCategories "posts/**.markdown" $ fromCapture "posts/cat/*.html"

    -- Static pages don't need comments and use a different layout
    match (fromList ["contact.markdown", "about.markdown", "404.markdown"]) $ do
        route   $ setExtension "html" 
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html"   defaultContext 
            >>= loadAndApplyTemplate "templates/default.html" defaultContext


    match allPattern $ do
        route   $ customRoute (preparePostString . toFilePath) `composeRoutes` (setExtension "html")
        compile $ getResourceBody
            >>= selectCustomPandocCompiler 
            >>= loadAndApplyTemplate "templates/post.html"   postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/comments.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx

    match ("posts/misc/*" .||. "posts/philosophy/*.markdown") $ do
        route $ gsubRoute ("posts/misc/") (const "") `composeRoutes` gsubRoute ("posts/philosophy/") (const "") `composeRoutes` setExtension "html"
        compile $ 
            pandocCompilerWithTransform defaultHakyllReaderOptions pandocWriterOptions processCodeBlocks
            >>= loadAndApplyTemplate "templates/misc.html" defaultContext 
            >>= loadAndApplyTemplate "templates/comments.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext 

    create ["archives.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx = mconcat
                  [  field "misc"       (\_ -> postList (return) ("posts/misc/*.markdown")),
                     field "philosophy" (\_ -> postList (return) ("posts/philosophy/*.markdown")),
                     field "posts"      (\_ -> postList recentFirst postPattern),
                     field "books"      (\_ -> postList recentFirst ("posts/books/*.markdown")),
                     field "dev"        (\_ -> postList recentFirst ("posts/dev/*.markdown")),
                     field "euler"        (\_ -> postList recentFirst ("posts/dev/PE/*.markdown")),
                     field "leetcode"        (\_ -> postList recentFirst ("posts/dev/leetcode/*.markdown")),
                     field "codility"        (\_ -> postList recentFirst ("posts/dev/codility/*.markdown")),
                     field "topcoder"        (\_ -> postList recentFirst ("posts/dev/topcoder/*.markdown")),
                     constField "title" "Archives",
                     defaultContext ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/archives_template.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx 

    create ["index.html"] $ do
        route idRoute
        compile $ do
            postBodies <- ((take 10) <$> (recentFirst =<< loadAllSnapshots postPattern "content")) >>= getPostBodies

            let indexCtx = mconcat
                  [  constField "posts" postBodies,
                     constField "title" "Home",
                     defaultContext ]

            makeItem postBodies
                >>= loadAndApplyTemplate "templates/index.html" indexCtx 
                >>= loadAndApplyTemplate "templates/default.html" indexCtx 


    create ["feed.rss"] $ do
      route idRoute
      compile $ do
          let feedCtx = postCtx `mappend` bodyField "description"
          posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots allPattern "content"
          renderRss feedConfig feedCtx posts

--------------------------------------------------------------------------------

-- Use the short title when showing in a page, such as in the archives.
-- Use full title when finally on the page
listTitleCtx :: Context a
listTitleCtx = field "listTitle" $ \item -> do
                 metadata <- getMetadata (itemIdentifier item)
                 return . fromJust . msum $ (flip M.lookup) metadata <$> ["short","title"]

postCtx :: Context String
postCtx = mconcat
    [  dateField "date" "%B %e, %Y",
       listTitleCtx,
       defaultContext ]

--------------------------------------------------------------------------------

getPostBodies :: [Item String] -> Compiler String
getPostBodies = return . concat . intersperse "<hr />" . map itemBody

postPattern =  "posts/*.markdown"
allPattern  =  "posts/*.markdown" .||. "posts/dev/**.markdown" .||. "posts/books/**.markdown"

postList sortFilter pattern = do
    posts   <- sortFilter =<< loadAll pattern
    itemTpl <- loadBody "templates/post-item.html"
    applyTemplateList itemTpl postCtx posts 
    {-applyTemplateList itemTpl postCtx (sortBy (\xs ys -> comparing length (toFilePath . itemIdentifier $ xs) (toFilePath.itemIdentifier $ys) `mappend` compare (itemIdentifier xs) (itemIdentifier ys) ) posts)-}

-- This gets rid of the date string in my .md post, and adds the "posts/" prefix
preparePostString :: String -> String
preparePostString = ((++) "posts/") . drop 11 . takeFileName

selectCustomPandocCompiler :: Item String -> Compiler (Item String)
selectCustomPandocCompiler item = do
    metadata <- getMetadata (itemIdentifier item)
    let wOptions = if (M.member "toc" metadata) then pandocWriterOptionsTOC else pandocWriterOptions
    pandocCompilerWithTransform defaultHakyllReaderOptions wOptions processCodeBlocks

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Rafal's Blog"
    , feedDescription = "Rafal's Blog"
    , feedAuthorName  = "Rafal Szymanski"
    , feedAuthorEmail = "http://rafal.io"
    , feedRoot        = "http://rafal.io"
    }

-- Pandoc options with Math Mode (no TOC)
pandocWriterOptions :: WriterOptions
pandocWriterOptions = defaultHakyllWriterOptions { 
  writerHTMLMathMethod = MathJax ""
}

-- For pages where I want a TOC
pandocWriterOptionsTOC :: WriterOptions
pandocWriterOptionsTOC = defaultHakyllWriterOptions { 
  writerHTMLMathMethod = MathJax "",
  writerTOCDepth = 3,
  writerTableOfContents = True,
  writerStandalone = True,
  writerTemplate = "<div class=\"toc\"><h3>Table of Contents</h3>\n$toc$\n</div>$body$",
  writerNumberSections = True
}


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
