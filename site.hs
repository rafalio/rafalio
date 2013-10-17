--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend)
import           Hakyll
import           Data.List (intersperse)
import           Text.Pandoc
import           Text.Pandoc.Shared
import           Data.List.Split (splitOn)
import           Hakyll.Core.Routes
import           Control.Monad

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    match "static/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "posts/img/**" $ do
        route $ gsubRoute "posts/" (const "")
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match (fromList ["contact.markdown", "about.markdown", "404.markdown"]) $ do
        route   $ setExtension "html" 
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html"   defaultContext 
            >>= loadAndApplyTemplate "templates/default.html" defaultContext

    match allPattern $ do
        route   $ customRoute (preparePostString . toFilePath) `composeRoutes` (setExtension "html")
        compile $ pandocCompilerWithTransform defaultHakyllReaderOptions pandocWriterOptions (headerShift 0)
            >>= loadAndApplyTemplate "templates/post.html"   postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/comments.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx

    match (fromGlob "posts/misc/*" .||. fromGlob "posts/philosophy/*") $ do
        route $ gsubRoute ("posts/misc/") (const "") `composeRoutes` gsubRoute ("posts/philosophy/") (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompilerWithTransform defaultHakyllReaderOptions pandocWriterOptions (headerShift 0)
            >>= loadAndApplyTemplate "templates/misc.html" defaultContext 
            >>= loadAndApplyTemplate "templates/comments.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext 

    create ["archives.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx =
                    field "misc"  (\_ -> postList (return) (fromGlob "posts/misc/*")) `mappend`
                    field "philosophy" (\_ -> postList (return) (fromGlob "posts/philosophy/*")) `mappend`
                    field "posts" (\_ -> postList recentFirst postPattern) `mappend`
                    field "books" (\_ -> postList recentFirst (fromGlob "posts/books/*")) `mappend`
                    constField "title" "Archives" `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archives_template.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx 


    create ["index.html"] $ do
        route idRoute
        compile $ do
            postBodies <- ((take 10) <$> (recentFirst =<< loadAllSnapshots postPattern "content")) >>= getPostBodies

            let indexCtx = 
                  constField "posts" postBodies  `mappend` 
                  constField "title" "Home" `mappend`  defaultContext 

            makeItem postBodies
                >>= loadAndApplyTemplate "templates/index.html" indexCtx 
                >>= loadAndApplyTemplate "templates/default.html" indexCtx 

    match "templates/*" $ compile templateCompiler

    create ["feed.rss"] $ do
      route idRoute
      compile $ do
          let feedCtx = postCtx `mappend` bodyField "description"
          posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots postPattern "content"
          renderRss feedConfig feedCtx posts

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

--------------------------------------------------------------------------------

getPostBodies :: [Item String] -> Compiler String
getPostBodies = return . concat . intersperse "<hr />" . map itemBody

postPattern =  fromGlob "posts/*" .||. fromGlob "posts/dev/*" 
allPattern  =  fromGlob "posts/*" .||. fromGlob "posts/dev/*" .||. fromGlob "posts/books/*"


{-postList :: ([Item String] -> [Item String]) -> Pattern -> Compiler String-}
postList sortFilter pattern = do
    posts   <- sortFilter =<< loadAll pattern
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list

-- This gets rid of the date string in my .md post, it's kind of ugly
filterDate :: String -> String
filterDate s = (\(a,b) -> (reverse b) ++ (drop 11 . reverse $ a)) $ span (/= '/') $ reverse s

-- 
getFilename :: String -> String
getFilename =  reverse . fst . span (/= '/') . reverse

preparePostString :: String -> String
preparePostString = ((++) "posts/") . getFilename . filterDate

feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = "Rafal's Blog"
    , feedDescription = "Rafal's thoughts"
    , feedAuthorName  = "Rafal Szymanski"
    , feedAuthorEmail = "http://rafal.io"
    , feedRoot        = "http://rafal.io"
    }

-- Add MathML rendering
pandocWriterOptions :: WriterOptions
pandocWriterOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathJax ""
    }
