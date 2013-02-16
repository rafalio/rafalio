--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative ((<$>))
import           Data.Monoid         (mappend)
import           Hakyll
import           Data.List (intersperse)
import           Text.Pandoc
import           Data.List.Split (splitOn)
import           Hakyll.Core.Routes

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    match "stuff/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match (fromList ["contact.markdown", "about.markdown"]) $ do
        route   $ setExtension "html" 
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html"   defaultContext 
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route   $ customRoute (filterDate . toFilePath) `composeRoutes` (setExtension "html")
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocWriterOptions
            >>= loadAndApplyTemplate "templates/post.html"   postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/comments.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "posts/misc/*" $ do
        route $ gsubRoute "posts/misc/" (const "") `composeRoutes` (setExtension "html")
        compile $ pandocCompilerWith defaultHakyllReaderOptions pandocWriterOptions
            >>= loadAndApplyTemplate "templates/misc.html" defaultContext 
            >>= loadAndApplyTemplate "templates/comments.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext 
            >>= relativizeUrls

    create ["archives.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx =
                    field "posts" (\_ -> postList recentFirst) `mappend`
                    constField "title" "Archives"              `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archives.html" archiveCtx
                >>= loadAndApplyTemplate "templates/page.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx 
                >>= relativizeUrls


    create ["index.html"] $ do
        route idRoute
        compile $ do
            postBodies <- ((take 7) <$> (recentFirst <$> loadAllSnapshots "posts/*" "content")) >>= getPostBodies

            let indexCtx = 
                  constField "posts" postBodies  `mappend` 
                  constField "title" "Home" `mappend`  defaultContext 

            makeItem postBodies
                >>= loadAndApplyTemplate "templates/index.html" indexCtx 
                >>= loadAndApplyTemplate "templates/default.html" indexCtx 
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    create ["feed.rss"] $ do
      route idRoute
      compile $ do
          let feedCtx = postCtx `mappend` bodyField "description"
          posts <- take 10 . recentFirst <$> loadAllSnapshots "posts/*" "content"
          renderRss feedConfig feedCtx posts

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

--------------------------------------------------------------------------------

getPostBodies :: [Item String] -> Compiler String
getPostBodies = return . concat . intersperse "<hr />" . map itemBody

postList :: ([Item String] -> [Item String]) -> Compiler String
postList sortFilter = do
    posts   <- sortFilter <$> loadAll "posts/*"
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl postCtx posts
    return list

-- This gets rid of the date string in my .md post, it's kind of ugly
filterDate :: String -> String
filterDate s = (\(a,b) -> (reverse b) ++ (drop 11 . reverse $ a)) $ span (/= '/') $ reverse s

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
    { writerHTMLMathMethod = MathML Nothing
    }
