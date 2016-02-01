{-# LANGUAGE OverloadedStrings #-}

import           Hakyll
import qualified Control.Exception as E
import           System.Exit
import           Data.Digest.Pure.SHA

import           Site.Patterns
import           Site.Contexts
import           Site.Util
import           Site.Constants

main :: IO ()
main = do
  config <- deployConfig
  hakyllWith config $ do
    match "static/**" $ route idRoute >> compile copyFileCompiler
    match "js/**" $ route idRoute >> compile copyFileCompiler
    match "fonts/**" $ route idRoute >> compile copyFileCompiler

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
    match (fromList ["contact.markdown", "about.markdown", "404.markdown", "projects.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html"  defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext


    -- Everything else does need it
    match allPattern $ do
        route   $ customRoute (preparePostString . toFilePath) `composeRoutes` (setExtension "html")
        compile $ do
            fileHash <- fmap (showDigest . sha1 . itemBody) getResourceLBS
            getResourceBody
              >>=selectCustomPandocCompiler
              >>= loadAndApplyTemplate "templates/post.html" (postCtx `mappend` (constField "fileHash" fileHash))
              >>= saveSnapshot "content"
              >>= loadAndApplyTemplate "templates/comments.html" postCtx
              >>= loadAndApplyTemplate "templates/default.html" postCtx

    create ["archives.html"] $ do
        route idRoute
        compile $ do
            let catListCtx = mconcat $ map (\(name, pat) -> field name (const $ postListRecent pat)) catMap

            let archiveCtx = mconcat [
                    catListCtx,
                    constField "title" "Archives",
                    defaultContext
                    ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/archives_template.html" archiveCtx
                >>= loadAndApplyTemplate "templates/page.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx

    create ["index.html"] $ do
        route idRoute
        compile $ do
            postBodies <- (take 10) <$> (recentFirst =<< loadAllSnapshots allNoMiscPattern "content")
            itemTmpl <- loadBody "templates/post-item-homepage.html"
            frontpageItems <- applyTemplateList itemTmpl postCtx postBodies

            let indexCtx = mconcat
                  [  constField "recentPosts" frontpageItems,
                     constField "title" "Rafal's Blog",
                     defaultContext ]

            makeItem frontpageItems
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= loadAndApplyTemplate "templates/page.html"   indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx

    create ["feed.rss"] $ do
      route idRoute
      compile $ do
          let feedCtx = postCtx `mappend` bodyField "description"
          posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots allNoMiscPattern "content"
          renderRss feedConfig feedCtx posts

deployConfig :: IO Configuration
deployConfig = E.catch (readFile "deployConfig.conf" >>=
  (\str -> return $ defaultConfiguration { deployCommand = str})) handler
  where
    handler :: IOError -> IO Configuration
    handler e = do
      putStrLn "There was an error opening your config file. Are you sure you have deployConfig.conf?"
      exitFailure
