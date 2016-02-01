{-# LANGUAGE OverloadedStrings #-}

module Site.Routing where

import Hakyll

import Site.Patterns
import Site.Contexts
import Site.Constants
import Site.Util

import Data.Digest.Pure.SHA

-- The route and compiler for all of the things under posts/
postsRoute =  match allPattern $ do
  route   $ customRoute (preparePostString . toFilePath) `composeRoutes` (setExtension "html")
  compile $ do
    fileHash <- fmap (showDigest . sha1 . itemBody) getResourceLBS
    getResourceBody
      >>=selectCustomPandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" (postCtx `mappend` (constField "fileHash" fileHash))
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/comments.html" postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx

-- Creating the archive.html which lists everything
mkArchiveRoute = create ["archives.html"] $ do
  route idRoute
  compile $ do
    makeItem ""
      >>= loadAndApplyTemplate "templates/archives_template.html" archiveCtx
      >>= loadAndApplyTemplate "templates/page.html" archiveCtx
      >>= loadAndApplyTemplate "templates/default.html" archiveCtx

-- Route for creating the main page
mkIndexRoute = create ["index.html"] $ do
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

-- Route for creating the RSS feed
mkFeedRoute = create ["feed.rss"] $ do
  route idRoute
  compile $ do
    let feedCtx = postCtx `mappend` bodyField "description"
    posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots allNoMiscPattern "content"
    renderRss feedConfig feedCtx posts

-- Routes for things css/js/templates etc
assortedStaticRoutes = do
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

-- Static pages don't need comments and use a different layout than the rest
mkStaticPages =
  match (fromList ["contact.markdown", "about.markdown", "404.markdown", "projects.markdown"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/page.html"  defaultContext
      >>= loadAndApplyTemplate "templates/default.html" defaultContext


