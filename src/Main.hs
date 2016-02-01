{-# LANGUAGE OverloadedStrings #-}

import           Hakyll
import qualified Control.Exception as E
import           System.Exit

import           Site.Contexts
import           Site.Routing

main :: IO ()
main = do
  config <- deployConfig
  hakyllWith config $ do
    postTags <- buildCategories "posts/**.markdown" $ fromCapture "posts/cat/*.html"

    assortedStaticRoutes
    mkStaticPages
    postsRoute
    mkArchiveRoute
    mkIndexRoute
    mkFeedRoute

deployConfig :: IO Configuration
deployConfig = E.catch (readFile "deployConfig.conf" >>=
  (\str -> return $ defaultConfiguration { deployCommand = str})) handler
  where
    handler :: IOError -> IO Configuration
    handler e = do
      putStrLn "There was an error opening your config file. Are you sure you have deployConfig.conf?"
      exitFailure

