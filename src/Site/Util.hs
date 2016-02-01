{-# LANGUAGE OverloadedStrings #-}

module Site.Util where

import Site.PandocProcessors

import Hakyll

import qualified Data.Map as M
import Data.Maybe
import Data.Time.Format
import Data.Time.Clock
import Data.List

import Control.Applicative
import System.FilePath (takeBaseName, takeDirectory, takeFileName)
import Text.Pandoc.Options

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

selectCustomPandocCompiler :: Item String -> Compiler (Item String)
selectCustomPandocCompiler item = do
    metadata <- getMetadata $ itemIdentifier item
    let hasToc   = M.member "toc" metadata
    let tocVal   = (M.lookup "toc" metadata >>= fmap fst . listToMaybe . reads) <|> (Just 4)

    let wOptions = if hasToc then (pandocWriterOptionsTOC {writerTOCDepth = fromJust tocVal}) else pandocWriterOptions

    let sections = M.member "notocsections" metadata
    let finalOptions = wOptions {writerNumberSections = not sections}

    pandocCompilerWithTransform defaultHakyllReaderOptions finalOptions processCodeBlocks

getPostBodies :: [Item String] -> Compiler String
getPostBodies = return . concat . intersperse "<hr />" . map itemBody

