{-# LANGUAGE OverloadedStrings #-}

module Site.Contexts where

import Hakyll
import Data.Maybe
import Data.List.Split
import qualified Data.Map as M
import Control.Monad

import Site.Patterns
import Site.Util

-- Archive context

catListCtx = mconcat $ map mkField catMap
  where 
    mkField (name,pat) = field name (const $ postListRecent pat)

archiveCtx = mconcat [
    catListCtx,
    constField "title" "Archives",
    defaultContext
  ]

-- Use the short title when showing in a page, such as in the archives.
-- Use full title when finally on the page
listTitleCtx :: Context a
listTitleCtx = field "listTitle" $ \item -> do
                 metadata <- getMetadata (itemIdentifier item)
                 return . fromJust . msum $ (flip M.lookup) metadata <$> ["short","title"]

postCtx :: Context String
postCtx = mconcat
    [  dateField "date" "%B %e, %Y",
       modificationTimeField "last_updated" "%B %e, %Y",
       field "disqus_id" (return . disqus_id),
       listTitleCtx,
       defaultContext ]
  where
    disqus_id i = last $ splitOn "/" (show $ itemIdentifier i)

postList sortFilter pattern = do
    posts   <- sortFilter =<< loadAll pattern
    itemTpl <- loadBody "templates/post-item.html"
    applyTemplateList itemTpl postCtx posts

-- Sort by recency
postListRecent pat = postList recentFirst pat

