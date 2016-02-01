module Site.Contexts where

import Hakyll
import Data.Maybe
import qualified Data.Map as M
import Control.Monad

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
       listTitleCtx,
       defaultContext ]

