{-# LANGUAGE OverloadedStrings #-}

module Site.Patterns where

import Hakyll

-- a pattern to match all my content
allPattern       =  foldl1 (.||.) (map snd catMap)
allNoMiscPattern =  allPattern .&&. (complement "posts/misc/*.markdown")

-- Category map from field name to where it's located in my drive
catMap :: [(String, Pattern)]
catMap = [
    ("travel",      "posts/travel/**.markdown"),
    ("restaurants", "posts/restaurants/**.markdown"),
    ("misc",        "posts/misc/*.markdown"),
    ("philosophy",  "posts/philosophy/*.markdown"),
    ("posts",       "posts/*.markdown"),
    ("reports",     "posts/reports/*.markdown"),
    ("books",       "posts/books/*.markdown"),
    ("dev",         "posts/dev/*.markdown"),
    ("euler",       "posts/dev/PE/*.markdown"),
    ("leetcode",    "posts/dev/leetcode/*.markdown"),
    ("codility",    "posts/dev/codility/*.markdown"),
    ("topcoder",    "posts/dev/topcoder/*.markdown")
  ]
