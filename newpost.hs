import Data.Time.Clock
import Data.Time.Calendar
import System.IO
import Data.List
import Control.Arrow
import Data.Char

today :: IO String
today = getCurrentTime >>= return . showGregorian . utctDay

main :: IO ()
main = do
  date <- today
  putStr "Enter the Blog Title: "
  hFlush stdout
  title <- getLine 
  let filename = makeFilename date title
  putStrLn $ "Writing new file: " ++ filename
  writeFile filename (makeBlock title) 
  putStrLn "Finished..."

type Title = String

-- Makes the top-level block
makeBlock :: Title -> String
makeBlock t = concat $ intersperse "\n" l
  where l = ["---","title: " ++ t, "short: ", "tags:", "---"]

-- Prepares the filename
makeFilename :: String -> Title -> String
makeFilename date title = concat [date, "-", f title, ".markdown"]
  where
    f = concat . intersperse "-" . words . map toLower . 
        filter (uncurry (||) . (isAlpha &&& isSpace))
