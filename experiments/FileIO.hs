import System.IO

getFileContents :: FilePath -> IO ()
getFileContents file = do
    contents <- readFile file
    putStr contents


main :: IO ()
main = do
    getFileContents "Sqrt.hs"