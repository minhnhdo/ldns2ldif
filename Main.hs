import System.Environment (getArgs, getProgName)
import System.IO (IOMode (..), withFile, hGetContents)

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then do
            progName <- getProgName
            putStrLn $ "Usage: " ++ progName ++ " <ldns-file>"
        else
            withFile (head args) ReadMode
                     (\h -> do
                        contents <- hGetContents h
                        putStr contents)
