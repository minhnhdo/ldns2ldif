import Control.Monad.State (State, state, runState)
import Data.List (intersperse)
import qualified Data.Text as T ( Text, singleton, pack
                                , unpack, replace, words, lines)
import System.Environment (getArgs, getProgName)
import System.IO (IOMode (..), withFile, hGetContents)

data Data = List [Data] | Atom String
type TokenStream = [String]

instance Show Data where
    show (List l) = show l
    show (Atom s) = s

tokenize :: T.Text -> [T.Text]
tokenize = concatMap T.words . T.lines
         . T.replace (T.singleton '(') (T.pack " ( ")
         . T.replace (T.singleton ')') (T.pack " ) ")

parseAtom :: State TokenStream Data
parseAtom = state $ \(name:tts) -> (Atom name, tts)

parseList :: State TokenStream Data
parseList = state $ \ts -> aux [] ts
    where aux :: [Data] -> [String] -> (Data, TokenStream)
          aux acc ts = case ts of
            (")":tts) -> (List $ reverse acc, tts)
            [] -> error "Unexpected end of token stream"
            _ -> let (d, nts) = runState parse ts
                  in aux (d:acc) nts

parse :: State TokenStream Data
parse = state $ \ts -> case ts of
    ("(":tts) -> runState parseList tts
    [] -> error "Unexpected end of token stream"
    _ -> runState parseAtom ts

parseMany :: TokenStream -> [Data]
parseMany = aux []
    where aux :: [Data] -> TokenStream -> [Data]
          aux acc [] = reverse acc
          aux acc nts = let (d, nnts) = runState parse nts
                        in aux (d:acc) nnts

join :: String -> [String] -> String
join sep ss = foldr (++) "" (intersperse sep ss)

skeleton :: String -> String -> [Data] -> (String, String)
skeleton cn base attrs =
    (newBase, join "\n" ["dn: " ++ newBase, "cn: " ++ cn, translateAttrs attrs])
    where newBase = "cn=" ++ cn ++ "," ++ base

translateAttrs :: [Data] -> String
translateAttrs ds =
    join "\n" [show (head l) ++ ": " ++ show (l !! 1) | (List l) <- ds]

translateRecord :: String -> Data -> String
-- Schema: (<cn> ((<key> <val>) ...))
translateRecord base (List [Atom cn, List attrs]) =
    translated ++ "\nobjectclass: dnsrrset\nobjectclass: dnszone"
    where (_, translated) = skeleton cn base attrs

translate :: Data -> String
-- Schema (<cn> <base> ((<key> <val) ...) <record>...)
translate (List (Atom cn:Atom base:List attrs:records)) =
    join "\n" $ (translated ++ "\nobjectclass: dnszone") : translatedRecords
    where (newBase, translated) = skeleton cn base attrs
          translatedRecords =
            ['\n' : translateRecord newBase record | record <- records]
translate ds = error "Wrong schema: " ++ show ds

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
                    let tokenStream = map T.unpack
                                    . tokenize
                                    . T.pack
                                    $ contents
                        ds = parseMany tokenStream
                    putStrLn . join "\n\n" . map translate $ ds)
