import Control.Applicative ((<$>), (<*>))
import Control.Monad.State (State, state, runState)
import Data.List (intersperse)
import qualified Data.Text as T ( Text, singleton, pack
                                , unpack, replace, words, lines)
import System.Environment (getArgs, getProgName)
import System.IO (IOMode (..), withFile, hGetContents)

data Atom = L [Atom] | Var String | Str String
    deriving Eq
data Data = List [Data] | Atom Atom
    deriving Eq
type TokenStream = [String]

instance Show Atom where
    show (L l) = foldr (\x acc -> show x ++ acc) "" l
    show (Var s) = s
    show (Str s) = s

instance Show Data where
    show (List l) = show l
    show (Atom a) = show a

tokenize :: T.Text -> [T.Text]
tokenize = concatMap T.words . T.lines
         . T.replace (T.singleton '(') (T.pack " ( ")
         . T.replace (T.singleton ')') (T.pack " ) ")

parseAtom :: State TokenStream Data
parseAtom = state $ \(atom:tts) -> (Atom $ disect atom, tts)
  where disect :: String -> Atom
        disect str = let r = filter (\a -> case a of
                                                Str [] -> False
                                                _ -> True)
                                  $ aux [] str
                     in case r of [x] -> x
                                  _ -> L r
          where aux :: [Atom] -> String -> [Atom]
                aux acc [] = acc
                aux acc s =
                    let (a, b) = break (== '#') s
                    in case b of
                       [] -> aux (Str a:acc) []
                       _ -> let (c, d) = break (== '#') (tail b)
                            in case d of
                               [] -> error "Parse error on atom"
                               _ -> aux (Str a:Var ("#" ++ c ++ "#"):acc)
                                        (tail d)

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
translateRecord base (List [Atom (Str cn), List attrs]) =
    translated ++ "\nobjectclass: dnsrrset\nobjectclass: dnszone"
    where (_, translated) = skeleton cn base attrs

translate :: Data -> String
-- Schema (<cn> <base> ((<key> <val) ...) <record>...)
translate (List (Atom (Str cn):Atom (Str base):List attrs:records)) =
    join "\n" $ (translated ++ "\nobjectclass: dnszone") : translatedRecords
    where (newBase, translated) = skeleton cn base attrs
          translatedRecords =
            ['\n' : translateRecord newBase record | record <- records]
translate ds = error $ "Wrong schema: " ++ show ds

replace' :: (Atom, Atom, Data) -> Data
replace' (v, value, f) = replace v value f

replace :: Atom -> Atom -> Data -> Data
replace v value (List l) = List . map (replace v value) $ l
replace v value (Atom (L l)) = Atom . L
                             . map (replaceAtom v value) $ l
replace v value (Atom a) = Atom $ replaceAtom v value a

replaceAtom :: Atom -> Atom -> Atom -> Atom
replaceAtom _ _ str@(Str _) = str
replaceAtom (Var n1) value v@(Var n2)
  | n1 == n2  = value
  | otherwise = v

extractAtom :: Data -> Atom
extractAtom (Atom a) = a

expandForEach :: Data -> [Data]
expandForEach
    (List [Atom (Var "#for-each#"), Atom var@(Var _), List values, form]) =
    map replace' $ (,,) <$> [var] <*> map extractAtom values <*> expand [form]
expandForEach x = [x]

expand :: [Data] -> [Data]
expand = concatMap expandForEach

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
                    putStrLn . join "\n\n" . map translate $ expand ds)
