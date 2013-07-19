import System.Environment
import System.IO
import Control.Applicative hiding ((<|>), many)
import Text.ParserCombinators.Parsec
import Control.Monad
import Numeric.LinearAlgebra

{--
pmat: pipe oriented matrix calculator

written by Ryuichi Ueda

The MIT License

Copyright (C) 2013 Ryuichi Ueda

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
--}

showUsage :: IO ()
showUsage = do hPutStr stderr
		("Usage    : pmat <opt> <file>\n" ++ 
		"Fri Jul 19 23:42:50 JST 2013\n")

version :: IO ()
version = do hPutStr stderr ("version 0.0020")

main :: IO ()
main = do
	args <- getArgs
	case args of
		[]         -> showUsage
		["-h"]     -> showUsage
		["--help"] -> showUsage
		["--ver"]  -> version
		["--v"]    -> version
		[opt,f]    -> readData f   >>= mainProc opt
		[opt]      -> readData "-" >>= mainProc opt

readData :: String -> IO String
readData "-" = getContents
readData f   = readFile f

mainProc :: String -> String -> IO ()
mainProc opt cs = putStr cs >> mainProc' (setOpt opt) (setMatrices cs)

---------------
-- data type --
---------------

data NMat = NMat (String,Matrix Double) deriving Show

data Option = Equation String Calc
            | Error String

data Calc = Terms Term [Op2]
          | Mat NMat

data Term = LongTerm StringMat [Op]
          | LongTermN Double [Op]

data Op = MulM String
           | MulN Double
           | PowN Int

data Op2 = OpMinus Term
         | OpPlus Term

data StringMat = StringMat String
               | EvaledMat NMat

--------------------------
-- execution and output --
--------------------------

mainProc' :: Option -> [NMat] -> IO ()
mainProc' (Equation ""   (Mat n)) ms = putStr . toStr $ n
mainProc' (Equation name (Mat n)) ms = putStr . toStr $ renameMat name n
                                       where renameMat new (NMat (x,m)) = NMat (new,m)
mainProc' (Equation name calc) ms = mainProc' (Equation name c ) ms
                                    where c = execCalc name calc ms

execCalc :: String -> Calc -> [NMat] -> Calc
execCalc name (Terms (LongTerm (EvaledMat m) []) []) ms = Mat m
execCalc name (Terms t [])       ms = Terms (evalTerm name t ms) []
execCalc name (Terms t (op:ops)) ms = execCalc name (Terms x ops) ms
                        where x = conTerms name y op ms
                              y = evalTerm name t ms

conTerms :: String -> Term -> Op2 -> [NMat] -> Term
conTerms _    x (OpMinus (LongTerm (EvaledMat n) [])) _ = LongTerm (EvaledMat $ matMinus (getM x) n) []
conTerms _    x (OpPlus  (LongTerm (EvaledMat n) [])) _ = LongTerm (EvaledMat $ matPlus (getM x) n) []
conTerms name x (OpMinus y)                          ms = conTerms name x (OpMinus $ evalTerm name y ms) ms
conTerms name x (OpPlus y)                           ms = conTerms name x (OpPlus $ evalTerm name y ms) ms

getM :: Term -> NMat
getM (LongTerm (EvaledMat m) []) = m

evalTerm :: String -> Term -> [NMat] -> Term
evalTerm name (LongTerm m ops)  ms = evalTerm' name m ops ms
evalTerm name (LongTermN d ops) ms = evalTerm''' name d ops ms

evalTerm' :: String -> StringMat -> [Op] -> [NMat] -> Term
evalTerm' name (EvaledMat e) ops ms = evalTerm'' name e ops ms
evalTerm' name (StringMat s) ops ms = evalTerm' name e ops ms
    where e = EvaledMat (getMat s ms)

evalTerm'' :: String -> NMat -> [Op] -> [NMat] -> Term
evalTerm'' name e [] ms = LongTerm (EvaledMat e) []
evalTerm'' name e (op:ops) ms = evalTerm'' name (conMat e op ms) ops ms 

evalTerm''' :: String -> Double -> [Op] -> [NMat] -> Term
evalTerm''' name d [] ms = LongTermN d []
evalTerm''' name d (op:ops) ms = evalTerm name x ms
     where x = conN d op ops ms

conMat :: NMat -> Op -> [NMat] -> NMat
conMat m (MulM s) ms = matMul m (getMat s ms)
conMat m (MulN d) ms = matNMul d m 

conN :: Double -> Op -> [Op] -> [NMat] -> Term
conN d (MulM s) ops ms = LongTerm (EvaledMat m) ops 
   where m = matNMul d (getMat s ms)
conN d (MulN f) ops ms = LongTermN (d*f) ops 

---------------------
-- matrix handling --
---------------------

-- get a matrix named "name"
getMat :: String -> [NMat] -> NMat
getMat nm ms = head $ filter ( f nm ) ms
                 where f nm (NMat (n,_)) = (nm == n)

matMul :: NMat -> NMat -> NMat
matMul (NMat x) (NMat y) = NMat ((fst x) ++ "*" ++ (fst y),  (snd x) <> (snd y))

matPlus :: NMat -> NMat -> NMat
matPlus (NMat x) (NMat y) = NMat ((fst x) ++ "+" ++ (fst y),  (snd x) + (snd y))

matMinus :: NMat -> NMat -> NMat
matMinus (NMat x) (NMat y) = NMat ((fst x) ++ "-" ++ (fst y),  (snd x) - (snd y))

matNMul :: Double -> NMat -> NMat
matNMul d (NMat y) = NMat (nm, nMultiply d (snd y))
                       where nm = (show d) ++ "*" ++ (fst y)

-- I can't use `*` for double-Matrix operation though I can it on ghci
nMultiply :: Double -> Matrix Double -> Matrix Double
nMultiply d m = ((r><r) arr) <> m
                where r = rows m
                      arr = [ f x d | x <- [0..(r*r-1)]] 
                      f n d = if (n `mod` r) == ( n `div` r) then d else 0.0

toStr :: NMat -> String
toStr (NMat (name,mat)) = unlines [ name ++ " " ++ t | t <- tos ]
                          where lns = toLists mat
                                tos = [ unwords [ show d | d <- ln ] | ln <- lns ] 

---------------------
-- handle of stdin --
---------------------

setMatrices :: String -> [NMat]
setMatrices cs = [ setMatrix m | m <- (getCluster $ lines cs) ]

setMatrix :: [String] -> NMat
setMatrix lns =  NMat (name,val)
                where name = head (words $ head lns)
                      val = (row><col) (concatMap toNums lns)
                      toNums str = [ read n | n <- ( drop 1 $ words str) ]
                      row = length lns
                      col = (length $ words $ head lns) - 1

getCluster :: [String] -> [[String]]
getCluster [] = []
getCluster lns = [fst d] ++ getCluster (snd d)
                      where d = span (compKey key ) lns
                            key = head $ words $ head lns
                            compKey a b = a == (head $ words b)

---------------------
-- the parser part --
---------------------

setOpt :: String -> Option
setOpt str = case parse parseOption "" str of
                                  Right opt -> opt
                                  Left err -> Error ( show err )

parseOption = try(equation) <|> try(onlyrhs) <?> "no terms"

equation = do a <- many1 letter
              char '='
              Equation a <$> calc

onlyrhs = Equation "" <$> calc

calc = Terms <$> term <*> (many (try(opPlus) <|> try(opMinus)) )

opPlus = char '+' >> OpPlus <$> term
opMinus = char '-' >> OpMinus <$> term

term = try(longterm) <|> longtermn

longterm = LongTerm <$> (StringMat <$> many1 letter) <*> many op
longtermn = LongTermN <$> (try(parseDouble) <|> parseInt) <*> many op

op = try(opMat) <|> try(opNum) <|> try(opInv)

opInv = string "^-1" >> (return . PowN) (-1)
opMat = char '*' >> many1 letter >>= return . MulM
opNum = char '*' >> (try(parseDouble) <|> parseInt) >>= return . MulN

parseDouble = do x <- many1 digit
                 char '.'
                 y <- many1 digit
                 return $ read (x++"."++y)

parseInt = do x <- many1 digit
              return $ (read x::Double)
