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
		"Sat Jul 20 12:54:37 JST 2013\n")

version :: IO ()
version = do hPutStr stderr ("version 0.0022")

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

data Option = Eq String Calc
            | Error String

data Calc = Terms Term [Op2]
          | AnsM NMat

data Term = TermS String [Op]
          | TermN Double [Op]
          | TermE NMat [Op]

data Op = MulM String
        | MulN Double
        | PowN Int

data Op2 = OpMinus Term
         | OpPlus Term

--------------------------
-- execution and output --
--------------------------

mainProc' :: Option -> [NMat] -> IO ()
mainProc' (Eq ""   (AnsM n)) ms = putStr . toStr $ n
mainProc' (Eq name (AnsM n)) ms = putStr . toStr $ renameMat name n
                                    where renameMat new (NMat (x,m)) = NMat (new,m)
mainProc' (Eq name calc)     ms = mainProc' (Eq name c ) ms
                                    where c = execCalc calc ms

execCalc :: Calc -> [NMat] -> Calc
execCalc (Terms (TermE m []) []) ms = AnsM m
execCalc (Terms t [])       ms = Terms (evalTerm t ms) []
execCalc (Terms t (op:ops)) ms = execCalc (Terms x ops) ms
                        where x = conTerms y op ms
                              y = evalTerm t ms

conTerms :: Term -> Op2 -> [NMat] -> Term
conTerms x (OpMinus (TermE n [])) _ = TermE (matMinus (getM x) n) []
conTerms x (OpPlus  (TermE n [])) _ = TermE (matPlus (getM x) n) []
conTerms x (OpMinus y)           ms = conTerms x (OpMinus $ evalTerm y ms) ms
conTerms x (OpPlus y)            ms = conTerms x (OpPlus $ evalTerm y ms) ms

getM :: Term -> NMat
getM (TermE m []) = m

evalTerm :: Term -> [NMat] -> Term
evalTerm (TermE e []) ms = TermE e []
evalTerm (TermN n []) ms = TermN n []
evalTerm (TermE e (op:ops)) ms = evalTerm (TermE (conMat e op ms) ops) ms 
evalTerm (TermN d ops) ms = evalTerm' d ops ms
evalTerm (TermS s ops) ms = evalTerm (TermE (getMat s ms) ops) ms

evalTerm' :: Double -> [Op] -> [NMat] -> Term
evalTerm' d (op:ops) ms = evalTerm x ms
     where x = conN d op ops ms

conMat :: NMat -> Op -> [NMat] -> NMat
conMat m (MulM s) ms = matMul m (getMat s ms)
conMat m (MulN d) ms = matNMul d m 

conN :: Double -> Op -> [Op] -> [NMat] -> Term
conN d (MulM s) ops ms = TermE m ops 
   where m = matNMul d (getMat s ms)
conN d (MulN f) ops ms = TermN (d*f) ops 

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
              Eq a <$> calc

onlyrhs = Eq "" <$> calc

calc = Terms <$> term <*> (many (try(opPlus) <|> try(opMinus)) )

opPlus = char '+' >> OpPlus <$> term
opMinus = char '-' >> OpMinus <$> term

term = try(longterm) <|> longtermn

longterm = TermS <$> (many1 letter) <*> many op
longtermn = TermN <$> (try(parseDouble) <|> parseInt) <*> many op

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
