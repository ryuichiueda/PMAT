import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Numeric.LinearAlgebra

{--
pmat

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
		"Tue Jul 16 21:21:55 JST 2013\n")

version :: IO ()
version = do hPutStr stderr
		("version 0.0015")

main :: IO ()
main = do
	args <- getArgs
	case args of
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
mainProc opt cs = do putStr cs
                     mainProc' (setOpt opt) (setMatrices cs)

---------------
-- data type --
---------------

data NMat = NMat (String,Matrix Double) deriving Show

data Option = Equation String Calc
            | Error String

data Calc = SingleTerm Term
            | Mat NMat

data OpSet = OpSetMat (Op1,String)
           | OpSetNum (Op1,Double)

data SingleMat = SingleMat String
               | EvaledMat NMat

data Term = LongTerm SingleMat [OpSet]
          | LongTermN Double [OpSet]

data Mat = Maybe NMat
data Op1 = Mul

--------------------------
-- execution and output --
--------------------------

mainProc' :: Option -> [NMat] -> IO ()
mainProc' (Equation name calc) ms = doCalc name calc ms

doCalc :: String -> Calc -> [NMat] -> IO ()
doCalc name (SingleTerm t) ms = doCalcTerm name t ms
doCalc ""   (Mat m) _ = putStr $ toStr m
doCalc name (Mat m) _ = putStr $ toStr $ renameMat name m

doCalcTerm :: String -> Term -> [NMat] -> IO ()
doCalcTerm name (LongTerm (EvaledMat s) []) ms = doCalc name (Mat s) ms
doCalcTerm name (LongTerm (SingleMat s) []) ms = doCalc name (Mat m) ms
                                                 where m = getMat s ms
doCalcTerm name (LongTerm (SingleMat s) [OpSetMat (op,m)]) ms = doCalcTerm name (LongTerm (EvaledMat t) []) ms
                                                       where t = matMul (getMat s ms) (getMat m ms)
doCalcTerm name (LongTerm (SingleMat s) (OpSetMat (op,m):es)) ms = doCalcTerm name (LongTerm (EvaledMat t) es) ms
                                                       where t = matMul (getMat s ms) (getMat m ms)
doCalcTerm name (LongTerm (EvaledMat s) (OpSetMat (op,m):es)) ms = doCalcTerm name (LongTerm (EvaledMat t) es) ms
                                                       where t = matMul s (getMat m ms)
doCalcTerm name (LongTermN d [OpSetMat (op,m)]) ms = doCalcTerm name (LongTerm (EvaledMat t) []) ms
                                                       where t = matNMul d (getMat m ms)
doCalcTerm name (LongTermN d (OpSetMat (op,m):es)) ms = doCalcTerm name (LongTerm (EvaledMat t) es) ms
                                                       where t = matNMul d (getMat m ms)
doCalcTerm name (LongTerm (SingleMat s) [OpSetNum (op,m)]) ms = doCalcTerm name (LongTerm (EvaledMat t) []) ms
                                                       where t = matNMul m (getMat s ms)

renameMat :: String -> NMat -> NMat
renameMat new (NMat (n,m)) = NMat (new,m)

---------------------
-- matrix handling --
---------------------

-- get a matrix named "name"
getMat :: String -> [NMat] -> NMat
getMat nm ms = head $ filter ( f nm ) ms
                 where f nm (NMat (n,_)) = (nm == n)

matMul :: NMat -> NMat -> NMat
matMul (NMat x) (NMat y) = NMat ((fst x) ++ "*" ++ (fst y),  (snd x) <> (snd y))

matNMul :: Double -> NMat -> NMat
matNMul d (NMat y) = NMat (nm, m)
                       where nm = (show d) ++ "*" ++ (fst y)
                             m =  nMultiply d (snd y) 

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

parseOption = try(equation) 
              <|> do c <- calc
                     return $ Equation "" (SingleTerm c)
              <?> "no terms"

equation = do a <- many1 letter
              char '='
              b <- calc
              return $ Equation a (SingleTerm b)

calc = term

term = try(longterm) <|> longtermn

longterm = do a <- many1 letter
              b <- many opset
              return $ LongTerm (SingleMat a) b

longtermn = do a <- try(parseDouble) <|> parseInt
               b <- many opset
               return $ LongTermN a b

opset = try(opsetMat) <|> try(opsetNum)

opsetMat = do char '*'
              a <- many1 letter
              return $ OpSetMat (Mul,a)

opsetNum = do char '*'
              a <- try(parseDouble) <|> parseInt
              return $ OpSetNum (Mul,a)

parseDouble = do x <- many1 digit
                 char '.'
                 y <- many1 digit
                 return $ read (x++"."++y)

parseInt = do x <- many1 digit
              return $ (read x::Double)
