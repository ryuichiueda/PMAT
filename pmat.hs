import System.Environment
import System.IO
import Text.ParserCombinators.Parsec
import Control.Monad
import Numeric.LinearAlgebra as LA

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
		"Tue Jul  2 16:40:21 JST 2013\n")

version :: IO ()
version = do hPutStr stderr
		("version 0.001")

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

data NMat = NMat (String,Matrix Double) deriving Show

mainProc :: String -> String -> IO ()
mainProc opt cs = do putStr cs
                     doCalc (setOpt opt) (setMatrices cs)

toStr :: NMat -> String
toStr (NMat (name,mat)) = unlines [ name ++ " " ++ t | t <- tos ]
                          where lns = LA.toLists mat
                                tos = [ unwords [ show d | d <- ln ] | ln <- lns ] 

-- ここを広げていく
doCalc :: Option -> [NMat] -> IO ()
doCalc (Term Mul lhs rhs) ms = putStr $ toStr $ matMultiply (getMat lhs ms) (getMat rhs ms)

getMat :: String -> [NMat] -> NMat
getMat name ms = head $ filter ( f name ) ms
                 where f name (NMat (n,_)) = (name == n)

matMultiply :: NMat -> NMat -> NMat
matMultiply (NMat x) (NMat y) = NMat ((fst x) ++ "*" ++ (fst y), LA.multiply (snd x) (snd y))

setMatrices :: String -> [NMat]
setMatrices cs = [ setMatrix m | m <- (getCluster $ lines cs) ]

setMatrix :: [String] -> NMat
setMatrix lns =  NMat (k,val)
                where k = head (words $ head lns)
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

data Option = Term Op String String
            | Mat (Maybe NMat)
            | Error String

data Mat = Maybe NMat
data Op = Mul

setOpt :: String -> Option
setOpt str = case parse parseOption "" str of
                                  Right opt -> opt
                                  Left err -> Error ( show err )

parseOption :: Parser Option
parseOption = do a <- many1 letter
                 char '*'
                 b <- many1 letter
                 return $ Term Mul a b
