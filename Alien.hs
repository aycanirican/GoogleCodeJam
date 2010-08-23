--
-- Google Code Jam: Alien Numbers
-- http://code.google.com/codejam/contest/dashboard?c=32003#
--
-- Aycan iRiCAN & M. Evrim Ulu 
-- aycan@core.gen.tr, evrim@core.gen.tr
--

module Main where

import Text.ParserCombinators.Parsec
import System.Environment
import Data.List
import Data.Maybe (fromJust)

-- Boilerplate taken from Brandon Simmons.
-- http://coder.bsimmons.name/blog/2009/08/haskell-boilerplate-for-google-codejam/
-- 

type Input = [Case] 
type Solution = [SolvedCase] 
type Output = [String]
type Case = (String,String,String)
type SolvedCase = String

algorithm :: Case -> SolvedCase
algorithm (a,b,c) = toSyms c $ toPoly (length c) $ fromPoly (length b) $ alienToPoly a b

caseParser :: Parser Case
caseParser = do
    a <- word
    b <- word
    c <- word
    return (a,b,c)

formatCase :: SolvedCase -> String
formatCase = id

main = do
    (f:_) <- getArgs
    file  <- readFile f
    let inp  = parseWith mainParser file
        solution        = map algorithm inp
        solutionStrings = map formatCase solution
        outp = zipWith (++) prefixes solutionStrings
    putStr $ unlines outp

parseWith p = either (error . show) id . parse p "" 

mainParser :: Parser Input
mainParser = do
    n  <- word
    ms <- count (read n) caseParser 
    return ms
    <?> "mainParser"

prefixes = [ "Case #" ++ show n ++ ": " | n <- [1..]]

wholeLine :: Parser String
wholeLine = manyTill anyChar (try newline) <?> "wholeLine"

whiteSepLine = manyTill spaceSepWord newline <?> "whiteSepLine"

word = do
    w <- many1 nonWhite
    spaces
    return w
    <?> "word"

spaceSepWord = do
    w <- many1 nonWhite
    many (char ' ')
    return w
    <?> "spaceSepWord"

twoWordsSepBy c = do
    x <- manyTill nonWhite (try$ char c)
    y <- many1 nonWhite
    many (char ' ')
    return (x,y)
    <?> "twoWordsSepBy"

nonWhite = noneOf " \v\f\t\r\n" <?> "nonWhite"

----
---- Solution 
----

alienToPoly :: String -> String -> [Int]
alienToPoly num digits = reverse $ map (fromJust . (`elemIndex` digits)) num

toSyms :: String -> [Int] -> String
toSyms dsyms ixs = map (dsyms!!) (reverse ixs)

-- functor which maps base_n numbers to intermediate polynomials
-- 6 / 3 = 2
-- x2 + x / x + 1 = x 

-- forall base > 0
toPoly :: Int -> Int -> [Int]
toPoly base n | n == 0 = [0]
              | base <= 0 = error "base zero or negative"
              | otherwise = go n
    where
      go 0 = []
      go n = let (q, r) = (n `divMod` base) in
             r:(go q)

fromPoly :: Int -> [Int] -> Int
fromPoly b = sum' . zipWith (*) (iterate (* b) 1)
    where sum' = foldl' (+) 0

