-- Google Code Jam: Problem A. Speaking in Tongues
-- http://code.google.com/codejam/contest/1460488/dashboard
--
-- Aycan iRiCAN
-- aycan@core.gen.tr

module Main where

import System.IO
import System.Environment
import Data.Maybe

i1 = "ejp mysljylc kd kxveddknmc re jsicpdrysi"
i2 = "rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd"
i3 = "de kr kd eoya kw aej tysr re ujdr lkgc jv"
hinti ="y qee"
 
o1 = "our language is impossible to understand"
o2 = "there are twenty six factorial possibilities"
o3 = "so it is okay if you want to just give up"
hinto ="a zoo"

d1 = hinti ++ i1 ++ i2 ++ i3
d2 = hinto ++ o1 ++ o2 ++ o3

-- *Main> sort . nub $ d1
-- "abcdefghijklmnopqrstuvwxy"
-- *Main> sort . nub $ d2
-- "abcdefghijklmnoprstuvwxyz"
--
-- hence 'z' -> 'q'

-- our key as an assoc list
key :: [(Char, Char)]
key = ('z', 'q'):zip d1 d2

translate :: String -> String
translate word = map (fromJust . (flip lookup $ key)) word

main = do
  f:[] <- getArgs
  withFile f ReadMode $ \h -> do
      numCase <- hGetLine h >>= return . read
      lines <- hGetContents h >>= return . zip [1..numCase] . lines
      mapM_ transLine lines
  return ()
      where transLine (i,l) = do
              putStrLn $ "Case #" ++ show i ++ ": " ++ translate l
