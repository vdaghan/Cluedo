module Main where

import Cluedo
import TestCases

{- TODO:
       * More code reuse
       * More composable functions
       * Use "lens"es for cleaner code
       * Question suggestion
       * GUI for practical use
-}

main :: IO ()
main = do
    Cluedo.print (gS newGame)
    where steps = testCase7
          gS  = foldl (\g f -> (f.g)) (head steps) (tail steps)
