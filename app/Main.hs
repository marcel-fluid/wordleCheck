

module Main(main) where

import Control.Monad.Trans.State

import WordleCheck 

main :: IO ()
main = do 
         fContents <- readFile "wordleWords.txt"
         let
            allWords   = words fContents 
            fiverWords = filter (\x -> length x == 5) allWords
            -- here is the resulting type of following line:
            -- *Main Control.Monad MTS> :t runStateT (runQuestions fivers)
            -- runStateT (runQuestions fivers) :: String -> IO ((), String)
         runStateT (runQuestions fiverWords) "" >> return ()