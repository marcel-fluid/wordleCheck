{-# LANGUAGE DataKinds #-}


-- import qualified Data.ByteString as IOMode
import System.IO
import Control.Monad
--import Data.Text.IO
--import Data.Text hiding (filter)

main :: IO ()
main = do 
    -- TODO: put the input into a separate fn (maybe helper?) that runs in a recursive loop.
    -- first checks if input is q for quit, or n for new. new clears the StateT state
    -- the state is the excluded letters.
       liftM runQuestions
    where
        runQuestions :: StateT String (IO ())
        runQuestions = do 
                        _              <- print "enter 5 chars upper case, use '_' for blank. eg. _OUL_"
                        inputLetters   <- getLine 
                        _              <- print "enter chars to exclude (upper case) eg. AREFGH"
                        excludeLetters <- getLine 
                        wordList       <- getWords fContents inputLetters excludeLetters
                        outputWords wordList
                        --TODO: put the exclude letters into the state
                        --TODO: check for 'new' and 'quit'
       
        fContents ::  IO String
        fContents = readFile "wordleWords.txt"     

        getWords :: String -> String -> String -> IO [String]
        getWords fileContents inputLetters exclLetters = do 
                                                let allWords   = words fileContents
                                                    fiverWords = filter (\x -> length x == 5) allWords
                                                remainingWords <- filterByExclusions fiverWords exclLetters
                                                filterByLetters remainingWords inputLetters
                 
        outputWords :: [String] -> IO ()
        outputWords []     = print "No words found"
        outputWords [x]    = print x 
        outputWords (x:xs) = print x >> outputWords xs 

        -- this caused issues with the file close operation, due to Lazy IO issues. 
        -- getFileContents :: IO Text 
        -- getFileContents = do 
        --                 fileH          <- openFile "wordleWords.txt" ReadMode
        --                 contents       <- hGetContents fileH
        --                 hClose fileH
        --                 return contents

filterByLetters :: [String] -> String -> IO [String] 
filterByLetters ws []     = return ws
filterByLetters ws [l]    = if l /= '_' && l /= ' ' 
                                then return $ filter (\x -> l == last x ) ws
                                else return ws   
filterByLetters ws (l:ls) = if l /= '_' && l /= ' ' 
                                then do
                                        let filtered = filter (\x -> l == (head (drop (5 - (length (l:ls))) x ))) ws 
                                        void $ print ("length filtered: " ++ show (length filtered))
                                        filterByLetters filtered ls
                                else filterByLetters ws ls    

filterByExclusions :: [String] -> String -> IO [String] 
filterByExclusions ws []     = return ws
filterByExclusions ws [l]    = return $ filter (\x -> not (l `elem` x )) ws
-- use the applicative pure to lift the filter call into the IO context
filterByExclusions ws (l:ls) = pure (filter (\x -> not (l `elem` x ))) <*> ( filterByExclusions ws ls )
