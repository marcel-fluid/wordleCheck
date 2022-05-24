{-# LANGUAGE DataKinds #-}


-- import qualified Data.ByteString as IOMode
import System.IO
import Control.Monad

main :: IO ()
main = do 
        _              <- print "enter 5 chars upper case, use '_' for blank. eg. _OUL_"
        inputLetters   <- getLine 
        _              <- print "enter chars to exclude (upper case) eg. AREFGH"
        excludeLetters <- getLine 
        fileH          <- openFile "wordleWords.txt" ReadMode
        contents       <- hGetContents fileH 
        wordList       <- getWords contents inputLetters excludeLetters
        outputWords wordList
        hClose fileH
    where
        getWords :: String -> String -> String -> IO [String]
        getWords fileContents inputLetters exclLetters = do 
                                                let allWords = words fileContents
                                                    fiverWords = filter (\x -> length x == 5) allWords
                                                remainingWords <- filterByExclusions fiverWords exclLetters
                                                filterByLetters remainingWords inputLetters
                 
        outputWords :: [String] -> IO ()
        outputWords []     = print "No words found"
        outputWords [x]    = print x 
        outputWords (x:xs) = print x >> outputWords xs 


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
