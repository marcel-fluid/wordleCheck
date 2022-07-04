{-# LANGUAGE DataKinds #-}


import System.IO
import Control.Monad
--import Control.Monad.State       hiding (put, get)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

type FileContents    = IO String 
type InputLetters    = String
type ExcludedLetters = String
type FiveLetterWords = [String]

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
   
runQuestions :: FiveLetterWords -> StateT String IO ()
runQuestions flw = do 
                currentExclusions <- get
                -- liftIO works here too, but lift is more general
                _                 <- lift (putStrLn "enter 5 chars upper case, use '_' for blank. eg. _OUL_")
                inputLetters      <- lift $ getLine
                if inputLetters == "QUIT" then do
                    return ()
                else
                    do
                    _              <- lift $ print $ "these are the current excluded letters: " ++ currentExclusions  
                    _              <- lift $ print "enter additional chars to exclude (upper case) eg. AREFGH"
                    _              <- lift $ print "(type 'RESET' to clear)"
                    excludeInput   <- lift $ getLine 
                    excludeLetters <- if excludeInput == "RESET" then 
                                            do 
                                                put "" 
                                                pure "" 
                                        else 
                                            do
                                                put (currentExclusions ++ excludeInput)
                                                pure (currentExclusions ++ excludeInput)
                    
                    wordList       <- lift $ getWords flw inputLetters excludeLetters
                    _              <- lift (outputWords wordList)
                    runQuestions flw


getWords :: FiveLetterWords -> InputLetters -> ExcludedLetters -> IO [String]
getWords flWords inputLetters exclLetters = do 
                                        -- using liftM here worked, but then I realized the code was reading the file every iteration, 
                                        -- so I moved it to main
                                        -- allWords       <- liftM words fileContents
                                        -- let fiverWords = filter (\x -> length x == 5) allWords
                                        remainingWords <- filterByExclusions flWords exclLetters
                                        filterByLetters remainingWords inputLetters
            
outputWords :: [String] -> IO ()
outputWords []     = print "No words found"
outputWords [x]    = print x 
outputWords (x:xs) = print x >> outputWords xs 

-- this caused issues with the file close operation, due to Lazy IO issues. Use readFile instead.
-- getFileContents :: IO Text 
-- getFileContents = do 
--                 fileH          <- openFile "wordleWords.txt" ReadMode
--                 contents       <- hGetContents fileH
--                 hClose fileH
--                 return contents

filterByLetters :: [String] -> InputLetters -> IO [String] 
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

filterByExclusions :: [String] -> ExcludedLetters -> IO [String] 
filterByExclusions ws []     = return ws
filterByExclusions ws [l]    = return $ filter (\x -> not (l `elem` x )) ws
-- use the applicative pure to lift the filter call into the IO context
filterByExclusions ws (l:ls) = pure (filter (\x -> not (l `elem` x ))) <*> ( filterByExclusions ws ls )
