module Main where

import System.Environment
import Control.Arrow(first)
import Text.RegexPR
import Data.Tree
import Data.List
import Data.Char
import HsModTree
import System.FilePath

main :: IO ()
main = do
	[ dir ] <- getArgs
	files <- fmap filterSource $ getDirectoryContentsRec dir
	dependList <- mapM ( depend dir files ) files
	let	depList = map (first $ gsubRegexPR "/" ".") dependList
		trees = map nubTree $ mergeTree $ map makeTree depList
	mapM_ (printModTreeLines dir) $ map (nub . flatten) trees

printModTreeLines :: FilePath -> [String] -> IO ()
printModTreeLines dir mns = do
	s <- fmap sum $ mapM (printModLines dir) mns
	putStrLn $ "total" ++ replicate 35 ' ' ++ show s
	putStrLn ""

printModLines :: FilePath -> String -> IO Int
printModLines dir mn = do
	let fp = modNameToFilePath mn
	lns <- fileLines dir fp
	putStrLn $ showModLines fp lns
	return lns

showModLines :: FilePath -> Int -> String
showModLines fp lns = fp ++ replicate (40 - length fp) ' ' ++ show lns

fileLines :: FilePath -> FilePath -> IO Int
fileLines dir = fmap codeLines . readFile . (dir </>)

modNameToFilePath :: String -> FilePath
modNameToFilePath = (++ ".hs") . gsubRegexPR "\\." "/"

codeLines :: String -> Int
codeLines code = length $ filter (not . all isSpace) $ lines $ deleteComms code

deleteComms :: String -> String
deleteComms = subRegexPR "module\\s+(.|\n)*?\\s+where" "" .
		gsubRegexPR "--.*\n" "" .
		gsubRegexPR "{-(?:[^-]|-(?!}))*-}" ""

testDeleteComms :: FilePath -> IO ()
testDeleteComms fp = readFile fp >>= putStr . deleteComms
