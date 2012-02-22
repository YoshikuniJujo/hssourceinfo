module Main where

import System.Environment
import Control.Arrow(first)
import Text.RegexPR
import HsModTree

main :: IO ()
main = do
	[ dir ] <- getArgs
	files <- fmap filterSource $ getDirectoryContentsRec dir
	dependList <- mapM ( depend dir files ) files
	let depList = map (first $ gsubRegexPR "/" ".") dependList
	mapM_ ( putStr . showTree [ ] . nubTree ) $
		mergeTree $ map makeTree depList
