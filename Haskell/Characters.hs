module Characters where

import Data.Char (toLower, isSpace)
import System.Random
import Data.List.Split

import Inventory

data Character = King | Guard | Wizard | Gardener | Cook | Butler deriving (Eq, Show)

allSuspects :: [Character]
allSuspects = [Gardener, Cook, Butler]

-- Function to shuffle a list
shuffle :: [a] -> IO [a]
shuffle xs = do
    gen <- newStdGen
    return $ shuffle' xs gen
    where
      shuffle' [] _ = []
      shuffle' l gen =
        let (index, newGen) = randomR (0, length l - 1) gen
            (first, (x : rest)) = splitAt index l
         in x : shuffle' (first ++ rest) newGen

-- Function to assign clues to characters
assignClues :: IO [(Character, [Clue])]
assignClues = do
    let clues = allClues
    let chars = allSuspects
    shuffledClues <- shuffle clues
    let (more, rest) = splitAt 3 shuffledClues
        (equalSplit1, equalSplit2) = splitAt 2 rest
    return $ zip (take 1 chars ++ drop 1 chars) ([more] ++ [equalSplit1, equalSplit2])