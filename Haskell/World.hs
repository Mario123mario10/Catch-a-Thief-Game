module World where

import System.Random
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Characters
import Inventory
import Places

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

assignClues :: IO [(Character, [Clue])]
assignClues = do
    let clues = allClues
    let chars = allSuspects
    shuffledClues <- shuffle clues
    shuffledChars <- shuffle chars
    let (more, rest) = splitAt 3 shuffledClues
        (equalSplit1, equalSplit2) = splitAt 2 rest
    return $ zip (take 1 shuffledChars ++ drop 1 shuffledChars) ([more] ++ [equalSplit1, equalSplit2])

assignMushrooms :: IO [Maybe (Place, (Item, Int))]
assignMushrooms = do
    let locations = insideRoom Forest
    mushrooms <- sequence $ replicate (length locations) (randomRIO (0, 2) :: IO Int)
    let totalMushrooms = sum mushrooms
        ratio = if totalMushrooms > 0 then fromIntegral 10 / fromIntegral totalMushrooms else 0
        adjustedMushrooms = map (\m -> round (fromIntegral m * ratio)) mushrooms
        totalAdjusted = sum adjustedMushrooms

        diff = 10 - totalAdjusted
        lastAdjusted = if diff /= 0 then take diff (cycle [1 | diff > 0]) else []
        finalMushrooms = zipWith (+) adjustedMushrooms (lastAdjusted ++ repeat 0)

        combined = zip locations finalMushrooms
    return $ map (\(place, count) -> if count == 0 then Nothing else Just (place, (Mushroom, count))) combined

assignToolParts :: [Place] -> IO [(Place, (Item, Int))]
assignToolParts places = do
    let numToolParts = 2
    randomPlaces <- take numToolParts . nub <$> (shuffle places)
    return $ map (\place -> (place, (ToolPart, 1))) randomPlaces

assignItems :: [(Character, [Clue])] -> IO [(Place, (Item, Int))]
assignItems characterClues = do
    let toolHandlePlace = Just (VaultDoor, (ToolHandle, 1))
    let butlersKeysPlace = Just (NightTable, (ButlersKeys, 1))
    let dirtPlace = Just (DirtMound, (Dirt, 100))
    
    let vaultKeyPlace = case find (\(char, clues) -> StolenVaultKey `elem` clues) characterClues of
                            Just (Gardener, _) -> Just (GardenPond, (VaultKey, 1))
                            Just (Cook, _) -> Just (Oven, (VaultKey, 1))
                            Just (Butler, _) -> Just (Mirror, (VaultKey, 1))
                            _ -> Nothing

    let coinPouchPlace = case find (\(char, clues) -> StolenCoins `elem` clues) characterClues of
                            Just (Gardener, _) -> Just (RoseBushes, (CoinPouch, 1))
                            Just (Cook, _) -> Just (BagOfFlour, (CoinPouch, 1))
                            Just (Butler, _) -> Just (Bed, (CoinPouch, 1))
                            _ -> Nothing

    let diamondPlace = case find (\(char, clues) -> StolenDiamond `elem` clues) characterClues of
                            Just (Gardener, _) -> Just (GardenerChest, (Diamond, 1))
                            Just (Cook, _) -> Just (CookChest, (Diamond, 1))
                            Just (Butler, _) -> Just (ButlerChest, (Diamond, 1))
                            _ -> Nothing

    mushroomPlaces <- assignMushrooms

    let assignedPlaces = catMaybes [toolHandlePlace, butlersKeysPlace, dirtPlace, vaultKeyPlace, coinPouchPlace, diamondPlace] ++ catMaybes mushroomPlaces
    let _allPlaces = allPlaces
    let emptyPlaces = _allPlaces \\ map fst assignedPlaces

    toolPartPlaces <- assignToolParts emptyPlaces

    return $ assignedPlaces ++ toolPartPlaces

getEvidenceDescription :: Map.Map Clue (Maybe Character) -> [String]
getEvidenceDescription evidenceMap =
    [ case maybeChar of
        Just character -> show clue ++ " - " ++ show character
        Nothing -> show clue ++ " - Unknown suspect"
    | (clue, maybeChar) <- Map.toList evidenceMap
    ]