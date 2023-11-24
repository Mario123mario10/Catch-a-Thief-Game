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
        combined = zip locations adjustedMushrooms
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

-- Check if an item is in a specific place within the list
isItemInPlace :: [(Place, (Item, Int))] -> Place -> Item -> Bool
isItemInPlace placesWithItems place item =
    any (\(p, (i, _)) -> p == place && i == item) placesWithItems

removeItemFromPlace :: [(Place, (Item, Int))] -> Place -> Item -> (Maybe (Item, Int), [(Place, (Item, Int))])
removeItemFromPlace items place item =
    let matchingItem = find (\(p, (i, _)) -> p == place && i == item) items
    in case matchingItem of
        Just foundItem ->
            let updatedItems = filter (\(p, _) -> p /= place || p == place && fst (snd foundItem) /= item) items
            in (Just (snd foundItem), updatedItems)
        Nothing -> (Nothing, items)

-- Function to add/update a clue with its associated maybe character in the evidence map
addEvidence :: Map.Map Clue (Maybe Character) -> Clue -> Maybe Character -> Map.Map Clue (Maybe Character)
addEvidence evidence clue maybeCharacter =
    case Map.lookup clue evidence of
        Just maybeChar ->
            case maybeChar of
                Just _ -> evidence -- Clue exists with a character, return the original evidence unchanged
                Nothing -> Map.insert clue maybeCharacter evidence -- Clue exists with 'Nothing', update it
        Nothing -> Map.insert clue maybeCharacter evidence -- Clue doesn't exist, add it with the given value

specifyClue :: Map.Map Clue (Maybe Character) -> [(Character, [Clue])] -> Clue -> Map.Map Clue (Maybe Character)
specifyClue evidence clues clue = 
    let character = whoIsGuilty clues clue
    in addEvidence evidence clue (Just character)

getEvidenceDescription :: Map.Map Clue (Maybe Character) -> [String]
getEvidenceDescription evidenceMap =
    [ case maybeChar of
        Just character -> show clue ++ " - " ++ show character
        Nothing -> show clue ++ " - Unknown suspect"
    | (clue, maybeChar) <- Map.toList evidenceMap
    ]