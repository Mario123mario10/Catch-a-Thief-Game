module Inventory where

import Data.Char (toLower, isSpace)
import Data.List

-----------------------------------
-- Items
-----------------------------------

data Item = Ladle | Rake | FeatherDuster | ToolHandle | ToolPart | VaultKey | ButlersKeys | Diamond | CoinPouch | Mushroom | Dirt deriving (Eq, Show)

stringToItem :: String -> Maybe Item
stringToItem str = case map toLower str of
    "toolhandle" -> Just ToolHandle
    "handle" -> Just ToolHandle
    "toolpart" -> Just ToolPart
    "part" -> Just ToolPart
    "parts" -> Just ToolPart
    "vaultkey" -> Just VaultKey
    "key" -> Just VaultKey
    "butlerskeys" -> Just ButlersKeys
    "keys" -> Just ButlersKeys
    "diamond" -> Just Diamond
    "coinpouch" -> Just CoinPouch
    "pouch" -> Just CoinPouch
    "mushroom" -> Just Mushroom
    "mushrooms" -> Just Mushroom
    "dirt" -> Just Dirt
    "soil" -> Just Dirt
    _ -> Nothing

getItemDescription :: Item -> [String]
getItemDescription item = 
    case item of
        Ladle -> ["", "Kitchen utensil used by Cook for serving soups and sauces."]
        Rake -> ["", "Gardening tool with tines used by Gardener for collecting leaves or debris."]
        FeatherDuster -> ["", "Cleaning tool made of feathers used by Butler to remove dust."]
        ToolHandle -> ["", "Broken part of a tool used to open the heavy vault door. There is blood on the broken part"]
        ToolPart -> ["", "Part of a tool used to open the heavy vault door"]
        VaultKey -> ["", "Key that opens the vault door"]
        ButlersKeys -> ["", "Keys that unlock every lock in the castle"]
        Diamond -> ["", "Precious gem, possibly from the vault"]
        CoinPouch -> ["", "Pouch containing coins believed to be from the vault"]
        Mushroom -> ["", "Magical mushroom"]
        Dirt -> ["", "Ordinary dirt. You can make quite a mess with this."]

formatItem :: (Item, Int) -> String
formatItem (item, count) =
    let itemCountStr = if count > 1 && item /= Dirt then "s" else "" in
        show item ++ itemCountStr ++ ": " ++ show count

getInventoryDescription :: [(Item, Int)] -> [String]
getInventoryDescription [] = []
getInventoryDescription inventory = map formatItem inventory

isItemInInventory :: [(Item, Int)] -> Item -> Bool
isItemInInventory inventory itemToFind = case find (\(item, _) -> item == itemToFind) inventory of
    Just _ -> True
    Nothing -> False

addItemToInventory :: [(Item, Int)] -> (Item, Int) -> [(Item, Int)]
addItemToInventory inventory newItem@(item, count) =
    case lookup item inventory of
        Just currentCount -> map (\(i, c) -> if i == item then (i, c + count) else (i, c)) inventory
        Nothing -> inventory ++ [newItem]

removeItemFromInventory :: [(Item, Int)] -> (Item, Int) -> Maybe [(Item, Int)]
removeItemFromInventory inventory itemToRemove@(item, countToRemove)
    | countToRemove <= 0 = Nothing
    | otherwise = case lookup item inventory of
        Just itemCount
            | itemCount >= countToRemove ->
                let updatedInventory = filter (\(i, c) -> i /= item || c /= 0) $ map (\(i, c) -> if i == item then (i, c - countToRemove) else (i, c)) inventory
                in Just updatedInventory
            | otherwise -> Nothing
        Nothing -> Nothing

hasRequiredItemsForTool :: [(Item, Int)] -> Bool
hasRequiredItemsForTool inventory =
    let toolHandleFound = any (\(item, count) -> item == ToolHandle && count >= 1) inventory
        toolPartsFound = any (\(item, count) -> item == ToolPart && count >= 2) inventory
    in
        toolHandleFound && toolPartsFound

-----------------------------------
-- Clues
-----------------------------------

data Clue = Tool | StolenVaultKey | StolenDiamond | GuardsClue | WizardsClue | StolenCoins | BloodStains deriving (Eq, Show)

allClues :: [Clue]
allClues = [Tool, StolenVaultKey, StolenDiamond, GuardsClue, WizardsClue, StolenCoins, BloodStains]

instance Ord Clue where
    compare clue1 clue2 = compare (show clue1) (show clue2)

stringToClue :: String -> Maybe Clue
stringToClue str = case map toLower str of
    "tool" -> Just Tool
    "ladle" -> Just Tool
    "rake" -> Just Tool
    "featherduster" -> Just Tool
    "duster" -> Just Tool
    "stolenvaultkey" -> Just StolenVaultKey
    "vaultkey" -> Just StolenVaultKey
    "key" -> Just StolenVaultKey
    "stolendiamond" -> Just StolenDiamond
    "diamond" -> Just StolenDiamond
    "guardsclue" -> Just GuardsClue
    "guardclue" -> Just GuardsClue
    "wizardsclue" -> Just WizardsClue
    "wizardclue" -> Just WizardsClue
    "crystals" -> Just WizardsClue
    "stolencoins" -> Just StolenCoins
    "coins" -> Just StolenCoins
    "pouch" -> Just StolenCoins
    "coinpouch" -> Just StolenCoins
    "bloodstains" -> Just BloodStains
    "blood" -> Just BloodStains
    "stains" -> Just BloodStains
    _ -> Nothing