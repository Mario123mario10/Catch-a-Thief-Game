module Inventory where

import Data.Char (toLower, isSpace)

import Places

-----------------------------------
-- Items
-----------------------------------

data Item = ToolHandle | VaultKey | ButlersKeys | Diamond | CoinPouch | Mushroom deriving (Eq, Show)

stringToItem :: String -> Maybe Item
stringToItem str = case map toLower str of
    "toolhandle" -> Just ToolHandle
    "handle" -> Just ToolHandle
    "vaultkey" -> Just VaultKey
    "key" -> Just VaultKey
    "butlerskeys" -> Just ButlersKeys
    "keys" -> Just ButlersKeys
    "diamond" -> Just Diamond
    "coinpouch" -> Just CoinPouch
    "pouch" -> Just CoinPouch
    "mushroom" -> Just Mushroom
    _ -> Nothing

-----------------------------------
-- Clues
-----------------------------------

data Clue = Tool | StolenVaultKey | StolenDiamond | GuardsClue | WizardsClue | StolenCoins | BloodStains deriving (Eq, Show)

allClues :: [Clue]
allClues = [Tool, StolenVaultKey, StolenDiamond, GuardsClue, WizardsClue, StolenCoins, BloodStains]