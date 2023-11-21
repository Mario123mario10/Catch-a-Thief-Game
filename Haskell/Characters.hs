module Characters where

import Data.Char (toLower, isSpace)

import Inventory

data Character = King | Guard | Wizard | Gardener | Cook | Butler deriving (Eq, Show)

allSuspects :: [Character]
allSuspects = [Gardener, Cook, Butler]

