module Places where

import Data.Char (toLower, isSpace)

-----------------------------------
-- Castle Layout
-----------------------------------

data Room = Hall | Corridor | GuardHouse | Courtyard | Vault | Kitchen | RoyalBedroom | Garden | ServantsHouse | WizardsTower | Forest deriving (Eq, Show)

-- Function to convert string to Room (case-insensitive)
stringToRoom :: String -> Maybe Room
stringToRoom str = case map toLower str of
    "hall" -> Just Hall
    "corridor" -> Just Corridor
    "guardhouse" -> Just GuardHouse
    "courtyard" -> Just Courtyard
    "vault" -> Just Vault
    "kitchen" -> Just Kitchen
    "royalbedroom" -> Just RoyalBedroom
    "garden" -> Just Garden
    "servantshouse" -> Just ServantsHouse
    "wizardstower" -> Just WizardsTower
    "forest" -> Just Forest
    _ -> Nothing

data Door = Door Room Room deriving (Eq, Show)

-- List of all rooms in the castle
allRooms :: [Room]
allRooms = [Hall, Corridor, GuardHouse, Courtyard, Vault, Kitchen, RoyalBedroom, Garden, ServantsHouse, WizardsTower, Forest]

-- List of all doors in the castle
allDoors :: [Door]
allDoors = [
    Door Hall Corridor, Door Hall GuardHouse, Door Hall Courtyard, Door Hall Vault,
    Door Corridor Kitchen, Door Corridor RoyalBedroom,
    Door Courtyard Garden, Door Courtyard ServantsHouse, Door Courtyard WizardsTower, Door Courtyard Forest
    ]

-- Function to get doors connected to a specific room
doorsFromRoom :: Room -> [Door]
doorsFromRoom room = filter (\(Door r1 _) -> r1 == room) allDoors

-- Function to check if two rooms are connected by a door
areConnected :: Room -> Room -> Bool
areConnected room1 room2 = any (\(Door r1 r2) -> (r1 == room1 && r2 == room2) || (r1 == room2 && r2 == room1)) allDoors

-- Function to get rooms connected to a specific room
roomsConnectedToRoom :: Room -> [Room]
roomsConnectedToRoom room = map (\(Door r1 r2) -> if r1 == room then r2 else r1) (filter (\(Door r1 r2) -> r1 == room || r2 == room) allDoors)

-- Function to print rooms connected to a specific room
printRoomsConnectedTo :: Room -> IO ()
printRoomsConnectedTo room = do
    let connectedRooms = roomsConnectedToRoom room
    putStrLn $ "From " ++ show room ++ " you can go to: " ++ show connectedRooms ++ "."

-- Function to move between rooms
moveToRoom :: Room -> Room -> IO Room
moveToRoom currentRoom destinationRoom =
  if areConnected currentRoom destinationRoom
    then do
        putStrLn $ "You moved from " ++ show currentRoom ++ " to " ++ show destinationRoom ++ "."
        return destinationRoom
    else do
        putStrLn $ "You can't move from " ++ show currentRoom ++ " to " ++ show destinationRoom ++ "."
        return currentRoom