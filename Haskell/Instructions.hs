module Instructions where

import Places
import Inventory
import Characters

data GameState = GameState { currentRoom :: Room, visitedRooms :: [Room], examining :: Maybe Place, clues :: [(Character, [Clue])] } deriving (Show)

instructionsText = [
    "Available commands are:",
    "",
    "instructions   -- to see these instructions.",
    "look           -- to look around the room.",
    "examine [Place] -- to examine specific place in a room.",
    "go to [Room]   -- to go to one of avaiable rooms.",
    "",
    "quit           -- to end the game and quit.",
    "dev            -- to see debug instructions.",
    ""
    ]

debugInstructionsText = [
    "Debug commands are:",
    "",
    "whereami       -- to see current location",
    "visited        -- to see all visited places",
    "gamestate      -- to see game state info",
    ""
    ]

-- print strings from list in separate lines
printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

goTo :: String -> GameState -> IO GameState
goTo roomStr gameState = do
    let current = currentRoom gameState
    let maybeRoom = stringToRoom roomStr
    case maybeRoom of
        Just destinationRoom -> do
            if areConnected current destinationRoom
                then do
                    putStrLn $ "You moved from " ++ show current ++ " to " ++ show destinationRoom ++ "."
                    let updatedVisitedRooms = if destinationRoom `elem` visitedRooms gameState
                                                then visitedRooms gameState
                                                else destinationRoom : visitedRooms gameState
                    let newGameState = gameState { currentRoom = destinationRoom, visitedRooms = updatedVisitedRooms, examining = Nothing }
                    let roomDescription = getRoomDescription destinationRoom (visitedRooms gameState)
                    printLines roomDescription
                    return newGameState
                else do
                    putStrLn $ "You can't move from " ++ show current ++ " to " ++ show destinationRoom ++ "."
                    return gameState
        Nothing -> do
            putStrLn "Invalid room name."
            return gameState

look :: GameState -> IO ()
look gameState = do
    let current = currentRoom gameState
    let roomDescription = getRoomDescription current []
    printLines roomDescription

examine :: String -> GameState -> IO GameState
examine placeStr gameState = do
    let current = currentRoom gameState
    let maybePlace = stringToPlace placeStr
    case maybePlace of
        Just place -> do
            let description = getPlaceDescription place
            printLines description

            let newGameState = gameState {examining = Just place}
            return newGameState
        Nothing -> do
            putStrLn "Invalid place name."
            return gameState