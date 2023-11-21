module Instructions where

import Printing
import Places
import Inventory
import Characters
import World

data GameState = GameState { currentRoom :: Room, visitedRooms :: [Room], examining :: Maybe Place, clues :: [(Character, [Clue])], items :: [(Place, (Item, Int))], inventory :: [(Item, Int)], evidence :: [(Clue, Maybe Character)] } deriving (Show)

instructionsText = [
    "Available commands are:",
    "",
    "instructions    -- to see these instructions.",
    "look            -- to look around the room.",
    "inventory       -- to see what's currently in inventory.",
    "",
    "examine [Place] -- to examine specific place in a room.",
    "take [Item]     -- to take avaiable item to inventory.",
    "go to [Room]    -- to go to one of avaiable rooms.",
    "",
    "quit            -- to end the game and quit.",
    "dev             -- to see debug instructions.",
    ""
    ]

debugInstructionsText = [
    "Debug commands are:",
    "",
    "whereami        -- to see current location",
    "visited         -- to see all visited places",
    "gamestate       -- to see game state info",
    ""
    ]

goTo :: String -> GameState -> IO GameState
goTo roomStr gameState = do
    let current = currentRoom gameState
    let maybeRoom = stringToRoom roomStr
    case maybeRoom of
        Just destinationRoom -> do
            if areConnected current destinationRoom
                then do
                    let moveMsg = ["", "You moved from " ++ show current ++ " to " ++ show destinationRoom ++ "."]
                    let updatedMoveMsg = case examining gameState of
                            Just place -> moveMsg ++ ["You stopped examining " ++ show place ++ "."]
                            Nothing -> moveMsg

                    printLines updatedMoveMsg

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

            let examiningMsg = ["", "You are examining " ++ show place ++ "."]
            let updatedexaminingMsg = case examining gameState of
                    Just oldPlace -> ["You stopped examining " ++ show oldPlace ++ "."] ++ examiningMsg
                    Nothing -> examiningMsg
            printLines updatedexaminingMsg

            let newGameState = gameState {examining = Just place}

            let itemsInPlace = filter (\(p, _) -> p == place) (items gameState)
            case itemsInPlace of
                [] -> return newGameState
                _ -> do
                    let itemDescriptions = map (formatItem . snd) itemsInPlace
                    printLines itemDescriptions
                    return newGameState
        Nothing -> do
            putStrLn "Invalid place name."
            return gameState

takeItem :: String -> GameState -> IO GameState
takeItem itemStr gameState = do
    let maybePlace = examining gameState
    case maybePlace of
        Just place -> do
            let itemsList = items gameState
            let maybeItem = stringToItem itemStr
            case maybeItem of
                Just item -> do
                    case removeItemFromPlace itemsList place item of
                        Just updatedItems -> do
                            putStrLn $ "\nYou took " ++ show item ++ "."
                            printLines $ getItemDescription item

                            let currentInventory = inventory gameState
                            let updatedinventory = addItemToInventory currentInventory (item, 1)

                            let newGameState = gameState { items = updatedItems, inventory = updatedinventory }
                            return newGameState
                        Nothing -> do
                            putStrLn $ "\nYou can't take " ++ show item ++ " from here."
                            return gameState
                Nothing -> do
                    putStrLn "Invalid item name."
                    return gameState
        Nothing -> do
            putStrLn "You must first examine a Place."
            return gameState