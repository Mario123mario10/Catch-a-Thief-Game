module Instructions where

import Data.Map (Map)
import qualified Data.Map as Map

import Printing
import Places
import Inventory
import Characters
import World

data GameState = GameState { 
    currentRoom :: Room, 
    visitedRooms :: [Room], 
    examining :: Maybe Place, 
    talking :: Maybe Character, 
    clues :: [(Character, [Clue])], 
    items :: [(Place, (Item, Int))], 
    inventory :: [(Item, Int)], 
    evidence :: Map.Map Clue (Maybe Character),
    gaveMushrooms :: Bool
    } deriving (Show)

instructionsText = [
    "Available commands are:",
    "",
    "instructions        -- to see these instructions.",
    "look                -- to look around the room.",
    "inventory           -- to see what's currently in inventory.",
    "evidence            -- to see all evidence we know about.",
    "",
    "talk to [Character] -- to start conversation with character.",
    "ask about [Clue]    -- to ask about clue.",
    "examine [Place]     -- to examine specific place in a room.",
    "take [Item]         -- to take avaiable item to inventory.",
    "go to [Room]        -- to go to one of avaiable rooms.",
    "",
    "quit                -- to end the game and quit.",
    "dev                 -- to see debug instructions.",
    ""
    ]

debugInstructionsText = [
    "Debug commands are:",
    "",
    "whereami            -- to see current location",
    "visited             -- to see all visited places",
    "clues               -- to see characters and their clues",
    "items               -- to see all places and their items",
    "gamestate           -- to see game state info",
    "spawn [Item]        -- to spawn item into inventory",
    "checktool           -- to check if tool can be created",
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

                    let newGameState = gameState { currentRoom = destinationRoom, visitedRooms = updatedVisitedRooms, examining = Nothing, talking = Nothing }
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

                            -- update inventory
                            let currentInventory = inventory gameState
                            let newInventory = addItemToInventory currentInventory (item, 1)

                            -- update evidence
                            let currentClues = clues gameState
                            let currentEvidence = evidence gameState
                            let newEvidence = case item of
                                    ToolHandle -> addEvidence currentEvidence BloodStains Nothing
                                    VaultKey -> addEvidence currentEvidence StolenVaultKey Nothing
                                    Diamond -> addEvidence currentEvidence StolenDiamond Nothing
                                    CoinPouch -> addEvidence currentEvidence StolenCoins Nothing
                                    _ -> currentEvidence

                            -- check if created a tool
                            if hasRequiredItemsForTool newInventory
                                then do
                                    let removedHandle = case removeItemFromInventory newInventory (ToolHandle, 1) of
                                            Just updatedInv -> updatedInv
                                            Nothing -> newInventory
                                    let removedParts = case removeItemFromInventory removedHandle (ToolPart, 2) of
                                            Just updatedInv -> updatedInv
                                            Nothing -> newInventory

                                    let tool = case whoIsGuilty currentClues Tool of
                                            Gardener -> Rake
                                            Cook -> Ladle
                                            Butler -> FeatherDuster
                                    let addedTool = addItemToInventory removedParts (tool, 1)

                                    putStrLn $ "\nFrom tool parts you successfuly crafted " ++ show tool ++ "."
                                    printLines $ getItemDescription tool

                                    let updatedEvidence = addEvidence newEvidence Tool Nothing

                                    return gameState { items = updatedItems, inventory = addedTool, evidence = updatedEvidence }
                                else 
                                    return gameState { items = updatedItems, inventory = newInventory, evidence = newEvidence }
                        Nothing -> do
                            putStrLn $ "\nYou can't take " ++ show item ++ " from here."
                            return gameState
                Nothing -> do
                    putStrLn "Invalid item name."
                    return gameState
        Nothing -> do
            putStrLn "You must first examine a Place."
            return gameState

spawnItem :: String -> GameState -> IO GameState
spawnItem itemStr gameState = do
    case stringToItem itemStr of
        Just item -> do
            putStrLn $ "\nYou took " ++ show item ++ "."
            printLines $ getItemDescription item

            let currentInventory = inventory gameState
                newInventory = addItemToInventory currentInventory (item, 1)
                updatedGameState = gameState { inventory = newInventory }
            return updatedGameState
        Nothing -> do
            putStrLn "Invalid item name."
            return gameState

talkTo :: String -> GameState -> IO GameState
talkTo charStr gameState = do
    let maybeCharacter = stringToCharacter charStr
    case maybeCharacter of
        Just character -> do
            let current = currentRoom gameState
            case getCharacterInRoom current of
                Just char -> do
                    if character == char
                        then do
                            let currentClues = clues gameState
                            let currentGaveMushrooms = gaveMushrooms gameState
                            let text = getCharacterText currentClues character currentGaveMushrooms
                            printLines text

                            -- update evidence
                            let currentClues = clues gameState
                            let currentEvidence = evidence gameState
                            let updatedEvidence = case character of
                                    Guard -> specifyClue currentEvidence currentClues GuardsClue
                                    Wizard -> 
                                        if currentGaveMushrooms == True
                                            then
                                                specifyClue currentEvidence currentClues WizardsClue
                                            else
                                                currentEvidence
                                    _ -> currentEvidence
 
                            return gameState {talking = Just character, evidence = updatedEvidence}
                        else do 
                            putStrLn "You can't talk to him right now."
                            return gameState
                Nothing -> return gameState
        Nothing -> do
            putStrLn "Invalid character."
            return gameState

askAbout :: String -> GameState -> IO GameState
askAbout clueStr gameState = do
    let maybeClue = stringToClue clueStr
    case maybeClue of
        Just clue -> do
            let maybeCharacter = talking gameState
            case maybeCharacter of
                Just character -> do
                    let currentEvidence = evidence gameState
                    if Map.member clue currentEvidence
                        then do
                            let currentClues = clues gameState
                            let text = getClueCharacterText currentClues character clue
                            printLines text

                            -- update evidence
                            let updatedEvidence = if isCharacterGuilty currentClues character clue
                                    then
                                        specifyClue currentEvidence currentClues clue
                                    else
                                        currentEvidence
                            
                            return gameState {evidence = updatedEvidence}
                        else do
                            putStrLn "What are you talking about?"
                            return gameState
                Nothing -> do
                    putStrLn "Who are you talking to? You must first talk to a Character."
                    return gameState
        Nothing -> do
            putStrLn "Invalid clue."
            return gameState
