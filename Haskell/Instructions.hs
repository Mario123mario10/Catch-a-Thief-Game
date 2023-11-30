module Instructions where

import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock

import Printing
import Places
import Inventory
import Characters
import World

data GameState = GameState { 
    startTime :: UTCTime,
    currentRoom :: Room, 
    visitedRooms :: [Room], 
    roomHistory :: [Room],
    examining :: Maybe Place, 
    talking :: Maybe Character, 
    clues :: [(Character, [Clue])], 
    items :: [(Place, (Item, Int))], 
    inventory :: [(Item, Int)], 
    evidence :: Map.Map Clue (Maybe Character),
    isButlerDistracted :: Bool,
    gaveMushrooms :: Bool,
    isServantsHouseLocked :: Bool,
    arePlacesLocked :: Map.Map Place Bool
    } deriving (Show)

instructionsText = [
    "Available commands are:",
    "",
    "instructions        -- to see these instructions.",
    "time                -- to see remaining time.",
    "look                -- to look around the room.",
    "inventory           -- to see what's currently in inventory.",
    "evidence            -- to see all evidence we know about.",
    "",
    "talk to [Character] -- to start conversation with character.",
    "ask about [Clue]    -- to ask about clue.",
    "give [Item]         -- to give all items of this type to character.",
    "examine [Place]     -- to examine specific place in a room.",
    "take [Item]         -- to take avaiable item to inventory.",
    "drop [Item]         -- to drop an item.",
    "go to [Room]        -- to go to one of avaiable rooms.",
    "unlock [Room/Place] -- to unlock locked room/place with keys",
    "accuse [Character]  -- to character and if correct win the game",
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
    "tp [Room]           -- to teleport to the room",
    ""
    ]

moveToRoom :: Room -> GameState -> IO GameState
moveToRoom destinationRoom gameState = do
    let current = currentRoom gameState
    let moveMsg = ["", "You moved from " ++ show current ++ " to " ++ show destinationRoom ++ "."]
    let updatedMoveMsg = case examining gameState of
            Just place -> moveMsg ++ ["You stopped examining " ++ show place ++ "."]
            Nothing -> moveMsg
    
    printLines updatedMoveMsg
    
    let servantsHouseLocked = isServantsHouseLocked gameState
    let updatedVisitedRooms = updateVisitedRooms current (visitedRooms gameState) servantsHouseLocked
    
    let newGameState = gameState { currentRoom = destinationRoom, visitedRooms = updatedVisitedRooms, examining = Nothing, talking = Nothing }
    let roomDescription = getRoomDescription destinationRoom (visitedRooms gameState) servantsHouseLocked
    printLines roomDescription
    return newGameState

tp :: String -> GameState -> IO GameState
tp roomStr gameState = do
    let current = currentRoom gameState
    let maybeRoom = stringToRoom roomStr
    case maybeRoom of
        Just destinationRoom -> do
            newGameState <- moveToRoom destinationRoom gameState
            return newGameState
        Nothing -> do
            putStrLn "Invalid room name."
            return gameState

goTo :: String -> GameState -> IO GameState
goTo roomStr gameState = do
    let current = currentRoom gameState
    let maybeRoom = stringToRoom roomStr
    case maybeRoom of
        Just destinationRoom -> do
            if areConnected current destinationRoom
                then do
                    newGameState <- moveToRoom destinationRoom gameState
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
    let servantsHouseLocked = isServantsHouseLocked gameState
    let roomDescription = getRoomDescription current [] servantsHouseLocked
    printLines roomDescription

examine :: String -> GameState -> IO GameState
examine placeStr gameState = do
    let current = currentRoom gameState
    let servantsHouseLocked = isServantsHouseLocked gameState
    let maybePlace = stringToPlace placeStr
    case maybePlace of
        Just place -> do
            if isPlaceinsideRoom current place servantsHouseLocked
                then if isPlaceLocked (arePlacesLocked gameState) place 
                    then do
                        putStrLn $ show place ++ " is locked."
                        return gameState
                    else do
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
                else do
                    putStrLn $ "You can't examine " ++ show place ++ " right now."
                    return gameState
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
                    if item == ButlersKeys && place == NightTable && (isButlerDistracted gameState) == False
                        then do
                            putStrLn $ "\nYou can't take " ++ show item ++ " while Butler is watching carefully."
                            return gameState
                        else do
                            let (maybeItemCount, updatedItems) = removeItemFromPlace itemsList place item
                            case maybeItemCount of
                                Just (_, count) -> do
                                    putStrLn $ "\nYou took " ++ show count ++ " " ++ show item ++ "."
                                    printLines $ getItemDescription item

                                    -- update inventory
                                    let currentInventory = inventory gameState
                                    let newInventory = addItemToInventory currentInventory (item, count)

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

giveItem :: String -> GameState -> IO GameState
giveItem itemStr gameState =
    case stringToItem itemStr of
        Just item -> do
            let maybeCharacter = talking gameState
            case maybeCharacter of
                Just character -> do
                    let currentInventory = inventory gameState
                    if item == Mushroom && character == Wizard
                        then do
                            (updatedInventory, wizardGotMushrooms) <- giveMushroomsToWizard currentInventory
                            if wizardGotMushrooms
                                then do
                                    let newGameState = gameState {inventory = updatedInventory, gaveMushrooms = wizardGotMushrooms} 
                                    updatedGameState <- talkTo "wizard" newGameState
                                    return updatedGameState
                                else do
                                    return gameState
                        else do
                            case find (\(i, _) -> i == item) currentInventory of
                                Just _ -> do
                                    putStrLn "'I don't need it. Stop bothering me!'"
                                Nothing -> do
                                    putStrLn "You don't have this item."
                            return gameState
                Nothing -> do
                    putStrLn "Who are you talking to? You must first talk to a Character."
                    return gameState
        Nothing -> do
            putStrLn "Invalid item name."
            return gameState

unlockRoomOrPlace :: String -> GameState -> IO GameState
unlockRoomOrPlace roomOrPlaceStr gameState = do
    let current = currentRoom gameState
    let servantsHouseLocked = isServantsHouseLocked gameState

    let currentInventory = inventory gameState
    if isItemInInventory currentInventory ButlersKeys
        then do
            case stringToRoom roomOrPlaceStr of
                Just room ->
                    if current == Courtyard
                        then do
                            putStrLn "\nYou unlocked ServantsHouse."
                            return gameState {isServantsHouseLocked = False}
                        else if current == ServantsHouse
                            then do
                                putStrLn "\nYou unlocked ServantsHouse."
                                let newGameState = gameState {visitedRooms = (visitedRooms gameState) ++ [current], isServantsHouseLocked = False}
                                look newGameState
                                return newGameState
                            else do
                                putStrLn "\nYou can't unlock anything from here."
                                return gameState
                Nothing ->
                    case stringToPlace roomOrPlaceStr of
                        Just place ->
                            if (elem place [GardenerChest, CookChest, ButlerChest]) && (isPlaceinsideRoom current place servantsHouseLocked)
                                then do
                                    putStrLn $ "\nYou unlocked " ++ show place ++ "."
                                    return gameState { arePlacesLocked = Map.insert place False (arePlacesLocked gameState) }
                                else do
                                        putStrLn $ "\nYou can't unlock " ++ show place ++ " from here."
                                        return gameState
                        Nothing -> do
                            putStrLn "Invalid input. Neither room nor a place."
                            return gameState
        else do
            putStrLn "You don't have any keys."
            return gameState

dropItem :: String -> GameState -> IO GameState
dropItem itemStr gameState = do
    let current = currentRoom gameState
    case stringToItem itemStr of
        Just item -> do
            let currentInventory = inventory gameState
            if item == Dirt && current == RoyalBedroom
                then 
                    case removeItemFromInventory (inventory gameState) (Dirt, 1) of
                        Just updatedInventory -> do
                            printLines ["", "You unnoticedly drop some dirt on the floor.", "'Someone spread dirt all over the floor', you yell to the Butler. 'Clean it up before king notices"]
                            return gameState {inventory = updatedInventory, isButlerDistracted = True}
                        Nothing -> do
                            putStrLn $ "You don't have " ++ show item ++ " in inventory."
                            return gameState
                else do
                    putStrLn "I don't see a reason to drop anything here!"
                    return gameState
        Nothing -> do
            putStrLn "Invalid item name."
            return gameState

accuseCharacter :: String -> GameState -> IO Bool
accuseCharacter charStr gameState =
    case stringToCharacter charStr of
        Just character -> do
            let currentClues = clues gameState
            if isCharacterTheThief currentClues character
                then do
                    printLines ["", "You win! \128077"]
                    return True
                else do
                    printLines ["", "You lose! \128546"]
                    return True
        Nothing -> return False
