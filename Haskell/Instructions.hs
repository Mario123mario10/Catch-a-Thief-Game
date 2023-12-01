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
    "back                -- to go to last room.",
    "unlock [Room/Place] -- to unlock locked room/place with keys",
    "accuse [Character]  -- to character and if correct win the game",
    "",
    "quit                -- to end the game and quit.",
    "dev                 -- to see debug instructions (cheat).",
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
            return newGameState {roomHistory = addToRoomHistroy (roomHistory newGameState) current}
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
                    return newGameState {roomHistory = addToRoomHistroy (roomHistory newGameState) current}
                else do
                    putStrLn $ "You can't move from " ++ show current ++ " to " ++ show destinationRoom ++ "."
                    return gameState
        Nothing -> do
            putStrLn "Invalid room name."
            return gameState

back :: GameState -> IO GameState
back gameState = do
    let history = roomHistory gameState
    case popFromRoomHistory history of
        Just (destinationRoom, updatedHistroy) -> do
            newGameState <- moveToRoom destinationRoom gameState
            return newGameState {roomHistory = updatedHistroy}
        Nothing -> do
            putStrLn "No room history."
            return gameState

look :: GameState -> IO ()
look gameState = do
    let current = currentRoom gameState
    let servantsHouseLocked = isServantsHouseLocked gameState
    let roomDescription = getRoomDescription current [] servantsHouseLocked
    printLines roomDescription

examine :: String -> GameState -> IO GameState
examine placeStr gameState = case stringToPlace placeStr of
    Just place ->
        let current = currentRoom gameState
            servantsHouseLocked = isServantsHouseLocked gameState
        in if isPlaceinsideRoom current place servantsHouseLocked
            then examineValidPlace place gameState
            else putStrLn ("You can't examine " ++ show place ++ " right now.") >> return gameState
    Nothing ->
        putStrLn "Invalid place name." >> return gameState

examineValidPlace :: Place -> GameState -> IO GameState
examineValidPlace place gameState =
    if isPlaceLocked (arePlacesLocked gameState) place
        then putStrLn (show place ++ " is locked.") >> return gameState
        else do
            let description = getPlaceDescription place
            printLines description

            let examiningMsg = ["", "You are examining " ++ show place ++ "."]
                updatedexaminingMsg = case examining gameState of
                    Just oldPlace -> ["You stopped examining " ++ show oldPlace ++ "."] ++ examiningMsg
                    Nothing -> examiningMsg
            printLines updatedexaminingMsg

            let newGameState = gameState { examining = Just place }
                itemsInPlace = filter (\(p, _) -> p == place) (items gameState)
            case itemsInPlace of
                [] -> return newGameState
                _ -> do
                    let itemDescriptions = map (formatItem . snd) itemsInPlace
                    printLines itemDescriptions
                    return newGameState

takeItem :: String -> GameState -> IO GameState
takeItem itemStr gameState = do
    let maybePlace = examining gameState
    case maybePlace of
        Just place -> do
            newGameState <- takeItemFromValidPlace place itemStr gameState
            return newGameState
        Nothing -> do
            putStrLn "You must first examine a Place."
            return gameState

takeItemFromValidPlace :: Place -> String -> GameState -> IO GameState
takeItemFromValidPlace place itemStr gameState = do
    let maybeItem = stringToItem itemStr
    case maybeItem of
        Just item -> do
            newGameState <- takeValidItem place item gameState
            return newGameState
        Nothing -> do
            putStrLn "Invalid item name."
            return gameState

takeValidItem :: Place -> Item -> GameState -> IO GameState
takeValidItem place item gameState =
    if cannotTakeItem item place gameState
        then cannotTakeItemMessage item >> return gameState
        else handleItemTake item place gameState

cannotTakeItem :: Item -> Place -> GameState -> Bool
cannotTakeItem item place gameState =
    item == ButlersKeys && place == NightTable && not (isButlerDistracted gameState)

cannotTakeItemMessage :: Item -> IO ()
cannotTakeItemMessage item = putStrLn $ "\nYou can't take " ++ show item ++ " while the Butler is watching carefully."

handleItemTake :: Item -> Place -> GameState -> IO GameState
handleItemTake item place gameState =
    let (maybeItemCount, updatedItems) = removeItemFromPlace (items gameState) place item
    in case maybeItemCount of
        Just (_, count) -> do
            putStrLn $ "\nYou took " ++ show count ++ " " ++ show item ++ "."
            printLines $ getItemDescription item
            let (updatedGameState, craftedItem) = updateGameStateWithItem item count gameState
            case craftedItem of
                Just crafted -> putStrLn $ "\nYou crafted " ++ show crafted ++ " using 2 toolparts and 1 toolhandle."
                Nothing -> return ()
            return updatedGameState { items = updatedItems }
        Nothing -> do 
            putStrLn $ "\nYou can't take " ++ show item ++ " from here."
            return gameState

updateEvidenceWithItem :: Item -> Map.Map Clue (Maybe Character) -> Map.Map Clue (Maybe Character)
updateEvidenceWithItem item currentEvidence = case item of
    ToolHandle -> addEvidence currentEvidence BloodStains Nothing
    VaultKey -> addEvidence currentEvidence StolenVaultKey Nothing
    Diamond -> addEvidence currentEvidence StolenDiamond Nothing
    CoinPouch -> addEvidence currentEvidence StolenCoins Nothing
    _ -> currentEvidence 

craftTool :: Item -> [(Item, Int)] -> Map.Map Clue (Maybe Character) -> GameState -> (GameState, Maybe Item)
craftTool item currentInventory currentEvidence gameState =
    let currentClues = clues gameState
        removedHandle = fromMaybe currentInventory (removeItemFromInventory currentInventory (ToolHandle, 1))
        removedParts = fromMaybe removedHandle (removeItemFromInventory removedHandle (ToolPart, 2))
        tool = case whoIsGuilty currentClues Tool of
            Gardener -> Rake
            Cook -> Ladle
            Butler -> FeatherDuster
        addedTool = addItemToInventory removedParts (tool, 1)
        updatedEvidence = addEvidence currentEvidence Tool Nothing
        craftedItem = if hasRequiredItemsForTool currentInventory
                        then Just tool
                        else Nothing
        updatedGameState = if hasRequiredItemsForTool currentInventory
                                then gameState { inventory = addedTool, evidence = updatedEvidence }
                                else gameState { inventory = currentInventory, evidence = currentEvidence }
    in (updatedGameState, craftedItem)

updateGameStateWithItem :: Item -> Int -> GameState -> (GameState, Maybe Item)
updateGameStateWithItem item count gameState =
    let currentInventory = inventory gameState
        newInventory = addItemToInventory currentInventory (item, count)
        newEvidence = updateEvidenceWithItem item (evidence gameState)
        (updatedGameState, craftedItem) = craftTool item newInventory newEvidence gameState
    in (updatedGameState, craftedItem)
    

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
talkTo charStr gameState =
    case stringToCharacter charStr of
        Just character -> handleCharacterTalk character gameState
        Nothing -> do
            putStrLn "Invalid character name."
            return gameState

handleCharacterTalk :: Character -> GameState -> IO GameState
handleCharacterTalk character gameState =
    case getCharacterInRoom (currentRoom gameState) of
        Just char ->
            if character == char
                then talkWithCharacter character gameState
                else do
                    putStrLn "You can't talk to him right now."
                    return gameState
        Nothing -> do
                putStrLn "You can't talk to him right now."
                return gameState

talkWithCharacter :: Character -> GameState -> IO GameState
talkWithCharacter character gameState = do
    let currentClues = clues gameState
    let currentGaveMushrooms = gaveMushrooms gameState
    let text = getCharacterText currentClues character currentGaveMushrooms
    printLines text

    let updatedEvidence = updateEvidenceWithCharacter character currentGaveMushrooms (evidence gameState) (clues gameState)
    return gameState { talking = Just character, evidence = updatedEvidence }

updateEvidenceWithCharacter :: Character -> Bool -> Map Clue (Maybe Character) -> [(Character, [Clue])] -> Map Clue (Maybe Character)
updateEvidenceWithCharacter character currentGaveMushrooms currentEvidence currentClues =
    case character of
        Guard -> specifyClue currentEvidence currentClues GuardsClue
        Wizard ->
            if currentGaveMushrooms
                then specifyClue currentEvidence currentClues WizardsClue
                else currentEvidence
        _ -> currentEvidence

askAbout :: String -> GameState -> IO GameState
askAbout clueStr gameState =
    case stringToClue clueStr of
        Just clue -> handleClue clue gameState
        Nothing -> do
            putStrLn "Invalid evidence."
            return gameState

handleClue :: Clue -> GameState -> IO GameState
handleClue clue gameState =
    case talking gameState of
        Just character -> handleCharacterWithClue clue character gameState
        Nothing -> do
            putStrLn "Invalid character."
            return gameState

handleCharacterWithClue :: Clue -> Character -> GameState -> IO GameState
handleCharacterWithClue clue character gameState =
    if Map.member clue (evidence gameState)
        then handleExistingClue clue character gameState
        else do
            putStrLn "What are you talking about?"
            return gameState

handleExistingClue :: Clue -> Character -> GameState -> IO GameState
handleExistingClue clue character gameState = do
    let currentClues = clues gameState
    let text = getClueCharacterText currentClues character clue
    printLines text

    let updatedEvidence = if isCharacterGuilty currentClues character clue
                            then specifyClue (evidence gameState) currentClues clue
                            else evidence gameState

    return gameState { evidence = updatedEvidence }

giveItem :: String -> GameState -> IO GameState
giveItem itemStr gameState =
    case stringToItem itemStr of
        Just item -> handleGivenItem item gameState
        Nothing -> handleInvalidItem gameState

handleGivenItem :: Item -> GameState -> IO GameState
handleGivenItem item gameState =
    case talking gameState of
        Just character -> handleGivenCharacter item character gameState
        Nothing -> do
            putStrLn "Who are you talking to? You must first talk to a Character before you can give him anything."
            return gameState

handleGivenCharacter :: Item -> Character -> GameState -> IO GameState
handleGivenCharacter item character gameState
    | item == Mushroom && character == Wizard = giveMushroomsToWizard gameState
    | otherwise = handleOtherItem item gameState

giveMushrooms :: [(Item, Int)] -> IO ([(Item, Int)], Bool)
giveMushrooms inventory = 
    let maybeUpdatedInventory = removeItemFromInventory inventory (Mushroom, 10)
    in
        case maybeUpdatedInventory of
            Just updatedInventory -> do
                putStrLn "You gave Wizard 10 mushrooms."
                return (updatedInventory, True)
            Nothing -> do
                putStrLn "'You don't have enough mushrooms! Come back when you have them.'"
                return (inventory, False)

giveMushroomsToWizard :: GameState -> IO GameState
giveMushroomsToWizard gameState = do
    let currentInventory = inventory gameState
    case find (\(i, _) -> i == Mushroom) currentInventory of
        Just _ -> do
            (updatedInventory, wizardGotMushrooms) <- giveMushrooms currentInventory
            if wizardGotMushrooms
                then do
                    let newGameState = gameState { inventory = updatedInventory, gaveMushrooms = wizardGotMushrooms }
                    updatedGameState <- talkTo "wizard" newGameState
                    return updatedGameState
                else return gameState
        Nothing -> do
            putStrLn "'I don't need it. Stop bothering me!'"
            return gameState

handleOtherItem :: Item -> GameState -> IO GameState
handleOtherItem item gameState = do
    let currentInventory = inventory gameState
    case find (\(i, _) -> i == item) currentInventory of
        Just _ -> do
            putStrLn "'I don't need it. Stop bothering me!'"
            return gameState
        Nothing -> do
            putStrLn "You don't have this item."
            return gameState

unlockRoomOrPlace :: String -> GameState -> IO GameState
unlockRoomOrPlace roomOrPlaceStr gameState = do
    let current = currentRoom gameState
    let currentInventory = inventory gameState

    if isItemInInventory currentInventory ButlersKeys
        then unlockWithKeys roomOrPlaceStr current gameState
        else do
            putStrLn "\nYou don't have any keys you could use."
            return gameState

unlockWithKeys :: String -> Room -> GameState -> IO GameState
unlockWithKeys roomOrPlaceStr current gameState =
    case stringToRoom roomOrPlaceStr of
        Just room ->
            if current == Courtyard
                then unlockRoom "ServantsHouse" gameState
                else if current == ServantsHouse
                    then unlockServantsHouse gameState
                    else do
                        putStrLn "\nYou can't unlock anything from here."
                        return gameState
        Nothing ->
            case stringToPlace roomOrPlaceStr of
                Just place ->
                    if elem place [GardenerChest, CookChest, ButlerChest] && isPlaceinsideRoom current place (isServantsHouseLocked gameState)
                        then unlockPlace place gameState
                        else do
                            putStrLn $ "\nYou can't unlock " ++ show place ++ " from here."
                            return gameState
                Nothing -> do
                    putStrLn "Invalid input. Neither room nor a place."
                    return gameState

unlockRoom :: String -> GameState -> IO GameState
unlockRoom roomName gameState = do
    putStrLn $ "\nYou unlocked " ++ roomName ++ "."
    return $ gameState { isServantsHouseLocked = False }

unlockServantsHouse :: GameState -> IO GameState
unlockServantsHouse gameState = do
    putStrLn "\nYou unlocked ServantsHouse."
    let newGameState = gameState { visitedRooms = visitedRooms gameState ++ [currentRoom gameState]
                                , isServantsHouseLocked = False }
    look newGameState
    return newGameState

unlockPlace :: Place -> GameState -> IO GameState
unlockPlace place gameState = do
    putStrLn $ "\nYou unlocked " ++ show place ++ "."
    let updatedPlacesLocked = Map.insert place False (arePlacesLocked gameState)
    return $ gameState { arePlacesLocked = updatedPlacesLocked }

dropItem :: String -> GameState -> IO GameState
dropItem itemStr gameState = do
    let current = currentRoom gameState
    case stringToItem itemStr of
        Just item ->
            let currentInventory = inventory gameState
            in if item == Dirt && current == RoyalBedroom
                then handleDroppingDirt gameState currentInventory
                else handleInvalidDrop gameState
        Nothing -> handleInvalidItem gameState

handleDroppingDirt :: GameState -> [(Item, Int)] -> IO GameState
handleDroppingDirt gameState currentInventory =
    case removeItemFromInventory currentInventory (Dirt, 1) of
        Just updatedInventory -> do
            printLines ["", "You unnoticedly drop some dirt on the floor.", "'Someone spread dirt all over the floor', you yell to the Butler. 'Clean it up before king notices'"]
            return gameState { inventory = updatedInventory, isButlerDistracted = True }
        Nothing -> do
            putStrLn $ "You don't have Dirt in inventory."
            return gameState

handleInvalidDrop :: GameState -> IO GameState
handleInvalidDrop gameState = do
    putStrLn "I don't see a reason to drop anything here!"
    return gameState

handleInvalidItem :: GameState -> IO GameState
handleInvalidItem gameState = do
    putStrLn "Invalid item name."
    return gameState

accuseCharacter :: String -> GameState -> IO Bool
accuseCharacter charStr gameState =
    case stringToCharacter charStr of
        Just character -> handleAccusation gameState character
        Nothing -> return False

handleAccusation :: GameState -> Character -> IO Bool
handleAccusation gameState character =
    if isCharacterTheThief (clues gameState) character
        then handleWin
        else handleLose

handleWin :: IO Bool
handleWin = do
    printLines ["", "You win! \128077"]
    return True

handleLose :: IO Bool
handleLose = do
    printLines ["", "You lose! \128546"]
    return True
