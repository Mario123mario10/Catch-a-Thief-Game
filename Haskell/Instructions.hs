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

moveToRoom :: Room -> GameState -> ([String], GameState)
moveToRoom destinationRoom gameState =
    let current = currentRoom gameState
        moveMsg = ["", "You moved from " ++ show current ++ " to " ++ show destinationRoom ++ "."]
        updatedMoveMsg = case examining gameState of
            Just place -> moveMsg ++ ["", "You stopped examining " ++ show place ++ "."]
            Nothing -> moveMsg
        
        servantsHouseLocked = isServantsHouseLocked gameState
        updatedVisitedRooms = updateVisitedRooms current (visitedRooms gameState) servantsHouseLocked
        
        newGameState = gameState { currentRoom = destinationRoom, visitedRooms = updatedVisitedRooms, examining = Nothing, talking = Nothing }
        roomDescription = getRoomDescription destinationRoom (visitedRooms gameState) servantsHouseLocked
    in (updatedMoveMsg ++ roomDescription, newGameState)

tp :: String -> GameState -> ([String], GameState)
tp roomStr gameState =
    let current = currentRoom gameState
        maybeRoom = stringToRoom roomStr
    in case maybeRoom of
        Just destinationRoom ->
            let (output, newGameState) = moveToRoom destinationRoom gameState
            in (output, newGameState { roomHistory = addToRoomHistroy (roomHistory newGameState) current })
        Nothing ->
            (["", "Invalid room name."], gameState)

goTo :: String -> GameState -> ([String], GameState)
goTo roomStr gameState =
    let current = currentRoom gameState
        maybeRoom = stringToRoom roomStr
    in case maybeRoom of
        Just destinationRoom ->
            if areConnected current destinationRoom
                then
                    let (output, newGameState) = moveToRoom destinationRoom gameState
                    in (output, newGameState { roomHistory = addToRoomHistroy (roomHistory newGameState) current })
                else (["", "You can't move from " ++ show current ++ " to " ++ show destinationRoom ++ "."], gameState)
        Nothing -> (["", "Invalid room name."], gameState)

back :: GameState -> ([String], GameState)
back gameState =
    let history = roomHistory gameState
    in case popFromRoomHistory history of
        Just (destinationRoom, updatedHistroy) ->
            let (text, newGameState) = moveToRoom destinationRoom gameState
            in (text, newGameState { roomHistory = updatedHistroy })
        Nothing ->
            (["No room history."], gameState)

look :: GameState -> [String]
look gameState =
    let current = currentRoom gameState
        servantsHouseLocked = isServantsHouseLocked gameState
    in getRoomDescription current [] servantsHouseLocked

examine :: String -> GameState -> ([String], GameState)
examine placeStr gameState =
    case stringToPlace placeStr of
        Just place ->
            let current = currentRoom gameState
                servantsHouseLocked = isServantsHouseLocked gameState
            in if isPlaceinsideRoom current place servantsHouseLocked
                then examineValidPlace place gameState
                else (["", "You can't examine " ++ show place ++ " right now."], gameState)
        Nothing ->
            (["Invalid place name."], gameState)

examineValidPlace :: Place -> GameState -> ([String], GameState)
examineValidPlace place gameState =
    if isPlaceLocked (arePlacesLocked gameState) place
        then ( [show place ++ " is locked."], gameState )
        else
            let description = getPlaceDescription place
                examiningMsg = ["", "You are examining " ++ show place ++ "."]
                updatedexaminingMsg = case examining gameState of
                    Just oldPlace -> ["", "You stopped examining " ++ show oldPlace ++ "."] ++ examiningMsg
                    Nothing -> examiningMsg
                newGameState = gameState { examining = Just place }
                itemsInPlace = filter (\(p, _) -> p == place) (items gameState)
                itemDescriptions = map (formatItem . snd) itemsInPlace
                output = description ++ updatedexaminingMsg ++ itemDescriptions
            in (output, newGameState)

takeItem :: String -> GameState -> ([String], GameState)
takeItem itemStr gameState =
    let maybePlace = examining gameState
    in case maybePlace of
        Just place ->
            takeItemFromValidPlace place itemStr gameState
        Nothing ->
            (["", "You must first examine a Place."], gameState)

takeItemFromValidPlace :: Place -> String -> GameState -> ([String], GameState)
takeItemFromValidPlace place itemStr gameState =
    let maybeItem = stringToItem itemStr
    in case maybeItem of
        Just item ->
            takeValidItem place item gameState
        Nothing ->
            (["Invalid item name."], gameState)

takeValidItem :: Place -> Item -> GameState -> ([String], GameState)
takeValidItem place item gameState =
    if cannotTakeItem item place gameState
        then (["", "You can't take " ++ show item ++ " while the Butler is watching carefully. You should distract this fanatic cleaner somehow."], gameState)
        else handleItemTake item place gameState

cannotTakeItem :: Item -> Place -> GameState -> Bool
cannotTakeItem item place gameState =
    item == ButlersKeys && place == NightTable && not (isButlerDistracted gameState)

removeItemFromPlace :: [(Place, (Item, Int))] -> Place -> Item -> (Maybe (Item, Int), [(Place, (Item, Int))])
removeItemFromPlace items place item =
    let matchingItem = find (\(p, (i, _)) -> p == place && i == item) items
    in case matchingItem of
        Just foundItem ->
            let updatedItems = filter (\(p, _) -> p /= place || p == place && fst (snd foundItem) /= item) items
            in (Just (snd foundItem), updatedItems)
        Nothing -> (Nothing, items)

handleItemTake :: Item -> Place -> GameState -> ([String], GameState)
handleItemTake item place gameState =
    let (maybeItemCount, updatedItems) = removeItemFromPlace (items gameState) place item
    in case maybeItemCount of
        Just (_, count) ->
            let output1 = ["You took " ++ show count ++ " " ++ show item ++ "."]
                output2 = getItemDescription item
                (updatedGameState, craftedItem) = updateGameStateWithItem item count gameState
                output3 = case craftedItem of
                    Just crafted -> ["You crafted " ++ show crafted ++ " using 2 toolparts and 1 toolhandle."]
                    Nothing -> []
                updatedGameState' = updatedGameState { items = updatedItems }
            in (output1 ++ output2 ++ output3, updatedGameState')
        Nothing ->
            (["You can't take " ++ show item ++ " from here."], gameState)

addEvidence :: Map.Map Clue (Maybe Character) -> Clue -> Maybe Character -> Map.Map Clue (Maybe Character)
addEvidence evidence clue maybeCharacter =
    case Map.lookup clue evidence of
        Just maybeChar ->
            case maybeChar of
                Just _ -> evidence
                Nothing -> Map.insert clue maybeCharacter evidence
        Nothing -> Map.insert clue maybeCharacter evidence

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

spawnItem :: String -> GameState -> ([String], GameState)
spawnItem itemStr gameState =
    case stringToItem itemStr of
        Just item ->
            let output1 = ["You took " ++ show item ++ "."]
                output2 = getItemDescription item
                currentInventory = inventory gameState
                newInventory = addItemToInventory currentInventory (item, 1)
                updatedGameState = gameState { inventory = newInventory }
            in (output1 ++ output2, updatedGameState)
        Nothing ->
            (["Invalid item name."], gameState)

talkTo :: String -> GameState -> ([String], GameState)
talkTo charStr gameState =
    case stringToCharacter charStr of
        Just character ->
            handleCharacterTalk character gameState
        Nothing ->
            (["Invalid character name."], gameState)

handleCharacterTalk :: Character -> GameState -> ([String], GameState)
handleCharacterTalk character gameState =
    case getCharacterInRoom (currentRoom gameState) of
        Just char ->
            if character == char
                then talkWithCharacter character gameState
                else (["You can't talk to him right now."], gameState)
        Nothing ->
            (["You can't talk to him right now."], gameState)

talkWithCharacter :: Character -> GameState -> ([String], GameState)
talkWithCharacter character gameState =
    let currentClues = clues gameState
        currentGaveMushrooms = gaveMushrooms gameState
        text = getCharacterText currentClues character currentGaveMushrooms
        updatedEvidence = updateEvidenceWithCharacter character currentGaveMushrooms (evidence gameState) (clues gameState)
    in (text, gameState { talking = Just character, evidence = updatedEvidence })

specifyClue :: Map.Map Clue (Maybe Character) -> [(Character, [Clue])] -> Clue -> Map.Map Clue (Maybe Character)
specifyClue evidence clues clue = 
    let character = whoIsGuilty clues clue
    in addEvidence evidence clue (Just character)

updateEvidenceWithCharacter :: Character -> Bool -> Map Clue (Maybe Character) -> [(Character, [Clue])] -> Map Clue (Maybe Character)
updateEvidenceWithCharacter character currentGaveMushrooms currentEvidence currentClues =
    case character of
        Guard -> specifyClue currentEvidence currentClues GuardsClue
        Wizard ->
            if currentGaveMushrooms
                then specifyClue currentEvidence currentClues WizardsClue
                else currentEvidence
        _ -> currentEvidence

askAbout :: String -> GameState -> ([String], GameState)
askAbout clueStr gameState =
    case stringToClue clueStr of
        Just clue ->
            handleClue clue gameState
        Nothing ->
            (["Invalid evidence."], gameState)

handleClue :: Clue -> GameState -> ([String], GameState)
handleClue clue gameState =
    case talking gameState of
        Just character ->
            handleCharacterWithClue clue character gameState
        Nothing ->
            (["Who are you talking to? You must first talk to a Character before you can ask him about anything."], gameState)

handleCharacterWithClue :: Clue -> Character -> GameState -> ([String], GameState)
handleCharacterWithClue clue character gameState =
    if Map.member clue (evidence gameState)
        then handleExistingClue clue character gameState
        else (["'You are asking wrong person, that's for sure!' said the " ++ show character ++ "."], gameState)

handleExistingClue :: Clue -> Character -> GameState -> ([String], GameState)
handleExistingClue clue character gameState =
    let currentClues = clues gameState
        text = getClueCharacterText currentClues character clue
        updatedEvidence = if isCharacterGuilty currentClues character clue
                            then specifyClue (evidence gameState) currentClues clue
                            else evidence gameState
    in (text, gameState { evidence = updatedEvidence })

giveItem :: String -> GameState -> ([String], GameState)
giveItem itemStr gameState =
    case stringToItem itemStr of
        Just item -> handleGivenItem item gameState
        Nothing -> handleInvalidItem gameState

handleGivenItem :: Item -> GameState -> ([String], GameState)
handleGivenItem item gameState =
    case talking gameState of
        Just character -> handleGivenCharacter item character gameState
        Nothing ->
            (["Who are you talking to? You must first talk to a Character before you can give him anything."], gameState)

handleGivenCharacter :: Item -> Character -> GameState -> ([String], GameState)
handleGivenCharacter item character gameState
    | item == Mushroom && character == Wizard = giveMushroomsToWizard gameState
    | otherwise = handleOtherItem item gameState

giveMushrooms :: [(Item, Int)] -> ([String], ([(Item, Int)], Bool))
giveMushrooms inventory =
    case removeItemFromInventory inventory (Mushroom, 10) of
        Just updatedInventory ->
            (["You gave Wizard 10 mushrooms."], (updatedInventory, True))
        Nothing ->
            (["'You don't have enough mushrooms! Come back when you have them.'"], (inventory, False))

giveMushroomsToWizard :: GameState -> ([String], GameState)
giveMushroomsToWizard gameState =
    let currentInventory = inventory gameState
    in case find (\(i, _) -> i == Mushroom) currentInventory of
        Just _ ->
            let (text1, (updatedInventory, wizardGotMushrooms)) = giveMushrooms currentInventory
            in if wizardGotMushrooms
                then
                    let newGameState = gameState { inventory = updatedInventory, gaveMushrooms = wizardGotMushrooms }
                        (text2, updatedGameState) = talkTo "wizard" newGameState
                    in (text1 ++ text2, updatedGameState)
                else (["'It's not enaugh! Come back when you have 10 mushrooms.'"], gameState)
        Nothing ->
            (["'You want to give me nothing?! Come back when you have 10 mushrooms.'"], gameState)

handleOtherItem :: Item -> GameState -> ([String], GameState)
handleOtherItem item gameState =
    let currentInventory = inventory gameState
    in case find (\(i, _) -> i == item) currentInventory of
        Just _ ->
            (["'I don't need it. Stop bothering me!'"], gameState)
        Nothing ->
            (["You don't have this item."], gameState)

unlockRoomOrPlace :: String -> GameState -> ([String], GameState)
unlockRoomOrPlace roomOrPlaceStr gameState =
    let current = currentRoom gameState
        currentInventory = inventory gameState
    in
        if isItemInInventory currentInventory ButlersKeys
            then unlockWithKeys roomOrPlaceStr current gameState
            else (["You don't have any keys you could use."], gameState)

unlockWithKeys :: String -> Room -> GameState -> ([String], GameState)
unlockWithKeys roomOrPlaceStr current gameState =
    case stringToRoom roomOrPlaceStr of
        Just room ->
            if current == Courtyard
                then unlockRoom "ServantsHouse" gameState
                else if current == ServantsHouse
                    then unlockServantsHouse gameState
                    else (["You can't unlock anything from here."], gameState)
        Nothing ->
            case stringToPlace roomOrPlaceStr of
                Just place ->
                    if elem place [GardenerChest, CookChest, ButlerChest] && isPlaceinsideRoom current place (isServantsHouseLocked gameState)
                        then unlockPlace place gameState
                        else (["You can't unlock " ++ show place ++ " from here."], gameState)
                Nothing ->
                    (["Invalid input. Neither room nor a place."], gameState)

unlockRoom :: String -> GameState -> ([String], GameState)
unlockRoom roomName gameState =
    (["You unlocked " ++ roomName ++ "."], gameState { isServantsHouseLocked = False })

unlockServantsHouse :: GameState -> ([String], GameState)
unlockServantsHouse gameState =
    let newGameState = gameState { visitedRooms = visitedRooms gameState ++ [currentRoom gameState]
                                , isServantsHouseLocked = False }
        text = look newGameState
    in (["You unlocked ServantsHouse."] ++ text, newGameState)

unlockPlace :: Place -> GameState -> ([String], GameState)
unlockPlace place gameState =
    let updatedPlacesLocked = Map.insert place False (arePlacesLocked gameState)
    in (["You unlocked " ++ show place ++ "."], gameState { arePlacesLocked = updatedPlacesLocked })

dropItem :: String -> GameState -> ([String], GameState)
dropItem itemStr gameState =
    let current = currentRoom gameState
    in case stringToItem itemStr of
        Just item ->
            let currentInventory = inventory gameState
            in if item == Dirt && current == RoyalBedroom
                then handleDroppingDirt gameState currentInventory
                else (["You can't drop this item here."], gameState)
        Nothing -> (["Invalid item name."], gameState)

handleDroppingDirt :: GameState -> [(Item, Int)] -> ([String], GameState)
handleDroppingDirt gameState currentInventory =
    case removeItemFromInventory currentInventory (Dirt, 1) of
        Just updatedInventory ->
            ( ["", "You unnoticedly drop some dirt on the floor.", "'Someone spread dirt all over the floor', you yell to the Butler. 'Clean it up before king notices!'"]
            , gameState { inventory = updatedInventory, isButlerDistracted = True }
            )
        Nothing ->
            (["You don't have Dirt in inventory."], gameState)

handleInvalidDrop :: GameState -> ([String], GameState)
handleInvalidDrop gameState =
    (["I don't see a reason to drop anything here!"], gameState)

handleInvalidItem :: GameState -> ([String], GameState)
handleInvalidItem gameState =
    (["Invalid item name."], gameState)

accuseCharacter :: String -> GameState -> ([String], Bool)
accuseCharacter charStr gameState =
    case stringToCharacter charStr of
        Just character -> handleAccusation gameState character
        Nothing -> (["Invalid character name."], False)

handleAccusation :: GameState -> Character -> ([String], Bool)
handleAccusation gameState character =
    if isCharacterTheThief (clues gameState) character
        then handleWin
        else handleLose

handleWin :: ([String], Bool)
handleWin = (["You win! \128077"], True)

handleLose :: ([String], Bool)
handleLose = (["You lose! \128546"], True)
