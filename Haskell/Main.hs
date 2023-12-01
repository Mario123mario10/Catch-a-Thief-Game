-- The germ of a text adventure game
-- Marcin Szlenk 2022

module Main where

import System.IO (hFlush, stdout)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock

import Printing
import Instructions
import Places
import Characters
import Inventory
import World

introductionText = [
    "",
    "====================================================",
    "    Mystery of a missing Diamond",
    "====================================================",
    "",
    "As a detective, you've been entrusted with",
    "solving the mystery of a missing diamond.",
    "To crack the case, you'll need to uncover",
    "scattered evidence hidden throughout the castle.",
    "",
    "Gather three pieces of evidence against",
    "a suspect to uncover the thief's identity.",
    "Use the command 'accuse [Character]'",
    "to make your accusation.",
    "A correct accusation leads to victory!"
    ]

printIntroduction = printLinesWithoutSplit introductionText
printInstructions = printLinesWithoutSplit instructionsText

initializeGame :: IO GameState
initializeGame = do
    let startingRoom = Hall
    let initialVisitedRooms = [Hall]
    
    clues <- assignClues
    items <- assignItems clues
    startTime <- getCurrentTime

    return GameState { 
        startTime = startTime,
        currentRoom = startingRoom, 
        visitedRooms = initialVisitedRooms, 
        roomHistory = [],
        examining = Nothing,
        talking = Nothing,
        clues = clues, 
        items = items, 
        inventory = [], 
        evidence = Map.empty,
        isButlerDistracted = False,
        gaveMushrooms = False,
        isServantsHouseLocked = True,
        arePlacesLocked = Map.fromList
            [ (GardenerChest, True)
            , (CookChest, True)
            , (ButlerChest, True)
            ]
        }

readCommand :: IO String
readCommand = do
    putStr "> "
    hFlush stdout
    getLine
    
gameLoop :: GameState -> IO ()
gameLoop gameState = do
    currentTime <- getCurrentTime
    let timePassed = diffUTCTime currentTime $ startTime gameState
        timePassedInSeconds = round $ realToFrac timePassed :: Int 

    cmd <- readCommand

    let maxGameTime = 25 * 60
    let timeRemaining = maxGameTime - timePassedInSeconds

    if timeRemaining >= 0
        then do
            case words cmd of
                ["instructions"] -> do 
                    printInstructions
                    gameLoop gameState
                ["time"] -> do
                    printLines ["", ("Time remaining: " ++ show  timeRemaining ++ ".")]
                    gameLoop gameState
                ["dev"] -> do 
                    printLinesWithoutSplit debugInstructionsText
                    gameLoop gameState
                ["look"] -> do 
                    look gameState
                    gameLoop gameState
                ["inventory"] -> do 
                    let currentInventory = inventory gameState
                    printLines $ getInventoryDescription currentInventory
                    gameLoop gameState
                ["evidence"] -> do 
                    let currentEvidence = evidence gameState
                    printLines $ getEvidenceDescription currentEvidence
                    gameLoop gameState
                ["talk", "to", charStr] -> do 
                    newGameState <- talkTo charStr gameState
                    gameLoop newGameState
                ["ask", "about", clueStr] -> do 
                    newGameState <- askAbout clueStr gameState
                    gameLoop newGameState
                ["give", itemStr] -> do 
                    newGameState <- giveItem itemStr gameState
                    gameLoop newGameState
                ["examine", placeStr] -> do 
                    newGameState <- examine placeStr gameState
                    gameLoop newGameState
                ["take", itemStr] -> do
                    newGameState <- takeItem itemStr gameState
                    gameLoop newGameState
                ["drop", itemStr] -> do
                    newGameState <- dropItem itemStr gameState
                    gameLoop newGameState
                ["go", "to", roomStr] -> do
                    newGameState <- goTo roomStr gameState
                    gameLoop newGameState
                ["back"] -> do
                    newGameState <- back gameState
                    gameLoop newGameState
                ["unlock", roomOrPlaceStr] -> do
                    newGameState <- unlockRoomOrPlace roomOrPlaceStr gameState
                    gameLoop newGameState
                ["visited"] -> do
                    let visited = visitedRooms gameState
                    putStrLn $ show visited
                    gameLoop gameState
                ["whereami"] -> do
                    let current = currentRoom gameState
                    putStrLn $ show current
                    gameLoop gameState
                ["clues"] -> do
                    let current = clues gameState
                    putStrLn $ show current
                    gameLoop gameState
                ["items"] -> do
                    let current = items gameState
                    putStrLn $ show current
                    gameLoop gameState
                ["gamestate"] -> do
                    putStrLn $ show gameState
                    gameLoop gameState
                ["spawn", itemStr] -> do
                    newGameState <- spawnItem itemStr gameState
                    gameLoop newGameState
                ["checktool"] -> do
                    let currentInventory = inventory gameState
                    putStrLn $ show $ hasRequiredItemsForTool currentInventory
                    gameLoop gameState
                ["tp", roomStr] -> do
                    newGameState <- tp roomStr gameState
                    gameLoop newGameState
                ["accuse", charStr] -> do
                    validAccusation <- accuseCharacter charStr gameState
                    if validAccusation
                        then return ()
                        else gameLoop gameState
                ["quit"] -> return ()
                _ -> do 
                    printLines ["Unknown command.", ""]
                    gameLoop gameState
        else do
            printLines ["You have ran out of time.", "You must accuse someone", ""]
            case words cmd of
                ["accuse", charStr] -> do
                    validAccusation <- accuseCharacter charStr gameState
                    if validAccusation
                        then return ()
                        else gameLoop gameState
                _ -> do
                    gameLoop gameState


    

main = do
    printIntroduction
    printInstructions

    gameState <- initializeGame
    look gameState
    gameLoop gameState

