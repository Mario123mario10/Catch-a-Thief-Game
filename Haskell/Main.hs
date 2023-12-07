-- The germ of a text adventure game
-- Marcin Szlenk 2022

module Main where

import System.IO (hFlush, stdout)
import Data.Char (toLower, isSpace)
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

maxGameTime = 25 * 60
    
gameLoop :: GameState -> IO ()
gameLoop gameState = do
    currentTime <- getCurrentTime
    let timePassed = diffUTCTime currentTime $ startTime gameState
        timePassedInSeconds = round $ realToFrac timePassed :: Int 
        timeRemaining = maxGameTime - timePassedInSeconds

    if timeRemaining >= 0
        then do
            cmd <- readCommand
            case words cmd of
                ["instructions"] -> do 
                    printInstructions
                    gameLoop gameState
                ["time"] -> do
                    let timePassed = diffUTCTime currentTime $ startTime gameState
                        timePassedInSeconds = round $ realToFrac timePassed :: Int 
                        timeRemaining = maxGameTime - timePassedInSeconds
                    printLines ["", ("Time remaining: " ++ show  timeRemaining ++ " seconds.")]
                    gameLoop gameState
                ["dev"] -> do 
                    printLinesWithoutSplit debugInstructionsText
                    gameLoop gameState
                ["look"] -> do 
                    printLines $ look gameState
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
                    let (text, newGameState) = talkTo charStr gameState
                    printLines text
                    gameLoop newGameState
                ["ask", "about", clueStr] -> do 
                    let (text, newGameState) = askAbout clueStr gameState
                    printLines text
                    gameLoop newGameState
                ["give", itemStr] -> do 
                    let (text, newGameState) = giveItem itemStr gameState
                    printLines text
                    gameLoop newGameState
                ["examine", placeStr] -> do 
                    let (text, newGameState) = examine placeStr gameState
                    printLines text
                    gameLoop newGameState
                ["take", itemStr] -> do
                    let (text, newGameState) = takeItem itemStr gameState
                    printLines text
                    gameLoop newGameState
                ["drop", itemStr] -> do
                    let (text, newGameState) = dropItem itemStr gameState
                    printLines text
                    gameLoop newGameState
                ["go", "to", roomStr] -> do
                    let (text, newGameState) = goTo roomStr gameState
                    printLines text
                    gameLoop newGameState
                ["back"] -> do
                    let (text, newGameState) = back gameState
                    printLines text
                    gameLoop newGameState
                ["unlock", roomOrPlaceStr] -> do
                    let (text, newGameState) = unlockRoomOrPlace roomOrPlaceStr gameState
                    printLines text
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
                    let (text, newGameState) = spawnItem itemStr gameState
                    printLines text
                    gameLoop newGameState
                ["checktool"] -> do
                    let currentInventory = inventory gameState
                    putStrLn $ show $ hasRequiredItemsForTool currentInventory
                    gameLoop gameState
                ["tp", roomStr] -> do
                    let (text, newGameState) = tp roomStr gameState
                    printLines text
                    gameLoop newGameState
                ["accuse", charStr] -> do
                    let (text, validAccusation) = accuseCharacter charStr gameState
                    if validAccusation
                        then do
                            printLines ["Are you sure? (Y/N)"]
                            sureStr <- readCommand
                            if map toLower sureStr `elem` ["y", "yes", "sure"]
                                then do
                                    printLines text
                                    return ()
                                else do
                                    printLines ["Take your time!"]
                                    gameLoop gameState
                        else gameLoop gameState
                ["quit"] -> return ()
                _ -> do 
                    printLines ["Unknown command.", ""]
                    gameLoop gameState
        else do
            printLines ["You have ran out of time.", "You must accuse someone", ""]
            cmd <- readCommand
            case words cmd of
                ["accuse", charStr] -> do
                    let (text, validAccusation) = accuseCharacter charStr gameState
                    if validAccusation
                        then do
                            printLines ["Are you sure? (Y/N)"]
                            sureStr <- readCommand
                            if map toLower sureStr `elem` ["y", "yes", "sure"]
                                then do
                                    printLines text
                                    return ()
                                else do
                                    printLines ["Accuse someone else then"]
                                    gameLoop gameState
                        else gameLoop gameState
                _ -> do
                    gameLoop gameState


    

main = do
    printIntroduction
    printInstructions

    gameState <- initializeGame
    let text = look gameState
    printLines text
    gameLoop gameState

