-- The germ of a text adventure game
-- Marcin Szlenk 2022

module Main where

import System.IO (hFlush, stdout)
import Data.Map (Map)
import qualified Data.Map as Map

import Printing
import Instructions
import Places
import Characters
import Inventory
import World

introductionText = [
    "A long time ago, in a galaxy far, far away...",
    "",
    "It is a period of civil war. Rebel",
    "spaceships, striking from a hidden",
    "base, have won their first victory",
    "against the evil Galactic Empire.",
    "",
    "During the battle, Rebel spies managed",
    "to steal secret plans to the Empire's",
    "ultimate weapon, the Death Star, an",
    "armored space station with enough",
    "power to destroy an entire planet.",
    "",
    "Pursued by the Empire's sinister agents,",
    "Princess Leia races home aboard her",
    "starship, custodian of the stolen plans",
    "that can save her people and restore",
    "freedom to the galaxy....",
    ""
    ]

printIntroduction = printLinesWithoutSplit introductionText
printInstructions = printLinesWithoutSplit instructionsText

initializeGame :: IO GameState
initializeGame = do
    let startingRoom = Hall
    let initialVisitedRooms = [Hall]
    
    clues <- assignClues
    items <- assignItems clues

    return GameState { 
        currentRoom = startingRoom, 
        visitedRooms = initialVisitedRooms, 
        examining = Nothing,
        talking = Nothing,
        clues = clues, 
        items = items, 
        inventory = [], 
        evidence = Map.empty, 
        gaveMushrooms = False
        }

readCommand :: IO String
readCommand = do
    putStr "> "
    hFlush stdout
    getLine
    
gameLoop :: GameState -> IO ()
gameLoop gameState = do
    cmd <- readCommand
    case words cmd of
        ["instructions"] -> do 
            printInstructions
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
        ["examine", placeStr] -> do 
            newGameState <- examine placeStr gameState
            gameLoop newGameState
        ["take", itemStr] -> do
            newGameState <- takeItem itemStr gameState
            gameLoop newGameState
        ["go", "to", roomStr] -> do
            newGameState <- goTo roomStr gameState
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
        ["quit"] -> return ()
        _ -> do 
            printLines ["Unknown command.", ""]
            gameLoop gameState

main = do
    printIntroduction
    printInstructions

    gameState <- initializeGame
    look gameState
    gameLoop gameState

