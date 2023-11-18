-- The germ of a text adventure game
-- Marcin Szlenk 2022

module Main where

import System.IO (hFlush, stdout)
import Places

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

instructionsText = [
    "Available commands are:",
    "",
    "instructions  -- to see these instructions.",
    "debug         -- to see debug instructions.",
    "look          -- to look around the room.",
    "go to [Room]  -- to go to one of avaiable rooms.",
    "quit          -- to end the game and quit.",
    ""
    ]

debugInstructionsText = [
    "Debug commands are:",
    "",
    "visited       -- to see all visited places",
    ""
    ]

data GameState = GameState { currentRoom :: Room, visitedRooms :: [Room] }

initializeGame :: GameState
initializeGame = GameState { currentRoom = Hall, visitedRooms = [] }

-- print strings from list in separate lines
printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)
                  
printIntroduction = printLines introductionText
printInstructions = printLines instructionsText

readCommand :: IO String
readCommand = do
    putStr "> "
    hFlush stdout -- Flush the output buffer to immediately display the prompt
    getLine

goTo :: String -> GameState -> IO GameState
goTo roomStr gameState = do
    let current = currentRoom gameState
    let maybeRoom = stringToRoom roomStr
    case maybeRoom of
        Just destinationRoom -> do
            newRoom <- moveToRoom current destinationRoom
            let newGameState = gameState { currentRoom = newRoom }
            if newRoom /= current
                then do
                    let roomDescription = getRoomDescription newRoom (visitedRooms gameState)
                    let updatedVisitedRooms = if destinationRoom `elem` visitedRooms gameState
                                                then visitedRooms gameState
                                                else destinationRoom : visitedRooms gameState
                    let updatedGameState = newGameState { visitedRooms = updatedVisitedRooms }
                    printLines roomDescription
                    printRoomsConnectedTo newRoom
                    return updatedGameState
                else do
                    return newGameState
        Nothing -> do
            putStrLn "Invalid room name."
            return gameState
    
gameLoop :: GameState -> IO ()
gameLoop gameState = do
    cmd <- readCommand
    case words cmd of
        ["instructions"] -> do 
            printInstructions
            gameLoop gameState
        ["debug"] -> do 
            printLines debugInstructionsText
            gameLoop gameState
        ["look"] -> do 
            let current = currentRoom gameState
            let roomDescription = getLongDescription current
            printLines roomDescription
            gameLoop gameState
        ["go", "to", roomStr] -> do
            newGameState <- goTo roomStr gameState
            gameLoop newGameState
        ["visited"] -> do
            let visited = visitedRooms gameState
            putStrLn $ show visited
            gameLoop gameState
        ["quit"] -> return ()
        _ -> do 
            printLines ["Unknown command.", ""]
            gameLoop gameState

main = do
    printIntroduction
    printInstructions
    gameLoop initializeGame

