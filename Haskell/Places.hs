module Places where

import Data.Char (toLower, isSpace)
import Data.Map (Map)
import qualified Data.Map as Map

import Characters

-----------------------------------
-- Castle Layout
-----------------------------------

data Room = Hall | Corridor | GuardHouse | Courtyard | Vault | Kitchen | RoyalBedroom | Garden | ServantsHouse | WizardsTower | Forest deriving (Eq, Show)

-- Function to convert string to Room (case-insensitive)
stringToRoom :: String -> Maybe Room
stringToRoom str = case map toLower str of
    "hall" -> Just Hall
    "corridor" -> Just Corridor
    "guardhouse" -> Just GuardHouse
    "guardshouse" -> Just GuardHouse
    "guardroom" -> Just GuardHouse
    "guardsroom" -> Just GuardHouse
    "courtyard" -> Just Courtyard
    "yard" -> Just Courtyard
    "vault" -> Just Vault
    "kitchen" -> Just Kitchen
    "cookroom" -> Just Kitchen
    "cooksroom" -> Just Kitchen
    "cookery" -> Just Kitchen
    "royalbedroom" -> Just RoyalBedroom
    "butlerroom" -> Just RoyalBedroom
    "butlersroom" -> Just RoyalBedroom
    "bedroom" -> Just RoyalBedroom
    "garden" -> Just Garden
    "servantshouse" -> Just ServantsHouse
    "servantsroom" -> Just ServantsHouse
    "servanthouse" -> Just ServantsHouse
    "servantroom" -> Just ServantsHouse
    "wizardstower" -> Just WizardsTower
    "wizardshome" -> Just WizardsTower
    "wizardsroom" -> Just WizardsTower
    "wizardtower" -> Just WizardsTower
    "wizardhome" -> Just WizardsTower
    "wizardroom" -> Just WizardsTower
    "forest" -> Just Forest
    _ -> Nothing

data Door = Door Room Room deriving (Eq, Show)

-- List of all rooms in the castle
allRooms :: [Room]
allRooms = [Hall, Corridor, GuardHouse, Courtyard, Vault, Kitchen, RoyalBedroom, Garden, ServantsHouse, WizardsTower, Forest]

-- List of all doors in the castle
allDoors :: [Door]
allDoors = [
    Door Hall Corridor, Door Hall GuardHouse, Door Hall Courtyard, Door Hall Vault,
    Door Corridor Kitchen, Door Corridor RoyalBedroom,
    Door Courtyard Garden, Door Courtyard ServantsHouse, Door Courtyard WizardsTower, Door Courtyard Forest
    ]

-- Function to get doors connected to a specific room
doorsFromRoom :: Room -> [Door]
doorsFromRoom room = filter (\(Door r1 _) -> r1 == room) allDoors

-- Function to check if two rooms are connected by a door
areConnected :: Room -> Room -> Bool
areConnected room1 room2 = any (\(Door r1 r2) -> (r1 == room1 && r2 == room2) || (r1 == room2 && r2 == room1)) allDoors

-- Function to get rooms connected to a specific room
roomsConnectedToRoom :: Room -> [Room]
roomsConnectedToRoom room = map (\(Door r1 r2) -> if r1 == room then r2 else r1) (filter (\(Door r1 r2) -> r1 == room || r2 == room) allDoors)

getCharacterInRoom :: Room -> Maybe Character
getCharacterInRoom room = case room of
    Hall -> Just King
    GuardHouse -> Just Guard
    Kitchen -> Just Cook
    RoyalBedroom -> Just Butler
    Garden -> Just Gardener
    WizardsTower -> Just Wizard
    _ -> Nothing

addToRoomHistroy :: [Room] -> Room -> [Room]
addToRoomHistroy history room = [room] ++ history

popFromRoomHistory :: [Room] -> Maybe (Room, [Room])
popFromRoomHistory [] = Nothing
popFromRoomHistory (x:xs) = Just (x, xs)

-- Function to get short room descriptions
getShortRoomDescription :: Room -> [String]
getShortRoomDescription room = case room of
    Vault -> ["", "Once the place where the diamond was stored, but now empty." ]
    Hall -> ["", "A grand, bustling chamber with a splendid throne at its center." ]
    Kitchen -> ["", "A bustling place filled with the king's dishes and the aroma of cooked meals." ]
    RoyalBedroom -> ["", "A room holding keys to every other room in the castle." ]
    Garden -> ["", "A vast garden filled with flourishing vegetables and vibrant flowers." ]
    GuardHouse -> ["", "A small chamber housing an array of weapons." ]
    WizardsTower -> ["", "An incredible space brimming with curious magical objects." ]
    ServantsHouse -> ["", "Bedrooms allocated for the castle's workforce." ]
    Forest -> ["", "A sprawling forest teeming with wild animals, plants, and mushrooms."  ]
    Courtyard -> ["", "The bustling heart of the castle."  ]
    Corridor -> ["", "A passage connecting the kitchen and royal bedroom, often frequented by the butler."  ]

-- Function to get longer room descriptions
getLongRoomDescription :: Room -> [String]
getLongRoomDescription room = case room of
    Garden -> [
        "",
        "You are in the castle garden. This is the place where the king likes to stay in spring and early summer.",
        "",
        "A trusted gardener makes sure that this place is full of colorful flowers and nutritious vegetables that the chef uses for his flavorful dishes.",
        "",
        "There is a small pond in the center of the garden. One can get to its other side quite easily, as well as to the bottom.",
        "",
        "Inhabitants of the castle enjoy throwing things into the pond from time to time, causing the gardener to be obliged to clean there.",
        "",
        "Sometimes he fishes out amazing treasures…"
        ]
    WizardsTower -> [
        "",
        "Here is the wizard’s house. It’s kind of like the kitchen… but different.",
        "",
        "The wizard prepares various elixirs, magic items here; he even modifies some plants and animals, so that they have miraculous features.",
        "",
        "Hence, there is a fireplace, which allows him to cook elixirs, a small workshop to generate items, collected animal parts and plants and a couple dozen books everywhere. Of course, a wand lies on the table."
        ]
    Forest -> [
        "Shhh… did you hear that sound? It was kind of… magical. This forest is like none other.",
        "",
        "Some say you can hear ghost voices here, others say that this is a place where you can meet a werewolf.",
        "",
        "One thing is for sure - you can find unusual plants and animals here. Maybe they have some special use, who knows?"
        ]
    Hall -> [
        "",
        "You are in the main hall of the castle. You can get to several places from here. Many people move around here too.",
        "",
        "Candle lanterns, even combined with thin beams of light from narrow windows, don’t make this place well lit.",
        "",
        "If you saw anyone in the distance, you could even mistake somebody for somebody, especially at night!"
        ]
    Kitchen -> [
        "",
        "Welcome to the kitchen! The aroma of fresh herbs, cooked vegetables and flavorful meat dishes fills the air.",
        "",
        "Inside this vast room there is a fireplace with a grate, separate fireplace to cook soup, large oven for roasted dishes, and a bread oven. There are kitchen utensils hanging on the wall but one hook is empty…"
        ]
    GuardHouse -> [
        "",
        "You are now in one of the castle guards’ rooms.",
        "",
        "It’s not very spacious, most of the space is taken by a bed and simple wooden wardrobe.",
        "",
        "Reportedly, he keeps some equipment in there. Stark and spartan style of the room suits the guard, without a doubt."
        ]
    RoyalBedroom -> [
        "",
        "You are now in the royal bedroom. You see a spacious room full of expensive furniture and numerous paintings.",
        "",
        "There are huge mirrors and an ornate bed that is probably so expensive that no servant could afford it even if he worked all his life for it.",
        "",
        "This is the place where the butler often stays, serving the king in every way possible."
        ]
    Courtyard -> [
        "",
        "You are in the courtyard. This is the middle of the castle from where you can get to many places."
        ]
    Vault -> [
        "",
        "This is a place of crime! King Alaric III keeps here his most precious treasures, passed down for many generations.",
        "",
        "What a great, imposing room it is! The walls are beautifully adorned, and the floor is made of polished marble.",
        "Plenty of gold bars, gold coins, sparkling jewels, silver artifacts, jewelry, antique weapons, pieces of art and many other valuable (or not, like a wooden shaft near the entrance) items lie and stand in the room.",
        "",
        "The most important one, the apple of king’s eye, ‘the diamond that adorned the crown of his ancestors for generations’, always kept in the center of the room, has been stolen by some petty, lousy thief. Oh, what a tragedy…",
        "",
        "Large, heavy-duty door did not help, since the thief must have had the key, as they were open after the incident.",
        "",
        "Hopefully that person gets caught and justice will be served. But, what are these bloodstains on the floor…"
        ]
    ServantsHouse -> [
        "",
        "You entered the king’s servants room. It's small and cramped, with little privacy.",
        "",
        "The beds are simple and utilitarian, with straw mattresses and rough woolen blankets and in front of the beds are servants’ wooden chests with personal belongings.",
        "",
        "The room is dimly lit with candles, and the air is smoky and filled with the smell of cooking food coming from a nearby kitchen."
        ]
    Corridor -> [
        "",
        "You are walking through the corridor.",
        "",
        "It is a dim passage of stone and and with the flickering glow of torches casting dancing shadows upon the walls.",
        "",
        "A red carpet silences the steps of guards and servants. The ceiling shows paintings of the kingdom’s history."
        ]

-----------------------------------
-- Castle Places
-----------------------------------

data Place = VaultDoor | GardenPond | RoseBushes | KingSculpture | DirtMound | Mirror | Bed | Wardrobe | NightTable | Oven | BagOfFlour | KitchenUnit | ButlerChest | CookChest | GardenerChest | Lakeshore | OldTree | HiddenClearing | MushroomPatch | WildlifeNest deriving (Show, Eq)

instance Ord Place where
    compare a b = compare (show a) (show b)

allPlaces :: [Place]
allPlaces = [VaultDoor, GardenPond, RoseBushes, KingSculpture, Mirror, Bed, Wardrobe, NightTable, Oven, BagOfFlour, KitchenUnit, ButlerChest, CookChest, GardenerChest, Lakeshore, OldTree, HiddenClearing, MushroomPatch, WildlifeNest]

-- Function to convert string to Place (case-insensitive)
stringToPlace :: String -> Maybe Place
stringToPlace str = case map toLower str of
    "vaultdoor" -> Just VaultDoor
    "door" -> Just VaultDoor
    "gardenpond" -> Just GardenPond
    "pond" -> Just GardenPond
    "rosebushes" -> Just RoseBushes
    "bushes" -> Just RoseBushes
    "bush" -> Just RoseBushes
    "roses" -> Just RoseBushes
    "rose" -> Just RoseBushes
    "kingsculpture" -> Just KingSculpture
    "kingculpture" -> Just KingSculpture
    "sculpture" -> Just KingSculpture
    "kingsstatue" -> Just KingSculpture
    "kingstatue" -> Just KingSculpture
    "statue" -> Just KingSculpture
    "dirtmound" -> Just DirtMound
    "dirt" -> Just DirtMound
    "mound" -> Just DirtMound
    "mirror" -> Just Mirror
    "bed" -> Just Bed
    "wardrobe" -> Just Wardrobe
    "nighttable" -> Just NightTable
    "table" -> Just NightTable
    "oven" -> Just Oven
    "bagofflour" -> Just BagOfFlour
    "flour" -> Just BagOfFlour
    "bag" -> Just BagOfFlour
    "kitchenunit" -> Just KitchenUnit
    "butlerchest" -> Just ButlerChest
    "cookchest" -> Just CookChest
    "gardenerchest" -> Just GardenerChest
    "lakeshore" -> Just Lakeshore
    "shore" -> Just Lakeshore
    "lake" -> Just Lakeshore
    "oldtree" -> Just OldTree
    "tree" -> Just OldTree
    "hiddenclearing" -> Just HiddenClearing
    "clearing" -> Just HiddenClearing
    "mushroompatch" -> Just MushroomPatch
    "patch" -> Just MushroomPatch
    "wildlifenest" -> Just WildlifeNest
    "nest" -> Just WildlifeNest
    _ -> Nothing

-- Function returning each Place inside of Room
insideRoom :: Room -> [Place]
insideRoom room = case room of
    Vault -> [VaultDoor]
    Garden -> [GardenPond, RoseBushes, KingSculpture, DirtMound]
    RoyalBedroom -> [Mirror, Bed, Wardrobe, NightTable]
    Kitchen -> [Oven, BagOfFlour, KitchenUnit]
    ServantsHouse -> [ButlerChest, CookChest, GardenerChest]
    Forest -> [Lakeshore, OldTree, HiddenClearing, MushroomPatch, WildlifeNest]
    _ -> []

isPlaceinsideRoom :: Room -> Place -> Bool -> Bool
isPlaceinsideRoom room place isServantsHouseLocked = 
    if room == ServantsHouse && isServantsHouseLocked
        then
            False
        else
            place `elem` insideRoom room

isPlaceLocked :: Map.Map Place Bool -> Place -> Bool
isPlaceLocked arePlacesLocked place =
    case Map.lookup place arePlacesLocked of
        Just unlocked -> unlocked
        Nothing -> False

-- Function checking if Room has Places
areInnerPlaces :: Room -> Bool
areInnerPlaces room = room `elem` [Vault, Garden, RoyalBedroom, Kitchen, ServantsHouse, Forest]

-- Function to check if a Room contains a specific Place
roomContainsPlace :: Room -> Place -> Bool
roomContainsPlace room place = place `elem` insideRoom room

getPlaceDescription :: Place -> [String]
getPlaceDescription place = case place of
    VaultDoor -> ["", "The heavy vault door seems too heavy for one person to open. It looks like someone used some kind of tool to move the door."]    
    GardenPond -> ["", "A tranquil pond in the garden."]
    RoseBushes -> ["", "Beautiful rose bushes in the garden."]
    KingSculpture -> ["", "A sculpture of the king in the garden." ]
    DirtMound -> ["", "A pile of dirt in the garden." ]
    Mirror -> ["", "An ornate mirror hanging on the wall."]
    Bed -> ["", "An elegantly designed bed in the room."]
    Wardrobe -> ["", "A wardrobe for clothes storage." ]
    NightTable -> ["", "Approaching the night table, you notice the butler watching you closely."]
    Oven -> ["", "A large oven for cooking dishes."]
    BagOfFlour -> ["", "A bag of flour used for cooking." ]
    KitchenUnit -> ["", "A unit used for kitchen storage." ]
    ButlerChest -> ["", "A chest belonging to the butler." ]
    CookChest -> ["", "A chest belonging to the cook." ]
    GardenerChest -> ["", "A chest belonging to the gardener." ]
    Lakeshore -> ["", "The serene shore of a tranquil lake."]
    OldTree -> ["", "An ancient, towering tree with sprawling branches."]
    HiddenClearing -> ["", "A secluded clearing, hidden from sight."]
    MushroomPatch -> ["", "A patch of mushrooms, some with vibrant colors."]
    WildlifeNest -> ["", "A nest, possibly home to some wildlife."]

-----------------------------------
-- Misc
-----------------------------------

-- Function to get short or long room descriptions based on visit status
getRoomDescription :: Room -> [Room] -> Bool -> [String]
getRoomDescription current visited isServantsHouseLocked =
    if current == ServantsHouse && isServantsHouseLocked == True
        then
            ["", "You are welcomed by a locked door. You won't get inside without a key. Royal Chamberlains used to carry keys for every lock on a single keychain."]
        else    
            let description = if current `elem` visited
                    then getShortRoomDescription current
                    else getLongRoomDescription current
        
                descriptionWithCharacters = 
                    case getCharacterInRoom current of
                        Just char -> description ++ ["", "Here you can talk to " ++ show char ++ "."]
                        Nothing -> description
            
                places = insideRoom current
                descriptionWithPlaces = if null places
                    then descriptionWithCharacters
                    else descriptionWithCharacters ++ ["", "You can examine the following places: " ++ show places ++ "." ]
                
                connectedRooms = roomsConnectedToRoom current
                descriptionWithRooms = descriptionWithPlaces ++ ["", "From " ++ show current ++ " you can go to: " ++ show connectedRooms ++ "." ]
            in descriptionWithRooms

updateVisitedRooms :: Room -> [Room] -> Bool -> [Room]
updateVisitedRooms current visited isServantsHouseLocked = 
    if current == ServantsHouse && isServantsHouseLocked == True
        then
            visited
        else
            if current `elem` visited
                then visited
                else current : visited