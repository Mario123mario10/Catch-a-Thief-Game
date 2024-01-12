"The germ of a text adventure game
Marcin Szlenk 2022"

"Game class

Responsibilities:
* print/read outputs/inputs
* handle game loop
* handle 'instructions' and 'halt' commands

Collaborators:
* ..."

Object extend [
    splitString: aString at: aNumber [
        | stream newString paragraphs doubleNewline uniqueChar |
        uniqueChar := '_'.
        newString := aString copyReplaceAll: 
            (String with: Character lf with: Character lf) with: uniqueChar.

        paragraphs := newString substrings: uniqueChar.
        stream := WriteStream on: (String new: aString size).
        
        paragraphs do: [ :par |
           | words line |
            words := par subStrings.
            line := ''.
            words do: [:each |
                | tempLine |
                tempLine := line, each, ' '.
                (tempLine size <= aNumber)
                ifTrue: [line := tempLine]
                ifFalse: [
                    stream nextPutAll: line; nl. 
                    line := each, ' '
                    ]
                ].
            stream nextPutAll: line; nl; nl.
        ].
        ^stream contents trimSeparators
    ]
]


Collection extend [
    shuffle [
        | n k r |
        n := self size.
        r := Random new seed: Time millisecondClockValue + (Random between: 1 and: 100) * 1000.
        [ n > 1 ] whileTrue: [
            k := r between: 1 and: n.
            self swap: n with: k.
            n := n - 1
        ].
        ^self
    ]

    toString [
        | string |
        string := ''.
        self do: [:each | 
            [ string := string, each asString, ', ' ]
                on: Exception
                do: [ :ex | string := string, each getName, ', ' ]
        ].
        string := string trimSeparators.
        self isEmpty ifFalse: [
            string := string allButLast
        ].
        ^string
    ]
].

"GameState"

Object subclass: GameState [
    | startTime endTime currentLocation visitedLocations examinedPlace 
        roomStack inventory talkingTo evidenceDict toolHandleOk toolPartsOk |

    GameState class >> new [
        | gs |
        gs := super new.
        gs init.
        ^gs
    ]

    init [
        toolHandleOk := false.
        toolPartsOk := false.
        currentLocation := nil.
        visitedLocations := Set new.
        roomStack := OrderedCollection new.
        inventory := Set new.
        evidenceDict := Dictionary new.
    ]

    start [        
        GameCharacter assignClues.
        Place assignItems.
        startTime := DateTime now.
        endTime := startTime + (Duration days: 0 hours: 0 minutes: 25 seconds: 0).
        ^self goTo: 'Hall' isInitMove: true isTp: false moveBack: false.
    ]

    getCurrentLocation [
        ^currentLocation
    ]

    getInventory [
        ^inventory
    ]

    getEvidence [
        ^evidenceDict
    ]

    timeExceeded [
        ^(DateTime now > endTime)
    ]

    getVisitedLocations [
        ^visitedLocations
    ]

    remainingTime [
        | remaining |
        remaining := (endTime - DateTime now).
        ^('You have ', remaining minutes asString, ' minutes and ',
            remaining seconds asString, ' seconds left.')
    ]

    back [
        | lastLocation |
        lastLocation := roomStack last.
        lastLocation ifNotNil: [
            roomStack removeLast.
            ^self goTo: (lastLocation getName) isInitMove: false isTp: false moveBack: true.
        ] ifNil: [^#('' 'No room history.' '' )]
    ]

    canToolBeCrafted [ 

        (self isInInventory: ToolPart) ifTrue: [
            ((self getFromInventory: ToolPart) getItemAmount = 2) ifTrue: [ 
                toolPartsOk := true 
            ]
        ].
        (self isInInventory: ToolHandle) ifTrue: [
            toolHandleOk := true
        ].
        ^(toolHandleOk & toolPartsOk) 
    ]

    goTo: newLocation isInitMove: initMove isTp: tp moveBack: isBack [
        | canMove allLocationsAsStrings visited message classSymbol previousLocation | 
        message := OrderedCollection new.
        allLocationsAsStrings := Location subclasses collect: [ :each | each name asString ].
        (allLocationsAsStrings includes: newLocation) ifTrue: [
            (initMove | tp | isBack) ifFalse: [ 
                canMove := false.
                currentLocation getConnectedLocations do: [ :location |
                    (location = newLocation)
                        ifTrue: [
                            canMove := true.
                        ]
                ]
            ] ifTrue: [ canMove := true ].

            canMove ifTrue: [
                visited := false.
                previousLocation := currentLocation.
                visitedLocations do: [ :location | 
                    (location getName = newLocation)
                        ifTrue: [ 
                            visited := true.
                            currentLocation := location.
                        ]
                ].
                visited ifFalse: [
                    classSymbol := Smalltalk at: newLocation asSymbol.
                    currentLocation := classSymbol getInstance.
                    visitedLocations add: currentLocation.
                ].
                
                talkingTo := nil.
                examinedPlace := nil.

                initMove ifFalse: [ 
                    message add: ''; add: 'You moved from ', previousLocation getName, ' to ', currentLocation getName, '.'; add: ''.
                ].
                
                isBack ifFalse: [roomStack addLast: previousLocation.].

                message addAll: currentLocation visit.
            ]

            ifFalse: [
                message add: ''; add: 'You can’t move from ', currentLocation getName, ' to ', newLocation, '.'; add: ''.
            ].
        ]
        ifFalse: [
            message add: ''; add: 'Invalid location name.'; add: ''.
        ].
        ^message.
    ]

    wizardGotMushrooms [ ^Wizard getInstance hasMushrooms ]

    talkTo: character [
        | message |
        message := OrderedCollection new.
        message add: ''.

        GameCharacter allSubclasses do: [:sub |
            sub name asString = character
                ifTrue: [
                    (currentLocation getCharacter) ifNotNil: [ :characterObj |
                        (characterObj getName = character)
                            ifTrue: [
                                message add: (characterObj talkToAtState: self).
                                talkingTo := characterObj.
                            ]
                            ifFalse: [message add: 'You can’t talk to him right now']
                    ] ifNil: [message add: 'You can’t talk to him right now'].
                    message add: ''.
                    ^message.
                ]
        ].
        message add: 'Invalid character name.'.
        message add: ''.
        ^message
    ]

    askAbout: clue [
        | message clueSymbol |
        message := OrderedCollection new.
        message add: ''.
        [ clueSymbol := Smalltalk at: clue asSymbol. ]
            on: Exception
            do: [:ex | message add: 'Invalid evidence.'; add: ''. ^message ].
        (clueSymbol superclass = Clue) ifTrue: [
            talkingTo ifNil: [
                message add: 'Who are you talking to? You must first talk to a Character before you can ask 
him about anything.'; add: ''.
                ^message
            ] ifNotNil: [
                (self gotEvidenceOf: clueSymbol) ifFalse: [
                    message add: '’You are asking wrong person, that’s for sure!’ said the ', talkingTo getName, '.'; 
                    add: ''. ^message
                ] ifTrue: [
                    (talkingTo isKindOf: Suspect) ifTrue: [
                        (clueSymbol getInstance getSuspect = talkingTo) ifTrue: [
                            message add: (talkingTo getEvidenceTextFor: clueSymbol); add: ''
                        ] ifFalse: [
                            message add: (talkingTo getEvidenceTextFor: False); add: ''
                        ]
                    ] ifFalse: [
                        message add: 'I don’t know much about it. Did you find the thief?'; add: ''
                    ]
                ]
            ]
        ] ifFalse: [ message add: 'Invalid evidence.'; add: ''. ^message ].

        ^message
    ]

    examine: place [
        | placeSymbol placeInstance message |
        message := OrderedCollection new.
        message add: ''.
        [ placeSymbol := Smalltalk at: place asSymbol. ]
            on: Exception
            do: [:ex | message add: 'Invalid place name.'; add: ''. ^message ].
        (Place getAllPlaces includes: placeSymbol) ifFalse: [
            message add: 'Invalid place name.'; add: ''. 
            ^message
        ].
        placeInstance := placeSymbol getInstance.
        (placeInstance getPlaceLocation = currentLocation) ifTrue: [
            [ 
                (currentLocation isLocked) ifTrue: [
                    message add: currentLocation getName, ' is locked now, you can’t examine anything.'; add: ''.
                    ^message
                ]
            ]
                on: Exception
                do: [:ex | "Do nothing if isLocked method does not exist in subclass" ].
            examinedPlace ifNotNil: [
                message add: 'You stopped examining ', examinedPlace getName, '.'; add: ''
            ].
            examinedPlace := placeInstance.
            message addAll: placeInstance examinePlace.
        ] ifFalse: [
            message add: 'You can’t examine ', place, ' right now.'; add: ''
        ].
        ^message.
    ]

    take: item [
        | message itemSymbol itemTaken |
        message := OrderedCollection new.
        message add: ''.
        [ itemSymbol := Smalltalk at: item asSymbol. ]
            on: Exception
            do: [ :ex | 
                message add: 'Invalid item name.'; add: ''.
                ^message
            ].
        examinedPlace ifNil: [
            message add: 'You must first examine a Place.'; add: ''.

        ] ifNotNil: [
            itemTaken := (examinedPlace takeItem: itemSymbol).
            itemTaken ifNil: [
                message add: 'You can’t take ', item, ' from here.'; add: ''
            ] ifNotNil: [
                | basicItem |
                basicItem := true.

                (itemTaken class = ButlersKeys) ifTrue: [
                    Butler getInstance isDistracted ifFalse: [
                        message add: 'You can’t take ButlersKeys while the Butler is watching carefully. You should
distract thisfanatic cleaner somehow.'; add: ''.
                        ^message
                    ]
                ].
                
                message add: 'You took ', itemTaken getItemAmount asString, ' ', 
                    itemTaken getName, '.'; add: ''; add: itemTaken getDescription; add: ''.
                (itemTaken class = Mushroom) ifTrue: [
                    | mushroomsAmount mushroomFound |
                    mushroomFound := false.
                    mushroomsAmount := itemTaken getItemAmount.
                    inventory do: [ :inventoryItem |
                        (inventoryItem class = Mushroom) ifTrue: [
                            mushroomFound := true.
                            inventoryItem setItemAmount: inventoryItem getItemAmount + mushroomsAmount
                        ]
                    ].
                    basicItem := false.
                    mushroomFound ifFalse: [ inventory add: itemTaken. ]
                ].

                (itemTaken class = ToolPart) ifTrue: [
                    | toolPartAmount toolPartFound |
                    toolPartFound := false.
                    toolPartAmount := itemTaken getItemAmount.
                    inventory do: [ :inventoryItem |
                        (inventoryItem class = ToolPart) ifTrue: [
                            toolPartFound := true.
                            inventoryItem setItemAmount: inventoryItem getItemAmount + toolPartAmount
                        ]
                    ].
                    basicItem := false.
                    toolPartFound ifFalse: [ inventory add: itemTaken. ]
                ].
                
                basicItem ifTrue: [ inventory add: itemTaken. ].

                (itemTaken class superclass = Evidence) ifTrue: [
                    self addEvidenceOf: (itemTaken getConnectedClue) of: (itemTaken whoIsGuilty).
                ].
                ((itemTaken isMemberOf: ToolPart) | (itemTaken isMemberOf: ToolHandle)) ifTrue: [
                    self canToolBeCrafted ifTrue: [
                        | newInventory craftToolResult tool |
                        newInventory := inventory select: [ :each | 
                            ((each isMemberOf: ToolPart) | (each isMemberOf: ToolHandle)) not
                        ].
                        craftToolResult := Item craftTool.
                        tool := craftToolResult at: 1.
                        newInventory add: tool.
                        message addAll: (craftToolResult at: 2).
                        inventory := newInventory.
                        self addEvidenceOf: (tool getConnectedClue) of: (tool whoIsGuilty).
                    ].
                ].
            ].
        ].
        ^message
    ]

    addEvidenceOf: clue of: guilty [
        evidenceDict at: clue put: guilty.
    ]

    gotEvidenceOf: clue [
        | evidenceMapped |
        evidenceMapped := evidenceDict keys collect: [ :each | each class ].
        ^evidenceMapped includes: clue
    ]

    drop: item [
        | message itemSymbol |
        message := OrderedCollection new.
        message add: ''.
        [ itemSymbol := Smalltalk at: item asSymbol. ]
            on: Exception
            do: [ :ex | 
                message add: 'Invalid item name.'; add: ''.
                ^message
            ].
        (itemSymbol superclass = Item) ifFalse: [
            message add: 'Invalid item name.'; add: ''.
            ^message
        ].
        ((itemSymbol = Dirt) and: [currentLocation class = RoyalBedroom]) ifTrue: [
            (inventory includes: (itemSymbol getInstance)) ifTrue: [
                Butler getInstance setDistracted: true.
                itemSymbol getInstance setItemAmount: (itemSymbol getInstance getItemAmount - 1).
                message add: 
'You unnoticedly drop some dirt on the floor. ’Someone spread dirt all over the 
floor’, you yell to the Butler. ’Clean it up before king notices!’'; add: ''.

            ] ifFalse: [
                message add: 'You don’t have Dirt in inventory.'; add: ''.
            ]
        ] ifFalse: [
            message add: 'You can’t drop this item here.'; add: ''
        ].
        ^message
    ]
    

    give: item [
        | inventoryMapped message itemSymbol |
        inventoryMapped := inventory collect: [ :each | each class ].
        message := OrderedCollection new.
        message add: ''.
        [ itemSymbol := Smalltalk at: item asSymbol. ]
            on: Exception
            do: [ :ex | 
                message add: 'Invalid item name.'; add: ''.
                ^message
            ].
        talkingTo ifNil: [
            message add: 'Who are you talking to? You must first talk to a Character before you can give 
him anything.'; add: ''.
        ] ifNotNil: [
            ((itemSymbol = Mushroom) and: [talkingTo class = Wizard]) ifTrue: [
                | mushroomsItem mushroomsAmount |
                (inventoryMapped includes: Mushroom) ifTrue: [
                    mushroomsItem := (inventory select: [ :each | each class = Mushroom ]) anyOne.
                    mushroomsAmount := mushroomsItem getItemAmount.
                    (mushroomsAmount = 10) ifTrue: [ message add: 'You gave Wizard 10 mushrooms.'; add: '' ].
                ] ifFalse: [
                    mushroomsAmount := 0.
                ].
                message add: (Wizard getInstance giveMushrooms: mushroomsAmount atState: self); add: ''.
            ] ifFalse: [
                (inventoryMapped includes: itemSymbol) ifTrue: [
                    message add: 'I don’t need it. Stop bothering me!'; add: ''
                ] ifFalse: [
                    message add: 'You don’t have this item.'; add: ''
                ]
            ]
        ].
        ^message
    ]

    isInInventory: item [
        | inventoryMapped |
        inventoryMapped := inventory collect: [ :each | each class ].
        ^inventoryMapped includes: item
    ]

    getFromInventory: item [
        (self isInInventory: item) ifTrue: [ 
            inventory select: [ :each | each class = item ]
        ] ifFalse: [ ^nil ].
        ^inventory anyOne
    ]


    unlock: gameObject [
        | message objectSymbol objectInstance |
        message := OrderedCollection new.
        message add: ''.
        [ objectSymbol := Smalltalk at: (gameObject asSymbol). ]
            on: Exception
            do: [ :ex | 
                message add: 'Invalid input. Neither location nor a place.'; add: ''.
                ^message
            ].
        (self isInInventory: ButlersKeys) ifTrue: [
            (objectSymbol superclass = Chest) ifTrue: [
                objectInstance := objectSymbol getInstance.
                (objectInstance getPlaceLocation ~= currentLocation) ifTrue: [
                    message add: 'You can’t unlock ', objectInstance getName, ' from here'; add: ''.
                    ^message
                ].
                objectInstance unlock.
                message add: 'You unlocked ', objectInstance getName, '.'; add: ''.
                ^message
            ].
            (objectSymbol = ServantsHouse) ifTrue: [
                objectInstance := objectSymbol getInstance.
                ((currentLocation ~= Courtyard getInstance) and: [currentLocation ~= ServantsHouse getInstance]) ifTrue: [
                    message add: 'You can’t unlock ', objectInstance getName, ' from here.'; add: ''.
                    ^message
                ]. 
                objectInstance := objectSymbol getInstance.
                objectInstance unlock.
                message add: 'You unlocked ', objectInstance getName, '.'; add: ''.
                (currentLocation = ServantsHouse getInstance) ifTrue: [ 
                    message addAll: objectInstance visit.
                ].
                ^message
            ] ifFalse: [
                | possiblyLockableTypes |
                possiblyLockableTypes := Set new.
                possiblyLockableTypes add: Place; add: Location; add: Item.
                possiblyLockableTypes := possiblyLockableTypes collect: [ :plt | plt name ].
                (possiblyLockableTypes includes: (objectSymbol superclass name)) ifFalse: [
                    message add: 'Invalid input. Neither location nor a place.'; add: ''.
                ] ifTrue: [
                    message add: objectSymbol name asString, ' does not have any lock.'; add: ''.
                ].
                ^message
            ].
        ] ifFalse: [
            message add: 'You don’t have any keys you could use.'; add: ''.
        ].
        ^message
    ]

    spawn: item [
        | currentItemAmount message itemSymbol itemInstance |
        message := OrderedCollection new.
        message add: ''.
        currentItemAmount := 0.
        [ itemSymbol := Smalltalk at: item asSymbol. ]
            on: Exception
            do: [ :ex | 
                message add: 'Invalid item name.'; add: ''.
                ^message
            ].
        (itemSymbol superclass = Item) ifFalse: [
            message add: 'Invalid item name.'; add: ''.
            ^message
        ].
        [ itemInstance := itemSymbol getInstance ]
            on: Exception
            do: [ :ex | itemInstance := itemSymbol new ].
        itemInstance setItemAmount: 1.
        inventory add: itemInstance.
        message add: 'You took ', itemInstance getItemAmount asString, ' ', 
        itemInstance getName, '.'; add: ''; add: itemInstance getDescription; add: ''.        
        ^message.
    ]
]

"Adventure"

Object subclass: Game [
    | instructions introduction devInstructions isGameOver |


    introductionText := 
'
==============================================================================
                       Mystery of a missing Diamond
==============================================================================

As a detective you’ve been entrusted with solving the mystery of a missing 
diamond. To crack the case you’ll need to uncover scattered evidence hidden 
throughout the castle. 

Gather three pieces of evidence against a suspect to uncover the thief’s 
identity. Use the command accuse [Character] to make your accusation. 
A correct accusation leads to victory!
'.

    instructionsText := 
'
Available commands are:

instructions            -- to see these instructions.
time                    -- to see remaining time.
look                    -- to look around the room.
inventory               -- to see what’s currently in inventory.
evidence                -- to see all evidence we know about.

talk to [Character]     -- to start conversation with character.
ask about [Clue]        -- to ask about clue.
give [Item]             -- to give all items of this type to character.
examine [Place]         -- to examine specific place in a room.
take [Item]             -- to take avaiable item to inventory.
drop [Item]             -- to drop an item.
go to [Location]        -- to go to one of avaiable rooms.
back                    -- to go to last room.
unlock [Location/Place] -- to unlock locked location/place with keys
accuse [Character]      -- to character and if correct win the game

quit                    -- to end the game and quit.
dev                     -- to see debug instructions (cheat)
'.
    devInstructionsText :=
'
Debug commands are:

whereami                -- to see current location
visited                 -- to see all visited places
clues                   -- to see characters and their clues
items                   -- to see all places and their items
gamestate               -- to see game state info
spawn [Item]            -- to spawn item into inventory
checktool               -- to check if tool can be created
tp [Room]               -- to teleport to the room
'.
 
    Game class >> new [
        | g |
        g := super new.
        g init.
        ^g
    ]

    init [
        FileStream fileIn: 'clues.st'.
        FileStream fileIn: 'characters.st'.
        FileStream fileIn: 'locations.st'.
        FileStream fileIn: 'places.st'.
        FileStream fileIn: 'items.st'.
        instructions := OrderedCollection new.
        instructions add: instructionsText. 
        devInstructions := OrderedCollection new.
        devInstructions add: devInstructionsText.
        introduction := OrderedCollection new.
        introduction add: introductionText.
        isGameOver := false
    ]

    "Print strings from array in separate lines."
    printLines: anArray [
        anArray do: [ :string | (string isNil) ifFalse: [ Transcript show: string; cr ]]
    ]

    printIntroduction [
       self printLines: introduction.
    ]

    printInstructions [
        self printLines: instructions.
    ]

    printDevInstructions [
        self printLines: devInstructions.
    ]

    readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine
    ]

    checkAccusation: accusation [
        ^(accusation = Suspect getThief getName).
    ]

    separateCommandFromArgument: input with: command [
        | argumentIndex argument |
        argumentIndex := command size + 1.
        argument := input copyFrom: argumentIndex to: input size.
        argument := argument trimSeparators.
        ^argument
    ]

    accuse: input [
        | cmd accusation yesAnswers |
        accusation := self separateCommandFromArgument: input with: 'accuse'.
        Transcript cr; show: 'Are you sure? (Y/N)'; cr; cr.
        cmd := self readCommand.
        yesAnswers := #('Y' 'y' 'yes' 'sure').
        (yesAnswers includes: cmd) ifTrue: [
            | result |
            result := self checkAccusation: accusation.
            result ifTrue: [
                Transcript cr; show: 'You win! 👍'; cr; cr.
            ] ifFalse: [
                Transcript cr; show: 'You lose! 😢'; cr; cr.
            ].
            isGameOver := true
        ] ifFalse: [
            Transcript show: 'Take your time!'; cr
        ].
    ]

    "Run the game."
    run [
        | cmd isUnknown gameState |

        self printIntroduction.
        self printInstructions.

        gameState := GameState new.
        self printLines: (gameState start).
        "Game loop."
        [isGameOver or: [gameState timeExceeded]] whileFalse: [

            isUnknown := true.
            cmd := self readCommand.

            cmd = 'instructions' ifTrue: [
                self printInstructions.
                isUnknown := false
            ].

            cmd = 'dev' ifTrue: [
                self printDevInstructions.
                isUnknown := false
            ].

            cmd = 'quit' ifTrue: [
                isGameOver := true.
                isUnknown := false
            ].

            cmd = 'clues' ifTrue: [
                | suspects orderedSuspects |
                Transcript cr.
                suspects := Suspect getSuspects.
                orderedSuspects := suspects asSortedCollection: [ :a :b | a getClues size > b getClues size].
                orderedSuspects do: [ :suspect |
                    Transcript show: suspect getName, ': ', suspect getClues toString; cr.
                ].
                Transcript cr.
                isUnknown := false
            ].

            cmd = 'evidence' ifTrue: [
                | message |
                message := OrderedCollection new.
                message add: ''.
                gameState getEvidence associationsDo: [ :evidence |
                    | guiltyAsString |
                    evidence value 
                        ifNil: [ guiltyAsString := 'Unknown suspect']
                        ifNotNil: [ guiltyAsString := evidence value getName].
                    message add: evidence key getName, ' - ', guiltyAsString.
                ].
                (gameState getEvidence isEmpty) ifTrue: [
                    message add: 'You have no evidence in the case.'
                ].
                message add: ''.
                self printLines: message.
                isUnknown := false
            ].

            (cmd startsWith: 'accuse') ifTrue: [
                self accuse: cmd.
                isUnknown := false
            ].

            cmd = 'time' ifTrue: [
                Transcript cr; show: gameState remainingTime; cr; cr.
                isUnknown := false
            ].
            
            
            cmd = 'back' ifTrue: [
                self printLines: gameState back.
                isUnknown := false
            ].

            cmd = 'checktool' ifTrue: [
                | message |
                message := OrderedCollection new.
                message add: ''; add: gameState canToolBeCrafted displayString; add: ''.
                self printLines: message.
                isUnknown := false
            ].

            (cmd startsWith: 'examine') ifTrue: [
                | message place |
                place := self separateCommandFromArgument: cmd with: 'examine'.
                message := gameState examine: place.
                self printLines: message.
                isUnknown := false
            ].

            cmd = 'items' ifTrue: [
                | items message chain |
                items := OrderedCollection new.
                Item allSubclasses do: [ :each | 
                    each allInstances do: [ :instance | 
                        items add: instance
                    ]
                ].
                chain := ''.
                items do: [ :item | 
                    (item getItemPlace) ifNotNil: [
                        chain := chain, item getItemPlace getName, ' - ', item getName, ': ',
                            item getItemAmount asString, (Character lf asString)
                    ]
                ].

                chain := chain trimSeparators.

                message := OrderedCollection new.
                message add: ''; add: chain; add: ''.

                self printLines: message.

                isUnknown := false
            ].

            cmd = 'inventory' ifTrue: [
                | message |
                message := OrderedCollection new.
                message add: ''.
                gameState getInventory do: [ :item | 
                    message add: item getName, ': ', item getItemAmount asString.
                ].
                (gameState getInventory size = 0) ifTrue: [ 
                    message add: 'The inventory is empty.'
                ].
                message add: ''.
                self printLines: message.
                isUnknown := false
            ].

            cmd = 'look' ifTrue: [
                | message |
                message := OrderedCollection new.
                message add: ''; addAll: (gameState getCurrentLocation getFullLocationDescription: true).
                self printLines: message.
                isUnknown := false
            ].

            cmd = 'whereami' ifTrue: [
                | message |
                message := OrderedCollection new.
                message add: ''; add: gameState getCurrentLocation getName; add: ''.
                self printLines: message.
                isUnknown := false
            ]. 
            
            cmd = 'visited' ifTrue: [
                | message |
                message := OrderedCollection new.
                message add: ''; add: 'Locations you visited: ', gameState getVisitedLocations toString; add: ''.
                self printLines: message.
                isUnknown := false
            ].

            (cmd startsWith: 'ask about') ifTrue: [
                | message clue |
                message := OrderedCollection new.
                clue := self separateCommandFromArgument: cmd with: 'ask about'.
                message addAll: (gameState askAbout: clue).
                self printLines: message.
                isUnknown := false
            ].

            (cmd startsWith: 'take') ifTrue: [
                | message item |
                message := OrderedCollection new.
                item := self separateCommandFromArgument: cmd with: 'take'.
                message addAll: (gameState take: item).
                self printLines: message.
                isUnknown := false
            ].

            (cmd startsWith: 'drop') ifTrue: [
                | message item |
                message := OrderedCollection new.
                item := self separateCommandFromArgument: cmd with: 'drop'.
                message addAll: (gameState drop: item).
                self printLines: message.
                isUnknown := false
            ].

            (cmd startsWith: 'give') ifTrue: [
                | message item |
                message := OrderedCollection new.
                item := self separateCommandFromArgument: cmd with: 'give'.
                message addAll: (gameState give: item).
                self printLines: message.
                isUnknown := false
            ].
 
            (cmd startsWith: 'unlock') ifTrue: [
                | message object |
                message := OrderedCollection new.
                object := self separateCommandFromArgument: cmd with: 'unlock'.
                message addAll: (gameState unlock: object).
                self printLines: message.
                isUnknown := false
            ].

            (cmd startsWith: 'spawn') ifTrue: [
                | message item |
                message := OrderedCollection new.
                item := self separateCommandFromArgument: cmd with: 'spawn'.
                message addAll: (gameState spawn: item).
                self printLines: message.
                isUnknown := false
            ].

            (cmd startsWith: 'talk to') ifTrue: [
                | message character |
                message := OrderedCollection new.
                character := self separateCommandFromArgument: cmd with: 'talk to'.
                message addAll: (gameState talkTo: character).
                self printLines: message.
                isUnknown := false
            ].

            (cmd startsWith: 'go to') ifTrue: [
                | message newLocation |
                message := OrderedCollection new.
                newLocation := self separateCommandFromArgument: cmd with: 'go to'.
                message addAll: (gameState goTo: newLocation isInitMove: false isTp: false moveBack: false).
                self printLines: message.
                isUnknown := false
            ].

            (cmd startsWith: 'tp') ifTrue: [
                | message newLocation |
                message := OrderedCollection new.
                newLocation := self separateCommandFromArgument: cmd with: 'tp'.
                message addAll: (gameState goTo: newLocation isInitMove: false isTp: true moveBack: false).
                self printLines: message.
                isUnknown := false
            ].

            isUnknown ifTrue: [
                self printLines: #('' 'Unknown command.' '')
            ]
        ].
        [isGameOver] whileFalse: [
            Transcript show: 'You have ran out of time. You must accuse someone.'; cr; cr.
            cmd := self readCommand.
            self accuse: cmd.
        ]
    ]
]

Game new run.
