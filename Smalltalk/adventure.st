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
        roomStack inventory talkingTo |

    GameState class >> new [
        | gs |
        gs := super new.
        gs init.
        ^gs
    ]

    init [
        currentLocation := nil.
        visitedLocations := Set new.
        roomStack := OrderedCollection new.
        inventory := Set new.
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

    timeExceeded [
        ^(DateTime now > endTime)
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

    goTo: newLocation isInitMove: initMove isTp: tp moveBack: isBack [
        "TODO: getConnectedLocations nie powinno zwracać listy stringów, a listę klas (lub obiektów za pomocą singleton)"
        | canMove visited message classSymbol previousLocation | 
        message := OrderedCollection new.
        (initMove or: [tp]) ifFalse: [ 
            canMove := false.
            "TODO: Można napisać isConnectedTo"
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
            "TODO: ten check mozna bez sprawdzania nazwy"
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

            initMove ifFalse: [ 
                message add: ''; add: 'You moved from ', previousLocation getName, ' to ', currentLocation getName, '.'; add: ''.
            ].
            
            isBack ifFalse: [roomStack addLast: previousLocation.].

            message add: currentLocation visit; add: ''.
        
            currentLocation getCharacter ifNotNil: [
                message add: 'Here you can talk to ', currentLocation getCharacter getName, '.'; add: ''.
            ].

            (currentLocation getPlaces notEmpty) ifTrue: [
                message add: 'You can examine following places: ', currentLocation placesToString, '.'; add: ''.
            ]. 

            message add: 'From ', currentLocation getName, ' you can go to ', currentLocation getConnectedLocations toString, '.'; add: ''.

        ]

        ifFalse: [
            message add: ''; add: 'You can’t move from ', currentLocation getName, ' to ', newLocation, '.'; add: ''.
        ].

        ^message.
    ]

    
    talkTo: character [
        "TODO: metoda talkTo w klasie GameCharacter"
        | message |
        message := OrderedCollection new.
        message add: ''.

        "TODO: metoda getAllCharacters w klasise GameCharacter"
        GameCharacter subclasses do: [:sub |
            sub name asString = character
                ifTrue: [
                    (currentLocation getCharacter) ifNotNil: [ :characterObj |
                        (characterObj getName = character)
                            ifTrue: [
                                message add: (characterObj getCharacterText).
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
            examinedPlace := placeInstance.
            message := placeInstance examinePlace.
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
                    toolPartFound ifFalse: [ inventory add: itemTaken. ]
                ].
                ((itemTaken class ~= ToolPart) and: [itemTaken class ~= Mushroom])
                    ifTrue: [ inventory add: itemTaken. ]
            ].
        ].
        ^message
    ]

    "TODO: inventory jako osobna klasa"
    
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
        ((itemSymbol = Dirt) and: [currentLocation class = RoyalBedroom]) ifTrue: [
            (inventory includes: (itemSymbol getInstance)) ifTrue: [
                Butler getInstance setDistracted: true.
                inventory remove: itemSymbol getInstance.
                message add: 'You unnoticedly drop some dirt on the floor.'; 
                add: '’Someone spread dirt all over the floor’, you yell to the Butler.';
                add: '’Clean it up before king notices!’'; add: ''.

            ] ifFalse: [
                message add: 'You don’t have Dirt in inventory.'; add: ''.
            ]
        ] ifFalse: [
            message add: 'You can’t drop this item here.'; add: ''
        ].
        ^message
        "TODO: message jako pole klasy game"
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
                message add: (Wizard getInstance giveMushrooms: mushroomsAmount); add: ''.
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
        | inventoryMapped message |
        inventoryMapped := inventory collect: [ :each | each class ].
        ^inventoryMapped includes: item
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
                    message add: objectInstance visit; add: ''.
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
]

"Adventure"

Object subclass: Game [
    | instructions isGameOver |


    introductionText := #(
        ''
        '===================================================='
        '    Mystery of a missing Diamond'
        '===================================================='
        ''
        'As a detective you''ve been entrusted with'
        'solving the mystery of a missing diamond.'
        'To crack the case you''ll need to uncover'
        'scattered evidence hidden throughout the castle.'
        ''
        'Gather three pieces of evidence against'
        'a suspect to uncover the thief''s identity.'
        'Use the command accuse [Character]'
        'to make your accusation.'
        'A correct accusation leads to victory!'
    ).

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
        isGameOver := false
    ]

    "Print strings from array in separate lines."
    printLines: anArray [
        anArray do: [ :string | (string isNil) ifFalse: [ Transcript show: string; cr ]]
    ]

    printIntroduction [
       self printLines: introductionText.
    ]

    printInstructions [
        self printLines: instructions.
    ]

    readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine
    ]

    checkAccusation: accusation [
        ^(accusation = GameCharacter getThief getName).
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
                Transcript cr; show: 'You win!'; cr; cr.
            ] ifFalse: [
                Transcript cr; show: 'You lose!'; cr; cr.
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

            cmd = 'quit' ifTrue: [
                isGameOver := true.
                isUnknown := false
            ].
            
            cmd = 'gen' ifTrue: [
                | numbers |
                numbers := self generateNumbers: 5 summingTo: 10.
                Transcript cr.
                numbers displayLines.
                Transcript cr.
                isUnknown := false
            ].
            
            cmd = 'diamond' ifTrue: [
                | diamond  suspect |
                diamond := StolenDiamond getInstance.
                suspect := diamond getSuspect.
                diamond identityHash printOn: Transcript.
                Transcript cr.
                Transcript show: suspect getCharacterText; cr.
                isUnknown := false
            ].
    
            cmd = 'ladle' ifTrue: [
                | ladle1 ladle2 ladle3 |
                ladle1 := Ladle getInstance.
                ladle2 := Ladle getInstance.
                " ladle3 := Ladle new. "
                Ladle allInstances displayLines.
                " Transcript cr.
                Transcript show: suspect getCharacterText; cr. "
                isUnknown := false
            ].
 
            cmd = 'part' ifTrue: [
                | part1 part2 part3 part4 |
                part1 := ToolPart new. 
                part2 := ToolPart new. 
                " part2 := ToolPart getInstance.  "
                ToolPart allInstances displayLines.

                " part4 := ToolPart getInstance.  "

                isUnknown := false
            ].
            cmd = 'shroom' ifTrue: [
                1 to: 5 do: [ :i |
                    Mushroom new.
                ].
                Mushroom allInstances displayLines.
                " part4 := ToolPart getInstance.  "

                isUnknown := false
            ].

            cmd = 'vdoor' ifTrue: [
                | place item |
                place := VaultDoor getInstance.
                Transcript show: place getDescription; cr.
                item := place getItem.
                Transcript show: item getDescription; cr.
                (item getItemAmount) printOn: Transcript.
                Transcript cr.
                " part4 := ToolPart getInstance.  "

                isUnknown := false
            ].

            cmd = 'butler' ifTrue: [
                
                Butler getInstance examine.

                isUnknown := false
            ].
              cmd = 'king' ifTrue: [
                
                King getInstance examine.

                isUnknown := false
            ].


            cmd = 'suspects' ifTrue: [
                Clue getAllClues do: [ :clue |
                    Transcript show: clue getName, ' - ', clue getSuspect getName; cr.
                ].
                " part4 := ToolPart getInstance.  "

                isUnknown := false
            ].

            cmd = 'clues' ifTrue: [
                | suspects orderedSuspects |
                Transcript cr.
                suspects := GameCharacter getSuspects.
                orderedSuspects := suspects asSortedCollection: [:a :b | a getClues size > b getClues size].
                orderedSuspects do: [ :suspect |
                    Transcript show: suspect getName, ': ', suspect getClues toString; cr.
                ].
                Transcript cr.
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
 
            cmd = 'places' ifTrue: [
                Place getAllPlaces displayLines.
                Chest getInstance examine.
                ButlerChest getInstance examine.
                GardenerChest getInstance examine.
                CookChest getInstance examine.
                DirtMound getInstance examine.
                Chest getAllChests displayLines.
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
                Item subclasses do: [ :each | 
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

            cmd = 'split' ifTrue: [
                | text |
                text := 'When you enter the kitchen as a knight, the cook seems surprised by your presence. He puts down the knife he was sharpening and shifts the egg from one hand to the other, suddenly becoming more careful.'.
                Transcript cr; show: (self splitString: text at: 80); cr.
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
