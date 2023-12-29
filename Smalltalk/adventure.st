"The germ of a text adventure game
Marcin Szlenk 2022"

"Game class

Responsibilities:
* print/read outputs/inputs
* handle game loop
* handle 'instructions' and 'halt' commands

Collaborators:
* ..."

Collection extend [
    shuffle [
        | n k r1 r2 |
        n := self size.
        r1 := Random new seed: Time millisecondClockValue.
        r2 := Random new seed: Time millisecondClockValue + (r1 between: 1 and: 100) * 1000.
        [ n > 1 ] whileTrue: [
            k := r2 between: 1 and: n.
            self swap: n with: k.
            n := n - 1
        ].
        ^self
    ]

    toString [
        | string |
        string := ''.
        self do: [:each | string := string, each asString, ', '].
        string := string trimSeparators.
        self isEmpty ifFalse: [
            string := string allButLast
        ].
        ^string
    ]
].

"GameState"

Object subclass: GameState [
    | currentLocation visitedLocations roomHistory |

    GameState class >> new [
        | gs |
        gs := super new.
        gs init.
        ^gs
    ]

    init [
        currentLocation := nil.
        visitedLocations := Set new.
        
    ]

    start [        
        GameCharacter assignClues.
        Place assignItems.
        ^self goTo: 'Hall' isInitMove: true isTp: false.
    ]

    getCurrentLocation [
        ^currentLocation
    ]
    
    goTo: newLocation isInitMove: initMove isTp: tp [
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
                            ifTrue: [message add: (characterObj getCharacterText)]
                            ifFalse: [message add: 'You can’t talk to him right now']
                    ] ifNil: [message add: 'You can’t talk to him right now'].
                    message add: ''.
                    ^message.
                ]
        ].
        message add: 'Invalid character name'.
        message add: ''.
        ^message
    ]

    examine: place [
        | placeSymbol message |
        placeSymbol := Smalltalk at: place asSymbol.
        message := (placeSymbol getInstance) examinePlace.
        ^message.
    ]

    " DO ANALIZY BO LADNIE WYPLULO CALY OBIEKT
    examine: place [
        | placeSymbol message |
        placeSymbol := Smalltalk at: place asSymbol.
        message := placeSymbol getInstance examine.
        ^message.
    ] "
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

instructions        -- to see these instructions.
time                -- to see remaining time.
look                -- to look around the room.
inventory           -- to see what’s currently in inventory.
evidence            -- to see all evidence we know about.

talk to [Character] -- to start conversation with character.
ask about [Clue]    -- to ask about clue.
give [Item]         -- to give all items of this type to character.
examine [Place]     -- to examine specific place in a room.
take [Item]         -- to take avaiable item to inventory.
drop [Item]         -- to drop an item.
go to [Location]    -- to go to one of avaiable rooms.
back                -- to go to last room.
unlock [Location/Place] -- to unlock locked location/place with keys
accuse [Character]  -- to character and if correct win the game

quit                -- to end the game and quit.
dev                 -- to see debug instructions (cheat)
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

    separateCommandFromArgument: input with: command [
        | argumentIndex argument |
        argumentIndex := command size + 1.
        argument := input copyFrom: argumentIndex to: input size.
        argument := argument trimSeparators.
        ^argument
    ]

    "Run the game."
    run [
        | cmd isUnknown gameState |

        self printIntroduction.
        self printInstructions.

        gameState := GameState new.
        self printLines: (gameState start).
        "Game loop."
        [isGameOver] whileFalse: [

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
                Transcript show: ''; cr.
                numbers displayLines.
                Transcript show: ''; cr.
                isUnknown := false
            ].
            
            cmd = 'diamond' ifTrue: [
                | diamond  suspect |
                diamond := StolenDiamond getInstance.
                suspect := diamond getSuspect.
                diamond identityHash printOn: Transcript.
                Transcript show: ''; cr.
                Transcript show: suspect getCharacterText; cr.
                isUnknown := false
            ].
    
            cmd = 'ladle' ifTrue: [
                | ladle1 ladle2 ladle3 |
                ladle1 := Ladle getInstance.
                ladle2 := Ladle getInstance.
                " ladle3 := Ladle new. "
                Ladle allInstances displayLines.
                " Transcript show: ''; cr.
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
                Transcript show: ''; cr.
                " part4 := ToolPart getInstance.  "

                isUnknown := false
            ].

            cmd = 'dirt' ifTrue: [
                | place item |
                place := DirtMound getInstance.
                Transcript show: place getDescription; cr.
                item := place getItem.
                Transcript show: item getDescription; cr.
                (item getItemAmount) printOn: Transcript.
                Transcript show: ''; cr.
                " part4 := ToolPart getInstance.  "

                isUnknown := false
            ].

            cmd = 'suspects' ifTrue: [
                Clue getAllClues do: [ :clue |
                    Transcript show: clue getName, ' - ', clue getSuspect getName; cr.
                ].
                " part4 := ToolPart getInstance.  "

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
                    chain := chain, item getItemPlace getName, ' - ', item getName, ': ', item getItemAmount asString, ', '
                ].

                chain := chain trimSeparators.
                chain := chain allButLast.

                message := OrderedCollection new.
                message add: ''; add: chain; add: ''.

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
                message addAll: (gameState goTo: newLocation isInitMove: false isTp: false).
                self printLines: message.
                isUnknown := false
            ].

            (cmd startsWith: 'tp') ifTrue: [
                | message newLocation |
                message := OrderedCollection new.
                newLocation := self separateCommandFromArgument: cmd with: 'tp'.
                message addAll: (gameState goTo: newLocation isInitMove: false isTp: true).
                self printLines: message.
                isUnknown := false
            ].

            isUnknown ifTrue: [
                self printLines: #('' 'Unknown command.' '')
            ]
        ]
    ]
]

Game new run.
