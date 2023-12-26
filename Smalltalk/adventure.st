"The germ of a text adventure game
Marcin Szlenk 2022"

"Game class

Responsibilities:
* print/read outputs/inputs
* handle game loop
* handle 'instructions' and 'halt' commands

Collaborators:
* ..."


Set extend [
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

Collection extend [
    shuffle [
        | n k r |
        n := self size.
        r := Random new seed: (Time now asSeconds * 1000).
        [ n > 1 ] whileTrue: [
            k := r between: 1 and: n.
            self swap: n with: k.
            n := n - 1
        ]
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
        ^self goTo: 'Hall' isInitMove: true.
    ]

    getCurrentLocation [
        ^currentLocation
    ]
    
    goTo: newLocation isInitMove: initMove [
        "TODO: getConnectedLocations nie powinno zwracać listy stringów, a listę klas (lub obiektów za pomocą singleton)"
        | canMove visited message classSymbol previousLocation | 
        message := OrderedCollection new.
        initMove ifFalse: [ 
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

            initMove ifFalse: [ 
                message add: ''.
                message add: 'You moved from ', previousLocation getName, ' to ', currentLocation getName, '.'.
                message add: ''.
            ].
           
            message add: currentLocation visit.
            message add: ''.
            message add: 'From ', currentLocation getName, ' you can go to ', currentLocation getConnectedLocations toString, '.'.
            message add: ''.

        ]
        ifFalse: [
            message add: ''.
            message add: 'You can’t move from ', currentLocation getName, ' to ', newLocation, '.'.
            message add: ''.
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
        | message |
        message := OrderedCollection new.
        message add: ''.
        " currentPlace examine. "
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
        FileStream fileIn: 'characters.st'.
        FileStream fileIn: 'locations.st'.
        FileStream fileIn: 'clues.st'.
        FileStream fileIn: 'places.st'.
        FileStream fileIn: 'items.st'.
        instructions := OrderedCollection new.
        instructions add: instructionsText.
        isGameOver := false
    ]

    assignClues [

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
                message addAll: (gameState goTo: newLocation isInitMove: false).
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
