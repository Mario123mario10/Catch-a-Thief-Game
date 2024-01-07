Object subclass: Item [
    | description itemPlace itemAmount |

    "Dictionary with subclasses keys and singleton instance as value"
    instances := Dictionary new.
    
    Item class >> new [
        ((self isMemberOf: ToolPart class) or: [self isMemberOf: Mushroom class])     
           ifTrue: [
                | i |
                i := super new.
                i init.
                ^i
            ]
            ifFalse: [ self error: 'Use getInstance class method' ]
    ]

    "Parent class method to get singleton instance of a subclass"
    Item class >> getInstance [ 
        | instance |
        (instances includesKey: self) ifFalse: [
            instance := super new.
            instances at: self put: instance.
            [ instance init ]
                on: Exception
                do: [ :ex | "Do nothing if init does not exist in subclass" ].
        ]
        ifTrue: [ instance := instances at: self].
        ^instance
    ]

    Item class >> craftTool [
        | thief tool message result |
        result := OrderedCollection new.
        message := OrderedCollection new.
        thief := Suspect getThief.
        (thief isMemberOf: Cook) ifTrue: [
            tool := Ladle getInstance
        ].
        (thief isMemberOf: Butler) ifTrue: [
            tool := FeatherDuster getInstance
        ].
        (thief isMemberOf: Gardener) ifTrue: [
            tool := Rake getInstance
        ].
        tool setItemAmount: 1.
        message add: 'You crafted ', tool getName, ' using 2 ToolPart and 1 ToolHandle'; add: ''.
        result add: tool; add: message.
        ^result.
    ]

    getDescription [ ^description ]
    getName [ ^self class name asString ]
    getItemPlace [ ^itemPlace ]
    getItemAmount [ ^itemAmount ]

    setItemPlace: place [ itemPlace := place ]
    setItemAmount: amount [ itemAmount := amount ]
]

Item subclass: Evidence [
    | connectedClue guilty |
    
    getConnectedClue [ ^connectedClue ]
    whoIsGuilty [ ^guilty ]

    setGuilty: guiltyCharacter [
        guilty := guiltyCharacter
    ]
]

Evidence subclass: Ladle [
    init [
        guilty := Cook getInstance.
        connectedClue := Tool getInstance.
        description := 'Kitchen utensil used by Cook for serving soups and sauces.'
    ]
]

Evidence subclass: Rake [
    init [
        guilty := Gardener getInstance.
        connectedClue := Tool getInstance.
        description := 'Gardening tool with tines used by Gardener for collecting leaves or debris.'
    ]
]

Evidence subclass: FeatherDuster [
    init [
        guilty := Butler getInstance.
        connectedClue := Tool getInstance.
        description := 'Cleaning tool made of feathers used by Butler to remove dust.'
    ]
]

Evidence subclass: ToolHandle [
    init [
        guilty := nil.
        connectedClue := BloodStains getInstance.
        description :=
'Broken part of a tool used to open the heavy vault door. There is blood on the
broken part...'
    ]
]

Evidence subclass: VaultKey [
    init [
        guilty := nil.
        connectedClue := StolenVaultKey getInstance.
        description := 'Key that opens the vault door.'
    ]
]


Evidence subclass: Diamond [
    init [
        guilty := nil.
        connectedClue := StolenDiamond getInstance.
        description := 'Precious gem, possibly from the vault.'
    ]
]

Evidence subclass: CoinPouch [
    init [
        guilty := nil.
        connectedClue := StolenCoins getInstance.
        description := 'Pouch containing coins believed to be from the vault.'
    ]
]

Item subclass: ButlersKeys [
    init [
        description := 'Keys that unlock every lock in the castle.'
    ]
]

Item subclass: Dirt [
    init [
        description := 'Ordinary dirt. You can make quite a mess with this.'
    ]
]

Item subclass: ToolPart [
    ToolPart class >> getInstance [
        self error: 'Use "new" class method, there is more than one ToolPart instance'
    ]

    init [
        description := 'Part of a tool used to open the heavy vault door.'
    ]
]


Item subclass: Mushroom [
    Mushroom class >> getInstance [
        self error: 'Use "new" class method, there is more than one Mushroom instance'
    ]

    init [
        description := 'Magical mushroom.'
    ]
]
