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
        ]
        ifTrue: [ instance := instances at: self].
        [ instance init ]
            on: Exception
            do: [ :ex | "Do nothing if init does not exist in subclass" ].
        ^instance
    ]

    getDescription [ ^description ]
    getName [ ^self class name asString ]
    getItemPlace [ ^itemPlace ]
    getItemAmount [ ^itemAmount ]

    setItemPlace: place [ itemPlace := place ]
    setItemAmount: amount [ itemAmount := amount ]
]

Item subclass: Ladle [
    init [
        description := 'Kitchen utensil used by Cook for serving soups and sauces.'
    ]
].

Item subclass: Rake [
    init [
        description := 'Gardening tool with tines used by Gardener for collecting leaves or debris.'
    ]
].

Item subclass: FeatherDuster [
    init [
        description := 'Cleaning tool made of feathers used by Butler to remove dust.'
    ]
].

Item subclass: ToolHandle [
    init [
        description :=
'Broken part of a tool used to open the heavy vault door. There is blood on the
broken part'
    ]
].

Item subclass: VaultKey [
    init [
        description := 'Key that opens the vault door'
    ]
].

Item subclass: ButlersKeys [
    init [
        description := 'Keys that unlock every lock in the castle'
    ]
].

Item subclass: Diamond [
    init [
        description := 'Precious gem, possibly from the vault'
    ]
].

Item subclass: CoinPouch [
    init [
        description := 'Pouch containing coins believed to be from the vault'
    ]
].

Item subclass: Dirt [
    init [
        description := 'Ordinary dirt. You can make quite a mess with this.'
    ]
].

Item subclass: ToolPart [
    instancesCount := 0.

    ToolPart class >> getInstance [
        self error: 'Use "new" class method, there is more than one ToolPart...'
    ]

    ToolPart class >> new [
        | i |
        (instancesCount < 2) ifTrue: [
            i := super new.
            " i init. "
            instancesCount := instancesCount + 1.
            ^i
        ]
        ifFalse: [
            self error: '...but not more than two :)'
        ]
    ]

    init [
        description := 'Part of a tool used to open the heavy vault door'
    ]
].


Item subclass: Mushroom [
    Mushroom class >> getInstance [
        self error: 'Use getInstance class method, there is more than one Mushroom'
    ]

    init [
        description := 'Magical mushroom'
    ]
]
