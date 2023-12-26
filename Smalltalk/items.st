Object subclass: Item [

    "Dictionary with subclasses keys and singleton instance as value"
    instances := Dictionary new.
    
    Item class >> new [
        | i |
        i := super new.
        ^i
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
]

Item subclass: Ladle [
    Ladle class >> new [
        self error: 'Use getInstance class method'
    ]
]

Item subclass: Rake [
    Rake class >> new [
        self error: 'Use getInstance class method'
    ]
]

Item subclass: FeatherDuster [
    FeatherDuster class >> new [
        self error: 'Use getInstance class method'
    ]
]

Item subclass: ToolHandle [
    ToolHandle class >> new [
        self error: 'Use getInstance class method'
    ]
]

Item subclass: ToolPart [
    ToolPart class >> new [
        self error: 'Use getInstance class method'
    ]
]

Item subclass: VaultKey [
    VaultKey class >> new [
        self error: 'Use getInstance class method'
    ]
]

Item subclass: ButlersKeys [
    ButlersKeys class >> new [
        self error: 'Use getInstance class method'
    ]
]

Item subclass: Diamond [
    Diamond class >> new [
        self error: 'Use getInstance class method'
    ]
]

Item subclass: CoinPouch [
    CoinPouch class >> new [
        self error: 'Use getInstance class method'
    ]
]

Item subclass: Mushroom [
    Mushroom class >> new [
        self error: 'Use getInstance class method'
    ]
]

Item subclass: Dirt [
    Dirt class >> new [
        self error: 'Use getInstance class method'
    ]
]
