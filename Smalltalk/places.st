Object subclass: Place [
    | description item placeLocation |

    "Dictionary with subclasses keys and singleton instance as value"
    instances := Dictionary new.

    Place class >> new [
        | p |
        p := super new.
        p init.
        ^p
    ]

    Place class >> getAllPlaces [
        ^(OrderedCollection new addAll: (Place allSubclasses select: [ :each | each ~= Chest ]))
    ] 

    "Parent class method to get singleton instance of a subclass"
    Place class >> getInstance [ 
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

    Place class >> assignItems [
        | keySuspect coinsSuspect diamondSuspect amounts allShrooms 
            shroomsPlaces toolPartsCandidates toolPartsPlaces |

        allShrooms := 10.
        shroomsPlaces := OrderedCollection new.
        shroomsPlaces addAll: (Forest getInstance getPlaces).

        DirtMound getInstance setItem: (Dirt getInstance) withAmount: 100.
        VaultDoor getInstance setItem: (ToolHandle getInstance) withAmount: 1.
        NightTable getInstance setItem: (ButlersKeys getInstance) withAmount: 1.

        keySuspect := StolenVaultKey getInstance getSuspect.
        (keySuspect isMemberOf: Gardener) 
            ifTrue: [GardenPond getInstance setItem: (VaultKey getInstance) withAmount: 1].
        (keySuspect isMemberOf: Cook) 
            ifTrue: [Oven getInstance setItem: (VaultKey getInstance) withAmount: 1].
        (keySuspect isMemberOf: Butler) 
            ifTrue: [Mirror getInstance setItem: (VaultKey getInstance) withAmount: 1].
    
        coinsSuspect := StolenCoins getInstance getSuspect.
        (coinsSuspect isMemberOf: Gardener) 
            ifTrue: [RoseBushes getInstance setItem: (CoinPouch getInstance) withAmount: 1].
        (coinsSuspect isMemberOf: Cook) 
            ifTrue: [BagOfFlour getInstance setItem: (CoinPouch getInstance) withAmount: 1].
        (coinsSuspect isMemberOf: Butler) 
            ifTrue: [Bed getInstance setItem: (CoinPouch getInstance) withAmount: 1].
    
        diamondSuspect := StolenDiamond getInstance getSuspect.
        (diamondSuspect isMemberOf: Gardener) 
            ifTrue: [GardenerChest getInstance setItem: (Diamond getInstance) withAmount: 1].
        (diamondSuspect isMemberOf: Cook) 
            ifTrue: [CookChest getInstance setItem: (Diamond getInstance) withAmount: 1].
        (diamondSuspect isMemberOf: Butler) 
            ifTrue: [ButlerChest getInstance setItem: (Diamond getInstance) withAmount: 1].

        amounts := self generateWeightedShroomAmounts: (shroomsPlaces size) for: allShrooms.
        1 to: (shroomsPlaces size) do: [ :i |
            | shroom |
            ((amounts at: i) ~= 0) ifTrue: [
                shroom := Mushroom new.
                (shroomsPlaces at: i) setItem: shroom withAmount: (amounts at: i).
            ].
        ].

        toolPartsCandidates := self getAllPlaces select: [ :each | 
            ((each getInstance getPlaceLocation) ~= Forest) and: [ each getInstance getItem isNil]
        ].
        toolPartsCandidates := toolPartsCandidates asOrderedCollection.
        toolPartsPlaces := Set new.
        [             
            | randomIndexes |
            randomIndexes := (1 to: toolPartsCandidates size) asOrderedCollection.
            1 to: 2 do: [ :i | 
                randomIndexes shuffle.
                toolPartsPlaces add: (toolPartsCandidates at: randomIndexes first).
                randomIndexes removeFirst.
            ].
        ] value.
        toolPartsPlaces do: [ :place |
            place getInstance setItem: ToolPart new withAmount: 1.
        ]
    ]
    
    Place class >> generateWeightedShroomAmounts: n for: x [
        | numbers sum i |
        numbers := OrderedCollection new.
        1 to: n do: [ :i | numbers add: 0 ].
        sum := 0.
        [sum ~= x] whileTrue: [
            | index currentAtIndex randomNumber |
            randomNumber := Random between: 1 and: 3.
            ((sum + randomNumber) <= x) ifTrue: [
                index := Random between: 1 and: 5.
                currentAtIndex := numbers at: index.
                currentAtIndex := currentAtIndex + randomNumber.
                numbers at: index put: currentAtIndex.
                sum := sum + randomNumber.
            ].
        ].
        ^numbers
    ]

    getDescription  [ ^description ]
    getPlaceLocation [ ^placeLocation ]
    getItem [ ^item ]
    getName [ ^self class name asString ]

    setPlaceLocation: location [ placeLocation := location ]
    setItem: gameItem withAmount: amount [
        item := gameItem.
        item setItemAmount: amount.
        item setItemPlace: self.
    ] 
    takeItem: gameItem [
        | itemInstance |
        item ifNotNil: [
            ((gameItem ~= Mushroom) and: [gameItem ~= ToolPart]) ifTrue: [
                itemInstance := gameItem getInstance.
                (item = itemInstance) ifTrue: [
                    item setItemPlace: nil.
                    item := nil.
                    ^itemInstance
                ] ifFalse: [
                    ^nil
                ]
            ] ifFalse: [
                (item class = gameItem) ifTrue: [ 
                    | tempItem |
                    tempItem := item.
                    item setItemPlace: nil.
                    item := nil.
                    ^tempItem
                ] ifFalse: [
                    ^nil
                ]
            ]
        ] ifNil: [ ^nil ]
    ]

    examinePlace [
        | message |
        message := OrderedCollection new.
        message add: ''; add: '', description.
        message add: ''; add: 'You are examining ', self getName, '.'; add: ''. 
        item ifNotNil: [
            message add: '', item getName, ': ', item getItemAmount asString; add: ''.
        ].
        ^message
    ]
]

Place subclass: VaultDoor [
   
    VaultDoor class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 
'The heavy vault door seems too heavy for one person to open. It looks like
someone used some kind of tool to move the door.'
    ]
]

Place subclass: GardenPond [
    
    GardenPond class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'A tranquil pond in the garden.'
    ]
]

Place subclass: RoseBushes [
    
    RoseBushes class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'Beautiful rose bushes in the garden.'
    ]
]

Place subclass: KingSculpture [
    
    KingSculpture class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'A sculpture of the king in the garden.' 
    ]
]

Place subclass: DirtMound [

    DirtMound class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'A pile of dirt in the garden.' 
    ]
]

Place subclass: Mirror [
    
    Mirror class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'An ornate mirror hanging on the wall.'
    ]
]

Place subclass: Bed [
    
    Bed class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'An elegantly designed bed in the room.'
    ]
]

Place subclass: Wardrobe [
    
    Wardrobe class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'A wardrobe for clothes storage.' 
    ]
]

Place subclass: NightTable [
    
    NightTable class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'Approaching the night table, you notice the butler watching you closely.'
    ]
]

Place subclass: Oven [
    
    Oven class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'A large oven for cooking dishes.'
    ]
]

Place subclass: BagOfFlour [
    
    BagOfFlour class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'A bag of flour used for cooking.' 
    ]
]

Place subclass: KitchenUnit [
    
    KitchenUnit class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'A unit used for kitchen storage.' 
    ]
]


Place subclass: Lakeshore [
    
    Lakeshore class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'The serene shore of a tranquil lake.'
    ]
]

Place subclass: OldTree [
    
    OldTree class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'An ancient, towering tree with sprawling branches.'
    ]
]

Place subclass: HiddenClearing [
    
    HiddenClearing class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'A secluded clearing, hidden from sight.'
    ]
]

Place subclass: MushroomPatch [
    
    MushroomPatch class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'A patch of mushrooms, some with vibrant colors.'
    ]
]

Place subclass: WildlifeNest [
    
    WildlifeNest class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'A nest, possibly home to some wildlife.'
    ]
]

Place subclass: Chest [
    | locked |
    Chest class >> new [
        self error: 'Use getInstance class method'
    ]

    Chest class >> getAllChests [
        ^(OrderedCollection new addAll: Chest subclasses)
    ]

    examinePlace [
        | message |
        message := OrderedCollection new.
        (placeLocation isLocked) ifTrue: [
            message add: ''; add: placeLocation getName, 
            ' is locked now, you can’t examine anything.'; add: ''.
        ] ifFalse: [
            locked ifTrue: [
                message add: ''; add: '', self getName, ' is locked.'; add: ''.
            ]
            ifFalse: [ message := super examinePlace ].
        ].
        ^message
    ]

    init [
        locked := true.
        description := 'A chest belonging to the butler.' 
    ]

    isLocked [ ^locked ]

    unlock [ locked := false ]
]

Chest subclass: ButlerChest [
    
    ButlerChest class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        super init.
        description := 'A chest belonging to the butler.' 
    ]

]

Chest subclass: CookChest [
    
    CookChest class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        super init.
        description := 'A chest belonging to the cook.' 
    ]
]

Chest subclass: GardenerChest [
    
    GardenerChest class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        super init.
        description := 'A chest belonging to the gardener.' 
    ]
]
