Object subclass: Place [
    | description locked item placeLocation |

    "Dictionary with subclasses keys and singleton instance as value"
    instances := Dictionary new.

    Place class >> new [
        | p |
        p := super new.
        p init.
        ^p
    ]

    Place class >> getAllPlaces [
        ^(OrderedCollection new addAll: (Place subclasses))
    ] 

    "Parent class method to get singleton instance of a subclass"
    Place class >> getInstance [ 
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

    Place class >> assignItems [
        | keySuspect coinsSuspect diamondSuspect |
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
  
    ]

    init [
        locked := false.
    ]

    isLocked [ ^locked ]
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
    unlock [ locked := false ]

    " examine [
        | message |
        message := OrderedCollection new.
        locked isTrue: [
            message add: ''; '', self class name, ' is locked'; ''
        ]
        ifFalse: [
            
        ].
    ] "
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

Place subclass: ButlerChest [
    
    ButlerChest class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'A chest belonging to the butler.' 
    ]
]

Place subclass: CookChest [
    
    CookChest class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'A chest belonging to the cook.' 
    ]
]

Place subclass: GardenerChest [
    
    GardenerChest class >> new [
        self error: 'Use getInstance class method'
    ]

    init [
        description := 'A chest belonging to the gardener.' 
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