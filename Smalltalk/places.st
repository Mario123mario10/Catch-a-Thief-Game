Object subclass: Place [
    | description locked items placeLocation |

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

    init [
        locked := false.
        items := Set new.
    ]

    isLocked [ ^locked ]
    getDescription  [ ^description ]
    getPlaceLocation [ ^placeLocation ]

    setPlaceLocation: location [ placeLocation := location ]
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