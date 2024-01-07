Object subclass: Location [

    | visited shortDescription longDescription connectedLocations character places |

    "Dictionary with subclasses keys and singleton instance as value"
    instances := Dictionary new.

    Location class >> new [
        | l |
        l := super new.
        l init.
        ^l
    ]
     
    "Parent class method to get singleton instance of a subclass"
    Location class >> getInstance [ 
        | instance |
        (instances includesKey: self) ifFalse: [
            instance := super new.
            instances at: self put: instance.
            [ instance init ]
            on: Exception
            do: [ :ex | "Do nothing if init does not exist in subclass" ].
        ]
        ifTrue: [ instance := instances at: self ].
        ^instance
    ]

    init [
        connectedLocations := Set new.
        places := Set new.
        character := nil.
        visited := false.
    ]

    visit [
        | message |
        message := self getFullLocationDescription: false.
        visited := true.
        ^message
    ]

    getFullLocationDescription: look [
        | description |
        description := OrderedCollection new.
        (visited not | look) ifTrue: [ description add: self getLongDescription; add: ''. ]
                ifFalse: [ description add: self getShortDescription; add: ''. ].
        character ifNotNil: [
            description add: 'Here you can talk to ', self getCharacter getName, '.'; add: ''.
        ].
        (places notEmpty) ifTrue: [
            description add: 'You can examine following places: ', self getPlaces toString, '.'; add: ''.
        ].
        description add: 'From ', self getName, ' you can go to ', self getConnectedLocations toString, '.'; add: ''.
        ^description.
    ]

    getShortDescription [ ^shortDescription ]
    getLongDescription [ ^longDescription ]
    getVisited [ ^visited ]
    getName [ ^self class name asString ]
    getConnectedLocations [ ^connectedLocations ]
    getPlaces [ ^places ]
    getCharacter [ ^character ]

    setVisited [ visited := true ]
    setCharacter: gameCharacter [
        character := gameCharacter.
        character setCharacterLocation: self.
    ]    
    addPlace: place [
        places add: place.
        place setPlaceLocation: self.
        ^self
    ]
]

Location subclass: Vault [
    init [
        super init.
        connectedLocations add: 'Hall'.
        self addPlace: VaultDoor getInstance.
        shortDescription := 'Once the place where the diamond was stored, but now empty.'.
        longDescription := 
'This is a place of crime! King Alaric III keeps here his most precious
treasures, passed down for many generations.

What a great, imposing room it is! The walls are beautifully adorned, and the
floor is made of polished marble.Plenty of gold bars, gold coins, sparkling
jewels, silver artifacts, jewelry, antique weapons, pieces of art and many
other valuable (or not, like a wooden shaft near the entrance) items lie and
stand in the room.

The most important one, the apple of king’s eye, ‘the diamond that adorned the
crown of his ancestors for generations’, always kept in the center of the room,
has been stolen by some petty, lousy thief. Oh, what a tragedy…

Large, heavy-duty door did not help, since the thief must have had the key, as
they were open after the incident.

Hopefully that person gets caught and justice will be served. But, what are
these bloodstains on the floor…'.
    ]

    Vault class >> new [
        self error: 'Use getInstance class method'
    ]
]

Location subclass: Hall [
    init [
        super init.
        connectedLocations add: 'Corridor'; add: 'GuardHouse'; add: 'Courtyard'; add: 'Vault'.
        self setCharacter: King getInstance.
        shortDescription := 'A grand, bustling chamber with a splendid throne at its center.'.
        longDescription := 
'You are in the main hall of the castle. You can get to several places from
here. Many people move around here too.

Candle lanterns, even combined with thin beams of light from narrow windows,
don’t make this place well lit.

If you saw anyone in the distance, you could even mistake somebody for
somebody, especially at night!'.
    ]

    Hall class >> new [
        self error: 'Use getInstance class method'
    ]
]

Location subclass: Kitchen [
    init [
        super init.
        self setCharacter: Cook getInstance.
        connectedLocations add: 'Corridor'.
        self addPlace: Oven getInstance; addPlace: BagOfFlour getInstance; addPlace: KitchenUnit getInstance.
        shortDescription := 'A bustling place filled with the king’s dishes and the aroma of cooked meals.'.
        longDescription := 
'Welcome to the kitchen! The aroma of fresh herbs, cooked vegetables and
flavorful meat dishes fills the air.

Inside this vast room there is a fireplace with a grate, separate fireplace to
cook soup, large oven for roasted dishes, and a bread oven. There are kitchen
utensils hanging on the wall but one hook is empty…'.

    ]

    Kitchen class >> new [
        self error: 'Use getInstance class method'
    ]
]

Location subclass: RoyalBedroom [
    init [
        super init.
        self setCharacter: Butler getInstance.
        connectedLocations add: 'Corridor'.
        self addPlace: Mirror getInstance; addPlace: Bed getInstance; addPlace: Wardrobe getInstance; addPlace: NightTable getInstance.
        shortDescription := 'A room holding keys to every other room in the castle.'.
        longDescription := 
'You are now in the royal bedroom. You see a spacious room full of expensive
furniture and numerous paintings.,

There are huge mirrors and an ornate bed that is probably so expensive that no
servant could afford it even if he worked all his life for it.

This is the place where the butler often stays, serving the king in every way
possible.'.

    ]

    RoyalBedroom class >> new [
        self error: 'Use getInstance class method'
    ]
]

Location subclass: Courtyard [
    init [
        super init.      
        connectedLocations addAll: #('Hall' 'Garden' 'ServantsHouse' 'WizardsTower' 'Forest').
        shortDescription := 'The bustling heart of the castle.'.
        longDescription := 
'You are in the courtyard. This is the middle of the castle from where you can
get to many places.'.

    ]

    Courtyard class >> new [
        self error: 'Use getInstance class method'
    ]
]

Location subclass: Garden [
    init [
        super init.      
        connectedLocations add: 'Courtyard'.
        self setCharacter: Gardener getInstance.
        self addPlace: GardenPond getInstance; addPlace: RoseBushes getInstance; addPlace: KingSculpture getInstance; addPlace: DirtMound getInstance.
        shortDescription := 'A vast garden filled with flourishing vegetables and vibrant flowers.'.
        longDescription := 
'You are in the castle garden. This is the place where the king likes to stay
in spring and early summer.

A trusted gardener makes sure that this place is full of colorful flowers and
nutritious vegetables that the chef uses for his flavorful dishes.

There is a small pond in the center of the garden. One can get to its other
side quite easily, as well as to the bottom.

Inhabitants of the castle enjoy throwing things into the pond from time to
time, causing the gardener to be obliged to clean there.

Sometimes he fishes out amazing treasures…'.

    ]

    Garden class >> new [
        self error: 'Use getInstance class method'
    ]
]

Location subclass: Corridor [
    init [
        super init.
        connectedLocations addAll: #('Hall' 'Kitchen' 'RoyalBedroom').
        shortDescription := 
'A passage connecting the kitchen and royal bedroom, often frequented by the
butler.'.
        longDescription := 
'You are walking through the corridor.

It is a dim passage of stone and and with the flickering glow of torches
casting dancing shadows upon the walls.

A red carpet silences the steps of guards and servants. The ceiling shows
paintings of the kingdom’s history.'.
    ]

    Corridor class >> new [
        self error: 'Use getInstance class method'
    ]
]

Location subclass: GuardHouse [
    init [
        super init.
        self setCharacter: Guard getInstance.
        connectedLocations add: 'Hall'.
        shortDescription := 'A small chamber housing an array of weapons.'.
        longDescription := 
'You are now in one of the castle guards’ rooms.

It’s not very spacious, most of the space is taken by a bed and simple wooden
wardrobe.

Reportedly, he keeps some equipment in there. Stark and spartan style of the
room suits the guard, without a doubt.'.

    ]

    GuardHouse class >> new [
        self error: 'Use getInstance class method'
    ]
] 

Location subclass: WizardsTower [
    init [
        super init.
        self setCharacter: Wizard getInstance.
        connectedLocations add: 'Courtyard'.
        shortDescription := 'An incredible space brimming with curious magical objects.'.
        longDescription := 
'Here is the wizard’s house. It’s kind of like the kitchen… but different.

The wizard prepares various elixirs, magic items here; he even modifies some
plants and animals, so that they have miraculous features.

Hence, there is a fireplace, which allows him to cook elixirs, a small workshop
to generate items, collected animal parts and plants and a couple dozen books
everywhere. Of course, a wand lies on the table.'.

    ]

    WizardsTower class >> new [
        self error: 'Use getInstance class method'
    ]
] 

Location subclass: Forest [
    init [
        super init.
        connectedLocations add: 'Courtyard'.
        self addPlace: Lakeshore getInstance; addPlace: OldTree getInstance; addPlace: HiddenClearing getInstance; addPlace: MushroomPatch getInstance; addPlace: WildlifeNest getInstance.
        shortDescription := 'A sprawling forest teeming with wild animals, plants, and mushrooms.'.
        longDescription := 
'Shhh… did you hear that sound? It was kind of… magical. This forest is like
none other.

Some say you can hear ghost voices here, others say that this is a place where
you can meet a werewolf.

One thing is for sure - you can find unusual plants and animals here. Maybe
they have some special use, who knows?'.

    ]

    Forest class >> new [
        self error: 'Use getInstance class method'
    ]
] 

Location subclass: ServantsHouse [
    | locked |
    init [
        super init.
        connectedLocations add: 'Courtyard'.
        self addPlace: ButlerChest getInstance; addPlace: CookChest getInstance; addPlace: GardenerChest getInstance.
        shortDescription := 'Bedrooms allocated for the castle’s workforce.'.
        longDescription := 
'You entered the king’s servants room. It’s small and cramped, with little
privacy.

The beds are simple and utilitarian, with straw mattresses and rough woolen
blankets and in front of the beds are servants’ wooden chests with personal
belongings.

The room is dimly lit with candles, and the air is smoky and filled with the
smell of cooking food coming from a nearby kitchen.'.
        locked := true.
    ]

    getFullLocationDescription: look [
        | description |
        description := OrderedCollection new.
        locked ifTrue: [
            description add: 'You are welcomed by a locked door. You won’t get inside without a key. Royal 
Chamberlains used to carry keys for every lock on a single keychain.'; add: ''.
        description add: 'From ', self getName, ' you can go to ', self getConnectedLocations toString, '.'; add: ''.
        ] ifFalse: [
            description := super getFullLocationDescription: look.
        ].
        ^description 
    ]
 
    visit [
        | message |
        message := self getFullLocationDescription: false.
        locked ifFalse: [
            visited := true.
        ].
        ^message
    ]

    isLocked [ ^locked ]

    unlock [ locked := false ]

    ServantsHouse class >> new [
        self error: 'Use getInstance class method'
    ]
] 
