Object subclass: Clue [
    | suspect |

    "Dictionary with subclasses keys and singleton instance as value"
    instances := Dictionary new.
    clues := nil.

    Clue class >> getAllClues [
        clues ifNil: [
            clues := OrderedCollection new.
            clues add: BloodStains getInstance; add: StolenVaultKey getInstance; add: WizardsClue getInstance; add: StolenCoins getInstance; add: StolenDiamond getInstance; add: Tool getInstance; add: GuardsClue getInstance. 
        ].
        ^clues
    ]

    Clue class >> getInstance [ 
        | instance |
        (instances includesKey: self) ifFalse: [
            instance := super new.
            instances at: self put: instance.
        ]
        ifTrue: [ 
            instance := instances at: self.
        ].
        [ instance init ]
            on: Exception
            do: [ :ex | "Do nothing if init does not exist in subclass" ].
        ^instance
    ]

    Clue class >> new [ 
        | c |
        c := super new.
        c init.
        ^c
    ]
    
    getName [ ^self class name asString ]
    getSuspect [ ^suspect ]

    setSuspect: randomSuspect [
        suspect := randomSuspect 
    ] 
]

Clue subclass: Tool [
    Tool class >> new [ 
        self error: 'Use getInstance class method'
    ]
]

Clue subclass: StolenVaultKey [
    StolenVaultKey class >> new [ 
        self error: 'Use getInstance class method'
    ]
]

Clue subclass: StolenDiamond [
    StolenDiamond class >> new [ 
        self error: 'Use getInstance class method'
    ]
]

Clue subclass: GuardsClue [
    GuardsClue class >> new [ 
        self error: 'Use getInstance class method'
    ]
]

Clue subclass: WizardsClue [
    WizardsClue class >> new [ 
        self error: 'Use getInstance class method'
    ]
]

Clue subclass: StolenCoins [
    StolenCoins class >> new [ 
        self error: 'Use getInstance class method'
    ]
]

Clue subclass: BloodStains [
    BloodStains class >> new [ 
        self error: 'Use getInstance class method'
    ]
]
