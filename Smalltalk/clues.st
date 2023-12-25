
Object subclass: Clue [
    Clue class >> getAllClues [
        ^(OrderedCollection new addAll: (Clue subclasses))
    ]

    Clue class >> new [ 
        | c |
        c := super new.
        c init.
        ^c
    ]

]

Clue subclass: Tool [
    Tool class >> new [ 
        | c |
        c := super new.
        c init.
        ^c
    ]
]

Clue subclass: StolenVaultKey [
    StolenVaultKey class >> new [ 
        | c |
        c := super new.
        c init.
        ^c
    ]
]

Clue subclass: StolenDiamond [
    StolenDiamond class >> new [ 
        | c |
        c := super new.
        c init.
        ^c
    ]
]

Clue subclass: GuardsClue [
    GuardsClue class >> new [ 
        | c |
        c := super new.
        c init.
        ^c
    ]
]

Clue subclass: WizardsClue [
    WizardsClue class >> new [ 
        | c |
        c := super new.
        c init.
        ^c
    ]
]

Clue subclass: StolenCoins [
    StolenCoins class >> new [ 
        | c |
        c := super new.
        c init.
        ^c
    ]
]

Clue subclass: BloodStains [
    BloodStains class >> new [ 
        | c |
        c := super new.
        c init.
        ^c
    ]
]
