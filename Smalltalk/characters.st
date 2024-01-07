"TODO: extract Suspect from GameCharacter"

Object subclass: GameCharacter [
    | characterText characterLocation clues |

    "Dictionary with subclasses keys and singleton instance as value"
    instances := Dictionary new.
    " suspects := nil. "

    GameCharacter class >> new [ 
        | gc |
        gc := super new.
        gc init.
        ^gc
    ]

    GameCharacter class >> assignClues [
        | shuffledSuspects shuffledClues three f_two s_two susIndices |
        shuffledSuspects := (Suspect getSuspects) shuffle.
        shuffledClues := (Clue getAllClues) shuffle.
        susIndices := OrderedCollection new.
        susIndices add: 1; add: 2; add: 3.
        susIndices shuffle.

        three := shuffledClues copyFrom: 1 to: 3.
        f_two := shuffledClues copyFrom: 4 to: 5.
        s_two := shuffledClues copyFrom: 6 to: 7.

        three do: [ :clue | clue setSuspect: (shuffledSuspects at: (susIndices at: 1)) . ].
        f_two do: [ :clue | clue setSuspect: (shuffledSuspects at: (susIndices at: 2)). ].
        s_two do: [ :clue | clue setSuspect: (shuffledSuspects at: (susIndices at: 3)). ].
    ]


    "Parent class method to get singleton instance of a subclass"
    GameCharacter class >> getInstance [ 
        | instance |
        (instances includesKey: self) ifFalse: [
            instance := super new.
            instance initParent.
            instances at: self put: instance.
            [ instance init ]
            on: Exception
            do: [ :ex | "Do nothing if init does not exist in subclass" ].
        ]
        ifTrue: [ instance := instances at: self ].    
        ^instance
    ]

    initParent [
        clues := Set new.
    ]

    talkToAtState: gameState [
        ^self getCharacterText
    ]

    getCharacterLocation [ ^characterLocation ]
    getCharacterText [ ^(self splitString: characterText at: 80) ]
    getName [ ^self class name asString ]
    getClues [ ^clues ]

    addClue: clue [
        clues add: clue.
    ]
    setCharacterLocation: location [ characterLocation := location ]
]
 
GameCharacter subclass: King [
    King class >> new [ 
        self error: 'Use getInstance class method'
    ]

    init [
        characterText := 
'You stand before King Alaric III, a unique figure in this day and age. His 
build exudes majesty as great as his position in the kingdom. With a 
distinctive crown adorned with sparkling jewels on his head and a purple velvet 
cloak embroidered with gold, he seems to embody royal power and pride. 
Nevertheless, his stern face shows traces not only of past victories but also 
of sadness. 

’I welcome you to our court,’ comes the king’s gruff voice. ’Unfortunately, we 
are faced with a tragic situation. People have been stealing gold from the 
royal treasury for a long time. The sums were insignificant, so I never 
bothered to catch the thief. However, unexpectedly, someone dared to plunder 
our most important jewel—the diamond that adorned the crown of my ancestors for 
generations.’ You can hear anger and anxiety in the ruler’s voice. 

’Your task is to find the villain responsible for this insolent theft. Look for 
clues; three pieces of evidence are all I need to convict the criminal without 
upsetting the nobility. If you need any information, start by talking to the 
guard who was supposed to be guarding the vault yesterday. Unfortunately, he 
was distracted during the robbery by an unexpected noise in the courtyard,’ the 
king explains, with a determined look on his face. 

’The thief was almost caught red-handed; the guard saw them run away. We 
managed to react quickly and put the entire castle on lockdown. Whoever the 
thief is, they’re still in the castle. Even through their incompetence, they 
managed to steal the key to the vault. Put all of my servants under suspicion…’ 

’Except for my loyal guard who has served me for many years and holds me in 
high regard. I am sure he would not betray me for money; his honor wouldn’t 
allow that. He’s in a guard room right now.’' 
 
    ]
] 
GameCharacter subclass: Suspect [
    | bloodStainsOptionalText evidenceTexts |

    Suspect class >> getSuspects [
        ^((Suspect subclasses collect: [ :each | each getInstance ]) asOrderedCollection)
    ]

    Suspect class >> getThief [
        | thief |
        thief := Suspect getSuspects at: 1.
        Suspect getSuspects do: [ :suspect |
            (suspect getClues size > thief getClues size) ifTrue: [
                thief := suspect
            ]
        ].
        ^thief
    ]

    getEvidenceTextFor: clue [
        ^(evidenceTexts at: clue)
    ]
]

Suspect subclass: Gardener [
    Gardener class >> new [ 
        self error: 'Use getInstance class method'
    ]

    init [
        evidenceTexts := Dictionary new.
        evidenceTexts at: Tool put: 
'When you hand over the rake with the rake stick you found to the gardener, his 
expression becomes more concerned. Without a word, he takes the tool and 
examines it carefully, holding it strangely. After a moment, he looks at you 
with a look of uncertainty in his eyes. 

’Did... did you find this near the vault?’ he asks carefully, trying to remain 
calm. ’It’s unbelievable. It’s really my rake stick, but I don’t know how it 
could have gotten there. I was not there during the robbery, I swear.’ 

The gardener seems nervous. ’I must have lost it while I was going to the 
garden, but I’ve never been inside the vault. It must be a bad omen. I think we 
need to report this to the king and the guards. What do you think?’ His 
uncertainty and concern are very visible now.'.
        evidenceTexts at: StolenVaultKey put:
'When you show the gardener the key to the vault you found, his eyes become wide
with surprise. Without a word, he takes the key and examines it carefully,
turning it over in his hands.

’This... this is the key to the vault,’ the gardener whispers, his voice
shaking with surprise. ’I don’t know how it ended up in the garden. I’m sure I
had nothing to do with it.’

The gardener seems concerned with the situation. ’It can’t be a coincidence.
How am I this unlucky,’ he says anxiously. ’We must inform the king and the
guards immediately. This may be key evidence in this case.’ His hands shake as
he hands you the key, asking you to proceed.'.
        evidenceTexts at: StolenDiamond put:
'The gardener turns pale. His eyes widen in horror, and his hands begin to
tremble. He seems completely stunned.

’No! It’s not me! I don’t know how that diamond ended up in my chest!’ the
gardener exclaims, trying to defend himself against the accusations. ’It must
be some kind of trap or plot. I don’t know which god I offended to be punished
so severely! I had nothing to do with it, really!’

The gardener seems completely devastated and terrified of the situation. You
see honest desperation and remorse in his eyes, trying to convince you of his
innocence.'.
        evidenceTexts at: GuardsClue put: 
'’What did he say?’ says the gardener with an expression of surprise. ’Why was
the guard watching me? Isn’t a servant allowed to dream about his king’s
treasures?’ He waits for a moment for your answer and then continues, ’He
chased me away after a while anyway. I didn’t see anything interesting. Why
would you bring it up?’'.
        evidenceTexts at: WizardsClue put:
'’Oh, the crystals!’ says the gardener with a look of embarrassment. ’It was
just an innocent joke on the guard. For so many years, he has been guarding our
castle, and nothing ever happens here. I just wanted to give him some
entertainment!’

’How could I have known that it would distract him from the vault just when the
thief was waiting for an opportunity?’ The gardener tries to convince me that
it was all innocent fun, but his expression suggests that he realizes the
seriousness of the situation and the erroneous mistake he made.'.
        evidenceTexts at: StolenCoins put:
'’This... this is impossible!’ the gardener exclaims with horror in his voice.
’How did this money end up in my garden? I am surely cursed.’

The gardener seems completely stunned and terrified by the situation. His
reaction suggests that he is completely devastated and has no idea how this
could all have happened.'.     
        evidenceTexts at: BloodStains put:
'’That wound? It... I must have done it to myself while I was working in the
garden,’ the gardener says, trying to defend himself. ’I was a bit engrossed in
pruning the roses and accidentally cut myself on some sharp thorns. It must
have happened yesterday while I was working on those bushes. Beautiful, aren’t
they?’'.     
        evidenceTexts at: False put:
'’It’s very interesting,’ says the gardener calmly. ’Why did you come to me with
this? I’m not a brainy fellow. I probably won’t be any help to you.’'.
    ]

    getCharacterText [
        (clues includes: BloodStains getInstance) ifTrue: [
            bloodStainsOptionalText := ' There is a fresh wound on his forearm, as if something cut him. '
        ] ifFalse: [
            bloodStainsOptionalText := ' ' 
        ].
        characterText := 
'While wandering through the royal garden, you come across a gardener. He is a 
middle-aged man with hard hands and dark spots on his clothes.', bloodStainsOptionalText, 
'His gaze shows signs of hard work and devotion to the royal 
garden. 

Surprised by your presence, the gardener puts down his tools and approaches 
you. ’What kind of surprise is this?’ he asks, leaning on his spade. ’The 
garden is currently closed for visitors; you shouldn’t be here without the 
king’s permission.’ 

Then he continues with a little concern, ’To tell you the truth, I don’t know 
anything about any robbery. I’ve been working here since dawn, and I treat 
these plants like my own children. I haven’t noticed anything suspicious. It 
must be some kind of mistake. Maybe the diamond will come back on its own; 
things like that happen. And besides, we all lose things sometimes. If you have 
any questions, we can discuss them with the king. But let me get back to my 
work.’ The gardener seems nervous and a bit concerned about the situation.'.
        ^super getCharacterText
    ]

] 

Suspect subclass: Cook [
    Cook class >> new [ 
        self error: 'Use getInstance class method'
    ]

    init [
        evidenceTexts := Dictionary new.
        evidenceTexts at: Tool put: 
'’What is this... This is my ladle!’ the cook exclaims, surprised. ’How did this
happen? Where did you find her? It must have been the work of that thief of
yours!’ You notice sweat on his forehead. It’s hard to tell if it’s because of
the heat or anxiety.'.
        evidenceTexts at: StolenVaultKey put:
'When you show the chef the key to the vault, his reaction is immediate. Outrage
flashes across his face. ’What is this doing here? What if it ended up in one
of the dishes? I have a reputation to uphold; I can’t afford turmoil in my
kitchen.’

After a while, he realizes what kind of key it is. ’This key... This key is
from the vault,’ mutters the cook with fear in his eyes. ’Where did you get it?
It’s not what you think!’ His reaction suggests that he is baffled by the key’s
presence.'.
        evidenceTexts at: StolenDiamond put:
'He turns even redder than before, and his eyes widen with surprise.

’It... it’s not what you think!’ the cook groans with fear in his voice, his
tone trembling with emotion. ’I have no idea how this diamond ended up in my
chest. That must be some kind of intrigue! Someone is trying to cheat you!’ The
cook continues, becoming more and more shaky as he considers the situation.

Suddenly, the cook starts accusing others in a frenzy. ’It’s the butler! He’s
the only one who has the key to our rooms!’ he shouts, trying to shift the
blame to another servant in the castle'.
        evidenceTexts at: GuardsClue put: 
'’I’m sorry if that looked suspicious,’ the cook says, a little sheepishly. ’But
it was only because I was curious about what was going on. All these rumors
about gold and diamonds in the vault had everyone thinking. I thought about how
gold could enhance my dishes. I never had any intention of stealing; it’s not
my style.’'.
        evidenceTexts at: WizardsClue put:
'’I don’t understand why the wizard pointed at me with that toy,’ the cook says
with disapproval in his voice. ’It was just a toy he gave me. It looked fun, so
I used it. I had no idea it would cause such a riot in the yard. It wasn’t my
intention to distract the guard. I certainly had no intention of helping any
criminal.’'.
        evidenceTexts at: StolenCoins put:
'’I have no idea how this gold ended up in my kitchen,’ says the cook with
horror in his voice. ’I don’t know whose fault it is, but the kitchen is no
place for thieves to hide their treasures!’ continues the cook. His hands begin
to shake, and his expression shows true terror. ’I hope I’m not on the suspect
list...’ he exclaims with hope in his voice, looking at you, hoping for
understanding.'.  
        evidenceTexts at: BloodStains put:
'’It’s not what you think,’ the cook groans, trying desperately to convince you
of his innocence. ’Last night, while chopping meat for tonight’s party, my
cleaver hit a hard bone. The blade of the cleaver bounced and I was incredibly
lucky to only cut my hand. That’s why I have that wound! It has nothing to do
with theft, really. All the servants can attest, that it’s true!’'.         
        evidenceTexts at: False put:
'’It’s not really about me, now is it?’ said the annoyed cook. ’You’d better
leave me in peace; I have cookery to run.’'.
    ]


    getCharacterText [
        (clues includes: BloodStains getInstance) ifTrue: [
            bloodStainsOptionalText := ' There is a fresh wound on his forearm, as if something cut him. '
        ] ifFalse: [
            bloodStainsOptionalText := ' ' 
        ].
        characterText := 
'You see a man wearing a white apron that is now slightly stained from working 
in the kitchen. There are traces of flour on his hands, and his face is 
slightly flushed from the heat.', bloodStainsOptionalText, 'The cook seems to be a hard-
working and busy man who devotes himself to his role in the royal kitchen with 
passion. 

When you enter the kitchen as a knight, the cook seems surprised by your 
presence. He puts down the knife he was sharpening and shifts the egg from one 
hand to the other, suddenly becoming more careful. 

’What are you looking for?’ asks the cook, a little concerned in his voice. 
’This place is closed; you shouldn’t be here without the king’s permission.’  
His reaction suggests he’s irritated by your presence in the kitchen and 
wonders what brought you there. 

’Oh? It’s about the robbery?’ says the cook with a bit of shock. ’Fair enough, 
if you must snoop around, then do what you must. Just don’t get in my way. 
People are trying to work here!’ he says, irritated.'.
        ^super getCharacterText
    ]
] 

Suspect subclass: Butler [
    | distracted |
    Butler class >> new [ 
        self error: 'Use getInstance class method'
    ]

    init [ 
        distracted := false.
        evidenceTexts := Dictionary new.
        evidenceTexts at: Tool put: 
'When the butler notices the duster, his eyes linger over the object for a
moment. He doesn’t seem surprised or concerned. Instead, after a moment’s cold
glance at the duster, he turns to you and says with unflinching seriousness,
’This is certainly interesting evidence, noble sir. However, I am not sure how
it can help clarify this matter. I can assure you that such a duster is widely
available, and many of us use it to perform everyday duties.’'.
        evidenceTexts at: StolenVaultKey put:
'’Ah, the key to the treasury. Yes, it is indeed an unusual discovery to stumble
upon. I am entrusted with various responsibilities in the king’s quarters and
the meticulous care for the royal chambers. However, the presence of the
treasury key here seems rather out of place. It might have been misplaced by
the thief when he was panicking. I assure you, it wasn’t my intention to have
it here,’ said the butler calmly.'.
        evidenceTexts at: StolenDiamond put:
'The butler looks at the diamond for a moment, his gaze seemingly astonished.
Then he shifts his gaze to you and says with a slight tremble in his voice,
’It’s... it’s the king’s diamond. How did you find it? I have no idea how it
came to be in my chest. It must be some mistake or intrigue. I swear that I had
nothing to do with it. Someone is setting me up!’'.
        evidenceTexts at: GuardsClue put: 
'’I understand the guard is concerned and is trying to solve this case,’ the
butler says, trying to remain calm. ’I’m sure that on that day, I was moving
around the castle as I always do. If the guard claims to have seen me at the
vault door, it could have been a chance encounter. But no one saw me enter the
vault or try to open the door, am I correct? If so, that’s just speculation.’'.
        evidenceTexts at: WizardsClue put:
'’It’s true that I received this crystal toy from the wizard at my request,’ the
butler admits. ’I had some misunderstanding with this guard, and I was planning
to use this toy as a kind of joke or a way to vent my dissatisfaction. However,
I had no idea that this toy could cause such turmoil and contribute to the
theft. I am innocent, and I am ready to explain the entire incident to the
king.’'.
        evidenceTexts at: StolenCoins put:
'When you present him with the purse of money, the butler seems surprised but
maintains his composure. He says with skillful conviction: ’I have no idea how
this purse could have ended up in the king’s bedroom. Of course, I am
responsible for making his majesty’s bed every day so I am here often, but that
does not mean I am guilty. Throughout my years of service at court, I have had
no problems with behavior or duties. It must be some kind of intrigue or an
attempt to discredit me.’'.  
        evidenceTexts at: BloodStains put:
'’It’s true that I have this wound on my cheek. I had to shave with a razor this
morning, and in a rush, I accidentally cut myself. This explains the presence
of the wound. Sorry for the confusion, but it really has nothing to do with
theft or any criminal activities. Now, if you’ll excuse me.’'.         
        evidenceTexts at: False put:
'’That is, of course, mighty interesting, sir! Is that all? If so, I must resume
to my duties.’'.
    ]


    getCharacterText [
        (clues includes: BloodStains getInstance) ifTrue: [
            bloodStainsOptionalText := ' There is a fresh wound on his forearm, as if something cut him. '
        ] ifFalse: [
            bloodStainsOptionalText := ' ' 
        ].
        characterText := 
'You stand before an unassuming, delicately built butler, seemingly one of the 
quieter figures at the court throughout the years. His pale face seems to blend 
into the background, and his eyes, filled with a mysterious glow, scan the 
surroundings. The butler wears a modest uniform that betrays his low status at 
court, yet it’s carefully ironed and clean.', bloodStainsOptionalText, ' 

The butler stands still and awaits a signal. After a while, he opens his mouth 
and speaks calmly. ’What brings you here, noble lord?’ 

’Theft? It must have happened while I was busy bringing books to the library,’ 
he confesses with a hint of sadness in his voice. ’I had no idea what was 
happening in the courtyard or the commotion at the vault. It’s really strange. 
I can assure you that I had nothing to do with it. I worked most of the day, 
unfortunately, there is no one to testify about it.’'.
        ^super getCharacterText
    ]

    isDistracted [
        ^distracted
    ]

    setDistracted: dirtDropped [
        distracted := dirtDropped
    ]
] 

GameCharacter subclass: Guard [
    Guard class >> new [ 
        self error: 'Use getInstance class method'
    ]

    init [
        characterText := 
'You are standing in front of the king’s loyal guard. His figure reveals that 
in his youth, he was an outstanding knight. However, the passage of time has 
left its mark on this once-powerful man. The guard wears chain mail and a 
breastplate and carries a halberd. His armor and weapons are in excellent 
condition, showing his care for his equipment. 

’Greetings, noble knight,’ the guard says, somewhat thoughtfully. ’Please let 
me keep my name to myself. I don’t want this disaster to taint my noble family. 
Lean in, and I’ll tell you what I know,’ he continues, encouraging you to come 
closer. 

’Just before the robbery, I noticed that the ', GuardsClue getInstance getSuspect getName, ' was moving 
around restlessly in the throne room. I had to loudly remind him of his duties 
because he stood there as if paralyzed and was examining the vault door from 
afar,’ the guard whispers. ’As you probably already know, I was standing guard 
over the vault and keeping watch until I heard a disturbing bang and 
immediately ran into the courtyard. However, I found nothing but thick smoke. 
It was definitely a diversion.’ 

’When I was returning to my post, I unexpectedly came across a thief while he 
was robbing the treasury. He managed to escape me, and unfortunately, I did not 
see who he was; my eyesight is not as good as it used to be.’ The guard 
finishes, expressing his helplessness. ’Maybe he lost something while escaping 
from the vault. You better check it out. No crime is perfect, and this one is 
definitely not an exception.'
    ]

    talkToAtState: gameState [
        | clue |
        clue := GuardsClue getInstance.
        gameState addEvidenceOf: clue of: clue getSuspect.
        ^super talkToAtState: gameState
    ]
] 

GameCharacter subclass: Wizard [
    | gotMushrooms afterPaymentText |
    Wizard class >> new [ 
        self error: 'Use getInstance class method'
    ]

    init [
        gotMushrooms := false.
        characterText := 
'You stand before the court wizard of King Alaric III, an old man with 
observant eyes. He has a long beard and a distinctive black sorcerer’s hat. He 
is well known throughout the kingdom, both for his astonishing sorcery and his 
arrogance, as well as his incredible wisdom. At first glance, it is clear that 
even the king himself would not dare to go against him. 

’Hello, soldier,’ says the wizard with a look of sneer. ’It appears that you 
have a reason to visit my humble chambers. I understand that you have come to 
ask me about the king’s jewel. As for my possible role in this matter, allow me 
to explain... I have no desire nor time to play vault busting. Stealing gold 
and diamonds from monarchs would just be boring. Besides, I have no use for 
these kinds of riches.’ 

However, the wizard continues, ’I was in my tower when I heard that noise in 
the courtyard. I know exactly who was responsible, and I know what was used to 
perform this. In fact, I gave him that item in return for a favor. I’m willing 
to share this information with you, but I won’t do it for free. You must 
collect 10 mushrooms from the magical forest surrounding the castle, and then I 
will be willing to share my knowledge.’ The wizard expresses his tiredness in 
his tone of voice.'.

        afterPaymentText := 
'When you gave the wizard magic mushrooms from the enchanted forest, his eyes
light up with momentary delight. He gently takes them from your hand and places
them on the table next to him, then reaches for his wand and uses his finger to
carve out the runes on one of the mushrooms. Everything around you begins to
glow with magical light, and the wizard carefully studies the ingredients he
has just obtained.

’Deep in the forest, in the shade of the trees, these mushrooms are a source of
great power,’ the wizard says solemnly and freezes for a second. After a while,
he wakes up as if from a trance. ’Now, what I promised.’ He shrugs and says,
’Some time ago, the ', WizardsClue getInstance getSuspect getName ,' was at my tower. He asked me for a
certain toy, which I gave to the prince for his birthday. I gave him a couple
of crystals, when one of them breaks, the other one explodes with a loud bang
and turns into a thick cloud of smoke. Naturally, this toy is completely
harmless,’ he explained.

’The ',  WizardsClue getInstance getSuspect getName, ' was trying to excuse himself, saying he needed this toy
for a certain prank. It was just innocent fun, he told me,’ the wizard says
mockingly. ’It seemed quite suspicious, but it is above my pay grade,’ the
wizard says dismissingly.

’I said what I knew. Now it is the time for you to leave my tower,’ the wizard
says, clearly implying that the conversation is over.'

    ]
    giveMushrooms: amount atState: gameState [
        (amount = 10) ifTrue: [
            gotMushrooms := true.
            ^self talkToAtState: gameState
        ].
        (amount = 0) ifTrue: [
            ^'’You want to give me nothing?! Come back when you have 10 mushrooms.’'
        ].
        ((amount < 10) and: [amount > 0]) ifTrue: [
            ^'’You don’t have enough mushrooms! Come back when you have them.’'
        ]
    ]
    
    hasMushrooms [
        ^gotMushrooms
    ]

    getCharacterText [ 
        gotMushrooms 
            ifFalse: [
                ^characterText
            ]
            ifTrue: [
                ^afterPaymentText
            ]
    ]

    talkToAtState: gameState [
        | clue |
        clue := WizardsClue getInstance.
        gotMushrooms ifTrue: [ gameState addEvidenceOf: clue of: clue getSuspect ].
        ^super talkToAtState: gameState
    ]
] 
