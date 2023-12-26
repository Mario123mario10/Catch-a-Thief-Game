"Characters"

Object subclass: GameCharacter [
    " | characterText characterLocation clues | "
    | characterText characterLocation |

    "Dictionary with subclasses keys and singleton instance as value"
    instances := Dictionary new.

    GameCharacter class >> new [ 
        | gc |
        gc := super new.
        gc init.
        ^gc
    ]

    GameCharacter class >> getSuspects [
        | suspects |
        suspects := OrderedCollection new.
        suspects add: Gardener; add: Cook; add: Butler.
        ^suspects.
    ]


    "Parent class method to get singleton instance of a subclass"
    GameCharacter class >> getInstance [ 
        | instance |
        (instances includesKey: self) ifFalse: [
            instance := super new.
            instances at: self put: instance.
        ]
        ifTrue: [ instance := instances at: self ].
        [ instance init ]
            on: Exception
            do: [ :ex | "Do nothing if init does not exist in subclass" ].
        ^instance
    ]

    init [
        characterText := 'Default text'.
        characterLocation := nil.
        "clues := Set new "
    ]

    getCharacterLocation [ ^characterLocation ]
    getCharacterText [ ^characterText ]
    getName [ ^self class name asString ]

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

GameCharacter subclass: Gardener [
    Gardener class >> new [ 
        self error: 'Use getInstance class method'
    ]

    init [
        characterText := 
'While wandering through the royal garden, you come across a gardener. He is a 
middle-aged man with hard hands and dark spots on his clothes. ++ 
optionalText ++  His gaze shows signs of hard work and devotion to the royal 
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
work.’ The gardener seems nervous and a bit concerned about the situation.' 
 
    ]
] 

GameCharacter subclass: Cook [
    Cook class >> new [ 
        self error: 'Use getInstance class method'
    ]

    init [
        characterText := 
'You see a man wearing a white apron that is now slightly stained from working 
in the kitchen. There are traces of flour on his hands, and his face is 
slightly flushed from the heat. ++ optionalText ++  The cook seems to be a hard-
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
People are trying to work here!’ he says, irritated.'
    ]
] 

GameCharacter subclass: Butler [
    Butler class >> new [ 
        self error: 'Use getInstance class method'
    ]

    init [
        characterText := 
'You stand before an unassuming, delicately built butler, seemingly one of the 
quieter figures at the court throughout the years. His pale face seems to blend 
into the background, and his eyes, filled with a mysterious glow, scan the 
surroundings. The butler wears a modest uniform that betrays his low status at 
court, yet it’s carefully ironed and clean. ++ optionalText 

The butler stands still and awaits a signal. After a while, he opens his mouth 
and speaks calmly. ’What brings you here, noble lord?’ 

’Theft? It must have happened while I was busy bringing books to the library,’ 
he confesses with a hint of sadness in his voice. ’I had no idea what was 
happening in the courtyard or the commotion at the vault. It’s really strange. 
I can assure you that I had nothing to do with it. I worked most of the day, 
unfortunately, there is no one to testify about it.’'
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

’Just before the robbery, I noticed that the  ++ show char ++  was moving 
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
definitely not an exception.’'
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
’Some time ago, the  ++ show char ++  was at my tower. He asked me for a
certain toy, which I gave to the prince for his birthday. I gave him a couple
of crystals, when one of them breaks, the other one explodes with a loud bang
and turns into a thick cloud of smoke. Naturally, this toy is completely
harmless,’ he explained.

’The  ++ show char ++  was trying to excuse himself, saying he needed this toy
for a certain prank. It was just innocent fun, he told me,’ the wizard says
mockingly. ’It seemed quite suspicious, but it is above my pay grade,’ the
wizard says dismissingly.

’I said what I knew. Now it is the time for you to leave my tower,’ the wizard
says, clearly implying that the conversation is over.'

    ]
    giveMushrooms [
        gotMushrooms := true.
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

] 
