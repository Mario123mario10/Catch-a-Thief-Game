
:- module(people, [first_say/2]).


first_say(king, ["You stand before King Alaric III, a unique figure in this day and age.", "<\n>", 
		"His build exudes a majesty as great as his position in the kingdom.", "<\n>",
		"With a unique crown decorated with sparkling jewels on his head and a purple velvet cloak embroidered with gold,", "<\n>",
		"he seems to be the embodiment of royal power and pride.", "<\n>",
		"Nevertheless, his stern face shows traces not only of past victories, but also of sadness.", "<\n>","<\n>",
		"'I welcome you to our court,' comes the king's gruff voice.", "<\n>", "<\n>",
		"'Unfortunately, we are faced with a tragic situation. People have been stealing gold from the royal treasury for a long time.", "<\n>",
		"The sums were insignificant, so I never bothered to catch the thief.", "<\n>",
		"However, unexpectedly, someone dared to plunder our most important jewel - the diamond", "<\n>",
		"that decorated the crown of my ancestors for generations.' You can hear anger and anxiety in the ruler's voice.", "<\n>","<\n>",
		"Your task is to find the villain responsible for this insolent theft.", "<\n>",
		"Look for clues, three pieces of evidence is all I need to convict the criminal without upsetting the nobility.", "<\n>",
		"If you need any information, start by talking to the guard who was supposed to guard the vault yesterday.", "<\n>","<\n>",
		"'Unfortunately, he was distracted during the robbery by an unexpected noise in the courtyard,'", "<\n>",
 		"the king explains, with a determined look on his face.", "<\n>","<\n>",
		"'The thief was almost caught red-handed, so we managed to react quickly and put the entire castle on lockdown.", "<\n>",
		"Whoever the thief is, he’s still in the castle.", "<\n>",
		"Put all of my servants under suspicion, except for my loyal guard who has served me for many years and holds me in high regard.", "<\n>",
		"I am sure he would not betray me for money, his honor wouldn’t allow that.'", "<\n>", "<\n>",
		"You will find him in a room to your left"]).

first_say(guard, ["You are standing in front of the king's loyal guard. His figure reveals that in his youth, he was an outstanding knight.", "<\n>",
		  "However, the passage of time has left its mark on this once-powerful man.", "<\n>",
		  "The guard wears chain mail and a breastplate and carries a halberd.", "<\n>",
		  "His armor and weapons are in excellent condition, showing his care for his equipment.", "<\n>","<\n>",
		  "'Greetings, noble knight,' the guard says, somewhat thoughtfully.", "<\n>", "<\n>",
		  "'Please let me keep my name to myself. I don't want this disaster to taint my noble family.", "<\n>",
		  "Lean in, and I'll tell you what I know,' he continues, encouraging you to come closer.", "<\n>","<\n>",
		  "'Just before the robbery, I noticed that the [gardener/cook/butler] was moving around restlessly in the throne room.", "<\n>",
	 	  "I had to loudly remind him of his duties because he stood there as if paralyzed and was examining the vault door from afar,'", "<\n>",
		  "the guard whispers.", "<\n>", "<\n>",
		  "'As you probably already know, I was standing guard over the vault and keeping watch until I heard a disturbing bang", "<\n>",
		  "and immediately ran into the courtyard. However, I found nothing but thick smoke. It was definitely a diversion.", "<\n>",
		  "When I was returning to my post, I unexpectedly came across a thief while he was robbing the treasury.", "<\n>",
		  "He managed to escape me, and unfortunately, I did not see who he was; my eyesight is not as good as it used to be.'", "<\n>",
		  "The guard finishes, expressing his helplessness.", "<\n>", "<\n>",
		  "'Maybe he lost something while escaping from the vault. You better check it out.", "<\n>",
		  "No crime is perfect, and this one is definitely not an exception.'"]).

first_say(wizard, ["You stand before the court wizard of King Alaric III, an old man with observant eyes.", "<\n>",
		   "He has a long beard and a distinctive black sorcerer's hat.", "<\n>",
		   "He is well known throughout the kingdom, both for his astonishing sorcery and his arrogance, as well as his incredible wisdom.", "<\n>",
		   "At first glance, it is clear that even the king himself would not dare to go against him.", "<\n>", "<\n>",
		   "'Hello, soldier,' says the wizard with a look of sneer.", "<\n>", "<\n>",
		   "'It appears that you have a reason to visit my humble chambers. I understand that you have come to ask me about the king's jewel.", "<\n>",
		   "As for my possible role in this matter, allow me to explain... I have no desire nor time to play vault busting.", "<\n>",
		   "Stealing gold and diamonds from monarchs would just be boring. Besides, I have no use for these kinds of riches.'", "<\n>", "<\n>",
		   "However, the wizard continues,", "<\n>", "<\n>",
		   "'I was in my tower when I heard that noise in the courtyard. I know exactly who was responsible,", "<\n>",
		   "and I know what was used to perform this. In fact, I gave him that item in return for a favor.", "<\n>",
		   "I'm willing to share this information with you, but I won't do it for free.", "<\n>",
		   "You must collect X mushrooms from the magical forest surrounding the castle, and then I will be willing to share my knowledge.'", "<\n>",
		   "The wizard expresses his tiredness in his tone of voice."]).

first_say(gardener, ["While wandering through the royal garden, you come across a gardener.", "<\n>",
		     "He is a middle-aged man with hard hands and dark spots on his clothes.", "<\n>",
		     "<wound>", "<\n>",
		     "His gaze shows signs of hard work and devotion to the royal garden.", "<\n>",
		     "Surprised by your presence, the gardener puts down his tools and approaches you.", "<\n>", "<\n>",
		     "'What kind of surprise is this?' he asks, leaning on his spade.", "<\n>","<\n>",
		     "'The garden is currently closed for visitors; you shouldn't be here without the king's permission.'", "<\n>", "<\n>",
		     "Then he continues with a little concern,", "<\n>", "<\n>",
		     "'To tell you the truth, I don't know anything about any robbery. I've been working here since dawn,", "<\n>",
		     "and I treat these plants like my own children. I haven't noticed anything suspicious. It must be some kind of mistake.", "<\n>",
		     "Maybe the diamond will come back on its own; things like that happen.", "<\n>",
		     "If you have any questions, we can discuss them with the king. But let me get back to my work.'", "<\n>",
		     "The gardener seems nervous and a bit concerned about the situation."]).

first_say(cook, ["You see a man wearing a white apron that is now slightly stained from working in the kitchen.", "<\n>",
		 "There are traces of flour on his hands, and his face is slightly flushed from the heat.", "<\n>",
		 "<wound>", "<\n>",
		 "The cook seems to be a hard-working and busy man who devotes himself to his role in the royal kitchen with passion.", "<\n>",
		 "When you enter the kitchen as a knight, the cook seems surprised by your presence.", "<\n>",
		 "He puts down the knife he was sharpening and shifts the egg from one hand to the other, suddenly becoming more careful.", "<\n>","<\n>",
		 "'What are you looking for?' asks the cook, a little concerned in his voice.", "<\n>","<\n>",
		 "'This place is closed; you shouldn't be here without the king's permission.'", "<\n>",
		 "His reaction suggests he's irritated by your presence in the kitchen and wonders what brought you there.", "<\n>",
		 "'Oh? It's about the robbery?' says the cook with a bit of shock.", "<\n>", "<\n>",
		 "'Fair enough, if you must snoop around, then do what you must. Just don't get in my way. People are trying to work here!'", "<\n>",
		 "he says, irritated."]).

first_say(butler, ["You stand before an unassuming, delicately built butler, seemingly one of the quieter figures at the court throughout the years.", "<\n>",
		   "His pale face seems to blend into the background, and his eyes, filled with a mysterious glow, scan the surroundings.", "<\n>",
		   "The butler wears a modest uniform that betrays his low status at court, yet it's carefully ironed and clean.", "<\n>",
		   "<wound>","<\n>",
		   "The butler stands still and awaits a signal. After a while, he opens his mouth and speaks calmly.", "<\n>", "<\n>",
		   "'What brings you here, noble lord?'", "<\n>",
		   "'Theft? It must have happened while I was busy bringing books to the library,' he confesses with a hint of sadness in his voice.", "<\n>", "<\n>", 
		   "'I had no idea what was happening in the courtyard or the commotion at the vault. It's really strange.", "<\n>",
		   "I can assure you that I had nothing to do with it. I worked most of the day, unfortunately, there is no one to testify about it.'"]).

		   








