
:- module(people, [first_say/2, after_mushrooms/2, before_mushrooms/2, end_talk/2, completed_item/2, vault_key_desc/2, pouch_desc/2, diamond_desc/2, unrelated_quest_desc/2, guard_sus_desc/2, wizard_sus_desc/2]).


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
		"I am sure he would not betray me for money, his honor wouldn’t allow that.'", "<\n>"]).

first_say(guard, ["You are standing in front of the king's loyal guard. His figure reveals that in his youth, he was an outstanding knight.", "<\n>",
		  "However, the passage of time has left its mark on this once-powerful man.", "<\n>",
		  "The guard wears chain mail and a breastplate and carries a halberd.", "<\n>",
		  "His armor and weapons are in excellent condition, showing his care for his equipment.", "<\n>","<\n>",
		  "'Greetings, noble knight,' the guard says, somewhat thoughtfully.", "<\n>", "<\n>",
		  "'Please let me keep my name to myself. I don't want this disaster to taint my noble family.", "<\n>",
		  "Lean in, and I'll tell you what I know,' he continues, encouraging you to come closer.", "<\n>","<\n>",
		  "'Just before the robbery, I noticed that the ", "<guard_sus>", " was moving around restlessly in the throne room.", "<\n>",
	 	  "I had to loudly remind him of his duties because he stood there as if paralyzed and was examining the vault door from afar,'", "<\n>",
		  "the guard whispers.", "<\n>", "<\n>",
		  "'As you probably already know, I was standing guard over the vault and keeping watch until I heard a disturbing bang", "<\n>",
		  "and immediately ran into the courtyard. However, I found nothing but thick smoke. It was definitely a diversion.", "<\n>",
		  "When I was returning to my post, I unexpectedly came across a thief while he was robbing the treasury.", "<\n>",
		  "He managed to escape me, and unfortunately, I did not see who he was; my eyesight is not as good as it used to be.'", "<\n>",
		  "The guard finishes, expressing his helplessness.", "<\n>", "<\n>",
		  "'Maybe he lost something while escaping from the vault. You better check it out.", "<\n>",
		  "No crime is perfect, and this one is definitely not an exception.'", "<\n>", "<\n>",
		  "Now you can go to the ", "<guard_sus>", " and ask him what he was doing near the throne room."]).

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
		   "You must collect ", "<number>", " mushrooms from the magical forest surrounding the castle, and then I will be willing to share my knowledge.'", "<\n>",
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

		   

after_mushrooms(wizard, ["When you give the wizard magic mushrooms from the enchanted forest, his eyes light up with momentary delight.", "<\n>",
			 "He gently takes them from your hand and places them on the table next to him,", "<\n>",
			 "then reaches for his wand and uses his finger to carve out the runes on one of the mushrooms.", "<\n>",
			 "Everything around you begins to glow with magical light, and the wizard carefully studies the ingredients he has just obtained.", "<\n>", "<\n>",
			 "'Deep in the forest, in the shade of the trees, these mushrooms are a source of great power,'", "<\n>",
			 "the wizard says solemnly and freezes for a second. After a while, he wakes up as if from a trance.", "<\n>", "<\n>",
			 "'Now, what I promised.' He shrugs and says,", "<\n>","<\n>",
			 "'Some time ago, the ", "<wizard_sus>", " was at my tower.", "<\n>",
			 "He asked me for a certain toy, which I gave to the prince for his birthday.", "<\n>",
			 "I gave him a couple of crystals, when one of them breaks, the other one explodes with a loud bang", "<\n>",
			 "and turns into a thick cloud of smoke. Naturally, this toy is completely harmless,' he explained.", "<\n>", "<\n>",
			 "'The ", "<wizard_sus>", " was trying to excuse himself, saying he needed this toy for a certain prank.", "<\n>",
			 "It was just innocent fun, he told me,' the wizard says mockingly.", "<\n>",
			 "'It seemed quite suspicious, but it is above my pay grade,' the wizard says dismissingly.", "<\n>", "<\n>",
			 "'I said what I knew. Now it is the time for you to leave my tower.' the wizard says, clearly implying that the conversation is over.","<\n>","<\n>",
			 "You know that the guard was distracted during the theft, this could be the cause.","<\n>",
			 "Now you can go to the ", "<wizard_sus>", " and find out why or whether he actually had something to do with it."]).




before_mushrooms(wizard, ["'I'm still waiting for my mushrooms, I won't tell you anything until I get my mushrooms.' said the wizard stubbornly."]).

end_talk(wizard, ["'You found who was guilty? No? I don't know anything else so go and leave me alone. I have many things to do.' said the wizard."]).



completed_item(gardener, ["When you hand over the rake with the rake stick you found to the gardener, his expression becomes more concerned.", "<\n>",
			  "Without a word, he takes the tool and examines it carefully, holding it strangely.", "<\n>",
			  "After a moment, he looks at you with a look of uncertainty in his eyes.", "<\n>", "<\n>",
			  "'Did... did you find this near the vault?' he asks carefully, trying to remain calm.", "<\n>", "<\n>",
			  "'It's unbelievable. It's really my rake stick, but I don't know how it could have gotten there. I was not there during the robbery, I swear.'", "<\n>",
			  "The gardener seems nervous. ", "<\n>", "<\n>",
			  "'I must have lost it while I was going to the garden, but I've never been inside the vault. It must be a bad omen."]).



completed_item(cook, ["'What is this... This is my ladle!' the cook exclaims, surprised.","<\n>", 
		      "'How did this happen? Where did you find her? It must have been the work of that thief of yours!'","<\n>",
		      "You notice sweat on his forehead. It's hard to tell if it's because of the heat or anxiety."]).



completed_item(butler, ["When the butler notices the duster, his eyes linger over the object for a moment. He doesn't seem surprised or concerned.","<\n>",
			"Instead, after a moment's cold glance at the duster, he turns to you and says with unflinching seriousness:","<\n>","<\n>",
			"'This is certainly interesting evidence, noble sir. However, I am not sure how it can help clarify this matter.","<\n>",
			"I can assure you that such a duster is widely available, and many of us use it to perform everyday duties."]).


vault_key_desc(gardener, ["When you show the gardener the key to the vault you found, his eyes become wide with surprise.", "<\n>",
			  "Without a word, he takes the key and examines it carefully, turning it over in his hands.", "<\n>","<\n>",
			  "'This... this is the key to the vault,' the gardener whispers, his voice shaking with surprise.", "<\n>","<\n>",
			  "I don't know how it ended up in the garden. I'm sure I had nothing to do with it.'", "<\n>",
			  "The gardener seems concerned with the situation.", "<\n>", "<\n>",
			  "'It can't be a coincidence. How am I this unlucky,' he says anxiously.", "<\n>", "<\n>"]).

vault_key_desc(cook, ["When you show the chef the key to the vault, his reaction is immediate. Outrage flashes across his face.", "<\n>",
		      "'What is this doing here? What if it ended up in one of the dishes? I have a reputation to uphold; I can't afford turmoil in my kitchen.'", "<\n>","<\n>",
		      "After a while, he realizes what kind of key it is.", "<\n>","<\n>",
		      "'This key... This key is from the vault,' mutters the cook with fear in his eyes.", "<\n>","<\n>",
		      "'Where did you get it? It's not what you think!' His reaction suggests that he is baffled by the key's presence."]).


vault_key_desc(butler, ["'Ah, the key to the treasury. Yes, it is indeed an unusual discovery to stumble upon.", "<\n>",
			"I am entrusted with various responsibilities in the king's quarters and the meticulous care for the royal chambers.", "<\n>",
			"However, the presence of the treasury key here seems rather out of place.", "<\n>",
			"It might have been misplaced by the thief when he was panicking. I assure you, it wasn't my intention to have it here,' said the butler calmly."]).

pouch_desc(gardener, ["'This... this is impossible!' the gardener exclaims with horror in his voice.", "<\n>",
		      "'How did this money end up in my garden? I am surely cursed.' The gardener seems completely stunned and terrified by the situation.", "<\n>",
		      "His reaction suggests that he is completely devastated and has no idea how this could all have happened."]).

pouch_desc(cook, ["'I have no idea how this gold ended up in my kitchen,' says the cook with horror in his voice.", "<\n>","<\n>",
		  "'I don't know whose fault it is, but the kitchen is no place for thieves to hide their treasures!' continues the cook.", "<\n>",
		  "His hands begin to shake, and his expression shows true terror.", "<\n>", "<\n>",
		  "'I hope I'm not on the suspect list...' he exclaims with hope in his voice, looking at you, hoping for understanding."]).

pouch_desc(butler, ["When you present him with the purse of money, the butler seems surprised but maintains his composure. He says with skillful conviction:", "<\n>", "<\n>", 
		    "I have no idea how this purse could have ended up in the king's bedroom.", "<\n>",
		    "Of course, I am responsible for making his majesty's bed every day so I am here often, but that does not mean I am guilty.", "<\n>",
		    "Throughout my years of service at court, I have had no problems with behavior or duties.", "<\n>",
		    "It must be some kind of intrigue or an attempt to discredit me.'"]).

diamond_desc(gardener, ["The gardener turns pale. His eyes widen in horror, and his hands begin to tremble. He seems completely stunned.", "<\n>", "<\n>",
			"'No! It's not me! I don't know how that diamond ended up in my chest!' the gardener exclaims, trying to defend himself against the accusations.", "<\n>", "<\n>",
		        "'It must be some kind of trap or plot. I don't know which god I offended to be punished so severely! I had nothing to do with it, really!'"]).



diamond_desc(cook, ["He turns even redder than before, and his eyes widen with surprise.", "<\n>", "<\n>",
		    "'It... it's not what you think!' the cook groans with fear in his voice, his tone trembling with emotion.", "<\n>", "<\n>",
		    "'I have no idea how this diamond ended up in my chest. That must be some kind of intrigue! Someone is trying to cheat you!'", "<\n>",
		    "The cook continues, becoming more and more shaky as he considers the situation.", "<\n>", "<\n>",
		    "Suddenly, the cook starts accusing others in a frenzy.", "<\n>", "<\n>",
		    "'It's the butler! He's the only one who has the key to our rooms!' he shouts, trying to shift the blame to another servant in the castle.'"]).


diamond_desc(butler, ["The butler looks at the diamond for a moment, his gaze seemingly astonished.", "<\n>",
		      "Then he shifts his gaze to you and says with a slight tremble in his voice,", "<\n>", "<\n>",
		      "'It's... it's the king's diamond. How did you find it? I have no idea how it came to be in my chest.", "<\n>",
		      "It must be some mistake or intrigue. I swear that I had nothing to do with it. Someone is setting me up!'", "<\n>", "<\n>",
		      "The gardener seems completely devastated and terrified of the situation.", "<\n>",	
		      "You see honest desperation and remorse in his eyes, trying to convince you of his innocence."]).


unrelated_quest_desc(gardener, ["'It's very interesting,' says the gardener calmly.", "<\n>","<\n>",
				"'Why did you come to me with this? I'm not a brainy fellow. I probably won't be any help to you.'"]).

unrelated_quest_desc(cook, ["'It's not really about me, now is it?' said the annoyed cook.", "<\n>", "<\n>", 
			    "'You'd better leave me in peace; I have cookery to run.'"]).


unrelated_quest_desc(butler, ["'That is, of course, mighty interesting, sir! Is that all? If so, I must resume my duties.'"]).


guard_sus_desc(gardener, ["'What did he say?' says the gardener with an expression of surprise.", "<\n>", "<\n>",
			  "'Why was the guard watching me? Isn't a servant allowed to dream about his king's treasures?' He waits for a moment for your answer and then continues,", "<\n>", "<\n>",
			  "'He chased me away after a while anyway. I didn't see anything interesting. Why would you bring it up?'"]).


guard_sus_desc(cook, ["'I'm sorry if that looked suspicious,' the cook says, a little sheepishly.", "<\n>", "<\n>",
		      "'But it was only because I was curious about what was going on. All these rumors about gold and diamonds in the vault had everyone thinking.", "<\n>",
		      "I thought about how gold could enhance my dishes. I never had any intention of stealing; it's not my style.'"]).


guard_sus_desc(butler, ["'I understand the guard is concerned and is trying to solve this case,' the butler says, trying to remain calm.", "<\n>", "<\n>",
			"'I'm sure that on that day, I was moving around the castle as I always do.", "<\n>",
			"If the guard claims to have seen me at the vault door, it could have been a chance encounter.", "<\n>",
			"But no one saw me enter the vault or try to open the door, am I correct? If so, that's just speculation.'"]).


wizard_sus_desc(gardener, ["'Oh, the crystals!' says the gardener with a look of embarrassment.", "<\n>", "<\n>",
			   "'It was just an innocent joke on the guard. For so many years, he has been guarding our castle, and nothing ever happens here.", "<\n>",
			   "I just wanted to give him some entertainment! ", "<\n>",
			   "How could I have known that it would distract him from the vault just when the thief was waiting for an opportunity?'", "<\n>",
			   "The gardener tries to convince me that it was all innocent fun, but his expression suggests that he realizes", "<\n>",
			   "the seriousness of the situation and the erroneous mistake he made."]).


wizard_sus_desc(cook, ["'I don't understand why the wizard pointed at me with that toy,' the cook says with disapproval in his voice.", "<\n>", "<\n>",
		       "It was just a toy he gave me. It looked fun, so I used it. I had no idea it would cause such a riot in the yard.", "<\n>",
		       "It wasn't my intention to distract the guard. I certainly had no intention of helping any criminal.'"]).

wizard_sus_desc(butler, ["'It's true that I received this crystal toy from the wizard at my request,' the butler admits.", "<\n>", "<\n>",
			 "'I had some misunderstanding with this guard, and I was planning to use this toy as a kind of joke or a way to vent my dissatisfaction.", "<\n>",
			 "However, I had no idea that this toy could cause such turmoil and contribute to the theft.", "<\n>",
			 "I am innocent, and I am ready to explain the entire incident to the king.'"]).


