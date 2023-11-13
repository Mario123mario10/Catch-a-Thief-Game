:- module(places, [place/2, door/2, person/2, whose/2, all_desc_place/2, inside_place/2, are_inner_places/1]).  

place(vault, "place where diamond was stored but there is nothing anymore").
place(hall, "big, crouded, splendid chamber with big throne in the center").
place(kitchen, "cooking place with the dishes for the king").
place(butler_room, "there are keys to every room on the castle").
place(garden, "big garden, there are vegetables growing and is many flowers").
place(guard_house, "small chamber with lots of weapons").
place(wizard_house, "small, incredible place with many curious magic subjects").
place(servants_house, "place with bedrooms for all workforces").
place(forest, "big forest with wild animals and many plants and mushrooms").
place(courtyard, "center of the castle").
place(corridor, "A corridor connecting the kitchen and the royal bedroom, where the butler often stays").

person(hall, king).
person(kitchen, cook).
person(butler_room, butler).
person(garden, gardener).
person(guard_house, guard).
person(wizard_house, wizard).

door(hall, corridor).
door(hall, guard_house).
door(hall, courtyard).
door(hall, vault).

door(corridor, kitchen).
door(corridor, butler_room).

door(courtyard, garden).
door(courtyard, servants_house).
door(courtyard, wizard_house).



whose(butler, butler_chest).
whose(cook, cook_chest).
whose(gardener, gardener_chest).

all_desc_place(garden, ["You are in the castle garden. This is the place where the king likes to stay in spring and early summer.", "<\n>",
			"A trusted gardener makes sure that this place is full of colorful flowers and nutritious vegetables", "<\n>",
			"that the chef uses for his flavorous dishes.", "<\n>", "<\n>",
 			"There is a small pond in the center of the garden. One can get to its other side quite easily, as well as to the bottom.", "<\n>", 
               		"Inhabitants of the castle enjoy throwing things into the pond from time to time,", "<\n>",
               		"causing the gardener to be obliged to clean there. Sometimes he fishes out amazing treasures…", "<\n>"]).

all_desc_place(wizard_house, ["Here is the wizard’s house. It’s kind of like the kitchen… but different.", "<\n>",
			      "The wizard prepares various elixirs, magic items here; he even modifies some plants and animals,", "<\n>",
			      "so that they have miraculous features.", "<\n>",
			      "Hence, there is a fireplace, which allows him to cook elixirs, a small workshop to generate items,", "<\n>",
			      "collected animal parts and plants and a couple dozen books everywhere. Of course, a wand lies on the table.", "<\n>"]).

all_desc_place(forest, ["Shhh… did you hear that sound? It was kind of… magical. This forest is like none other.", "<\n>",
			"Some say you can hear ghost voices here, others say that this is a place where you can meet a werewolf.", "<\n>",
			"One thing is for sure - you can find unusual plants and animals here. Maybe they have some special use, who knows?", "<\n>"]).


all_desc_place(hall, ["You start in the main hall of the castle. You can get to several places from here. Many people move around here too.", "<\n>",
		      "Candle lanterns, even combined with thin beams of light from narrow windows, don’t make this place well lit.", "<\n>",
		      "If you saw anyone in the distance, you could even mistake somebody for somebody, especially at night!", "<\n>"]).


all_desc_place(kitchen, ["Welcome to the kitchen! The aroma of fresh herbs, cooked vegetables and flavorous meat dishes fills the air.", "<\n>",
			 "Inside this vast room there is a fireplace with a grate, separate fireplace to cook soup,", "<\n>",
			 "large oven for roasted dishes, and a bread oven. There are kitchen utensils hanging on the wall but one hook is empty…", "<\n>"]).


all_desc_place(guard_house, ["You are now in one of the castle guards’ rooms.", "<\n>",
			    "It’s not very spacious, most of the space is taken by a bed and simple wooden wardrobe.", "<\n>",
			    "Reportedly, he keeps some equipment in there. Stark and spartan style of the room suits the guard, without a doubt.", "<\n>"]).

all_desc_place(butler_room, ["You are now in the royal bedroom. You see a spacious room full of expensive furniture and numerous paintings.", "<\n>",
			     "There are huge mirrors and an ornate bed that is probably so expensive", "<\n>",
			     "that no servant could afford it even if he worked all his life for it.", "<\n>",
			     "This is the place where the butler often stays, serving the king in every way possible.", "<\n>"]).

all_desc_place(courtyard, ["You are in the courtyard. This is the middle of the castle from where you can get to many places.", "<\n>", "<\n>"]).  



/* places where key vault could be */

inside_place(garden, garden_pond).
inside_place(butler_room, mirror).
inside_place(kitchen, oven).

/* places where pouch could be */

inside_place(garden, rose_bushes).
inside_place(butler_room, bed).
inside_place(kitchen, bag_of_floor).

/* places where nothing could be */

inside_place(garden, king_sculpture).
inside_place(butler_room, wardrobe).
inside_place(kitchen, kitchen_unit). 


are_inner_places(kitchen).
are_inner_places(butler_room).
are_inner_places(garden).


