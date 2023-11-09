:- module(places, [place/2, door/2, person/2, whose/2, all_desc_place/2]).  

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

person(hall, king).
person(kitchen, cook).
person(butler_room, butler).
person(garden, gardener).
person(guard_house, guard).
person(wizard_house, wizard).

/*person(servants_house, cook).
person(servants_house, butler).
person(servants_house, gardener).*/
 
door(vault, hall).
door(hall, butler_room).
door(butler_room, courtyard).
door(courtyard, garden).
door(courtyard, servants_house).
door(guard_house, hall).
door(courtyard, wizard_house).
door(hall, kitchen).
door(wizard_house, forest). 

whose(butler, butler_chest).
whose(cook, cook_chest).
whose(gardener, gardener_chest).

/*
all_desc_place(vault, "a").
all_desc_place(hall, "big, crouded, splendid chamber with big throne in the center").
all_desc_place(kitchen, "cooking place with the dishes for the king").
all_desc_place(butler_room, "there are keys to every room on the castle").
all_desc_place(garden, "You are in the castle garden.\n
	       This is the place where the king likes to stay in spring and early summer.\n
	       A trusted gardener makes sure that this place is full of colorful flowers and nutritious vegetables \n
	       that the chef uses for his flavorous dishes.\n\n
	       There is a small pond in the center of the garden.\n
	       One can get to its other side quite easily, as well as to the bottom.\n
	       Inhabitants of the castle enjoy throwing things into the pond from time to time,\n
	       causing the gardener to be obliged to clean there. Sometimes he fishes out amazing treasures…").



all_desc_place(guard_house, "small chamber with lots of weapons").
all_desc_place(wizard_house, "small, incredible place with many curious magic subjects").
all_desc_place(servants_house, "place with bedrooms for all workforces").
all_desc_place(forest, "big forest with wild animals and many plants and mushrooms").
all_desc_place(courtyard, "center of the castle").

print_s(_, []).
print_s(Place, [H|L]):-
	write(H),nl,
	print_s(Place, L).

print_s(garden, ["You are in", "the castle garden"]).
*/
all_desc_place(garden, ["You are in the castle garden.", "<\n>",
			"This is the place where the king likes to stay in spring and early summer.", "<\n>",
			"A trusted gardener makes sure that this place is full of colorful flowers and nutritious vegetables", "<\n>",
			"that the chef uses for his flavorous dishes.", "<\n>", "<\n>",
 			"There is a small pond in the center of the garden.", "<\n>",
               		"One can get to its other side quite easily, as well as to the bottom.", "<\n>", 
               		"Inhabitants of the castle enjoy throwing things into the pond from time to time,", "<\n>",
               		"causing the gardener to be obliged to clean there. Sometimes he fishes out amazing treasures…"]).

