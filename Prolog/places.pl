:- module(places, [place/2, door/2, person/2]).  

place(vault, "place where diamond was stored but there is nothing anymore").
place(hall, "big, crouded, splendid chamber with big throne in the center").
place(kitchen, "cooking place with the dishes for the king").
place(butler_room, "there are keys to every room on the castle").
place(garden, "big garden, there are vegetables growing and is many flowers").
place(guard_house, "small chamber with lots of weapons").
place(wizard_house, "small, incredible place with many curious magic subjects").
place(servants_house, "place with bedrooms for all workforces, this is the place where cook, gardener and butler are sleeping. Each of them has 1 chest. You can go to these chest using command go_to_chest(<Person>)").
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

chest(butler).
chest(gardener).
chest(cook).

has_chests(servants_house).

