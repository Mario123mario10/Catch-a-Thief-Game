:- module(world, [is_locked/1, holding/1, able_to_talk/1, thing_at/2, first_time/1, is_first_say/1, is_person/1, is_quest/1, wound/2]).

:- dynamic is_locked/1, holding/1, thing_at/2, first_time/1, is_first_say/1, chosen_thief/1.
:- retractall(is_locked(_)), retractall(thing_at(_, _)), retractall(holding(_)), retractall(chosen_thief(_)).


/* with new quests add there places, where quests are doing by the players*/ 

is_quest(butler_room).
is_quest(garden).
is_quest(servants_house).




is_locked(butler_chest).
is_locked(gardener_chest).
is_locked(cook_chest).
is_locked(servants_house).

able_to_talk(butler).
able_to_talk(gardener).
able_to_talk(cook).
able_to_talk(king).
able_to_talk(wizard).
able_to_talk(guard).

thing_at(soil, garden).
thing_at(keys, butler_room).

first_time(vault).
first_time(hall).
first_time(kitchen).
first_time(butler_room).
first_time(garden).
first_time(guard_house).
first_time(wizard_house).
first_time(servants_house).
first_time(forest).
first_time(courtyard).

is_first_say(cook).
is_first_say(butler).
is_first_say(gardener).
is_first_say(guard).
is_first_say(wizard).
is_first_say(king).

is_person(hall).
is_person(kitchen).
is_person(butler_room).
is_person(garden).
is_person(guard_house).
is_person(wizard_house).



wound(gardener, "There is a fresh wound on his forearm, as if something cut him.").
wound(cook, "You notice a fresh blade wound on his hand.").
wound(butler, "There is a fresh wound on his left cheek; he cut himself with something.").

