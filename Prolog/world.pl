:- module(world, [is_locked/1, holding/1, able_to_talk/1, thing_at/2, first_time/1, is_person/1, is_quest/1, wound/2, having_mushrooms/1, i_am_at/1, i_was_at/1, i_am_at_inner_place/1, places_list/1, belongs/2, end_time/1, start_time/1, time_up/1, hold_diamond_to_talk/1, hold_vault_key_to_talk/1, hold_pouch_to_talk/1]).


:- dynamic is_locked/1, holding/1, thing_at/2, first_time/1, chosen_thief/1, having_mushrooms/1, i_am_at/1, i_was_at/1, i_am_at_inner_place/1, places_list/1, end_time/1, start_time/1, time_up/1, hold_diamond_to_talk/1, hold_vault_key_to_talk/1, hold_pouch_to_talk/1.

:- retractall(is_locked(_)), retractall(thing_at(_, _)), retractall(holding(_)), retractall(chosen_thief(_)), retractall(having_mushrooms(_)), retractall(i_am_at(_)), retractall(i_was_at(_)), retractall(i_am_at_inner_place(_)), retractall(places_list(_)), retractall(end_time(_)), retractall(start_time(_)), retractall(time_up(_)), retractall(hold_diamond_to_talk(_)), retractall(hold_vault_key_to_talk(_)), retractall(hold_pouch_to_talk(_)).


/* with new quests add there places, where quests are doing by the players*/ 

time_up(no).

having_mushrooms(0).

i_am_at(hall).
i_was_at(hall).


places_list([]).


is_quest(royal_bedroom).
is_quest(garden).
is_quest(servants_house).
is_quest(forest).
is_quest(wizard_house).
is_quest(vault).
is_quest(guard_house).

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
thing_at(keys, royal_bedroom).
thing_at(mushroom, forest).
thing_at(handle, vault).


first_time(vault).
first_time(hall).
first_time(kitchen).
first_time(royal_bedroom).
first_time(garden).
first_time(guard_house).
first_time(wizard_house).
first_time(servants_house).
first_time(forest).
first_time(courtyard).
first_time(corridor).

is_first_say(cook).
is_first_say(butler).
is_first_say(gardener).
is_first_say(guard).
is_first_say(wizard).
is_first_say(king).

is_person(hall).
is_person(kitchen).
is_person(royal_bedroom).
is_person(garden).
is_person(guard_house).
is_person(wizard_house).



wound(gardener, "There is a fresh wound on his forearm, as if something cut him.").
wound(cook, "You notice a fresh blade wound on his hand.").
wound(butler, "There is a fresh wound on his left cheek; he cut himself with something.").


belongs(rake, gardener).
belongs(ladle, cook).
belongs(duster, butler).



