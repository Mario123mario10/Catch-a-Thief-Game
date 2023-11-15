:- module(plot, [went_to_servants_house/1, need_soil/1, went_to_butler_room/1, went_again_to_butler_room/1, butler_busy/1, choose_thief_by_machine/0, prepare_diamond/0, check_quests/1, sus_ratio/2, thief/1, has_wound/1, prepare_wound/0, inc_sus_ratio/1, is_vault_key/1, is_pouch/1, prepare_objects/0, prepare_sus/0, guard_sus/1, wizard_sus/1, needed_mushrooms/1, prepare_needed_mushrooms/0, went_to_wizard_house/1, gave_mushrooms/1, went_to_vault/1, sec_part/1, third_part/1, took_sec_part/1, took_third_part/1]).

:- dynamic went_to_servants_house/1, need_soil/1, went_to_butler_room/1, went_again_to_butler_room/1, butler_busy/1, sus_ratio/2, thief/1, has_wound/1, is_vault_key/1, is_pouch/1, guard_sus/1, wizard_sus/1, needed_mushrooms/1, went_to_wizard_house/1, gave_mushrooms/1, sec_part/1, third_part/1, without_sec_part_places/1, took_sec_part/1, took_third_part/1.

:- retractall(went_to_servants_house(_)), retractall(need_soil(_)), retractall(went_to_butler_room(_)), retractall(went_again_to_butler_room(_)), retractall(butler_busy(_)), retractall(sus_ratio(_, _)), retractall(thief(_)), retractall(has_wound(_)), retractall(is_vault_key(_)), retractall(is_pouch(_)), retractall(guard_sus(_)), retractall(wizard_sus(_)), retractall(needed_mushrooms(_)), retractall(went_to_wizard_house(_)), retractall(gave_mushrooms(_)), retractall(went_to_vault(_)), retractall(sec_part(_)), retractall(third_part(_)), retractall(without_sec_part_places(_)), retractall(took_sec_part(_)), retractall(took_third_part(_)).


:- [world].

went_to_servants_house(no).
need_soil(no).
went_to_butler_room(no).
went_again_to_butler_room(no).
butler_busy(no).
went_to_wizard_house(no).
went_to_vault(no).
gave_mushrooms(no).
took_sec_part(no).
took_third_part(no).

sus_ratio(gardener, 0).
sus_ratio(cook, 0).
sus_ratio(butler, 0).

without_sec_part_places([]).

choose(List, Elt) :-
        length(List, Length),
        random(0, Length, Index),
        nth0(Index, List, Elt).


choose_thief_by_machine() :-
        choose([cook, butler, gardener], Thief),
        assert(thief(Thief)).


prepare_diamond() :-
        choose([cook, butler, gardener], Has_diamond),
        assert(has_diamond(Has_diamond)),
        whose(Has_diamond, Chest),
        assert(thing_at(diamond, Chest)).

prepare_wound() :-
	choose([cook, butler, gardener], Has_wound),
	assert(has_wound(Has_wound)).

prepare_objects() :-
	choose([rose_bushes, bed, bag_of_floor], Pouch_place),
	assert(is_pouch(Pouch_place)),
	assert(thing_at(pouch, Pouch_place)),
	
	choose([garden_pond, mirror, oven], Key_place),
	assert(is_vault_key(Key_place)),
	assert(thing_at(vault_key, Key_place)), 
	
	choose([hall, kitchen, guard_house, wizard_house, servants_house, forest, courtyard, corridor], Sec_part_place),
	assert(sec_part(Sec_part_place)),
	assert(thing_at(sec_part, Sec_part_place)),
	
	add_to_third_part_list([hall, kitchen, guard_house, wizard_house, servants_house, forest, courtyard, corridor]),
	without_sec_part_places(List),
	choose(List, Third_part_place),
	assert(third_part(Third_part_place)),
	assert(thing_at(third_part, Third_part_place)).

add_to_third_part_list([]).
	
add_to_third_part_list(Places_list) :-
	thing_at(sec_part, Sec_part_place),
	[H|T] = Places_list,
	\+ =(H, Sec_part_place),
	without_sec_part_places(List), 
	append(List, [H], New_list),

	retract(without_sec_part_places(_)),
	assert(without_sec_part_places(New_list)),

	add_to_third_part_list(T),!.

add_to_third_part_list(Places_list) :-
	[_|T] = Places_list,
	add_to_third_part_list(T),!.
	

prepare_sus() :-
	choose([cook, butler, gardener], Guard_sus),
	assert(guard_sus(Guard_sus)),
	choose([cook, butler, gardener], Wizard_sus),	
	assert(wizard_sus(Wizard_sus)).

prepare_needed_mushrooms() :-
	random(5, 15, Number),
	assert(needed_mushrooms(Number)).

check_quests(servants_house) :-
        went_to_servants_house(no),
        assert(went_to_servants_house(yes)),
        retract(went_to_servants_house(no)),!.


check_quests(garden) :-
        went_to_butler_room(yes),
        need_soil(no),
        assert(need_soil(yes)),
        retract(need_soil(no)),!.

check_quests(butler_room) :-
        went_to_servants_house(yes),
        went_to_butler_room(no),
        assert(went_to_butler_room(yes)),
        retract(went_to_butler_room(no)),!.

check_quests(butler_room) :-
        need_soil(yes),
        went_again_to_butler_room(no),
        assert(went_again_to_butler_room(yes)),
        retract(went_again_to_butler_room(no)).


check_quests(_).


inc_sus_ratio(Person) :-
        sus_ratio(Person, Ratio),
        \+ Ratio is 2,
        New_ratio is Ratio + 1,
        retract(sus_ratio(Person, Ratio)),
        assert(sus_ratio(Person, New_ratio)),
        write("Now suspiciousness ratio of "), write(Person), write(" is equal "), write(New_ratio), write("."),!, nl.

inc_sus_ratio(Person) :-
        sus_ratio(Person, Ratio),
        New_ratio is Ratio + 1,
        retract(sus_ratio(Person, Ratio)),
        assert(sus_ratio(Person, New_ratio)),
        write("Suspiciousness ratio of "), write(Person), write(" is equal 3."), nl,
        write("Now you know that "), write(Person), write(" is the thief!"), !, nl.

inc_sus_ratio(_) :-
        write("You can't increase the non-suspect ratio.").




