:- module(plot, [went_to_servants_house/1, need_soil/1, went_to_butler_room/1, went_again_to_butler_room/1, butler_busy/1, choose_thief_by_machine/0, prepare_diamond/0, check_quests/1, sus_ratio/2, thief/1, has_wound/1, prepare_wound/0, inc_sus_ratio/1, is_vault_key/1, is_pouch/1, prepare_objects/0, prepare_guard_sus/0, guard_sus/1]).

:- dynamic went_to_servants_house/1, need_soil/1, went_to_butler_room/1, went_again_to_butler_room/1, butler_busy/1, sus_ratio/2, thief/1, has_wound/1, is_vault_key/1, is_pouch/1, guard_sus/1.
:- retractall(went_to_servants_house(_)), retractall(need_soil(_)), retractall(went_to_butler_room(_)), retractall(went_again_to_butler_room(_)), retractall(butler_busy(_)), retractall(sus_ratio(_, _)), retractall(thief(_)), retractall(has_wound(_)), retractall(is_vault_key(_)), retractall(is_pouch(_)), retractall(guard_sus(_)).

:- [world].

went_to_servants_house(no).
need_soil(no).
went_to_butler_room(no).
went_again_to_butler_room(no).
butler_busy(no).


sus_ratio(gardener, 0).
sus_ratio(cook, 0).
sus_ratio(butler, 0).




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
	assert(thing_at(vault_key, Key_place)). 	

prepare_guard_sus() :-
	choose([cook, butler, gardener], Is_sus),
	assert(guard_sus(Is_sus)).	

check_quests(Place) :-
        =(Place, servants_house),
        went_to_servants_house(no),
        assert(went_to_servants_house(yes)),
        retract(went_to_servants_house(no)),!.


check_quests(Place) :-
        =(Place, garden),
        went_to_butler_room(yes),
        need_soil(no),
        assert(need_soil(yes)),
        retract(need_soil(no)),!.

check_quests(Place) :-
        =(Place, butler_room),
        went_to_servants_house(yes),
        went_to_butler_room(no),
        assert(went_to_butler_room(yes)),
        retract(went_to_butler_room(no)),!.

check_quests(Place) :-
        =(Place, butler_room),
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




