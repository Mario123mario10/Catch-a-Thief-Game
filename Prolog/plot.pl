:- module(plot, [went_to_servants_house/1, need_soil/1, went_to_butler_room/1, went_again_to_butler_room/1, butler_busy/1, choose_thief/0, prepare_diamond/0, check_quests/1]).

:- dynamic went_to_servants_house/1, need_soil/1, went_to_butler_room/1, went_again_to_butler_room/1, butler_busy/1, sus_ratio/2.
:- retractall(went_to_servants_house(_)), retractall(need_soil(_)), retractall(went_to_butler_room(_)), retractall(went_again_to_butler_room(_)), retractall(butler_busy(_)), retractall(sus_ratio(_, _)).



went_to_servants_house(no).
need_soil(no).
went_to_butler_room(no).
went_again_to_butler_room(no).
butler_busy(no).


sus_ratio(gardener, 0).
sus_ratio(cook, 0).
sus_ratio(butler, 0).


check_ratio(Thing, What) :-
        =(Thing, diamond),
        whose(Person, What),
        inc_sus_ratio(Person),!.

check_ratio(_, _).


choose(List, Elt) :-
        length(List, Length),
        random(0, Length, Index),
        nth0(Index, List, Elt).


choose_thief() :-
        choose([cook, butler, gardener], Thief),
        assert(thief(Thief)).


prepare_diamond() :-
        choose([cook, butler, gardener], Has_diamond),
        assert(has_diamond(Has_diamond)),
        whose(Has_diamond, Chest),
        assert(thing_at(diamond, Chest)).


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
