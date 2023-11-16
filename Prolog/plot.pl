:- module(plot, [went_to_servants_house/1, need_soil/1, went_to_royal_bedroom/1, went_again_to_royal_bedroom/1, butler_busy/1, prepare_diamond/0, check_quests/1, sus_ratio/2, thief/1, has_wound/1, prepare_wound/0, inc_sus_ratio/1, is_vault_key/1, is_pouch/1, guard_sus/1, wizard_sus/1, needed_mushrooms/1, prepare_needed_mushrooms/0, went_to_wizard_house/1, gave_mushrooms/1, went_to_vault/1, sec_part/1, third_part/1, took_sec_part/1, took_third_part/1, thief_tool/1, prepare_pouch/0, prepare_vault_key/0, prepare_tool/0, prepare_guard_sus/0, prepare_wizard_sus/0, prepare_parts/0, first_go/1, end_of_the_game/1, start_game_info/2]).

:- dynamic went_to_servants_house/1, need_soil/1, went_to_royal_bedroom/1, went_again_to_royal_bedroom/1, butler_busy/1, sus_ratio/2, thief/1, has_wound/1, is_vault_key/1, is_pouch/1, guard_sus/1, wizard_sus/1, needed_mushrooms/1, went_to_wizard_house/1, gave_mushrooms/1, sec_part/1, third_part/1, without_sec_part_places/1, took_sec_part/1, took_third_part/1, thief_tool/1, machine_sus_ratio/2, first_choice_person/1, first_go/1, end_of_the_game/1.

:- retractall(went_to_servants_house(_)), retractall(need_soil(_)), retractall(went_to_royal_bedroom(_)), retractall(went_again_to_royal_bedroom(_)), retractall(butler_busy(_)), retractall(sus_ratio(_, _)), retractall(thief(_)), retractall(has_wound(_)), retractall(is_vault_key(_)), retractall(is_pouch(_)), retractall(guard_sus(_)), retractall(wizard_sus(_)), retractall(needed_mushrooms(_)), retractall(went_to_wizard_house(_)), retractall(gave_mushrooms(_)), retractall(went_to_vault(_)), retractall(sec_part(_)), retractall(third_part(_)), retractall(without_sec_part_places(_)), retractall(took_sec_part(_)), retractall(took_third_part(_)), retractall(thief_tool(_)), retractall(machine_sus_ratio(_)), retractall(first_choice_person(_)), retractall(first_go(_)), retractall(end_of_the_game(_)).


:- [world].

went_to_servants_house(no).
need_soil(no).
went_to_royal_bedroom(no).
went_again_to_royal_bedroom(no).
butler_busy(no).
went_to_wizard_house(no).
went_to_vault(no).
gave_mushrooms(no).
took_sec_part(no).
took_third_part(no).
first_go(yes).

sus_ratio(gardener, 0).
sus_ratio(cook, 0).
sus_ratio(butler, 0).

without_sec_part_places([]).

machine_sus_ratio(gardener, 0).
machine_sus_ratio(cook, 0).
machine_sus_ratio(butler, 0).

choose(List, Elt) :-
        length(List, Length),
        random(0, Length, Index),
        nth0(Index, List, Elt).

prepare_diamond() :-	
        choose([cook, butler, gardener], Has_diamond),
	diamont_things(Has_diamond).

diamont_things(Has_diamond) :-
	add_sus_ratio(Has_diamond),

	assert(has_diamond(Has_diamond)),
        whose(Has_diamond, Chest),
        assert(thing_at(diamond, Chest)),!.

prepare_wound() :-	
	choose([cook, butler, gardener], Has_wound),
	wound_things(Has_wound).	

wound_things(Has_wound) :-
	add_sus_ratio(Has_wound),

	assert(has_wound(Has_wound)),!.

prepare_pouch() :-	
	choose([rose_bushes, bed, bag_of_floor], Pouch_place),
	pouch_things(Pouch_place).

pouch_things(Pouch_place) :-
	inside_place(Place, Pouch_place),
	person(Place, Person),
	add_sus_ratio(Person),
	\+ machine_sus_ratio(Person, 3),

	assert(is_pouch(Pouch_place)),
	assert(thing_at(pouch, Pouch_place)),!.

pouch_things(Pouch_place) :-
	inside_place(Place, Pouch_place),
	person(Place, Person),
	assert(thief(Person)),

	assert(is_pouch(Pouch_place)),
	assert(thing_at(pouch, Pouch_place)).	


prepare_vault_key() :-	
	choose([garden_pond, mirror, oven], Key_place),
	vault_key_things(Key_place).

vault_key_things(Key_place) :-
	inside_place(Place, Key_place),
	person(Place, Person),

	\+ thief(Person),	
	\+ machine_sus_ratio(Person, 2),

	add_sus_ratio(Person),
	
	assert(is_vault_key(Key_place)),
	assert(thing_at(vault_key, Key_place)),!. 

vault_key_things(Key_place) :-	
	inside_place(Place, Key_place),
	person(Place, Person),
	thief(Person),
	prepare_vault_key(),!.

vault_key_things(Key_place) :-
	\+ thief(_),
	inside_place(Place, Key_place),
	person(Place, Person),
	assert(thief(Person)),
	add_sus_ratio(Person),
	
	assert(is_vault_key(Key_place)),
	assert(thing_at(vault_key, Key_place)),!.

vault_key_things(_) :-
	prepare_vault_key(),!.


	
prepare_tool() :-
	choose([rake, ladle, duster], Tool),
	belongs(Tool, Person),
	tool_things(Person).	

tool_things(Person) :-
	\+ thief(Person),
	\+ machine_sus_ratio(Person, 2),

	add_sus_ratio(Person),
	
	belongs(Tool, Person),
	assert(thief_tool(Tool)),!.

tool_things(Person) :-
	thief(Person),
	prepare_tool(),!.

tool_things(Person) :-
	\+ thief(_),
	assert(thief(Person)),
	add_sus_ratio(Person),	

	belongs(Tool, Person),	
	assert(thief_tool(Tool)),!.

tool_things(_) :-
	prepare_tool(),!.

prepare_guard_sus() :-
	choose([cook, butler, gardener], Guard_sus),
	guard_sus_things(Guard_sus).

guard_sus_things(Guard_sus) :-
	\+ thief(Guard_sus),
	\+ machine_sus_ratio(Guard_sus, 2),
	
	add_sus_ratio(Guard_sus),

	assert(guard_sus(Guard_sus)),!.

guard_sus_things(Guard_sus) :-	
	thief(Guard_sus),
	prepare_guard_sus(),!.

guard_sus_things(Guard_sus) :-
	\+ thief(_),
	assert(thief(Guard_sus)),
	add_sus_ratio(Guard_sus),	

	assert(guard_sus(Guard_sus)),!.

guard_sus_things(_) :-
	prepare_guard_sus(),!.


prepare_wizard_sus() :-
	choose([cook, butler, gardener], Wizard_sus),
	wizard_sus_things(Wizard_sus).

wizard_sus_things(Wizard_sus) :-
	\+ thief(Wizard_sus),
	\+ machine_sus_ratio(Wizard_sus, 2),	
	
	add_sus_ratio(Wizard_sus),

	assert(wizard_sus(Wizard_sus)),!.

wizard_sus_things(Wizard_sus) :-	
	thief(Wizard_sus),
	prepare_wizard_sus(),!.

wizard_sus_things(Wizard_sus) :-
	\+ thief(_),
	assert(thief(Wizard_sus)),
	add_sus_ratio(Wizard_sus),	

	assert(wizard_sus(Wizard_sus)),!.

wizard_sus_things(_) :-
	prepare_wizard_sus(),!.



add_sus_ratio(Person) :-
	machine_sus_ratio(Person, Number),
	New_number is Number + 1,
	retract(machine_sus_ratio(Person, _)),
	assert(machine_sus_ratio(Person, New_number)).


prepare_parts() :-
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
	

prepare_needed_mushrooms() :-
	random(5, 15, Number),
	assert(needed_mushrooms(Number)).

check_quests(servants_house) :-
        went_to_servants_house(no),
        assert(went_to_servants_house(yes)),
        retract(went_to_servants_house(no)),!.


check_quests(garden) :-
        went_to_royal_bedroom(yes),
        need_soil(no),
        assert(need_soil(yes)),
        retract(need_soil(no)),!.

check_quests(royal_bedroom) :-
        went_to_servants_house(yes),
        went_to_royal_bedroom(no),
        assert(went_to_royal_bedroom(yes)),
        retract(went_to_royal_bedroom(no)),!.

check_quests(royal_bedroom) :-
        need_soil(yes),
        went_again_to_royal_bedroom(no),
        assert(went_again_to_royal_bedroom(yes)),
        retract(went_again_to_royal_bedroom(no)).


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



start_game_info(info, ["Your task is to track down the thief and tag him with the 'choose_thief(<Person>)' command.","<\n>",
		       "You win if the person you tagged was indeed the thief.","<\n>","<\n>",
		       "During the gameplay, you will come across clues that will let you know that the person is more suspicious.","<\n>",
		       "You can increase the suspicion index with the 'inc_sus_ratio(<Person>).' command.","<\n>",
		       "If you get 3 suspicions on someone then you are sure that this person is guilty.","<\n>","<\n>",
		       "The whole game takes 20 minutes. After that time, you have to choose a thief even if you are not sure if he actually is one.","<\n>",
		       "The game starts as you go in any direction.  You can check how much time you have with the 'check_time.' command.","<\n>",
		       "Below you have the rest of the instructions, which are worth reading before starting the game.","<\n>",
		       "You can always see all the instructions again by writing 'instructions.' Good luck!"]).

