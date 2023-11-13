:- module(instructions, [instructions/0, search/1, drop/1, take/1, open/1, go/1, look/0, back/0, go_to_chest/1, talk/1, choose_thief/1, sure/0, holding/0, full_desc_place/1, i_am_at/1, i_was_at/1, approach/1, i_am_at_inner_place/1]).

:- dynamic places_list/1, i_am_at/1, i_was_at/1, i_am_at_inner_place/1.
:- retractall(places_list(_)), retractall(i_am_at(_)), retractall(i_was_at(_)), retractall(i_am_at_inner_place(_)).

:- [world].
:- [plot].
:- [places].

instructions :-
        nl,
        write("Enter commands using standard Prolog syntax."), nl,
        write("Attention! Elements in angle brackets <> mean that the element you want to check is to be entered there."),nl,
        write("In 'Commands that only check something' heading, if command has an argument, than as an argument write X."),nl,
	write("Then you know which element/elements satisfy the condition."),nl,nl,
        write("Commands that change something:"), nl,
        write("start.                       -- to start the game."), nl,
        write("go(<Place>)                  -- to go to that place."), nl,
        write("back.                        -- to go to the previous place."), nl,
	write("approach(<Inner_place>)      -- to approach one of the inner places of the place you are right now."), nl,
        write("take(<Object>).              -- to pick up an object."), nl,
        write("drop(<Object>).              -- to put down an object."), nl,
        write("search(<Container>).         -- to search something in an object."), nl,
        write("go_to_chest(<Person>).       -- to go to the chest of the chosen person."), nl,
        write("talk(<Person>).              -- to talk to the chosen person."), nl,
        write("open(<Container>).           -- to open an object."), nl,
        write("choose_thief(<Person>).      -- to check if you are right, who the thief is."), nl,
	write("inc_sus_ratio(<Person>).     -- to increase suspiciousness ratio for person you think is more suspicious."),nl,
        write("halt.                        -- to end the game and quit."), nl,nl,
        write("Commands that only check something:"), nl,
        write("instructions.                -- to see this message again."), nl,
        write("look.                        -- to check the most important things about the place you are right now."), nl,
	write("full_desc_place(<Place>)     -- to check look and history about the place"),nl,
	write("i_am_at(<Place>).            -- to check where you are right now."), nl,
        write("i_was_at(<Place>).           -- to check where you were earlier."), nl,
        write("is_locked(<Person>_chest).   -- to check if person's chest is locked."), nl,
        write("holding(<Object>).           -- to check if you are holding an object."), nl,
	write("holding.                     -- to check all things you are holding right now."), nl, 
nl.

i_am_at_inner_place(_).

i_am_at(courtyard).
i_was_at(courtyard).

holding :-
	\+ holding(_),
	write("You are not holding anything"),!,nl.

holding :-
	write("You are now holding:"), nl,
	holding(What),
	write("-"), write(What),nl,
	fail.

holding.



take(What) :-
        holding(What),
        write('You''re already holding it!'),!, nl.

take(What) :-
        i_am_at(Place),
        thing_at(What, Place),
        take_thing(What, Place),
	write("You succesfully took "), write(What),nl,
        retract(thing_at(What, Place)),
        assert(holding(What)),!.

take(What) :-
	i_am_at(Place),
	\+ thing_at(What, Place),
	i_am_at_inner_place(Inner_place),
	thing_at(What, Inner_place),
	write("You succesfully took "), write(What),nl,
	retract(thing_at(What, Inner_place)),
	assert(holding(What)),!.


take(soil) :-!.

take(keys) :-
        (went_to_servants_house(no); \+ thing_at(soil, butler_room)),
        write("You can't take that!"),!,nl.

take(keys) :-!.

take(_) :-
        write("You can't take that!"),!,nl.


take_thing(soil, garden) :-
        assert(holding(soil)), !, fail.

take_thing(keys, butler_room) :-
        thing_at(soil, butler_room),
        went_to_servants_house(yes),
        butler_busy(yes),
        write("You successfully take the keys! Now run before the butler see you!"),!,nl.

take_thing(keys, butler_room) :-
        \+ thing_at(soil, butler_room),!, fail.

take_thing(keys, butler_room) :-
        went_to_servants_house(no),!, fail.

take_thing(keys, butler_room) :-
        write("The butler is no longer busy, try scatter the soil again."),!, fail.


drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(thing_at(X, Place)),
        drop_thing(X, Place),!.


drop(_) :-
        write('You aren''t holding it!'),nl.


drop_thing(soil, butler_room) :-
        went_to_servants_house(yes),
        write("You successfully drop soil. You tell butler that there is soil everywhere and to clean it."),nl,
	write("Butler agree with you and start cleaning. Now is your chance! Grab the keys!"),
        assert(butler_busy(yes)),!, nl.

drop_thing(soil, butler_room) :-
        write("You don't know what it will do yet you cheater!"),!,nl.


drop_thing(_, _).




search(What) :-
        \+ is_locked(What),
        thing_at(Thing, What),
        write('You found '), write(Thing),!,nl.

search(What) :-
        is_locked(What),
        write(What), write(' is locked'),!,nl.


search(What) :-
        write(What), write(' is empty'),nl.



open(What) :-
        is_locked(What),
        holding(keys),
        retract(is_locked(What)),
        write(What), write(' is open'),nl,
        open_thing(What),!.

open(What) :-
        \+is_locked(What),
        write(What), write(' is arleady open'),!,nl.

open(_) :-
        write("You don't have a key!"),nl.

open_thing(servants_house) :-
        write("You see place with bedrooms for all workforces,"),nl,
 	write("this is the place where cook, gardener and butler are sleeping."),nl,
	write("Each of them has 1 chest. You can go to these chest"),!,nl.

open_thing(What) :-
        (=(What, cook_chest);=(What, butler_chest);=(What, gardener_chest)),
        write("Your set of keys can open "), write(What), write("!"),!,nl.

open_thing(_).



go(Place) :-
	i_am_at(Here),
        (door(Here,Place);door(Place,Here)),
        retract(i_am_at(Here)),
        assert(i_am_at(Place)),
        retract(i_was_at(_)),
        assert(i_was_at(Here)),
        !, look.

go(_) :-
        write('You can''t go that way.'),nl.


back() :-
        i_am_at(Here),
        i_was_at(There),
        retract(i_am_at(Here)),
        assert(i_am_at(There)),
        retract(i_was_at(There)),
        assert(i_was_at(Here)), look.




look :-
        i_am_at(Place),

        first_time(Place),
        full_desc_place(Place),nl,

        is_person(Place),
	notice_people(Place),nl,

	are_inner_places(Place),
        notice_inner_places(Place),nl,
	
	is_quest(Place),
        check_quests(Place),
        after_enter(Place),
        after_leave(Place),nl,
	
        where_go(Place),
	after_look(Place),!.

look :-
        i_am_at(Place),

	\+ first_time(Place),
        describe(Place),nl,
	
	is_person(Place),
        notice_people(Place),nl,
	
	are_inner_places(Place),
        notice_inner_places(Place),nl,

        is_quest(Place),
	check_quests(Place),
        after_enter(Place),
        after_leave(Place),nl,
	
        where_go(Place),
	after_look(Place),!.


look :-
	i_am_at(Place),

	\+ is_person(Place),
	
	are_inner_places(Place),
        notice_inner_places(Place),nl,

	is_quest(Place),
	check_quests(Place),
	after_enter(Place),
	after_leave(Place),nl,

	where_go(Place),
	after_look(Place),!.

look :-
	i_am_at(Place),
	
	\+ are_inner_places(Place),
	
	is_quest(Place),
	check_quests(Place),
	after_enter(Place),
	after_leave(Place),nl,

	where_go(Place),
	after_look(Place),!.

look :- 
	i_am_at(Place),

	\+ is_quest(Place),

	where_go(Place),
	after_look(Place).


after_look(Place) :-	
        retract(first_time(Place)),!.

after_look(_).


full_desc_place(Place) :-
        all_desc_place(Place, Desc),
        print_string(Desc, _).

full_desc_person(Person) :-
	first_say(Person, Desc),	
	print_string(Desc, Person).

print_string(Desc, Person) :-
        [H|T] = Desc,
        \+ =(H,"<\n>"),
	\+ =(H, "<wound>"),
        write(H),
        print_string(T, Person),!.

print_string([], _) :- !.

print_string(Desc, Person) :-
        [H|T] = Desc,
	=(H,"<\n>"),	
        nl,
        print_string(T, Person),!.

print_string(Desc, Person) :-
	[_|T] = Desc,
	has_wound(Person),
	wound(Person, Wound),
	write(Wound),
	print_string(T, Person),!.
		
print_string(Desc, Person) :-
	[_|T] = Desc,
	print_string(T, Person),!.




describe(Place) :- write('You are at '), write(Place), nl,
        place(Place, Description),
        write(Description), nl.

places_list([]).

where_go(Place) :-
        write("From here you can go to "), print_way(Place), nl.



print_way(Place) :-
        (door(Place, Way);door(Way, Place)),
	places_list(List),
	append(List, [Way], New_list),
	assert(places_list(New_list)),
	retract(places_list(List)),fail.

print_way(_) :- 
	places_list(List),
	print_places(List).

print_places(List) :-
	last(List, Last),        
	[H|T] = List,
	\+ =(H, Last),
	write(H), write(", "), print_places(T),!.

print_places(List) :-
	last(List, Last),        
	write(Last), write("."),
	retract(places_list(_)),
	assert(places_list([])),!.

print_places(_).

notice_people(Place) :-

        person(Place, X),
        write('There is a '), write(X), write(' here you can talk to'),nl,
        fail.

notice_people(_).
/* These rules set up a loop to mention all the objects
   in your vicinity. */


notice_inner_places(Place) :-
        write("You see certain places that you can approach, maybe you'll find something interesting there:"),nl,
	print_inner_places(Place).
	

print_inner_places(Place) :-	
	inside_place(Place, Thing),
	write("-"), write(Thing), nl,
	fail.

print_inner_places(_).

go_to_chest(Person) :-
        i_am_at(servants_house),
        \+ is_locked(servants_house),
        write("You are near the chest of "), write(Person), nl,
        write("You can now open it"),!.

go_to_chest(Person) :-
        i_am_at(servants_house),
        write("You can't go to "), write(Person), write(" chest where servants house is locked"),!,nl.

go_to_chest(_) :-
        write("You are not in the servants house"),nl.


approach(Inner_place) :-
	write("You are near the "), write(Inner_place), write("."),	
	retract(i_am_at_inner_place(_)),
	assert(i_am_at_inner_place(Inner_place)).

talk(Person) :-
        able_to_talk(Person),
        i_am_at(Place),
        person(Place, Person),
        is_first_say(Person),
	full_desc_person(Person),        
	retract(is_first_say(Person)),!.

talk(Thing) :-
        \+ able_to_talk(Thing),
        write("You can not talk to a "), write(Thing), write("!"),nl.


talk(Person) :-
        i_am_at(Place),
        \+ person(Place, Person),
        person(Right_place, Person),
        write("You can meet that person only in "), write(Right_place),!,nl.

talk(Person) :-
        \+ is_first_say(Person),
        write("This is not a first statement"),!,nl.

talk(_).


after_enter(butler_room) :-
        went_again_to_butler_room(yes),
        holding(soil),
        write("You can now distract the butler by dropping the soil."),!,nl.

after_enter(butler_room) :-
        went_again_to_butler_room(yes),
        \+ holding(soil),
        write("You don't have soil so you can't distract the butler."),!,nl.

after_enter(butler_room) :-
        went_to_servants_house(yes),
        write("You see that there is a set of keys. Maybe you can open servant's house with one of them?"),nl,
        write("Try to distract the butler with soil by scattering it in the hallway."), nl,
        write("Then try to take the set of keys."),!,nl.

after_enter(butler_room) :-
        went_to_servants_house(no),
        write("You see that there are is a set of keys on the table. But the butler is watching it too."),nl,
	write("Maybe you can take it and use it on something in the future?"),!,nl.


after_enter(garden) :-
        need_soil(no),
        write("You see there's a lot of soil here. You are wondering if it will ever come in handy"),!,nl.

after_enter(garden) :-
        need_soil(yes),
        write("You see there's a lot of soil here."),!, nl.


after_enter(servants_house) :-
        \+ holding(keys),
        is_locked(servants_house),
        write("Oh no! Servants house is closed. Maybe you can discover something interesting there."),nl,
        write("Get the door key and open the servants house"),!,nl.

after_enter(servants_house) :-
        holding(keys),
        is_locked(servants_house),
        write("You can now enter the servants house"),!,nl.


after_enter(_).

after_leave(butler_room) :-
        butler_busy(yes),
        assert(butler_busy(no)),
        retract(butler_busy(yes)),!.

after_leave(_).



choose_thief(Thief) :-
        retractall(chosen_thief(_)),
        write("Attention, you have only one chance who the thief is. If you are sure write 'sure.'"),nl,
        assert(chosen_thief(Thief)).
sure :-
        thief(Thief),
        chosen_thief(Ch_thief),
        =(Thief, Ch_thief),
        write("It was the thief! You won"),!,nl,
        finish.
sure :-
        \+ chosen_thief(_),
        write("You haven't chosen any thief yet"),!,nl.

sure :-
        thief(Thief),
        write("It wasn't the thief. You lose. The thief was "), write(Thief),nl,
        finish.



