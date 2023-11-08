/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, thing_at/2, holding/1, add_path/3, contain/2, is_locked/1, thief/1, has_diament/1, went_to_servants_house/1, need_soil/1, went_to_butler_room/1, went_again_to_butler_room/1, i_was_at/1.
:- retractall(thing_at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(holding(_)), retractall(thief(_)), retractall(has_key(_)), retractall(is_locked(_)), retractall(went_to_servants_house(_)), retractall(need_soil(_)), retractall(went_to_butler_room(_)), retractall(went_again_to_butler_room(_)), retractall(i_was_at(_)).

:- [places].

i_was_at(courtyard).
i_am_at(courtyard).

went_to_servants_house(no).
need_soil(no).
went_to_butler_room(no).
went_again_to_butler_room(no).
/*container_at(chest, otherplace).*/

/*contain(chest, robe).*/

is_locked(butler_chest).
is_locked(gardener_chest).
is_locked(cook_chest).

able_to_talk(butler).
able_to_talk(gardener).
able_to_talk(cook).
able_to_talk(king).
able_to_talk(wizard).
able_to_talk(guard).

thing_at(soil, garden).
thing_at(key, butler_room).
/* These rules describe how to pick up an object. */

take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        thing_at(X, Place),
	take_thing(X, Place),
        retract(thing_at(X, Place)),
        assert(holding(X)),
        write('OK.'),!,nl.

take(soil) :-!.

take(_) :-
        write("You can't take that!"),
        nl.

take_thing(soil, garden) :-
	assert(holding(soil)), !, fail.

take_thing(key, butler_room) :-
	thing_at(soil, butler_room),
	went_to_servants_house(yes),
	write("You successfully take the key! Now run before the butler see you!"),!.

take_thing(key, butler_room) :-
	!, fail.

take_thing(_, _).

/* These rules describe how to put down an object. */

drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(thing_at(X, Place)),
        drop_thing(X, Place),!, nl.


drop(_) :-
        write('You aren''t holding it!'),
        nl.


drop_thing(soil, butler_room) :-	
	went_to_servants_house(yes),
	write("You successfully drop soil. You tell butler that there is soil everywhere and to clean it. Butler agree with you and start cleaning. Now is your chance! Grab the key with command take(key)!"),!.

drop_thing(soil, butler_room) :-
	write("You don't know what it will do yet you cheater!"),!.

	
drop_thing(_, _).
	
		
/* these rules describe what to do with chest */
search(X) :-
	\+is_locked(X),
	contain(X, Thing),
	write('You found '), write(Thing), nl,
	assert(holding(Thing)),
	retract(contain(X, Thing)),!.
	
search(X) :-
	is_locked(X),
	write(X), write(' is locked'),!,nl.

search(X) :-
	write(X), write(' is empty'),nl.

	
open_chest(What) :-
	is_locked(What),
	holding(key),
	retract(holding(key)),
        retract(is_locked(What)),
	write(What), write(' is open'),!,nl.

open_chest(What) :-
	\+is_locked(What),
	write(What), write(' is arleady open'),!,nl.

open_chest(_) :-
	write("You don't have a key!"),nl. 
	
/* Those rules describe game preparations */

/*choose([], []).*/


choose(List, Elt) :-
        length(List, Length),
        random(0, Length, Index),
        nth0(Index, List, Elt).


choose_thief() :-
	choose([cook, butler, gardener], Thief),
	assert(thief(Thief)).


prepare_diament() :-
	choose([cook, butler, gardener], Has_diament),
	assert(has_diament(Has_diament)).

/* These rules define the direction letters as calls to go/1. */

go(Place) :-
        i_am_at(Here),
	(door(Here,Place);door(Place,Here)),
	retract(i_am_at(Here)),
        assert(i_am_at(Place)),
	retract(i_was_at(_)),
	assert(i_was_at(Here)),
        !, look.

go(_) :-
        write('You can''t go that way.').


back() :-
	i_am_at(Here),
	i_was_at(There),
	retract(i_am_at(Here)),
	assert(i_am_at(There)),
	retract(i_was_at(There)),
	assert(i_was_at(Here)), look.	

/* This rule tells how to look about you. */

look :-
        i_am_at(Place),
        describe(Place),nl,
        /*notice_objects_at(Place);*/
	notice_persons(Place),nl,
	check_quests(Place), nl,
	after_enter(Place),nl.
	



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

describe(Place) :- write('You are at '), write(Place), nl, 
	place(Place, Description),
	write(Description), nl,
	write("You can go to "), (print_way(Place); nl).



print_way(Way) :-
	(door(Way, X);door(X, Way)),
	write(X), write(", "),
	fail.

/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_objects_at(Place) :-
        thing_at(X, Place),
        write('There is a '), write(X), write(' here.'), nl,
        
	container_at(Y, Place),
	
        write('There is a '), write(Y), write(' here.'), nl,
	fail.

notice_objects_at(_).

notice_persons(Place) :-
	
        person(Place, X),
        write('There is a '), write(X), write(' here you can talk to'), nl,
	fail.

notice_persons(_).
/* This rule tells how to die. */

go_to_chest(Person) :-
	i_am_at(servants_house),
	write("You are near the chest of "), write(Person), nl,
	write("You can now open it using command open_chest(<person>_chest)"), !.

go_to_chest(_) :-
	write("You are not in the servants house").

	
	

talk(Person) :-
	able_to_talk(Person),
	i_am_at(Place),
	person(Place, Person),
	write("Hello "), write(Person),!.

talk(Person) :-
	able_to_talk(Person),
	person(Place, Person),
	write("You can meet that person only in "), write(Place),!.


talk(Thing) :-
	write("You can not talk to a "), write(Thing), write("!").
	
after_enter(butler_room) :-
	went_again_to_butler_room(yes),
	holding(soil),
	write("You can now distract the butler. Try using scatter(soil)."),!.

after_enter(butler_room) :-
	went_again_to_butler_room(yes),
	\+ holding(soil),
	write("You don't have soil so you can't distract the butler"),!.

after_enter(butler_room) :-
	went_to_servants_house(yes),
	write("You see that there are many keys. Maybe you can open servant's house with one of them?"),nl,
	write("continue quest: Try to distract the butler with soil from the garden by scattering it in the hallway. Then try to take the key."),!,nl.

after_enter(butler_room) :-
	went_to_servants_house(no),
	write("You see that there are many keys there. Maybe you can take one and use it on something in the future?"),!.


after_enter(garden) :-
	need_soil(no),
	write("You see there's a lot of soil here. You are wondering if it will ever come in handy"),!,nl.

after_enter(garden) :-
	need_soil(yes),
	write("You see there's a lot of land here. You can now take it to distract the butler with command take(soil)."),!, nl. 

after_enter(servants_house) :-
	write("quest started - get the door key and open servants house"),!,nl.

after_enter(_) :-
	write("There is nothing special yet").

	
die :-
        finish.


/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

finish :-
        nl,
        write('The game is over. Please enter the "halt." command.'),
        nl.


/* This rule just writes out game instructions. */

instructions :-
        nl,
        write('Enter commands using standard Prolog syntax.'), nl,
        write('Available commands are:'), nl,
        write('start.             -- to start the game.'), nl,
        write('n.  s.  e.  w.     -- to go in that direction.'), nl,
        write('take(Object).      -- to pick up an object.'), nl,
        write('drop(Object).      -- to put down an object.'), nl,
        write('look.              -- to look around you again.'), nl,
        write('instructions.      -- to see this message again.'), nl,
        write('halt.              -- to end the game and quit.'), nl,
        nl.


/* This rule prints out instructions and tells where you are. */

start :-
        instructions,
        look,
	

	/*add_path(someplace, n, otherplace),
	add_path(otherplace, s, someplace),
	add_path(otherplace, n, someplace).*/
	choose_thief(),		
	prepare_diament().	
		
/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

/*describe(someplace) :- write('You are someplace.'), nl.
describe(otherplace) :- write('You are otherplace.'), nl. */



