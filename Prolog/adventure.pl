/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, thing_at/2, holding/1, add_path/3, contain/2, is_locked/1, thief/1, has_key/1.
:- retractall(thing_at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(holding(_)), retractall(thief(_)), retractall(has_key(_)).

:- [places].


i_am_at(courtyard).


container_at(chest, otherplace).

/*contain(chest, robe).*/

is_locked(butler_chest).
is_locked(gardener_chest).
is_locked(cook_chest).



/* These rules describe how to pick up an object. */

take(X) :-
        holding(X),
        write('You''re already holding it!'),
        !, nl.

take(X) :-
        i_am_at(Place),
        thing_at(X, Place),
        retract(thing_at(X, Place)),
        assert(holding(X)),
        write('OK.'),
        !, nl.

take(_) :-
        write('I don''t see it here.'),
        nl.


/* These rules describe how to put down an object. */

drop(X) :-
        holding(X),
        i_am_at(Place),
        retract(holding(X)),
        assert(thing_at(X, Place)),
        write('OK.'),
        !, nl.

drop(_) :-
        write('You aren''t holding it!'),
        nl.


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

	
open(What) :-
	is_locked(What),
	holding(key),
	retract(holding(key)),
        retract(is_locked(What)),
	write(What), write(' is open'),!,nl.

open(What) :-
	\+is_locked(What),
	write(What), write(' is arleady open'),!,nl.

open(_) :-
	write("You don't have a key!"),nl. 
	

/*choose([], []).*/

choose(List, Elt) :-
        length(List, Length),
        random(0, Length, Index),
        nth0(Index, List, Elt).


choose_thief() :-
	choose([cook, butler, gardener], Thief),
	assert(thief(Thief)).


prepare_key() :-
	choose([cook, butler, gardener], Has_key),
	assert(has_key(Has_key)).

/* These rules define the direction letters as calls to go/1. */

go(Place) :-
        i_am_at(Here),
	(door(Here,Place);door(Place,Here)),
	retract(i_am_at(Here)),
        assert(i_am_at(Place)),
        !, look.

go(_) :-
        write('You can''t go that way.').


/* This rule tells how to look about you. */

look :-
        i_am_at(Place),
        describe(Place),
        nl,
        /*notice_objects_at(Place);*/
	notice_persons(Place),
        nl.


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
        write('There is a '), write(X), write(' here.'), nl,
	fail.

notice_persons(_).
/* This rule tells how to die. */

go_to_chest(Person) :-
	i_am_at(servants_house),
	write("You are near the chest of "), write(Person),!.

go_to_chest(_) :-
	write("You are not in the servants house").
	

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
	prepare_key().	
		
/* These rules describe the various rooms.  Depending on
   circumstances, a room may have more than one description. */

/*describe(someplace) :- write('You are someplace.'), nl.
describe(otherplace) :- write('You are otherplace.'), nl. */



