/* <The name of this game>, by <your name goes here>. */

:- retractall(alive(_)).

:- [places].
:- [instructions].
:- [plot].
:- [world].
:- [people].


die :-
        finish.


/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */


finish :-
        write('The game is over. Please enter the "halt." command.'), nl.


/* This rule prepare the game */
start :-
        instructions,nl,

	i_am_at(Place),
	notice_people(Place),nl,
	full_desc_place(Place),nl,
	where_go(Place),

	choose_thief_by_machine(),		
	prepare_diamond(),
	prepare_wound(),
	prepare_objects().	
