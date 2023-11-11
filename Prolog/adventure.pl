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
        instructions,
        look,
	choose_thief_by_machine(),		
	prepare_diamond(),
	prepare_wound().	
