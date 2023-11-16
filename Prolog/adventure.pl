/* <The name of this game>, by <your name goes here>. */

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
        write('The game is over. Please enter the "halt." command.'), nl,
	assert(end_of_the_game(yes)).

/* This rule prepare the game */
start :-
     	write("				Who stole the diamond?"),nl,nl,
	write("		Game created by Mariusz Pakulski, Ignacy Dąbkowski, Jakub Jabłoński."),nl,nl, 
	i_am_at(Place),
	notice_people(Place),nl,
	
	write("				Game description"),	
	start_game_info(info, Desc),nl,nl,
	print_string(Desc, _),nl,nl,

	instructions,nl,
	full_desc_place(Place),nl,
	retract(first_time(Place)),
	where_go(Place),

	prepare_diamond(),
	prepare_wound(),
	prepare_pouch(),
	prepare_vault_key(),
	prepare_tool(),
	prepare_guard_sus(),
	prepare_wizard_sus(),
	prepare_parts(),
	prepare_needed_mushrooms().
	
