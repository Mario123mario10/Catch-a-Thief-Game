/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, holding/1, thief/1, has_diamond/1, i_was_at/1, chosen_thief/1, sus_ratio/2.
:- retractall(i_am_at(_)), retractall(alive(_)), retractall(holding(_)), retractall(thief(_)), retractall(i_was_at(_)), retractall(chosen_thief(_)), retractall(sus_ratio(_, _)).

:- [places].
:- [instructions].
:- [plot].
:- [world].

i_am_at(courtyard).
i_was_at(courtyard).

notice_persons(Place) :-
	
        person(Place, X),
        write('There is a '), write(X), write(' here you can talk to'), nl,
	fail.

notice_persons(_).
/* This rule tells how to die. */

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

	
	

talk(Person) :-
	able_to_talk(Person),
	i_am_at(Place),
	person(Place, Person),
	write("Hello "), write(Person),!, nl.

talk(Person) :-
	able_to_talk(Person),
	person(Place, Person),
	write("You can meet that person only in "), write(Place),!,nl.


talk(Thing) :-
	write("You can not talk to a "), write(Thing), write("!"),nl.


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
	write("You see that there are is a set of keys on the table. But the butler is watching it too. Maybe you can take it and use it on something in the future?"),!,nl.


after_enter(garden) :-
	need_soil(no),
	write("You see there's a lot of soil here. You are wondering if it will ever come in handy"),!,nl.

after_enter(garden) :-
	need_soil(yes),
	write("You see there's a lot of soil here."),!, nl. 

after_enter(servants_house) :-
	\+ holding(keys),
	is_locked(servants_house),
	write("Oh no! Servants house is closed. If you want to go there maybe you can discover something interesting."),nl,
	write("Get the door key and open servants house"),!,nl.

after_enter(servants_house) :-
	holding(keys),
	is_locked(servants_house),
	write("You can now enter the servants house"),!,nl.


after_enter(_) :-
	write("There is nothing special yet"),nl.

after_leave(butler_room) :-
	butler_busy(yes),
	assert(butler_busy(no)),
	retract(butler_busy(yes)),!.

after_leave(_).	

die :-
        finish.


/* Under UNIX, the "halt." command quits Prolog but does not
   remove the output window. On a PC, however, the window
   disappears before the final output can be seen. Hence this
   routine requests the user to perform the final "halt." */

finish :-
        write('The game is over. Please enter the "halt." command.'), nl.
	
choose_the_thief(Thief) :-
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




/* This rule prepare the game */
start :-
        instructions,
        look,
	choose_thief(),		
	prepare_diamond().	
