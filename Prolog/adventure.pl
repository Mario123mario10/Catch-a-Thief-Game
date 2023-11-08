/* <The name of this game>, by <your name goes here>. */

:- dynamic i_am_at/1, thing_at/2, holding/1, is_locked/1, thief/1, has_diamond/1, went_to_servants_house/1, need_soil/1, went_to_butler_room/1, went_again_to_butler_room/1, i_was_at/1, chosen_thief/1, sus_ratio/2, butler_busy/1.
:- retractall(thing_at(_, _)), retractall(i_am_at(_)), retractall(alive(_)), retractall(holding(_)), retractall(thief(_)), retractall(is_locked(_)), retractall(went_to_servants_house(_)), retractall(need_soil(_)), retractall(went_to_butler_room(_)), retractall(went_again_to_butler_room(_)), retractall(i_was_at(_)), retractall(chosen_thief(_)), retractall(sus_ratio(_, _)), retractall(butler_busy(_)).

:- [places].

i_am_at(courtyard).
i_was_at(courtyard).

went_to_servants_house(no).
need_soil(no).
went_to_butler_room(no).
went_again_to_butler_room(no).
butler_busy(no).


is_locked(butler_chest).
is_locked(gardener_chest).
is_locked(cook_chest).
is_locked(servants_house).

able_to_talk(butler).
able_to_talk(gardener).
able_to_talk(cook).
able_to_talk(king).
able_to_talk(wizard).
able_to_talk(guard).

thing_at(soil, garden).
thing_at(keys, butler_room).

sus_ratio(gardener, 0).
sus_ratio(cook, 0).
sus_ratio(butler, 0).

/* These rules describe how to pick up an object. */

take(What) :-
        holding(What),
        write('You''re already holding it!'),!, nl.

take(What) :-
        i_am_at(Place),
        thing_at(What, Place),
	take_thing(What, Place),
        retract(thing_at(What, Place)),
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


/* These rules describe how to put down an object. */

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
	write("You successfully drop soil. You tell butler that there is soil everywhere and to clean it. Butler agree with you and start cleaning. Now is your chance! Grab the key!"),
	assert(butler_busy(yes)),!, nl.

drop_thing(soil, butler_room) :-
	write("You don't know what it will do yet you cheater!"),!,nl.

	
drop_thing(_, _).
	
		
/* these rules describe what to do with chest */

search(What) :-
	\+ is_locked(What),
	thing_at(Thing, What),
	write('You found '), write(Thing), nl,
	assert(holding(Thing)),
	retract(thing_at(Thing, What)),
	check_ratio(Thing, What),!.

search(What) :-
	is_locked(What),
	write(What), write(' is locked'),!,nl.


search(What) :-
	write(What), write(' is empty'),nl.


check_ratio(Thing, What) :-	
	=(Thing, diamond),
	whose(Person, What),
	inc_sus_ratio(Person),!.

check_ratio(_, _).
	
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
	write("You see place with bedrooms for all workforces, this is the place where cook, gardener and butler are sleeping. Each of them has 1 chest. You can go to these chest"),!,nl.

open_thing(What) :-
	(=(What, cook_chest);=(What, butler_chest);=(What, gardener_chest)),
	write("Your set of keys can open "), write(What), write("!"),!,nl.

open_thing(_).	

/* Those rules describe game preparations */

/*choose([], []).*/


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
        write('You can''t go that way.'),nl.


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
        describe(Place),
        /*notice_objects_at(Place);*/
	notice_persons(Place),
	check_quests(Place),
	after_enter(Place),
	after_leave(Place).

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
        
	/*container_at(Y, Place),
	
        write('There is a '), write(Y), write(' here.'), nl,*/
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
	write("You don't have soil so you can't distract the butler"),!,nl.

after_enter(butler_room) :-
	went_to_servants_house(yes),
	write("You see that there is a set of keys. Maybe you can open servant's house with one of them?"),nl,
	write("continue quest: Try to distract the butler with soil from the garden by scattering it in the hallway."), nl,
 	write("Then try to take the set of keys."),!,nl.

after_enter(butler_room) :-
	went_to_servants_house(no),
	write("You see that there are is a set of keys on the table. But the butler is watching it too. Maybe you can take it and use it on something in the future?"),!,nl.


after_enter(garden) :-
	need_soil(no),
	write("You see there's a lot of soil here. You are wondering if it will ever come in handy"),!,nl.

after_enter(garden) :-
	need_soil(yes),
	write("You see there's a lot of land here. You can now take it to distract the butler"),!, nl. 

after_enter(servants_house) :-
	\+ holding(keys),
	is_locked(servants_house),
	write("quest started - get the door key and open servants house"),!,nl.

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



/* This rule just writes out game instructions. */

instructions :-
        nl,
	write("Enter commands using standard Prolog syntax."), nl,
	write("Attention! Elements in angle brackets <> mean that the element you want to check is to be entered there."),nl,
	write("If you want to check if any element satisfies the condition, enter X."),nl,
	write("This is only possible with commands that check something"), nl,nl,
	write("Commands that change something:"), nl,        
        write("start.			    -- to start the game."), nl,
        write("go(<Place>)		    -- to go in that direction."), nl,
	write("back.			    -- to go to the previous place."), nl,
        write("take(<Object>).		    -- to pick up an object."), nl,
        write("drop(<Object>).		    -- to put down an object."), nl,
	write("search(<Container>).	    -- to search something in an object."), nl,
	write("go_to_chest(<Person>).	    -- to go to the chest of the chosen person."), nl,
	write("talk(<Person>).		    -- to talk to the chosen person."), nl,
	write("open(<Container>).	    -- to open an object."), nl,
	write("choose_the_thief(<Person>). -- to check if you are right, who the thief is."), nl,
        write("halt.			    -- to end the game and quit."), nl,nl, 
	write("Commands that only check something:"), nl,
	write("instructions.                -- to see this message again."), nl,
	write("look.                        -- to look around you again."), nl,
	write("i_am_at(<Place>).            -- to check where you are right now."), nl,
	write("i_was_at(<Place>).           -- to check where you were earlier."), nl,
	write("is_locked(<Person>_chest).   -- to check if person's chest is locked."),nl,	
	write("holding(<Objects>).          -- to check what you are having right now."),nl,
	write("(print ; whenever you want to check another thing you are holding)."), nl,
nl.
	


/* This rule prepare the game */
start :-
        instructions,
        look,
	choose_thief(),		
	prepare_diamond().	
