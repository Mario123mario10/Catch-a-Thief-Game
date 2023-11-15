:- module(instructions, [instructions/0, search/1, drop/1, take/1, open/1, go/1, look/0, back/0, talk/1, choose_thief/1, sure/0, holding/0, full_desc_place/1, approach/1, full_desc_person/2, notice_people/1, where_go/1, give/1]).


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
	write("give(<Object/Objects>)       -- to give something to someone who is in that place."), nl,
        write("drop(<Object>).              -- to put down an object."), nl,
        write("search(<Container>).         -- to search something in an object."), nl,
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

holding :-
	\+ holding(_),
	write("You are not holding anything"),!,nl.

holding :-
	write("You are now holding:"), nl,
	holding(What),
	write("-"), write(What),nl,
	fail.

holding.

give(mushrooms) :-
	i_am_at(wizard_house),
	gave_mushrooms(no),
	went_to_wizard_house(yes),
	having_mushrooms(Having),
	needed_mushrooms(Needed),
	
	Having > Needed,
	New_having is Having - Needed,
	
	retract(having_mushrooms(_)),
	assert(having_mushrooms(New_having)),
	
	retract(gave_mushrooms(no)),
	assert(gave_mushrooms(yes)),
	write("You succesfully gave mushrooms to the wizard"),nl,
	after_mushrooms(wizard, Desc),
	print_string(Desc, wizard),!,nl.
		
give(mushrooms) :-
	\+ i_am_at(wizard_house),
	write("There isn't anyone at this place who would want that"),!,nl.

give(mushrooms) :-
	gave_mushrooms(yes),
	write("Wizard is shoked and is thanking you for giving him the mushrooms again."),!,nl. 	


give(mushrooms) :-
	write("You don't have enough mushrooms. Come here again once you have the right amount of them."),!,nl.



take(mushroom) :-
	i_am_at(forest),
	having_mushrooms(Number),
	New_number is Number + 1,
	retract(having_mushrooms(_)),
	assert(having_mushrooms(New_number)),
	write("You successfully took mashroom. Now you have "), write(New_number), write(" mushrooms"),!,nl.

take(mushroom) :-
	\+ i_am_at(forest),
	write("You are not in the forest"),!.


take(keys) :-
        (went_to_servants_house(no); \+ thing_at(soil, butler_room); \+ i_am_at(butler_room)),
        write("You can't take that!"),!,nl.

take(keys) :-
	i_am_at(butler_room),
	thing_at(keys, butler_room),
        butler_busy(yes),
        write("You successfully took the keys! Now run before the butler see you!"),!,nl,
        retract(thing_at(keys, butler_room)),
        assert(holding(keys)),!.

take(keys) :- 
	\+ thing_at(keys, butler_room),
	write("You arleady took the keys"),!,nl.	

take(keys) :-
	butler_busy(no),
        write("The butler is no longer busy, try scatter the soil again."),!,nl.


take(soil) :-
        i_am_at(garden),
        thing_at(soil, garden),
	write("You succesfully took soil"),nl,
        assert(holding(soil)), !.

take(soil) :- 
	\+ i_am_at(garden),
	write("You can't take that from here."),!,nl.

take(second_part) :-
	i_am_at(Place),
	thing_at(sec_part, Place),
	went_to_vault(yes),
	write("You successfully took a second part but you still don't know what it is."),nl,
	retract(thing_at(sec_part, Place)),
	assert(holding(sec_part)),
	retract(took_sec_part(no)),
	assert(took_sec_part(yes)),!.

take(second_part) :-
	i_am_at(Place),
	\+ thing_at(sec_part, Place),
	write("You can't take it from here"),!,nl.

take(second_part) :-
	went_to_vault(no),
	write("How do you know it can be here you cheater?"),!,nl.

take(third_part) :-
	i_am_at(Place),
	thing_at(third_part, Place),
	went_to_vault(yes),
	took_sec_part(yes),

	write("You successfully took a third part and now you know what it is!"),nl,
	thief_tool(Tool),
	write("It is a "), write(Tool), write("!"),nl,
	write("Talk with the person you think could have used that."),nl,		

	retract(thing_at(third_part, Place)),
	assert(holding(third_part)),!.

take(third_part) :-
	i_am_at(Place),
	\+ thing_at(third_part, Place),
	write("You can't take it from here"),!,nl.


take(third_part) :-
	(went_to_vault(no);took_sec_part(no)),
	write("How do you know it can be here you cheater?"),!,nl.


take(What) :-
        holding(What),
        write("You are already holding it!"),!, nl.


take(What) :-
        i_am_at(Place),
        thing_at(What, Place),
	write("You successfully took a "), write(What),nl,
        retract(thing_at(What, Place)),
        assert(holding(What)),!.


take(What) :-
	i_am_at(Place),
	\+ thing_at(What, Place),
	i_am_at_inner_place(Inner_place),
	thing_at(What, Inner_place),
	write("You successfully took a "), write(What),nl,
	retract(thing_at(What, Inner_place)),
	assert(holding(What)),!.



take(_) :-
        write("You can't take that!"),!,nl.


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
	write("Butler agree with you and start cleaning."),
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
 	write("this is the place where cook, gardener and butler are normally sleeping."),nl,
	write("Each of them has 1 chest. Maybe when you approach and search these chests you will discover something."),!,nl.

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
        full_desc_place(Place),

        is_person(Place),
	notice_people(Place),nl,

	are_inner_places(Place),
        notice_inner_places(Place),nl,
	
	is_tool_part(Place),
	notice_tool_part(Place),nl,	

	is_quest(Place),
        check_quests(Place),
        after_enter(Place),
        after_leave(Place),
	
        where_go(Place),
	after_look(Place),!.

look :-
        i_am_at(Place),

	\+ first_time(Place),
        describe(Place),
	
	is_person(Place),
        notice_people(Place),nl,
	
	are_inner_places(Place),
        notice_inner_places(Place),nl,

	is_tool_part(Place),
	notice_tool_part(Place),nl,		

        is_quest(Place),
	check_quests(Place),
        after_enter(Place),
        after_leave(Place),
	
        where_go(Place),
	after_look(Place),!.


look :-
	i_am_at(Place),

	\+ is_person(Place),
	
	are_inner_places(Place),
        notice_inner_places(Place),nl,
	
	is_tool_part(Place),
	notice_tool_part(Place),nl,	

	is_quest(Place),
	check_quests(Place),
	after_enter(Place),
	after_leave(Place),

	where_go(Place),
	after_look(Place),!.

look :-
	i_am_at(Place),
	
	\+ are_inner_places(Place),

	is_tool_part(Place),
	notice_tool_part(Place),nl,	
	
	is_quest(Place),
	check_quests(Place),
	after_enter(Place),
	after_leave(Place),

	where_go(Place),
	after_look(Place),!.

look :-
	i_am_at(Place),

	\+ is_tool_part(Place),
	
	is_quest(Place),
	check_quests(Place),
	after_enter(Place),
	after_leave(Place),

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


is_tool_part(Place) :-
	(sec_part(Place);third_part(Place)).

notice_tool_part(Place) :-
	went_to_vault(yes),
	sec_part(Place),
	took_sec_part(no),	
	write("You see the second part of the tool that the thief probably used, maybe that was his escape route."),!,nl.


notice_tool_part(Place) :-
	went_to_vault(yes),
	took_sec_part(yes),	
	third_part(Place),
	took_sec_part(yes),
	took_third_part(no),
	write("You see the third part of the tool that the thief probably used, now you can check what the item is."),!,nl.
	
notice_tool_part(_).


full_desc_place(Place) :-
        all_desc_place(Place, Desc),
        print_string(Desc, _).

full_desc_person(Desc, Person) :-
	first_say(Person, Desc),	
	print_string(Desc, Person).

print_string(Desc, Person) :-
        [H|T] = Desc,
        \+ =(H,"<\n>"),
	\+ =(H, "<wound>"),
	\+ =(H, "<guard_sus>"),
	\+ =(H, "<number>"),
	\+ =(H, "<wizard_sus>"),
        write(H),
        print_string(T, Person),!.

print_string([], _) :- !.

print_string(Desc, Person) :-
        [H|T] = Desc,
	=(H,"<\n>"),	
        nl,
        print_string(T, Person),!.

print_string(Desc, Person) :-
	[H|T] = Desc,
	=(H,"<wound>"),
	has_wound(Person),
	wound(Person, Wound),
	write(Wound),
	print_string(T, Person),!.

print_string(Desc, Person) :-
	[H|T] = Desc,
	=(H,"<wound>"),
	print_string(T, Person),!.

print_string(Desc, Person) :-
	[H|T] = Desc,
	=(H,"<guard_sus>"),
	guard_sus(Who),
	write(Who),
	print_string(T, Person),!.
	
print_string(Desc, Person) :-
	[H|T] = Desc,
	=(H, "<number>"),
	needed_mushrooms(Number),
	write(Number),
	print_string(T, Person),!.

print_string(Desc, Person) :-
	[_|T] = Desc,
	wizard_sus(Who),
	write(Who),
	print_string(T, Person),!.
	

describe(Place) :- write('You are at '), write(Place), nl,
        place(Place, Description),
        write(Description),nl.


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


notice_people(guard_house) :-
	first_time(guard_house),nl,
	first_say(guard, Desc),
	full_desc_person(Desc, guard),!,nl. 	

notice_people(Place) :-
	first_time(Place),nl,
        person(Place, Person),
	first_say(Person, Desc),
	full_desc_person(Desc, Person),!,nl.


notice_people(Place) :-
	person(Place, Person),
        write('There is a '), write(Person), write(' here you can talk to'),nl.



/* These rules set up a loop to mention all the objects
   in your vicinity. */

notice_inner_places(servants_house) :-
	\+ is_locked(servants_house),	
        write("You see certain places that you can approach, maybe you'll find something interesting there:"),nl,
	print_inner_places(servants_house),!.

notice_inner_places(servants_house) :-
	is_locked(servants_house),!.

notice_inner_places(Place) :-
        write("You see certain places that you can approach, maybe you'll find something interesting there:"),nl,
	print_inner_places(Place).
	

print_inner_places(Place) :-	
	inside_place(Place, Thing),
	write("-"), write(Thing), nl,
	fail.

print_inner_places(_).

approach(Chest) :-
	i_am_at(servants_house),
	inside_place(servants_house, Chest),
	approach_chest(Chest),
	retract(i_am_at_inner_place(_)),
	assert(i_am_at_inner_place(Chest)),!.	


approach(Inner_place) :-
	i_am_at(Place),
	inside_place(Place, Inner_place), 
	write("You are near the "), write(Inner_place), write("."),	
	retract(i_am_at_inner_place(_)),
	assert(i_am_at_inner_place(Inner_place)),!.

approach(Inner_place) :-
	i_am_at(Place),
	\+ inside_place(Place, Inner_place),
	write("You can't approach it").


approach_chest(Chest) :-
	i_am_at(servants_house),
	retract(i_am_at_inner_place(_)),
	assert(i_am_at_inner_place(Chest)),

	\+ is_locked(servants_house),
	whose(Person, Chest),
	write("You are now near the "), write(Person), write("'s chest."),!,nl.	
	
approach_chest(_) :-
	\+ i_am_at(servants_house),
	write("You are not in servants house"),!,nl.

approach_chest(Chest) :-
	i_am_at(servants_house),
	is_locked(servants_house),
	whose(Person, Chest),
	write("You can't go to "), write(Person), write("'s chest when servants house is locked."),!,nl.


approach_chests(_).	

talk(Person) :-
        able_to_talk(Person),
        i_am_at(Place),
        person(Place, Person),
	first_time(Place),
	first_say(Person, Desc),
	full_desc_person(Desc, Person),!.        

talk(Thing) :-
        \+ able_to_talk(Thing),
        write("You can not talk to a "), write(Thing), write("!"),!,nl.


talk(Person) :-
        i_am_at(Place),
        \+ person(Place, Person),
        person(Right_place, Person),
        write("You can meet that person only in "), write(Right_place),!,nl.

talk(Person) :-
        person(Place, Person),
	\+ first_time(Place),
	next_talk(Person),!.


talk(_).

next_talk(wizard) :-
	gave_mushrooms(no),
	before_mushrooms(wizard, Desc),
	print_string(Desc, wizard),!,nl.

next_talk(wizard) :-
	end_talk(wizard, Desc),
	print_string(Desc, wizard),!,nl.

next_talk(_).

after_enter(butler_room) :-
        went_again_to_butler_room(yes),
        holding(soil),!.

after_enter(butler_room) :-
        went_again_to_butler_room(yes),
        \+ holding(soil),
        write("You don't have soil so you can't distract the butler."),!,nl,nl.

after_enter(butler_room) :-
        went_to_servants_house(yes),
        write("You see that there is a set of keys. Maybe you can open servant's house with one of them?"),nl,
        write("Try to distract the butler with soil by scattering it there."),nl,
	write("While he will be cleaning you will quickly take the set of keys"),!,nl,nl.

after_enter(butler_room) :-
        went_to_servants_house(no),
        write("You see that there is a set of keys on the table. But the butler is watching it too."),nl,
	write("Maybe you can take it and use it on something in the future?"),!,nl,nl.


after_enter(garden) :-
        need_soil(no),
        write("You see there's a lot of soil here. You are wondering if it will ever come in handy"),!,nl,nl.

after_enter(garden) :-
        need_soil(yes),
        write("You see there's a lot of soil here."),!, nl,nl.


after_enter(servants_house) :-
        \+ holding(keys),
        is_locked(servants_house),
        write("Oh no! Servants house is closed. Maybe you can discover something interesting there."),nl,
        write("Get the door key and open the servants house"),!,nl,nl.

after_enter(servants_house) :-
        holding(keys),
        is_locked(servants_house),
        write("Servants house is closed"),!,nl,nl.

after_enter(servants_house) :-
	write("You see there are 3 chests; butler's chest, gardener's chest and cook's chest you can approach."),!,nl,nl.

after_enter(forest) :-
	went_to_wizard_house(no),
	write("You see there are a lot of mushrooms here. You are wondering if it will ever come in handy"),!,nl,nl.

after_enter(forest) :-
	went_to_wizard_house(yes),
	write("You see there are a lot of mushrooms here."),!,nl,nl.

after_enter(wizard_house) :-	
	went_to_wizard_house(no),
	retract(went_to_wizard_house(no)),
	assert(went_to_wizard_house(yes)),!.

after_enter(vault) :-
	went_to_vault(no),
	retract(went_to_vault(no)),
	assert(went_to_vault(yes)),!.

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



