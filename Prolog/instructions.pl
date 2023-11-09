:- module(instructions, [instructions/0, search/1, drop/1, take/1, open/1, go/1, look/0, back/0, describe/1]).


instructions :-
        nl,
        write("Enter commands using standard Prolog syntax."), nl,
        write("Attention! Elements in angle brackets <> mean that the element you want to check is to be entered there."),nl,
        write("If you want to check if any element satisfies the condition, enter X."),nl,
        write("This is only possible with commands that check something"), nl,nl,
        write("Commands that change something:"), nl,
        write("start.                       -- to start the game."), nl,
        write("go(<Place>)                  -- to go in that direction."), nl,
        write("back.                        -- to go to the previous place."), nl,
        write("take(<Object>).              -- to pick up an object."), nl,
        write("drop(<Object>).              -- to put down an object."), nl,
        write("search(<Container>).         -- to search something in an object."), nl,
        write("go_to_chest(<Person>).       -- to go to the chest of the chosen person."), nl,
        write("talk(<Person>).              -- to talk to the chosen person."), nl,
        write("open(<Container>).           -- to open an object."), nl,
        write("choose_the_thief(<Person>). -- to check if you are right, who the thief is."), nl,
        write("halt.                        -- to end the game and quit."), nl,nl,
        write("Commands that only check something:"), nl,
        write("instructions.                -- to see this message again."), nl,
        write("look.                        -- to look around you again."), nl,
        write("i_am_at(<Place>).            -- to check where you are right now."), nl,
        write("i_was_at(<Place>).           -- to check where you were earlier."), nl,
        write("is_locked(<Person>_chest).   -- to check if person's chest is locked."),nl,
        write("holding(<Objects>).          -- to check what you are having right now."),nl,
        write("(print ; whenever you want to check another thing you are holding)."), nl,
nl.

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
        retract(first_time(Place)),
        full_describe(Place),
        where_go(Place),
        /*notice_objects_at(Place);*/
        notice_persons(Place),
        check_quests(Place),
        after_enter(Place),
        after_leave(Place),!.

look :-
        i_am_at(Place),
        describe(Place),
        where_go(Place),
        /*notice_objects_at(Place);*/
        notice_persons(Place),
        check_quests(Place),
        after_enter(Place),
        after_leave(Place).




full_describe(Place) :-
        all_desc_place(Place, Desc),
        print_string(Desc).

print_string(Desc) :-
        [H|T] = Desc,
        \+ =(H,"<\n>"),
        write(H),
        print_string(T),!.

print_string([]) :- !.

print_string(Desc) :-
        [_|T] = Desc,
        nl,
        print_string(T),!.


describe(Place) :- write('You are at '), write(Place), nl,
        place(Place, Description),
        write(Description), nl.



where_go(Place) :-
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
