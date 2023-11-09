:- module(world, [is_locked/1, able_to_talk/1, thing_at/2, first_time/1]).

:- dynamic is_locked/1, thing_at/2, first_time/1.
:- retractall(is_locked(_)), retractall(thing_at(_, _)).


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

first_time(vault).
first_time(hall).
first_time(kitchen).
first_time(butler_room).
first_time(garden).
first_time(guard_house).
first_time(wizard_house).
first_time(servants_house).
first_time(forest).
first_time(courtyard).
