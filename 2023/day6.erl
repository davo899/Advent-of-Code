% Advent of Code 2023 - Day 6
-module(day6).
-export([part1/0, part2/0]).

dists(T) -> dists(T, 0).
dists(0, _) -> [0];
dists(RT, Speed) -> [Speed * RT|dists(RT - 1, Speed + 1)].

prod([]) -> 1;
prod([X|L]) -> X * prod(L).

part1() ->
    [Times|[Records|[]]] = lists:map(fun reader:get_ints/1, reader:readlines('input/day6.txt')),
    prod(
        lists:map(
            fun({TS, Record}) -> 
                length(lists:filter(fun(Time) -> Time > Record end, TS))
            end,
            lists:zip(lists:map(fun dists/1, Times), Records)
        )
    ).

part2() ->
    [Times|[Records|[]]] = lists:map(fun reader:get_ints/1, reader:readlines('input/day6.txt')),
    Time = list_to_integer(lists:flatten(lists:map(fun integer_to_list/1, Times))),
    Record = list_to_integer(lists:flatten(lists:map(fun integer_to_list/1, Records))),
    length(lists:filter(fun(T) -> T > Record end, dists(Time))).
