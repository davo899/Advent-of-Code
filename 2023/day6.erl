% Advent of Code 2023 - Day 6
-module(day6).

-import(lists,[seq/2, droplast/1, min/1, max/1, sum/1, delete/2, map/2, last/1, member/2, flatten/1, enumerate/1, all/2, any/2, nthtail/2, split/2]).
-import(sets,[new/0, add_element/2, del_element/2, filter/2, from_list/1, intersection/1, intersection/2, is_disjoint/2, is_element/2]).
-import(string, [prefix/2, tokens/2, strip/3, to_integer/1]).
-import(math, [pow/2]).

-import(reader,[readlines/1, get_ints/1]).
-import(deque,[new_deque/0, appendleft/2, appendright/2, popleft/1, popright/1, to_list/1]).

-export([part1/0, part2/0]).

dists(T) -> dists(T, 0).
dists(0, _) -> [0];
dists(RT, Speed) -> [Speed * RT|dists(RT - 1, Speed + 1)].

prod([]) -> 1;
prod([X|L]) -> X * prod(L).

part1() ->
    [Times|[Records|[]]] = lists:map(fun reader:get_ints/1, readlines('input/day6.txt')),
    prod(
        lists:map(
            fun({TS, Record}) -> 
                length(lists:filter(fun(Time) -> Time > Record end, TS))
            end,
            lists:zip(lists:map(fun dists/1, Times), Records)
        )
    ).

part2() ->
    [Times|[Records|[]]] = lists:map(fun reader:get_ints/1, readlines('input/day6.txt')),
    Time = list_to_integer(lists:flatten(lists:map(fun integer_to_list/1, Times))),
    Record = list_to_integer(lists:flatten(lists:map(fun integer_to_list/1, Records))),
    length(lists:filter(fun(T) -> T > Record end, dists(Time))).
