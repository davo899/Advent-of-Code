% Advent of Code 2023 - Day 4
-module(day4).
-import(reader,[readlines/1, get_ints/1]).
-import(lists,[droplast/1, min/1, max/1, sum/1, delete/2, map/2, last/1, member/2, flatten/1, enumerate/1, all/2, any/2, nthtail/2, split/2]).
-import(deque,[new_deque/0, appendleft/2, appendright/2, popleft/1, popright/1, to_list/1]).
-import(sets,[new/0, add_element/2, del_element/2, filter/2, from_list/1, intersection/1, intersection/2, is_disjoint/2, is_element/2]).
-import(string, [prefix/2, tokens/2, strip/3, to_integer/1]).
-import(math, [pow/2]).
-export([part1/0, part2/0]).

part1() -> 
    lists:sum(
    lists:map(
        fun(N) -> if N > 0 -> pow(2, N - 1); true -> 0 end end,
    lists:map(
        fun({Wins, Have}) -> length(sets:to_list(sets:intersection(sets:from_list(Wins), sets:from_list(Have)))) end,
    lists:map(
        fun(NS) -> lists:split(10, NS) end,
    lists:map(
        fun([_|NS]) -> NS end,
    lists:map(
        fun reader:get_ints/1,
        readlines('input/day4.txt')
    )))))).

cards([]) -> 0;
cards([C|CS]) -> 1 + cards(CS, C).
cards(_, 0) -> 0;
cards([C|CS], N) -> cards([C|CS]) + cards(CS, N-1).

cards2([]) -> 0;
cards2([C|CS]) -> cards([C|CS]) + cards2(CS). 

part2() -> 
    cards2(
    lists:map(
        fun({Wins, Have}) -> length(sets:to_list(sets:intersection(sets:from_list(Wins), sets:from_list(Have)))) end,
    lists:map(
        fun(NS) -> lists:split(10, NS) end,
    lists:map(
        fun([_|NS]) -> NS end,
    lists:map(
        fun reader:get_ints/1,
        readlines('input/day4.txt')
    ))))).
