% Advent of Code 2023 - Day 4
-module(day4).
-import(reader,[readlines/1, get_ints/1]).
-import(lists,[sum/1, map/2, split/2]).
-import(math, [pow/2]).
-export([part1/0, part2/0]).

part1() -> 
    lists:sum(
        lists:map(fun(N) -> if N > 0 -> pow(2, N - 1); true -> 0 end end,
        lists:map(fun({Wins, Have}) -> length(sets:to_list(sets:intersection(sets:from_list(Wins), sets:from_list(Have)))) end,
        lists:map(fun(NS) -> lists:split(10, NS) end,
        lists:map(fun([_|NS]) -> NS end,
        lists:map(fun reader:get_ints/1,
            readlines('input/day4.txt')
        )))))
    ).

cards([]) -> 0;
cards([C|CS]) -> 1 + cards(CS, C).
cards(_, 0) -> 0;
cards([C|CS], N) -> cards([C|CS]) + cards(CS, N-1).

cards2([]) -> 0;
cards2([C|CS]) -> cards([C|CS]) + cards2(CS). 

part2() -> 
    cards2(
        lists:map(fun({Wins, Have}) -> length(sets:to_list(sets:intersection(sets:from_list(Wins), sets:from_list(Have)))) end,
        lists:map(fun(NS) -> lists:split(10, NS) end,
        lists:map(fun([_|NS]) -> NS end,
        lists:map(fun reader:get_ints/1,
            readlines('input/day4.txt')
        ))))
    ).
