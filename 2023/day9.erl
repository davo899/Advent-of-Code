% Advent of Code 2023 - Day 9
-module(day9).
-export([part1/0, part2/0]).

next_seq([_]) -> [];
next_seq([I1|[I2|IS]]) ->
    [(I2 - I1)|next_seq([I2|IS])].

sum_lasts(IS) ->
    End = lists:all(fun(I) -> I == 0 end, IS),
    if
        End -> 0;
        true -> lists:last(IS) + sum_lasts(next_seq(IS))
    end.

part1() ->
    lists:sum(
        lists:map(fun sum_lasts/1,
        lists:map(fun reader:get_ints/1,
            reader:readlines('input/day9.txt')
        ))
    ).

diff_firsts([N|IS]) ->
    End = lists:all(fun(I) -> I == 0 end, [N|IS]),
    if
        End -> 0;
        true -> N - diff_firsts(next_seq([N|IS]))
    end.

part2() ->
    lists:sum(
        lists:map(fun diff_firsts/1,
        lists:map(fun reader:get_ints/1,
            reader:readlines('input/day9.txt')
        ))
    ).
