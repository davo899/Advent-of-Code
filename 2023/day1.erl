% Advent of Code 2023 - Day 1
-module(day1).
-export([part1/0, part2/0]).

digits([]) -> [];
digits([C|Line]) ->
    N = C - 48,
    if
        N < 0 -> digits(Line);
        N > 9 -> digits(Line);
        true  -> [integer_to_list(N)|digits(Line)]
    end.

calib([D|Digits]) -> D ++ lists:last([D|Digits]).

part1() -> lists:sum(
    lists:map(fun list_to_integer/1,
    lists:map(fun calib/1,
    lists:map(fun digits/1,
        reader:readlines('input/day1.txt')
    )))).

word_digits([]) -> [];
word_digits([C|T]) ->
    N = C - 48,
    case N >= 0 andalso N < 10 of
        true -> [integer_to_list(N)|word_digits(T)];
        _ ->
            S = [C|T],
            One = string:prefix(S, "one"),
            Two = string:prefix(S, "two"),
            Three = string:prefix(S, "three"),
            Four = string:prefix(S, "four"),
            Five = string:prefix(S, "five"),
            Six = string:prefix(S, "six"),
            Seven = string:prefix(S, "seven"),
            Eight = string:prefix(S, "eight"),
            Nine = string:prefix(S, "nine"),
            if
                One /= nomatch -> ["1"|word_digits(T)];
                Two /= nomatch -> ["2"|word_digits(T)];
                Three /= nomatch -> ["3"|word_digits(T)];
                Four /= nomatch -> ["4"|word_digits(T)];
                Five /= nomatch -> ["5"|word_digits(T)];
                Six /= nomatch -> ["6"|word_digits(T)];
                Seven /= nomatch -> ["7"|word_digits(T)];
                Eight /= nomatch -> ["8"|word_digits(T)];
                Nine /= nomatch -> ["9"|word_digits(T)];
                true -> word_digits(T)
            end
    end.

part2() -> lists:sum(
    lists:map(fun list_to_integer/1,
    lists:map(fun calib/1,
    lists:map(fun word_digits/1,
        reader:readlines('input/day1.txt')
    )))).
