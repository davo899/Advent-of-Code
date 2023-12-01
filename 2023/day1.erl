% Advent of Code 2023 - Day 1
-module(day1).
-import(reader,[readlines/1]).
-import(lists,[droplast/1, min/1, max/1, sum/1, delete/2, map/2, last/1, member/2, flatten/1, enumerate/1, all/2, any/2, nthtail/2]).
-import(deque,[new_deque/0, appendleft/2, appendright/2, popleft/1, popright/1, to_list/1]).
-import(sets,[new/0, add_element/2, del_element/2, filter/2, from_list/1, intersection/1, intersection/2, is_disjoint/2, is_element/2]).
-import(string, [prefix/2]).
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

part1() -> lists:sum(lists:map(fun list_to_integer/1, lists:map(fun calib/1, lists:map(fun digits/1, readlines('day1.txt'))))).

word_digits([]) -> [];
word_digits([C|T]) ->
    N = C - 48,
    case N >= 0 andalso N < 10 of
        true -> [integer_to_list(N)|word_digits(T)];
        _ ->
            S = [C|T],
            One = prefix(S, "one"),
            Two = prefix(S, "two"),
            Three = prefix(S, "three"),
            Four = prefix(S, "four"),
            Five = prefix(S, "five"),
            Six = prefix(S, "six"),
            Seven = prefix(S, "seven"),
            Eight = prefix(S, "eight"),
            Nine = prefix(S, "nine"),
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

part2() -> lists:sum(lists:map(fun list_to_integer/1, lists:map(fun calib/1, lists:map(fun word_digits/1, readlines('day1.txt'))))).
