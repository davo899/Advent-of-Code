% Advent of Code 2023 - Day 2
-module(day2).
-export([part1/0, part2/0]).

get_words(L) -> string:tokens(L, " ").

possible(L) -> possible(L, 12, 14, 13).
possible([], _, _, _) -> true;
possible([_], _, _, _) -> true;
possible([SN|[C|L]], RedMax, BlueMax, GreenMax) ->
    Colour = string:strip(string:strip(C, right, $;), right, $,),
    try
        N = list_to_integer(SN),
        case Colour of
            "red" -> if 
                N > RedMax -> false;
                true -> possible([C|L], RedMax, BlueMax, GreenMax)
            end;
            "blue" -> if 
                N > BlueMax -> false;
                true -> possible([C|L], RedMax, BlueMax, GreenMax)
            end;
            "green" -> if 
                N > GreenMax -> false;
                true -> possible([C|L], RedMax, BlueMax, GreenMax)
            end
        end
    catch
        error:badarg -> possible([C|L], RedMax, BlueMax, GreenMax)
    end.

id_sum(L) -> id_sum(L, 1).
id_sum([], _) -> 0;
id_sum([B|L], I) ->
    if
        B -> I + id_sum(L, I + 1);
        true -> id_sum(L, I + 1)
    end.

part1() -> id_sum(
    lists:map(fun possible/1,
    lists:map(fun get_words/1,
    lists:map(fun lists:droplast/1,
        reader:readlines('input/day2.txt')
    )))).

min_cubes(L) -> min_cubes(L, -1, -1, -1).
min_cubes([_], RedMax, BlueMax, GreenMax) -> RedMax * BlueMax * GreenMax;
min_cubes([SN|[C|L]], RedMax, BlueMax, GreenMax) ->
    Colour = string:strip(string:strip(C, right, $;), right, $,),
    try
        N = list_to_integer(SN),
        case Colour of
            "red" -> if 
                N > RedMax -> min_cubes([C|L], N, BlueMax, GreenMax);
                true -> min_cubes([C|L], RedMax, BlueMax, GreenMax)
            end;
            "blue" -> if 
                N > BlueMax -> min_cubes([C|L], RedMax, N, GreenMax);
                true -> min_cubes([C|L], RedMax, BlueMax, GreenMax)
            end;
            "green" -> if 
                N > GreenMax -> min_cubes([C|L], RedMax, BlueMax, N);
                true -> min_cubes([C|L], RedMax, BlueMax, GreenMax)
            end
        end
    catch
        error:badarg -> min_cubes([C|L], RedMax, BlueMax, GreenMax)
    end.

part2() -> lists:sum(
    lists:map(fun min_cubes/1,
    lists:map(fun get_words/1,
    lists:map(fun lists:droplast/1,
        reader:readlines('input/day2.txt')
    )))).
