% Advent of Code 2023 - Day 2
-module(day2).
-import(reader,[readlines/1, get_ints/1]).
-import(lists,[droplast/1, min/1, max/1, sum/1, delete/2, map/2, last/1, member/2, flatten/1, enumerate/1, all/2, any/2, nthtail/2]).
-import(deque,[new_deque/0, appendleft/2, appendright/2, popleft/1, popright/1, to_list/1]).
-import(sets,[new/0, add_element/2, del_element/2, filter/2, from_list/1, intersection/1, intersection/2, is_disjoint/2, is_element/2]).
-import(string, [prefix/2, tokens/2, strip/3]).
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
        readlines('day2.txt')
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
        readlines('day2.txt')
    )))).
