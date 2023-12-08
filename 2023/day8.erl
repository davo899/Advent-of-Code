% Advent of Code 2023 - Day 8
-module(day8).
-compile({no_auto_import,[size/1]}).

-import(lists,[foldl/3, sort/2, seq/2, droplast/1, min/1, max/1, sum/1, delete/2, map/2, last/1, member/2, flatten/1, enumerate/1, all/2, any/2, nthtail/2, split/2]).
-import(string, [prefix/2, tokens/2, strip/3, to_integer/1]).
-import(math, [pow/2]).
-import(maps,[new/0, is_key/2, update/3, get/2, put/3, size/1, filter/2, fold/3, to_list/1, from_list/1, keys/1]).

-import(reader,[readlines/1, get_ints/1]).

-export([part1/0, part2/0, find2/4]).

entries(Lines) -> entries(Lines, maps:new()).
entries([], Map) -> Map;
entries([L|Lines], Map) ->
    {LHS, R} = lists:split(3, L),
    {_, R2} = lists:split(4, R),
    {Left, R3} = lists:split(3, R2),
    {_, R4} = lists:split(2, R3),
    {Right, _} = lists:split(3, R4),
    entries(Lines, maps:put(LHS, {Left, Right}, Map)).

move(Dir, Map, Current) ->
    {L, R} = maps:get(Current, Map),
    if
        Dir == $L -> L;
        Dir == $R -> R
    end.

find(T, T, _, _, _) -> 0;
find(T, C, M, [], SDirs) -> find(T, C, M, SDirs, SDirs);
find(Target, Current, Map, [Dir|Dirs], SDirs) -> 
    1 + find(Target, move(Dir, Map, Current), Map, Dirs, SDirs).

part1() -> 
    [Dirs|[_|Lines]] = lists:map(fun lists:droplast/1, readlines('input/day8.txt')),
    Map = entries(Lines),
    find("ZZZ", "AAA", Map, Dirs, Dirs).

find2(C, M, [], SDirs) -> find2(C, M, SDirs, SDirs);
find2(Current, Map, [Dir|Dirs], SDirs) -> 
    Ended = lists:last(Current) == $Z,
    if
        Ended -> 0;
        true -> 1 + find2(move(Dir, Map, Current), Map, Dirs, SDirs)
    end.

gcd(A,B) when A == 0; B == 0 -> 0;
gcd(A,B) when A == B -> A;
gcd(A,B) when A > B -> gcd(A-B, B);
gcd(A,B) -> gcd(A, B-A).

lcm(A,B) -> (A*B) div gcd(A, B).

part2() -> 
    [Dirs|[_|Lines]] = lists:map(fun lists:droplast/1, readlines('input/day8.txt')),
    Map = entries(Lines),
    Dists = lists:map(
            fun(C) -> find2(C, Map, Dirs, Dirs) end,
            lists:filter(
                fun(K) -> lists:last(K) == $A end,
                maps:keys(Map)
            )
        ),
    lists:foldl(fun lcm/2, 1, Dists).
