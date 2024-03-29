% Advent of Code 2023 - Day 8
-module(day8).
-export([part1/0, part2/0]).

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
    [Dirs|[_|Lines]] = lists:map(fun lists:droplast/1, reader:readlines('input/day8.txt')),
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
    [Dirs|[_|Lines]] = lists:map(fun lists:droplast/1, reader:readlines('input/day8.txt')),
    Map = entries(Lines),
    Dists = lists:map(
            fun(C) -> find2(C, Map, Dirs, Dirs) end,
            lists:filter(
                fun(K) -> lists:last(K) == $A end,
                maps:keys(Map)
            )
        ),
    lists:foldl(fun lcm/2, 1, Dists).
