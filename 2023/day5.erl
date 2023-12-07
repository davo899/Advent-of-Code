% Advent of Code 2023 - Day 5
-module(day5).

-import(lists,[min/1, map/2, flatten/1]).
-import(reader,[readlines/1, get_ints/1]).

-export([part1/0, part2/0]).

get_map([R|L]) when length(R) /= 3 -> get_map(L);
get_map(L) -> get_map(L, []).
get_map([], M) -> {M, []};
get_map([Entry|L], M) -> if
        length(Entry) == 3 ->
            [DR|[SR|[RL|[]]]] = Entry,
            get_map(L, [{DR, SR, RL}|M]);
        true ->
            {M, [Entry|L]}
    end.

apply_map([], X) -> X;
apply_map([{DR, SR, RL}|M], X) -> 
    if
        X >= SR andalso X < SR + RL -> DR + (X - SR);
        true -> apply_map(M, X)
    end.

part1() -> 
    [Seeds|L] = lists:map(fun reader:get_ints/1, readlines('input/day5.txt')),
    {SeedSoil, L2} = get_map(L),
    {SoilFert, L3} = get_map(L2),
    {FertWater, L4} = get_map(L3),
    {WaterLight, L5} = get_map(L4),
    {LightTemp, L6} = get_map(L5),
    {TempHum, L7} = get_map(L6),
    {HumLoc, _} = get_map(L7),
    lists:min(
        lists:map(fun(X) -> apply_map(HumLoc, X) end,
        lists:map(fun(X) -> apply_map(TempHum, X) end,
        lists:map(fun(X) -> apply_map(LightTemp, X) end,
        lists:map(fun(X) -> apply_map(WaterLight, X) end,
        lists:map(fun(X) -> apply_map(FertWater, X) end,
        lists:map(fun(X) -> apply_map(SoilFert, X) end,
        lists:map(fun(X) -> apply_map(SeedSoil, X) end,
            Seeds
        )))))))
    ).

seed_ranges([]) -> [];
seed_ranges([S|[R|L]]) -> [{S, R}|seed_ranges(L)].

apply_map_to_range(M, R) -> apply_map_to_range(M, R, false).
apply_map_to_range([], _, true) -> [];
apply_map_to_range([], {S, R}, _) -> [{S, R}];
apply_map_to_range([{DR, SR, RL}|M], {S, R}, Mapped) -> if
        S + R =< SR orelse S >= SR + RL -> apply_map_to_range(M, {S, R}, Mapped);
        S >= SR -> if
            S + R =< SR + RL -> [{DR + (S - SR), R}];
            true -> [{DR + (S - SR), (SR + RL) - S}|apply_map_to_range(M, {SR + RL, (S + R) - (SR + RL)}, false)]
        end;
        true -> if
            S + R =< SR + RL -> [{DR, (S + R) - SR}|apply_map_to_range(M, {S, SR - S}, false)];
            true -> [{DR, RL}|apply_map_to_range(M, {S, SR - S}, false) ++ apply_map_to_range(M, {SR + RL, (S + R) - (SR + RL)}, false)]
        end
    end.

part2() -> 
    [Seeds|L] = lists:map(fun reader:get_ints/1, readlines('input/day5.txt')),
    SeedRanges = seed_ranges(Seeds),
    {SeedSoil, L2} = get_map(L),
    {SoilFert, L3} = get_map(L2),
    {FertWater, L4} = get_map(L3),
    {WaterLight, L5} = get_map(L4),
    {LightTemp, L6} = get_map(L5),
    {TempHum, L7} = get_map(L6),
    {HumLoc, _} = get_map(L7),
    lists:min(
        lists:map(fun({S, _}) -> S end,
        lists:flatten(lists:map(fun(X) -> apply_map_to_range(HumLoc, X) end,
        lists:flatten(lists:map(fun(X) -> apply_map_to_range(TempHum, X) end,
        lists:flatten(lists:map(fun(X) -> apply_map_to_range(LightTemp, X) end,
        lists:flatten(lists:map(fun(X) -> apply_map_to_range(WaterLight, X) end,
        lists:flatten(lists:map(fun(X) -> apply_map_to_range(FertWater, X) end,
        lists:flatten(lists:map(fun(X) -> apply_map_to_range(SoilFert, X) end,
        lists:flatten(lists:map(fun(X) -> apply_map_to_range(SeedSoil, X) end,
            SeedRanges
        )))))))))))))))
    ).
