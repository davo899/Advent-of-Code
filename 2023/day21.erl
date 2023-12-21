% Advent of Code 2023 - Day 21
-module(day21).
-export([part1/0, part2/0]).

next_reachable(Current, GridMap) ->
    Plots = maps:get($., GridMap),
    sets:union(lists:map(
        fun({X, Y}) -> sets:intersection(
            sets:from_list([{X + 1, Y}, {X - 1, Y}, {X, Y + 1}, {X, Y - 1}]),
            Plots
        ) end,
        sets:to_list(Current)
    )).

reachable_after(Current, _, 0) -> Current;
reachable_after(Current, GridMap, N) ->
    reachable_after(next_reachable(Current, GridMap), GridMap, N - 1).

part1() ->
    Grid = lists:map(fun lists:droplast/1, reader:readlines('input/day21.txt')),
    GridMap = reader:get_grid_map("#.S", Grid),
    [Start] = sets:to_list(maps:get($S, GridMap)),
    NGridMap = maps:put($., sets:add_element(Start, maps:get($., GridMap)), GridMap),
    sets:size(reachable_after(sets:from_list([Start]), NGridMap, 64)).

mod(X, Y) when X > 0 -> X rem Y;
mod(X, Y) when X < 0 ->   
    K = (-X div Y) + 1,
    PositiveX = X + K*Y,
    PositiveX rem Y;
mod(0, _) -> 0.

get_near_dists(Deque, Map, MX, MY, Plots) ->
    Done = deque:is_empty(Deque),
    if
        Done -> Map;
        true ->
            {{X, Y, D}, NDeque} = deque:popleft(Deque),
            {TX, TY} = {X div MX, Y div MY},
            IsPlot = sets:is_element({mod(X, MX), mod(Y, MY)}, Plots),
            IsSeen = maps:is_key({X, Y}, Map),
            if
                not IsPlot orelse IsSeen orelse abs(TX) > 4 orelse abs(TY) > 4 ->
                    get_near_dists(NDeque, Map, MX, MY, Plots);
                true ->
                    NMap = maps:put({X, Y}, D, Map),
                    NNDeque = lists:foldl(
                        fun({FX, FY}, FDeque) -> deque:appendright(FDeque, {FX, FY, D + 1}) end,
                        NDeque,
                        [{X + 1, Y}, {X - 1, Y}, {X, Y + 1}, {X, Y - 1}]
                    ),
                    get_near_dists(NNDeque, NMap, MX, MY, Plots)
            end
    end.

solve(D, V, L, MX) ->
    case get({solve, D, V, L, MX}) of
        undefined ->
            AMT = (L - D) div MX,
            LMod = mod(L, 2),
            R = lists:sum(lists:map(
                fun(X) -> 
                    DMod = mod(D + (MX * X), 2),
                    if
                        D + (MX * X) =< L andalso DMod == LMod -> if
                            V == 2 -> X + 1;
                            true -> 1
                        end;
                        true -> 0
                    end
                end,
                lists:seq(1, AMT)
            )),
            put({solve, D, V, L, MX}, R),
            R;

        N -> N
    end.

opt(_, _, _, 4, _, _, _, _) -> 0;
opt(X, Y, 4, TY, MX, MY, NearDists, L) -> opt(X, Y, -3, TY + 1, MX, MY, NearDists, L);
opt(X, Y, TX, TY, MX, MY, NearDists, L) ->
    D = maps:get({(TX * MX) + X, (TY * MY) + Y}, NearDists),
    DMod = mod(D, 2),
    LMod = mod(L, 2),
    Ans = if
        DMod == LMod andalso D =< L -> 1;
        true -> 0
    end,
    if
        (TX == -3 orelse TX == 3) andalso (TY == -3 orelse TY == 3) -> Ans + solve(D, 2, L, MX);
        TX == -3 orelse TX == 3 orelse TY == -3 orelse TY == 3 -> Ans + solve(D, 1, L, MX);
        true -> Ans
    end + opt(X, Y, TX + 1, TY, MX, MY, NearDists, L).

solve2(_, MY, _, MY, _, _) -> 0;
solve2(MX, Y, MX, MY, NearDists, L) -> solve2(0, Y + 1, MX, MY, NearDists, L);
solve2(X, Y, MX, MY, NearDists, L) ->
    CanReach = maps:is_key({X, Y}, NearDists),
    if
        CanReach -> opt(X, Y, -3, -3, MX, MY, NearDists, L) + solve2(X + 1, Y, MX, MY, NearDists, L);
        true -> solve2(X + 1, Y, MX, MY, NearDists, L)
    end.

part2() ->
    Grid = lists:map(fun lists:droplast/1, reader:readlines('input/day21.txt')),
    [R|_] = Grid,
    {MX, MY} = {length(R), length(Grid)},
    GridMap = reader:get_grid_map("#.S", Grid),
    [{SX, SY}] = sets:to_list(maps:get($S, GridMap)),
    NGridMap = maps:put($., sets:add_element({SX, SY}, maps:get($., GridMap)), GridMap),
    Plots = maps:get($., NGridMap),
    NearDists = get_near_dists(deque:from_list([{SX, SY, 0}]), maps:new(), MX, MY, Plots),
    solve2(0, 0, MX, MY, NearDists, 26501365).
