% Advent of Code 2023 - Day 17
-module(day17).
-export([part1/0, part2/0]).

move_crucible([], Dists, _, _, _) -> Dists;
move_crucible([{X, Y, Direction, Consecutive, Dist}|Queue], Dists, CoordMap, MX, MY) ->
    Seen = maps:is_key({X, Y, Direction, Consecutive}, Dists),
    if
        Seen -> move_crucible(Queue, Dists, CoordMap, MX, MY);
        true ->
            NDists = maps:put({X, Y, Direction, Consecutive}, Dist, Dists),
            NQueue =
                lists:sort(fun({_, _, _, _, D1}, {_, _, _, _, D2}) -> D1 =< D2 end,
                lists:filter(fun(A) -> A /= invalid end,
                lists:map(
                    fun({I, {DX, DY}}) ->
                        NX = X + DX,
                        NY = Y + DY,
                        NDirection = I,
                        NConsecutive = if
                            Direction /= NDirection -> 1;
                            true -> Consecutive + 1
                        end,
                        if
                            0 =< NX andalso
                            NX < MX andalso
                            0 =< NY andalso
                            NY < MY andalso
                            NConsecutive =< 3 andalso
                            ((((NDirection - 1) + 2) rem 4) + 1) /= Direction ->
                                {NX, NY, NDirection, NConsecutive, Dist + (maps:get({NX, NY}, CoordMap) - 48)};

                            true -> invalid
                        end
                    end,
                    lists:enumerate([{0, 1}, {1, 0}, {0, -1}, {-1, 0}])
                )) ++ Queue),
            move_crucible(NQueue, NDists, CoordMap, MX, MY)
    end.

part1() ->
    Grid = lists:map(fun lists:droplast/1, reader:readlines('input/day17.txt')),
    [R|_] = Grid,
    CoordMap = reader:get_coord_map("123456789", Grid),
    lists:min(
    maps:values(
    maps:filter(
        fun({X, Y, _, _}, _) -> X == length(R) - 1 andalso Y == length(Grid) - 1 end,
        move_crucible([{0, 0, -1, 0, 0}], maps:new(), CoordMap, length(R), length(Grid))
    ))).

move_crucible2([], Dists, _, _, _) -> Dists;
move_crucible2([{X, Y, Direction, Consecutive, Dist}|Queue], Dists, CoordMap, MX, MY) ->
    Seen = maps:is_key({X, Y, Direction, Consecutive}, Dists),
    if
        Seen -> move_crucible2(Queue, Dists, CoordMap, MX, MY);
        true ->
            NDists = maps:put({X, Y, Direction, Consecutive}, Dist, Dists),
            NQueue =
                lists:sort(fun({_, _, _, _, D1}, {_, _, _, _, D2}) -> D1 =< D2 end,
                lists:filter(fun(A) -> A /= invalid end,
                lists:map(
                    fun({I, {DX, DY}}) ->
                        NX = X + DX,
                        NY = Y + DY,
                        NDirection = I,
                        NConsecutive = if
                            Direction /= NDirection -> 1;
                            true -> Consecutive + 1
                        end,
                        if
                            0 =< NX andalso
                            NX < MX andalso
                            0 =< NY andalso
                            NY < MY andalso
                            NConsecutive =< 10 andalso
                            (Direction == NDirection orelse Consecutive >= 4 orelse Consecutive == -1) andalso
                            ((((NDirection - 1) + 2) rem 4) + 1) /= Direction ->
                                {NX, NY, NDirection, NConsecutive, Dist + (maps:get({NX, NY}, CoordMap) - 48)};

                            true -> invalid
                        end
                    end,
                    lists:enumerate([{0, 1}, {1, 0}, {0, -1}, {-1, 0}])
                )) ++ Queue),
            move_crucible2(NQueue, NDists, CoordMap, MX, MY)
    end.

part2() ->
    Grid = lists:map(fun lists:droplast/1, reader:readlines('input/day17.txt')),
    [R|_] = Grid,
    CoordMap = reader:get_coord_map("123456789", Grid),
    lists:min(
    maps:values(
    maps:filter(
        fun({X, Y, _, _}, _) -> X == length(R) - 1 andalso Y == length(Grid) - 1 end,
        move_crucible2([{0, 0, -1, -1, 0}], maps:new(), CoordMap, length(R), length(Grid))
    ))).
