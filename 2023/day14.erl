% Advent of Code 2023 - Day 14
-module(day14).
-export([part1/0, part2/0]).

get_map(Grid) -> get_map(Grid, maps:put(cube, sets:new(), 
                                maps:put(round, sets:new(),
                                maps:new()
                            )), 0, 0).
get_map([], Map, _, _) -> Map;
get_map([[]|G], Map, _, Y) -> get_map(G, Map, 0, Y + 1);
get_map([[$.|R]|G], Map, X, Y) -> get_map([R|G], Map, X + 1, Y);
get_map([[$O|R]|G], Map, X, Y) -> get_map([R|G],
    maps:put(round, sets:add_element({X, Y}, maps:get(round, Map)), Map),
    X + 1, Y);
get_map([[$#|R]|G], Map, X, Y) -> get_map([R|G],
    maps:put(cube, sets:add_element({X, Y}, maps:get(cube, Map)), Map),
    X + 1, Y).

shift_north_row([], _) -> [];
shift_north_row([{PX, -1}|PS], Obstacles) -> [{PX, 0}|shift_north_row(PS, Obstacles)];
shift_north_row([{PX, PY}|PS], Obstacles) ->
    IsElem = sets:is_element({PX, PY}, Obstacles),
    if
        IsElem -> [{PX, PY + 1}|shift_north_row(PS, Obstacles)];
        true -> shift_north_row([{PX, PY - 1}|PS], Obstacles)
    end.

shift_north(GridMap) -> shift_north(GridMap, 1).
shift_north(GridMap, Y) ->
    Done = sets:is_empty(sets:filter(fun({_, PY}) -> PY >= Y end,
        maps:get(round, GridMap)
    )),
    if
        Done -> GridMap;
        true ->
            RoundsRow = sets:filter(fun({_, PY}) -> PY == Y end,
                maps:get(round, GridMap)
            ),
            Obstacles = sets:union(
                [sets:filter(fun({_, PY}) -> PY < Y end,
                    maps:get(round, GridMap)
                ),
                sets:filter(fun({_, PY}) -> PY < Y end,
                    maps:get(cube, GridMap)
                )]
            ),
            NewRow = sets:from_list(shift_north_row(sets:to_list(RoundsRow), Obstacles)),
            shift_north(maps:put(round, 
                sets:union(
                    sets:subtract(maps:get(round, GridMap), RoundsRow),
                    NewRow
                ),
                GridMap
            ), Y + 1)
    end.

part1() ->
    Grid = lists:map(fun lists:droplast/1, reader:readlines('input/day14.txt')),
    MY = length(Grid),
    GridMap = get_map(Grid),
    lists:sum(
    lists:map(
        fun({_, PY}) -> MY - PY end,
        sets:to_list(maps:get(round, shift_north(GridMap)))
    )).

shift_south_row([], _, _) -> [];
shift_south_row([{PX, MaxY}|PS], Obstacles, MaxY) -> [{PX, MaxY - 1}|shift_south_row(PS, Obstacles, MaxY)];
shift_south_row([{PX, PY}|PS], Obstacles, MaxY) ->
    IsElem = sets:is_element({PX, PY}, Obstacles),
    if
        IsElem -> [{PX, PY - 1}|shift_south_row(PS, Obstacles, MaxY)];
        true -> shift_south_row([{PX, PY + 1}|PS], Obstacles, MaxY)
    end.

shift_south(GridMap, MaxY) -> shift_south(GridMap, MaxY - 2, MaxY).
shift_south(GridMap, Y, MaxY) ->
    Done = sets:is_empty(sets:filter(fun({_, PY}) -> PY =< Y end,
        maps:get(round, GridMap)
    )),
    if
        Done -> GridMap;
        true ->
            RoundsRow = sets:filter(fun({_, PY}) -> PY == Y end,
                maps:get(round, GridMap)
            ),
            Obstacles = sets:union(
                [sets:filter(fun({_, PY}) -> PY > Y end,
                    maps:get(round, GridMap)
                ),
                sets:filter(fun({_, PY}) -> PY > Y end,
                    maps:get(cube, GridMap)
                )]
            ),
            NewRow = sets:from_list(shift_south_row(sets:to_list(RoundsRow), Obstacles, MaxY)),
            shift_south(maps:put(round, 
                sets:union(
                    sets:subtract(maps:get(round, GridMap), RoundsRow),
                    NewRow
                ),
                GridMap
            ), Y - 1, MaxY)
    end.

shift_west_row([], _) -> [];
shift_west_row([{-1, PY}|PS], Obstacles) -> [{0, PY}|shift_west_row(PS, Obstacles)];
shift_west_row([{PX, PY}|PS], Obstacles) ->
    IsElem = sets:is_element({PX, PY}, Obstacles),
    if
        IsElem -> [{PX + 1, PY}|shift_west_row(PS, Obstacles)];
        true -> shift_west_row([{PX - 1, PY}|PS], Obstacles)
    end.

shift_west(GridMap) -> shift_west(GridMap, 1).
shift_west(GridMap, X) ->
    Done = sets:is_empty(sets:filter(fun({PX, _}) -> PX >= X end,
        maps:get(round, GridMap)
    )),
    if
        Done -> GridMap;
        true ->
            RoundsRow = sets:filter(fun({PX, _}) -> PX == X end,
                maps:get(round, GridMap)
            ),
            Obstacles = sets:union(
                [sets:filter(fun({PX, _}) -> PX < X end,
                    maps:get(round, GridMap)
                ),
                sets:filter(fun({PX, _}) -> PX < X end,
                    maps:get(cube, GridMap)
                )]
            ),
            NewRow = sets:from_list(shift_west_row(sets:to_list(RoundsRow), Obstacles)),
            shift_west(maps:put(round, 
                sets:union(
                    sets:subtract(maps:get(round, GridMap), RoundsRow),
                    NewRow
                ),
                GridMap
            ), X + 1)
    end.

shift_east_row([], _, _) -> [];
shift_east_row([{MaxX, PY}|PS], Obstacles, MaxX) -> [{MaxX - 1, PY}|shift_east_row(PS, Obstacles, MaxX)];
shift_east_row([{PX, PY}|PS], Obstacles, MaxX) ->
    IsElem = sets:is_element({PX, PY}, Obstacles),
    if
        IsElem -> [{PX - 1, PY}|shift_east_row(PS, Obstacles, MaxX)];
        true -> shift_east_row([{PX + 1, PY}|PS], Obstacles, MaxX)
    end.

shift_east(GridMap, MaxX) -> shift_east(GridMap, MaxX - 2, MaxX).
shift_east(GridMap, X, MaxX) ->
    Done = sets:is_empty(sets:filter(fun({PX, _}) -> PX =< X end,
        maps:get(round, GridMap)
    )),
    if
        Done -> GridMap;
        true ->
            RoundsRow = sets:filter(fun({PX, _}) -> PX == X end,
                maps:get(round, GridMap)
            ),
            Obstacles = sets:union(
                [sets:filter(fun({PX, _}) -> PX > X end,
                    maps:get(round, GridMap)
                ),
                sets:filter(fun({PX, _}) -> PX > X end,
                    maps:get(cube, GridMap)
                )]
            ),
            NewRow = sets:from_list(shift_east_row(sets:to_list(RoundsRow), Obstacles, MaxX)),
            shift_east(maps:put(round, 
                sets:union(
                    sets:subtract(maps:get(round, GridMap), RoundsRow),
                    NewRow
                ),
                GridMap
            ), X - 1, MaxX)
    end.

check_skip(GridMap, Seen, I, J) ->
    IsElem = maps:is_key(GridMap, Seen),
    if
        IsElem ->
            SI = maps:get(GridMap, Seen),
            K = (J - I) div (I - SI),
            if
                K > 1 -> {true, I + ((K - 1) * (I - SI))};
                true -> {true, I}
            end;
        true ->
            {false, maps:put(GridMap, I, Seen)}
    end.

iterate(_, _, _, _, _, I, J) when I > J -> bad;
iterate(GridMap, _, _, _, _, J, J) -> GridMap;
iterate(GridMap, MX, MY, north, Seen, I, J) ->
    GridMap2 = shift_north(GridMap),
    case check_skip(GridMap2, Seen, I, J) of
        {true, N} -> iterate(GridMap2, MX, MY, west, Seen, N + 1, J);
        {false, NextSeen} -> iterate(GridMap2, MX, MY, west, NextSeen, I + 1, J)
    end;
iterate(GridMap, MX, MY, west, Seen, I, J) ->
    GridMap2 = shift_west(GridMap),
    case check_skip(GridMap2, Seen, I, J) of
        {true, N} -> iterate(GridMap2, MX, MY, south, Seen, N + 1, J);
        {false, NextSeen} -> iterate(GridMap2, MX, MY, south, NextSeen, I + 1, J)
    end;
iterate(GridMap, MX, MY, south, Seen, I, J) ->
    GridMap2 = shift_south(GridMap, MY),
    case check_skip(GridMap2, Seen, I, J) of
        {true, N} -> iterate(GridMap2, MX, MY, east, Seen, N + 1, J);
        {false, NextSeen} -> iterate(GridMap2, MX, MY, east, NextSeen, I + 1, J)
    end;
iterate(GridMap, MX, MY, east, Seen, I, J) ->
    GridMap2 = shift_east(GridMap, MX),
    case check_skip(GridMap2, Seen, I, J) of
        {true, N} -> iterate(GridMap2, MX, MY, north, Seen, N + 1, J);
        {false, NextSeen} -> iterate(GridMap2, MX, MY, north, NextSeen, I + 1, J)
    end.

part2() ->
    Grid = lists:map(fun lists:droplast/1, reader:readlines('input/day14.txt')),
    [R|_] = Grid,
    MX = length(R),
    MY = length(Grid),
    GridMap = get_map(Grid),
    lists:sum(
    lists:map(
        fun({_, PY}) -> MY - PY end,
        sets:to_list(maps:get(round,
            iterate(GridMap, MX, MY, north, maps:new(), 0, (1000000000 * 4) - 1)
        ))
    )).
