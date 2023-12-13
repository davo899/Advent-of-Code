% Advent of Code 2023 - Day 13
-module(day13).
-export([part1/0, part2/0]).

get_grids(Input) -> get_grids(Input, []).
get_grids([], G) -> [G];
get_grids([""|Input], G) -> [G|get_grids(Input, [])];
get_grids([Line|Input], G) -> get_grids(Input, G ++ [Line]).

grid_to_set(G) -> grid_to_set(G, sets:new(), 0, 0).
grid_to_set([], Set, _, _) -> Set;
grid_to_set([[]|G], Set, _, Y) -> grid_to_set(G, Set, 0, Y + 1);
grid_to_set([[$#|L]|G], Set, X, Y) ->
    grid_to_set([L|G], sets:add_element({X, Y}, Set), X + 1, Y);
grid_to_set([[$.|L]|G], Set, X, Y) ->
    grid_to_set([L|G], Set, X + 1, Y).

get_horizontal_mirrors({[G|_], Set}) -> 
    N = lists:search(
        fun(X) -> X /= 0 end,
        lists:map(fun(I) -> get_horizontal_mirror(I, Set) end,
            lists:seq(0, length(G))
        )
    ),
    case N of
        {value, Value} -> Value;
        false -> 0
    end.
get_horizontal_mirror(X, Set) ->
    OffSet = sets:from_list(lists:map(
        fun({PX, PY}) -> {PX - X + 0.5, PY} end,
        sets:to_list(Set)
    )),
    MinX = lists:min(lists:map(fun({PX, _}) -> PX end, sets:to_list(OffSet))),
    MaxX = lists:max(lists:map(fun({PX, _}) -> PX end, sets:to_list(OffSet))),
    MaxDist = min(abs(MinX), abs(MaxX)),
    OffSetFiltered = sets:filter(fun({PX, _}) -> abs(PX) =< MaxDist end, OffSet),
    Left = sets:filter(fun({PX, _}) -> PX < 0 end, OffSetFiltered),
    Right = sets:from_list(lists:map(
        fun({PX, PY}) -> {-PX, PY} end,
        sets:to_list(sets:subtract(OffSetFiltered, Left))
    )),
    Valid = sets:is_subset(Left, Right) andalso sets:is_subset(Right, Left),
    if
        Valid -> X;
        true -> 0
    end.

get_vertical_mirrors({G, Set}) -> 
    N = lists:search(
        fun(X) -> X /= 0 end,
        lists:map(fun(I) -> get_vertical_mirror(I, Set) end,
            lists:seq(0, length(G))
        )
    ),
    case N of
        {value, Value} -> Value;
        false -> 0
    end.
get_vertical_mirror(Y, Set) ->
    OffSet = sets:from_list(lists:map(
        fun({PX, PY}) -> {PX, PY - Y + 0.5} end,
        sets:to_list(Set)
    )),
    MinY = lists:min(lists:map(fun({_, PY}) -> PY end, sets:to_list(OffSet))),
    MaxY = lists:max(lists:map(fun({_, PY}) -> PY end, sets:to_list(OffSet))),
    MaxDist = min(abs(MinY), abs(MaxY)),
    OffSetFiltered = sets:filter(fun({_, PY}) -> abs(PY) =< MaxDist end, OffSet),
    Up = sets:filter(fun({_, PY}) -> PY < 0 end, OffSetFiltered),
    Down = sets:from_list(lists:map(
        fun({PX, PY}) -> {PX, -PY} end,
        sets:to_list(sets:subtract(OffSetFiltered, Up))
    )),
    Valid = sets:is_subset(Up, Down) andalso sets:is_subset(Down, Up),
    if
        Valid -> Y;
        true -> 0
    end.

part1() ->
    Input = lists:map(
        fun lists:droplast/1, reader:readlines('input/day13.txt')
    ),
    Grids = get_grids(Input),
    GridSets = lists:map(fun grid_to_set/1, Grids),
    VMirrors = lists:map(fun get_vertical_mirrors/1,
        lists:zip(Grids, GridSets)
    ),
    HMirrors = lists:map(fun get_horizontal_mirrors/1,
        lists:zip(Grids, GridSets)
    ),
    lists:sum(lists:map(
        fun({V, H}) -> if
            V /= 0 -> 100 * V;
            true -> H
        end end,
        lists:zip(VMirrors, HMirrors)
    )).



get_horizontal_mirrors2({[G|_], Set}) -> 
    N = lists:search(
        fun(X) -> X /= 0 end,
        lists:map(fun(I) -> get_horizontal_mirror2(I, Set) end,
            lists:seq(0, length(G))
        )
    ),
    case N of
        {value, Value} -> Value;
        false -> 0
    end.
get_horizontal_mirror2(X, Set) ->
    OffSet = sets:from_list(lists:map(
        fun({PX, PY}) -> {PX - X + 0.5, PY} end,
        sets:to_list(Set)
    )),
    MinX = lists:min(lists:map(fun({PX, _}) -> PX end, sets:to_list(OffSet))),
    MaxX = lists:max(lists:map(fun({PX, _}) -> PX end, sets:to_list(OffSet))),
    MaxDist = min(abs(MinX), abs(MaxX)),
    OffSetFiltered = sets:filter(fun({PX, _}) -> abs(PX) =< MaxDist end, OffSet),
    Left = sets:filter(fun({PX, _}) -> PX < 0 end, OffSetFiltered),
    Right = sets:from_list(lists:map(
        fun({PX, PY}) -> {-PX, PY} end,
        sets:to_list(sets:subtract(OffSetFiltered, Left))
    )),
    Diff = sets:size(sets:subtract(Left, Right)) + sets:size(sets:subtract(Right, Left)),
    if
        Diff == 1 -> X;
        true -> 0
    end.

get_vertical_mirrors2({G, Set}) -> 
    N = lists:search(
        fun(X) -> X /= 0 end,
        lists:map(fun(I) -> get_vertical_mirror2(I, Set) end,
            lists:seq(0, length(G))
        )
    ),
    case N of
        {value, Value} -> Value;
        false -> 0
    end.
get_vertical_mirror2(Y, Set) ->
    OffSet = sets:from_list(lists:map(
        fun({PX, PY}) -> {PX, PY - Y + 0.5} end,
        sets:to_list(Set)
    )),
    MinY = lists:min(lists:map(fun({_, PY}) -> PY end, sets:to_list(OffSet))),
    MaxY = lists:max(lists:map(fun({_, PY}) -> PY end, sets:to_list(OffSet))),
    MaxDist = min(abs(MinY), abs(MaxY)),
    OffSetFiltered = sets:filter(fun({_, PY}) -> abs(PY) =< MaxDist end, OffSet),
    Up = sets:filter(fun({_, PY}) -> PY < 0 end, OffSetFiltered),
    Down = sets:from_list(lists:map(
        fun({PX, PY}) -> {PX, -PY} end,
        sets:to_list(sets:subtract(OffSetFiltered, Up))
    )),
    Diff = sets:size(sets:subtract(Up, Down)) + sets:size(sets:subtract(Down, Up)),
    if
        Diff == 1 -> Y;
        true -> 0
    end.

part2() ->
    Input = lists:map(
        fun lists:droplast/1, reader:readlines('input/day13.txt')
    ),
    Grids = get_grids(Input),
    GridSets = lists:map(fun grid_to_set/1, Grids),
    VMirrors = lists:map(fun get_vertical_mirrors2/1,
        lists:zip(Grids, GridSets)
    ),
    HMirrors = lists:map(fun get_horizontal_mirrors2/1,
        lists:zip(Grids, GridSets)
    ),
    lists:sum(lists:map(
        fun({V, H}) -> if
            V /= 0 -> 100 * V;
            true -> H
        end end,
        lists:zip(VMirrors, HMirrors)
    )).
