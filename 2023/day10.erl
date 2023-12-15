% Advent of Code 2023 - Day 10
-module(day10).
-export([part1/0, part2/0]).

get_start(G) -> get_start(G, 0, 0).
get_start([[]|G], _, Y) -> get_start(G, 0, Y + 1);
get_start([[C|R]|G], X, Y) ->
    case C of
        $S -> {X, Y};
        _ -> get_start([R|G], X + 1, Y)
    end.

add_pipe(Side, Pos, Map) ->
    Pipes = maps:get(Side, Map),
    Pipes2 = sets:add_element(Pos, Pipes),
    maps:put(Side, Pipes2, Map).

get_pipes(G) -> get_pipes(G,
    maps:put(left, sets:new(),
    maps:put(right, sets:new(),
    maps:put(up, sets:new(),
    maps:put(down, sets:new(),
        maps:new()
    )))),
    0, 0
).
get_pipes([], Map, _, _) -> Map;
get_pipes([[]|G], Map, _, Y) -> get_pipes(G, Map, 0, Y + 1);
get_pipes([[P|R]|G], Map, X, Y) ->
    case P of
        $- ->
            Map2 = add_pipe(left, {X, Y}, Map),
            Map3 = add_pipe(right, {X, Y}, Map2),
            get_pipes([R|G], Map3, X + 1, Y);
        $| ->
            Map2 = add_pipe(up, {X, Y}, Map),
            Map3 = add_pipe(down, {X, Y}, Map2),
            get_pipes([R|G], Map3, X + 1, Y);
        $L ->
            Map2 = add_pipe(up, {X, Y}, Map),
            Map3 = add_pipe(right, {X, Y}, Map2),
            get_pipes([R|G], Map3, X + 1, Y);
        $J ->
            Map2 = add_pipe(left, {X, Y}, Map),
            Map3 = add_pipe(up, {X, Y}, Map2),
            get_pipes([R|G], Map3, X + 1, Y);
        $7 ->
            Map2 = add_pipe(left, {X, Y}, Map),
            Map3 = add_pipe(down, {X, Y}, Map2),
            get_pipes([R|G], Map3, X + 1, Y);
        $F ->
            Map2 = add_pipe(down, {X, Y}, Map),
            Map3 = add_pipe(right, {X, Y}, Map2),
            get_pipes([R|G], Map3, X + 1, Y);
        $S ->
            Map6 = add_pipe(left, {X, Y}, Map),
            Map5 = add_pipe(right, {X, Y}, Map6),
            Map4 = add_pipe(up, {X, Y}, Map5),
            Map3 = add_pipe(down, {X, Y}, Map4),
            get_pipes([R|G], Map3, X + 1, Y);

        _ -> get_pipes([R|G], Map, X + 1, Y)
    end.

loop(X, Y, Pipes) -> loop(X, Y, Pipes, sets:new()).
loop(X, Y, Pipes, Seen) ->
    CanLeft = (not sets:is_element({X - 1, Y}, Seen)) andalso sets:is_element({X, Y}, maps:get(left, Pipes)) andalso sets:is_element({X - 1, Y}, maps:get(right, Pipes)),
    CanRight = (not sets:is_element({X + 1, Y}, Seen)) andalso sets:is_element({X, Y}, maps:get(right, Pipes)) andalso sets:is_element({X + 1, Y}, maps:get(left, Pipes)),
    CanUp = (not sets:is_element({X, Y - 1}, Seen)) andalso sets:is_element({X, Y}, maps:get(up, Pipes)) andalso sets:is_element({X, Y - 1}, maps:get(down, Pipes)),
    CanDown = (not sets:is_element({X, Y + 1}, Seen)) andalso sets:is_element({X, Y}, maps:get(down, Pipes)) andalso sets:is_element({X, Y + 1}, maps:get(up, Pipes)),
    if
        CanLeft -> 1 + loop(X - 1, Y, Pipes, sets:add_element({X, Y}, Seen));
        CanRight -> 1 + loop(X + 1, Y, Pipes, sets:add_element({X, Y}, Seen));
        CanUp -> 1 + loop(X, Y - 1, Pipes, sets:add_element({X, Y}, Seen));
        CanDown -> 1 + loop(X, Y + 1, Pipes, sets:add_element({X, Y}, Seen));
        true -> 0
    end.

part1() -> 
    G = reader:readlines('input/day10.txt'),
    Pipes = get_pipes(G),
    {X, Y} = get_start(G),
    round(loop(X, Y, Pipes) / 2).

is_outside(X, Y, LoopMap) -> is_outside(0, LoopMap, X, Y).
is_outside(Ups, _, -1, _) -> Ups rem 2 == 0;
is_outside(Ups, LoopMap, X, Y) ->
    IsUp = sets:is_element({X, Y}, maps:get(up, LoopMap)),
    if
        IsUp -> is_outside(Ups + 1, LoopMap, X - 1, Y);
        true -> is_outside(Ups, LoopMap, X - 1, Y)
    end.

row_length([R|_]) -> length(R).

new_row(L, Y) -> new_row(L, Y, sets:new()).
new_row(0, _, Set) -> Set;
new_row(L, Y, Set) -> new_row(L - 1, Y, sets:add_element({L - 1, Y - 1}, Set)).

new_grid(X, Y) -> new_grid(X, Y, sets:new()).
new_grid(_, 0, Set) -> Set;
new_grid(X, Y, Set) -> new_grid(X, Y - 1, sets:union(Set, new_row(X, Y))).

get_loop(X, Y, Pipes) -> get_loop(X, Y, Pipes, sets:new()).
get_loop(X, Y, Pipes, Seen) ->
    CanLeft = (not sets:is_element({X - 1, Y}, Seen)) andalso sets:is_element({X, Y}, maps:get(left, Pipes)) andalso sets:is_element({X - 1, Y}, maps:get(right, Pipes)),
    CanRight = (not sets:is_element({X + 1, Y}, Seen)) andalso sets:is_element({X, Y}, maps:get(right, Pipes)) andalso sets:is_element({X + 1, Y}, maps:get(left, Pipes)),
    CanUp = (not sets:is_element({X, Y - 1}, Seen)) andalso sets:is_element({X, Y}, maps:get(up, Pipes)) andalso sets:is_element({X, Y - 1}, maps:get(down, Pipes)),
    CanDown = (not sets:is_element({X, Y + 1}, Seen)) andalso sets:is_element({X, Y}, maps:get(down, Pipes)) andalso sets:is_element({X, Y + 1}, maps:get(up, Pipes)),
    if
        CanLeft -> get_loop(X - 1, Y, Pipes, sets:add_element({X, Y}, Seen));
        CanRight -> get_loop(X + 1, Y, Pipes, sets:add_element({X, Y}, Seen));
        CanUp -> get_loop(X, Y - 1, Pipes, sets:add_element({X, Y}, Seen));
        CanDown -> get_loop(X, Y + 1, Pipes, sets:add_element({X, Y}, Seen));
        true -> sets:add_element({X, Y}, Seen)
    end.

filter_loop(PipeMap, LoopSet) ->
    maps:put(left, sets:intersection(maps:get(left, PipeMap), LoopSet),
    maps:put(right, sets:intersection(maps:get(right, PipeMap), LoopSet),
    maps:put(up, sets:intersection(maps:get(up, PipeMap), LoopSet),
    maps:put(down, sets:intersection(maps:get(down, PipeMap), LoopSet),
        maps:new()
    )))).

fix_start({SX, SY}, LoopMap) ->
    HasLeft = sets:is_element({SX - 1, SY}, maps:get(right, LoopMap)),
    HasRight = sets:is_element({SX + 1, SY}, maps:get(left, LoopMap)),
    HasUp = sets:is_element({SX, SY - 1}, maps:get(down, LoopMap)),
    HasDown = sets:is_element({SX, SY + 1}, maps:get(up, LoopMap)),
    if
        not HasLeft -> Left2 = sets:del_element({SX, SY}, maps:get(left, LoopMap));
        true -> Left2 = maps:get(left, LoopMap)
    end,
    if
        not HasRight -> Right2 = sets:del_element({SX, SY}, maps:get(right, LoopMap));
        true -> Right2 = maps:get(right, LoopMap)
    end,
    if
        not HasUp -> Up2 = sets:del_element({SX, SY}, maps:get(up, LoopMap));
        true -> Up2 = maps:get(up, LoopMap)
    end,
    if
        not HasDown -> Down2 = sets:del_element({SX, SY}, maps:get(down, LoopMap));
        true -> Down2 = maps:get(down, LoopMap)
    end,
    maps:put(left, Left2,
    maps:put(right, Right2,
    maps:put(up, Up2,
    maps:put(down, Down2,
        maps:new()
    )))).

part2() -> 
    G = reader:readlines('input/day10.txt'),
    {SX, SY} = get_start(G),
    PipeMap = get_pipes(G),
    LoopSet = get_loop(SX, SY, PipeMap),
    LoopMap = fix_start({SX, SY}, filter_loop(PipeMap, LoopSet)),
    NonLoopSet = sets:subtract(new_grid(row_length(G), length(G)), LoopSet),
    sets:size(sets:filter(
        fun({X, Y}) -> not is_outside(X, Y, LoopMap) end,
        NonLoopSet
    )).
