% Advent of Code 2023 - Day 16
-module(day16).
-export([part1/0, part2/0]).

move_laser(X, _, _, _, _, _, Seen) when X < 0 -> Seen;
move_laser(X, _, MaxX, _, _, _, Seen) when X >= MaxX -> Seen;
move_laser(_, Y, _, _, _, _, Seen) when Y < 0 -> Seen;
move_laser(_, Y, _, MaxY, _, _, Seen) when Y >= MaxY -> Seen;
move_laser(X, Y, MaxX, MaxY, left, GridMap, Seen) ->
    IsSeen = sets:is_element({X, Y, left}, Seen),
    if
        IsSeen -> Seen;
        true ->
            NextSeen = sets:add_element({X, Y, left}, Seen),
            IsLeftMirror = sets:is_element({X, Y}, maps:get($\\, GridMap)),
            IsRightMirror = sets:is_element({X, Y}, maps:get($/, GridMap)),
            IsSplit = sets:is_element({X, Y}, maps:get($|, GridMap)),
            if
                IsLeftMirror ->
                    move_laser(X, Y - 1, MaxX, MaxY, up, GridMap, NextSeen);
                IsRightMirror ->
                    move_laser(X, Y + 1, MaxX, MaxY, down, GridMap, NextSeen);
                IsSplit -> 
                    NextSeen1 = move_laser(X, Y - 1, MaxX, MaxY, up, GridMap, NextSeen),
                    move_laser(X, Y + 1, MaxX, MaxY, down, GridMap,
                        sets:union([NextSeen, NextSeen1])
                    );
                true -> move_laser(X - 1, Y, MaxX, MaxY, left, GridMap, NextSeen)
            end
    end;
move_laser(X, Y, MaxX, MaxY, right, GridMap, Seen) ->
    IsSeen = sets:is_element({X, Y, right}, Seen),
    if
        IsSeen -> Seen;
        true ->
            NextSeen = sets:add_element({X, Y, right}, Seen),
            IsLeftMirror = sets:is_element({X, Y}, maps:get($\\, GridMap)),
            IsRightMirror = sets:is_element({X, Y}, maps:get($/, GridMap)),
            IsSplit = sets:is_element({X, Y}, maps:get($|, GridMap)),
            if
                IsLeftMirror ->
                    move_laser(X, Y + 1, MaxX, MaxY, down, GridMap, NextSeen);
                IsRightMirror ->
                    move_laser(X, Y - 1, MaxX, MaxY, up, GridMap, NextSeen);
                IsSplit -> 
                    NextSeen1 = move_laser(X, Y - 1, MaxX, MaxY, up, GridMap, NextSeen),
                    move_laser(X, Y + 1, MaxX, MaxY, down, GridMap,
                        sets:union([NextSeen, NextSeen1])
                    );
                true -> move_laser(X + 1, Y, MaxX, MaxY, right, GridMap, NextSeen)
            end
    end;
move_laser(X, Y, MaxX, MaxY, up, GridMap, Seen) ->
    IsSeen = sets:is_element({X, Y, up}, Seen),
    if
        IsSeen -> Seen;
        true ->
            NextSeen = sets:add_element({X, Y, up}, Seen),
            IsLeftMirror = sets:is_element({X, Y}, maps:get($\\, GridMap)),
            IsRightMirror = sets:is_element({X, Y}, maps:get($/, GridMap)),
            IsSplit = sets:is_element({X, Y}, maps:get($-, GridMap)),
            if
                IsLeftMirror ->
                    move_laser(X - 1, Y, MaxX, MaxY, left, GridMap, NextSeen);
                IsRightMirror ->
                    move_laser(X + 1, Y, MaxX, MaxY, right, GridMap, NextSeen);
                IsSplit -> 
                    NextSeen1 = move_laser(X - 1, Y, MaxX, MaxY, left, GridMap, NextSeen),
                    move_laser(X + 1, Y, MaxX, MaxY, right, GridMap,
                        sets:union([NextSeen, NextSeen1])
                    );
                true -> move_laser(X, Y - 1, MaxX, MaxY, up, GridMap, NextSeen)
            end
    end;
move_laser(X, Y, MaxX, MaxY, down, GridMap, Seen) ->
    IsSeen = sets:is_element({X, Y, down}, Seen),
    if
        IsSeen -> Seen;
        true ->
            NextSeen = sets:add_element({X, Y, down}, Seen),
            IsLeftMirror = sets:is_element({X, Y}, maps:get($\\, GridMap)),
            IsRightMirror = sets:is_element({X, Y}, maps:get($/, GridMap)),
            IsSplit = sets:is_element({X, Y}, maps:get($-, GridMap)),
            if
                IsLeftMirror ->
                    move_laser(X + 1, Y, MaxX, MaxY, right, GridMap, NextSeen);
                IsRightMirror ->
                    move_laser(X - 1, Y, MaxX, MaxY, left, GridMap, NextSeen);
                IsSplit -> 
                    NextSeen1 = move_laser(X - 1, Y, MaxX, MaxY, left, GridMap, NextSeen),
                    move_laser(X + 1, Y, MaxX, MaxY, right, GridMap,
                        sets:union([NextSeen, NextSeen1])
                    );
                true -> move_laser(X, Y + 1, MaxX, MaxY, down, GridMap, NextSeen)
            end
    end.

part1() ->
    Grid = lists:map(fun lists:droplast/1, reader:readlines('input/day16.txt')),
    GridMap = reader:get_grid_map([$|, $-, $/, $\\], Grid),
    [R|_] = Grid,
    Energized = move_laser(
        0, 0, length(R), length(Grid), right, GridMap, sets:new()
    ),
    sets:size(sets:from_list(
        lists:map(fun({X, Y, _}) -> {X, Y} end, sets:to_list(Energized))
    )).

part2() -> 
    Grid = lists:map(fun lists:droplast/1, reader:readlines('input/day16.txt')),
    GridMap = reader:get_grid_map([$|, $-, $/, $\\], Grid),
    [R|_] = Grid,
    lists:max(
    lists:flatmap(
        fun(I) -> [
            sets:size(sets:from_list(
                lists:map(fun({X, Y, _}) -> {X, Y} end, sets:to_list(move_laser(
                    I, 0, length(R), length(Grid), down, GridMap, sets:new()
                )))
            )),
            sets:size(sets:from_list(
                lists:map(fun({X, Y, _}) -> {X, Y} end, sets:to_list(move_laser(
                    I, length(Grid) - 1, length(R), length(Grid), up, GridMap, sets:new()
                )))
            ))
        ] end,
        lists:seq(0, length(R) - 1)
    ) ++
    lists:flatmap(
        fun(I) -> [
            sets:size(sets:from_list(
                lists:map(fun({X, Y, _}) -> {X, Y} end, sets:to_list(move_laser(
                    0, I, length(R), length(Grid), right, GridMap, sets:new()
                )))
            )),
            sets:size(sets:from_list(
                lists:map(fun({X, Y, _}) -> {X, Y} end, sets:to_list(move_laser(
                    length(R) - 1, I, length(R), length(Grid), left, GridMap, sets:new()
                )))
            ))
        ] end,
        lists:seq(0, length(Grid) - 1)
    )).
