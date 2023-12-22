% Advent of Code 2023 - Day 22
-module(day22).
-export([part1/0, part2/0]).

intersects({X1, Y1, Z1}, {X2, Y2, Z2}, {X3, Y3, Z3}) ->
    (X2 =< X1 andalso X1 =< X3) andalso
    (Y2 =< Y1 andalso Y1 =< Y3) andalso
    (Z2 =< Z1 andalso Z1 =< Z3).
intersects({{X1, Y1, Z1}, {X2, Y2, Z2}}, {{X3, Y3, Z3}, {X4, Y4, Z4}}) ->
    lists:any(
        fun({X, Y, Z}) -> intersects({X, Y, Z}, {X3, Y3, Z3}, {X4, Y4, Z4}) end,
        if
            X1 /= X2 -> lists:map(fun(X) -> {X, Y1, Z1} end, lists:seq(min(X1, X2), max(X1, X2)));
            Y1 /= Y2 -> lists:map(fun(Y) -> {X1, Y, Z1} end, lists:seq(min(Y1, Y2), max(Y1, Y2)));
            true     -> lists:map(fun(Z) -> {X1, Y1, Z} end, lists:seq(min(Z1, Z2), max(Z1, Z2)))
        end
    ).

fall({{X1, Y1, Z1}, {X2, Y2, Z2}}, GroundBricks, DependsMap) ->
    Grounded = min(Z1, Z2) == 1,
    if
        Grounded -> {
            maps:put(maps:size(GroundBricks), {{X1, Y1, Z1}, {X2, Y2, Z2}}, GroundBricks),
            DependsMap
        };
        true ->
            Next = {{X1, Y1, Z1 - 1}, {X2, Y2, Z2 - 1}},
            Depends = maps:keys(maps:filter(
                fun(_, GroundBrick) -> intersects(Next, GroundBrick) end,
                GroundBricks
            )),
            Landed = length(Depends) > 0,
            if
                Landed -> {
                    maps:put(maps:size(GroundBricks), {{X1, Y1, Z1}, {X2, Y2, Z2}}, GroundBricks),
                    maps:put(maps:size(GroundBricks), Depends, DependsMap)
                };
                true -> fall(Next, GroundBricks, DependsMap)
            end
    end.

part1() ->
    Bricks = 
        lists:sort(
            fun({{_, _, Z11}, {_, _, Z12}}, {{_, _, Z21}, {_, _, Z22}}) -> 
                min(Z11, Z12) =< min(Z21, Z22)
            end,
        lists:map(
            fun([X1|[Y1|[Z1|[X2|[Y2|[Z2]]]]]]) ->
                {{X1, Y1, Z1}, {X2, Y2, Z2}}
            end,
            lists:map(fun reader:get_ints/1, reader:readlines('input/day22.txt'))
        )),
    FallingBricks = lists:filter(
        fun({{_, _, Z1}, {_, _, Z2}}) -> min(Z1, Z2) > 1 end,
        Bricks
    ),
    GroundBricks = maps:from_list(
        lists:enumerate(0,
        lists:filter(
            fun({{_, _, Z1}, {_, _, Z2}}) -> min(Z1, Z2) == 1 end,
            Bricks
        ))
    ),
    {FinalBricks, DependsMap} = lists:foldl(
        fun(FallingBrick, {FGroundBricks, FDependsMap}) ->
            fall(FallingBrick, FGroundBricks, FDependsMap)
        end,
        {GroundBricks, maps:new()},
        FallingBricks
    ),
    length(
    lists:filter(
        fun(N) -> not sets:is_element([N], sets:from_list(maps:values(DependsMap))) end,
        lists:seq(0, maps:size(FinalBricks) - 1)
    )).

chain_reaction(Disintegrated, DependsMap) ->
    ToDisintegrate = maps:keys(maps:filter(
        fun(K, V) ->
            not sets:is_element(K, Disintegrated) andalso
            lists:all(fun(X) -> sets:is_element(X, Disintegrated) end, V)
        end,
        DependsMap
    )),
    case ToDisintegrate of
        [] -> sets:size(Disintegrated);
        ToDisintegrate -> chain_reaction(
            sets:union([
                sets:from_list(ToDisintegrate),
                Disintegrated
            ]),
            DependsMap
        )
    end.

part2() ->
    Bricks = 
        lists:sort(
            fun({{_, _, Z11}, {_, _, Z12}}, {{_, _, Z21}, {_, _, Z22}}) -> 
                min(Z11, Z12) =< min(Z21, Z22)
            end,
        lists:map(
            fun([X1|[Y1|[Z1|[X2|[Y2|[Z2]]]]]]) ->
                {{X1, Y1, Z1}, {X2, Y2, Z2}}
            end,
            lists:map(fun reader:get_ints/1, reader:readlines('input/day22.txt'))
        )),
    FallingBricks = lists:filter(
        fun({{_, _, Z1}, {_, _, Z2}}) -> min(Z1, Z2) > 1 end,
        Bricks
    ),
    GroundBricks = maps:from_list(
        lists:enumerate(0,
        lists:filter(
            fun({{_, _, Z1}, {_, _, Z2}}) -> min(Z1, Z2) == 1 end,
            Bricks
        ))
    ),
    {FinalBricks, DependsMap} = lists:foldl(
        fun(FallingBrick, {FGroundBricks, FDependsMap}) ->
            fall(FallingBrick, FGroundBricks, FDependsMap)
        end,
        {GroundBricks, maps:new()},
        FallingBricks
    ),
    lists:sum(
    lists:map(
        fun(N) -> chain_reaction(sets:from_list([N]), DependsMap) - 1 end,
        lists:seq(0, maps:size(FinalBricks) - 1)
    )).
