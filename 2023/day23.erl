% Advent of Code 2023 - Day 23
-module(day23).
-export([part1/0, part2/0]).

move(_, TY, Visited, _, TY) -> sets:size(Visited);
move(X, Y, Visited, CoordMap, TY) ->
    NVisited = sets:add_element({X, Y}, Visited),
    CouldMoveTo = case maps:get({X, Y}, CoordMap) of
        $^ -> [{X, Y - 1}];
        $> -> [{X + 1, Y}];
        $v -> [{X, Y + 1}];
        $< -> [{X - 1, Y}];
        _ -> [{X, Y - 1}, {X + 1, Y}, {X, Y + 1}, {X - 1, Y}]
    end,
    CanMoveTo = lists:filter(
        fun(P) ->
            not sets:is_element(P, Visited) andalso
            maps:is_key(P, CoordMap) andalso
            maps:get(P, CoordMap) /= $#
        end,
        CouldMoveTo
    ),
    case CanMoveTo of
        [] -> -134217729; 
        CanMoveTo -> lists:max(
            lists:map(
                fun({FX, FY}) -> move(FX, FY, NVisited, CoordMap, TY) end,
                CanMoveTo
            )
        )
    end.

part1() ->
    Grid = lists:map(fun lists:droplast/1, reader:readlines('input/day23.txt')),
    CoordMap = reader:get_coord_map(".#>v<^", Grid),
    TY = lists:max(lists:map(fun({_, Y}) -> Y end, maps:keys(CoordMap))),
    move(1, 0, sets:new(), CoordMap, TY).

get_graph(Nodes, CoordMap) -> get_graph(
    Nodes, Nodes,
    maps:from_list(lists:map(
        fun(P) -> {P, maps:new()} end,
        Nodes
    )),
    CoordMap
).
get_graph([], _, Graph, _) -> Graph;
get_graph([_|Nodes1], [], Graph, CoordMap) ->
    get_graph(Nodes1, Nodes1, Graph, CoordMap);
get_graph([{X1, Y1}|Nodes1], [{X2, Y2}|Nodes2], Graph, CoordMap) ->
    if
        (X1 == X2 andalso Y1 == Y2) orelse (X1 /= X2 andalso Y1 /= Y2) ->
            get_graph([{X1, Y1}|Nodes1], Nodes2, Graph, CoordMap);
        true ->
            Connected = lists:all(
                fun(P) ->
                    maps:get(P, CoordMap) /= $# andalso
                    not lists:member(P, maps:keys(Graph))
                end,
                if
                    Y1 == Y2 ->
                        lists:map(fun(X) -> {X, Y1} end, lists:seq(min(X1, X2) + 1, max(X1, X2) - 1));
                    X1 == X2 ->
                        lists:map(fun(Y) -> {X1, Y} end, lists:seq(min(Y1, Y2) + 1, max(Y1, Y2) - 1))
                end
            ),
            NGraph = if
                Connected -> 
                    Dist = abs(X1 - X2) + abs(Y1 - Y2),
                    maps:put(
                        {X2, Y2},
                        maps:put({X1, Y1}, Dist, maps:get({X2, Y2}, Graph)),
                    maps:put(
                        {X1, Y1},
                        maps:put({X2, Y2}, Dist, maps:get({X1, Y1}, Graph)),
                        Graph
                    ));
                true -> Graph
            end,
            get_graph([{X1, Y1}|Nodes1], Nodes2, NGraph, CoordMap)
    end.

get_nodes(CoordMap) ->
    maps:keys(maps:filter(
        fun({X, Y}, V) ->
            Horizontals = length(lists:filter(
                fun(P) ->
                    maps:is_key(P, CoordMap) andalso
                    maps:get(P, CoordMap) /= $#
                end,
                [{X + 1, Y}, {X - 1, Y}]
            )),
            Verticals = length(lists:filter(
                fun(P) ->
                    maps:is_key(P, CoordMap) andalso
                    maps:get(P, CoordMap) /= $#
                end,
                [{X, Y + 1}, {X, Y - 1}]
            )),
            V /= $# andalso
            (Horizontals + Verticals > 2 orelse (Horizontals == 1 andalso Verticals == 1))
        end,
        CoordMap
    )).

move2(_, TY, _, _, TY) -> 0;
move2(X, Y, Visited, Graph, TY) ->
    NVisited = sets:add_element({X, Y}, Visited),
    CanMoveTo = maps:filter(
        fun(K, _) -> not sets:is_element(K, Visited) end,
        maps:get({X, Y}, Graph)
    ),
    NoMoves = maps:size(CanMoveTo) == 0,
    if
        NoMoves -> -134217729;
        true -> lists:max(
            maps:values(
            maps:map(
                fun({FX, FY}, Dist) ->
                    Dist + move2(FX, FY, NVisited, Graph, TY)
                end,
                CanMoveTo
            ))
        )
    end.

part2() ->
    Grid = lists:map(fun lists:droplast/1, reader:readlines('input/day23.txt')),
    CoordMap = reader:get_coord_map(".#>v<^", Grid),
    TX = lists:max(lists:map(fun({X, _}) -> X end, maps:keys(CoordMap))),
    TY = lists:max(lists:map(fun({_, Y}) -> Y end, maps:keys(CoordMap))),
    Graph = get_graph([{1, 0}|[{TX - 1, TY}|get_nodes(CoordMap)]], CoordMap),
    move2(1, 0, sets:new(), Graph, TY).
