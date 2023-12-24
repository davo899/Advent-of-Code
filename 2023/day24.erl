% Advent of Code 2023 - Day 24
-module(day24).
-export([part1/0, part2/0, get_all_pairs/1]).

get_future_intersect({{{X1, Y1}, {DX1, DY1}}, {{X2, Y2}, {DX2, DY2}}}) ->
    G1 = DY1 / DX1,
    G2 = DY2 / DX2,
    C1 = Y1 - (G1 * X1),
    C2 = Y2 - (G2 * X2),
    if
        G1 == G2 -> if
            C1 == C2 -> any;
            true -> none
        end;
        true ->
            X = (C2 - C1) / (G1 - G2),
            Y = (G1 * X) + C1,
            T1 = (X - X1) / DX1,
            T2 = (X - X2) / DX2,
            if
                T1 < 0 orelse T2 < 0 -> none;
                true -> {X, Y}
            end
    end.

get_all_pairs([X|L]) -> get_all_pairs([X|L], L).
get_all_pairs([_], _) -> [];
get_all_pairs([_|[P1|P1S]], []) -> get_all_pairs([P1|P1S], P1S);
get_all_pairs([P1|P1S], [P2|P2S]) -> [{P1, P2}|get_all_pairs([P1|P1S], P2S)].

part1() ->
    Points =
        lists:map(fun([X1|[Y1|[_|[X2|[Y2|[_]]]]]]) -> {{X1, Y1}, {X2, Y2}} end,
        lists:map(fun reader:get_ints/1,
            reader:readlines('input/day24.txt')
        )),
    length(
    lists:filter(
        fun(P) -> case P of
            none -> false;
            {X, Y} ->
                200000000000000 =< X andalso
                X =< 400000000000000 andalso
                200000000000000 =< Y andalso
                Y =< 400000000000000
        end end,
        lists:map(
            fun get_future_intersect/1,
            get_all_pairs(Points)
        )
    )).

% Used online system of equations solver with the first 3 points.
part2() -> 848947587263033.
