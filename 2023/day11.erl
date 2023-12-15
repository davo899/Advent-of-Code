% Advent of Code 2023 - Day 11
-module(day11).
-export([part1/0, part2/0]).

get_galaxies(G) -> get_galaxies(G, sets:new(), 0, 0).
get_galaxies([], Set, _, _) -> Set;
get_galaxies([[]|G], Set, _, Y) -> get_galaxies(G, Set, 0, Y + 1);
get_galaxies([[C|R]|G], Set, X, Y) ->
    case C of
        $# -> get_galaxies([R|G], sets:add_element({X, Y}, Set), X + 1, Y);
        $. -> get_galaxies([R|G], Set, X + 1, Y)
    end.

expand_rows(-1, Set, _) -> Set;
expand_rows(Y, Set, N) ->
    ShouldDouble = sets:size(sets:filter(fun({_, PY}) -> PY == Y end, Set)) == 0,
    if
        ShouldDouble -> expand_rows(Y - 1,
            sets:from_list(lists:map(
                fun({PX, PY}) -> if
                    PY > Y -> {PX, PY + N};
                    true -> {PX, PY}
                end end,
                sets:to_list(Set)
            )), N);

        true -> expand_rows(Y - 1, Set, N)
    end.

expand_cols(-1, Set, _) -> Set;
expand_cols(X, Set, N) ->
    ShouldDouble = sets:size(sets:filter(fun({PX, _}) -> PX == X end, Set)) == 0,
    if
        ShouldDouble -> expand_cols(X - 1,
            sets:from_list(lists:map(
                fun({PX, PY}) -> if
                    PX > X -> {PX + N, PY};
                    true -> {PX, PY}
                end end,
                sets:to_list(Set)
            )), N);

        true -> expand_cols(X - 1, Set, N)
    end.

dists(L) -> dists(L, L, L).
dists(_, [], _) -> [];
dists([], [_|L], [_|SL]) -> dists(SL, L, SL);
dists([{X1, Y1}|L1], [{X2, Y2}|L2], SL) ->
    [abs(X1 - X2) + abs(Y1 - Y2)|dists(L1, [{X2, Y2}|L2], SL)].

part1() -> 
    G = lists:map(fun lists:droplast/1, reader:readlines('input/day11.txt')),
    Galaxies = get_galaxies(G),
    [R|_] = G,
    Galaxies2 = expand_rows(length(G), Galaxies, 1),
    Galaxies3 = expand_cols(length(R), Galaxies2, 1),
    lists:sum(dists(sets:to_list(Galaxies3))).

part2() ->
    G = lists:map(fun lists:droplast/1, reader:readlines('input/day11.txt')),
    Galaxies = get_galaxies(G),
    [R|_] = G,
    Galaxies2 = expand_rows(length(G), Galaxies, 999999),
    Galaxies3 = expand_cols(length(R), Galaxies2, 999999),
    lists:sum(dists(sets:to_list(Galaxies3))).
