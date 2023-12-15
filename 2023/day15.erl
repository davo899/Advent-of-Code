% Advent of Code 2023 - Day 15
-module(day15).
-export([part1/0, part2/0]).

hash_line([], N) -> [N];
hash_line([$,|L], N) -> [N|hash_line(L, 0)];
hash_line([C|L], N) -> hash_line(L, ((N + C) * 17) rem 256).

part1() -> 
    [L|_] = lists:map(fun lists:droplast/1, reader:readlines('input/day15.txt')),
    lists:sum(hash_line(L, 0)).

get_steps([], _) -> [];
get_steps([$,|L], Label) -> get_steps(L, Label);
get_steps([$=|L], Label) ->
    {N, Rest} = string:to_integer(L),
    [{lists:reverse(Label), N}|get_steps(Rest, [])];
get_steps([$-|L], Label) ->
    [{lists:reverse(Label), del}|get_steps(L, [])];
get_steps([C|L], Label) -> get_steps(L, [C|Label]).

perform_steps([], Boxes) -> Boxes;
perform_steps([{Label, del}|Steps], Boxes) ->
    [Index|_] = hash_line(Label, 0),
    perform_steps(Steps, maps:put(
        Index,
        lists:filter(fun({L, _}) -> L /= Label end, maps:get(Index, Boxes)),
        Boxes
    ));
perform_steps([{Label, N}|Steps], Boxes) ->
    [Index|_] = hash_line(Label, 0),
    InBox = lists:any(fun({L, _}) -> L == Label end, maps:get(Index, Boxes)),
    if
        InBox ->
            perform_steps(Steps, maps:put(
                Index,
                lists:map(fun({L, LN}) ->
                    if
                        L == Label -> {L, N};
                        true -> {L, LN}
                    end end,
                    maps:get(Index, Boxes)
                ),
                Boxes
            ));
        true ->
            perform_steps(Steps, maps:put(
                Index,
                [{Label, N}|maps:get(Index, Boxes)],
                Boxes
            ))
    end.

part2() ->
    [L|_] = lists:map(fun lists:droplast/1, reader:readlines('input/day15.txt')),
    Steps = get_steps(L, []),
    Boxes = maps:from_list(lists:map(fun(N) -> {N, []} end, lists:seq(0, 255))),
    NewBoxes = perform_steps(Steps, Boxes),
    lists:sum(
    lists:flatmap(fun({I, Box}) ->
        lists:map(fun({Slot, {_, Focal}}) ->
            (I + 1) * Slot * Focal end,
        Box) end,
    lists:map(fun({I, Box}) -> {I, lists:enumerate(lists:reverse(Box))} end,
        maps:to_list(NewBoxes)
    ))).
