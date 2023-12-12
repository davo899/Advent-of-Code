% Advent of Code 2023 - Day 12
-module(day12).
-export([part1/0, part2/0]).

group_size(_, []) -> 0;
group_size(C, [LC|Line]) ->
    if
        LC == C -> 1 + group_size(C, Line);
        true -> 0
    end.

valid([], []) -> true;
valid([], [_|_]) -> false;
valid([$#|_], []) -> false;
valid([$#|Line], [Group|Groups]) ->
    Size = group_size($#, [$#|Line]),
    if
        Size /= Group -> false;
        true ->
            {_, Rest} = lists:split(Size - 1, Line),
            valid(Rest, Groups)
    end;
valid([_|Line], Groups) -> valid(Line, Groups).

arrangements(L) -> arrangements([], L, 0).
arrangements(L, [], _) -> [lists:reverse(L)];
arrangements(L, [$ |_], _) -> [lists:reverse(L)];
arrangements(L, [C|R], X) ->
    case C of
        $. -> arrangements([C|L], R, X + 1);
        $# -> arrangements([C|L], R, X + 1);
        $? ->
            arrangements([$.|L], R, X + 1) ++ arrangements([$#|L], R, X + 1)
    end.

part1() -> 
    Input = lists:map(fun lists:droplast/1, reader:readlines('input/day12.txt')),
    Groups = lists:map(fun reader:get_ints/1, Input),
    length(lists:filter(fun(B) -> B end,
        lists:flatmap(fun({AS, G}) -> lists:map(fun(A) -> valid(A, G) end, AS) end,
            lists:zip(lists:map(fun arrangements/1, Input), Groups)
        )
    )).

get_spring_part(L) -> get_spring_part([], L).
get_spring_part(L, []) -> lists:reverse(L);
get_spring_part(L, [$ |_]) -> lists:reverse(L);
get_spring_part(L, [C|R]) -> get_spring_part([C|L], R).

quintuple(L) -> L ++ L ++ L ++ L ++ L.
quintuple(L, S) -> L ++ [S|L] ++ [S|L] ++ [S|L] ++ [S|L].

valid_arrangements(LS) -> valid_arrangements(LS, maps:new()).
valid_arrangements([], _) -> 0;
valid_arrangements([L|LS], Memo) ->
    {N, NextMemo} = valid_arrangements(L, Memo),
    N + valid_arrangements(LS, NextMemo);
valid_arrangements({L, GS}, Memo) ->
    State = {L, 0, GS},
    NextMemo = valid_arrangements(State, Memo),
    {maps:get(State, NextMemo), NextMemo};
valid_arrangements(State, Memo) ->
    Seen = maps:is_key(State, Memo),
    if
        Seen -> Memo;
        true ->
            case State of
                {L, G, [G]} ->
                    NextMemo = Memo,
                    Valid = lists:all(fun(C) -> C /= $# end, L),
                    if
                        Valid -> N = 1;
                        true -> N = 0
                    end;
                {[], _, _} ->
                    NextMemo = Memo,
                    N = 0;

                {[$.|L], 0, GS} ->
                    Next = {L, 0, GS},
                    NextMemo = valid_arrangements(Next, Memo),
                    N = maps:get(Next, NextMemo);
                {[$.|L], G, [G|[G2|GS]]} ->
                    Next = {L, 0, [G2|GS]},
                    NextMemo = valid_arrangements(Next, Memo),
                    N = maps:get(Next, NextMemo);
                {[$.|_], _, _} ->
                    NextMemo = Memo,
                    N = 0;

                {[$#|_], G, [G|_]} ->
                    NextMemo = Memo,
                    N = 0;
                {[$#|L], G, GS} ->
                    Next = {L, G + 1, GS},
                    NextMemo = valid_arrangements(Next, Memo),
                    N = maps:get(Next, NextMemo);

                {[$?|L], G, GS} ->
                    Next1 = {[$.|L], G, GS},
                    NextMemo1 = valid_arrangements(Next1, Memo),
                    Next2 = {[$#|L], G, GS},
                    NextMemo = valid_arrangements(Next2, NextMemo1),
                    N = maps:get(Next1, NextMemo) + maps:get(Next2, NextMemo)
            end,
            maps:put(State, N, NextMemo)
    end.

part2() -> 
    Input = lists:map(fun lists:droplast/1,
        reader:readlines('input/day12.txt')
    ),
    Springs = lists:map(fun(L) -> quintuple(L, $?) end,
        lists:map(fun get_spring_part/1, Input)
    ),
    Groups = lists:map(fun quintuple/1, lists:map(fun reader:get_ints/1, Input)),
    valid_arrangements(lists:zip(Springs, Groups)).
