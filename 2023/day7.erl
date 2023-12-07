% Advent of Code 2023 - Day 7
-module(day7).
-compile({no_auto_import,[size/1]}).
-import(lists,[sort/2, sum/1, map/2, enumerate/1, split/2]).
-import(string, [to_integer/1]).
-import(maps,[new/0, is_key/2, update/3, get/2, put/3, size/1, filter/2, to_list/1, from_list/1]).
-import(reader,[readlines/1, get_ints/1]).
-export([part1/0, part2/0]).

to_hand(Cards) -> to_hand(Cards, maps:new()).
to_hand([], Map) -> Map;
to_hand([Card|Cards], Map) ->
    IsKey = maps:is_key(Card, Map),
    if
        IsKey -> to_hand(Cards, maps:update(Card, maps:get(Card, Map) + 1, Map));
        true -> to_hand(Cards, maps:put(Card, 1, Map))
    end.

score(Hand) ->
    case maps:size(Hand) of
        1 -> 7;
        2 -> 
            Fours = maps:size(maps:filter(fun(_, Value) -> Value == 4 end, Hand)),
            if
                Fours == 1 -> 6;
                true -> 5
            end;
        3 ->
            Threes = maps:size(maps:filter(fun(_, Value) -> Value == 3 end, Hand)),
            if
                Threes == 1 -> 4;
                true -> 3
            end;
        4 -> 2;
        5 -> 1
    end.

card_value(Card) ->
    case Card of
        $A -> 12;
        $K -> 11;
        $Q -> 10;
        $J -> 9;
        $T -> 8;
        $9 -> 7;
        $8 -> 6;
        $7 -> 5;
        $6 -> 4;
        $5 -> 3;
        $4 -> 2;
        $3 -> 1;
        $2 -> 0
    end.

second_ordering([C1|CS1], [C2|CS2]) ->
    V1 = card_value(C1),
    V2 = card_value(C2),
    if
        V1 == V2 -> second_ordering(CS1, CS2);
        V1 < V2 -> true;
        V1 > V2 -> false
    end.

part1() -> 
    lists:sum(
    lists:map(
        fun({I, {_, _, Bid}}) -> I * Bid end,
    lists:enumerate(
    lists:sort(
        fun({CS1, V1, _}, {CS2, V2, _}) ->
            if
                V1 < V2 -> true;
                V1 > V2 -> false;
                true -> second_ordering(CS1, CS2)
            end
        end,
    lists:map(
        fun({Cards, Bid}) -> {Cards, score(to_hand(Cards)), Bid} end,
    lists:map(
        fun(S) ->
            {L, R} = lists:split(5, S),
            [_|R2] = R,
            {B, _} = string:to_integer(R2),
            {L, B} end,
        readlines('input/day7.txt')
    )))))).

score2(Hand) ->
    HasJokers = maps:is_key($J, Hand),
    if
        HasJokers -> J = maps:get($J, Hand);
        true -> J = 0
    end,
    if
        J == 5 -> 6;
        true -> 
            Classes = lists:reverse(
                lists:sort(
                lists:map(
                    fun({_, V}) -> V end,
                    lists:filter(
                        fun({K, _}) -> K /= $J end,
                        maps:to_list(Hand)
                    )
                ))
            ),
            [C1|CS1] = Classes,
            if
                C1 + J == 5 -> 6;
                C1 + J == 4 -> 5;
                true ->
                    [C2|_] = CS1,
                    if
                        C1 + J == 3 andalso C2 == 2 -> 4;
                        C1 + J == 3 -> 3;
                        C1 == 2 andalso C2 == 2 -> 2;
                        C1 + J == 2 -> 1;
                        true -> 0
                    end
            end
    end.

card_value2(Card) ->
    case Card of
        $A -> 12;
        $K -> 11;
        $Q -> 10;
        $J -> -1;
        $T -> 8;
        $9 -> 7;
        $8 -> 6;
        $7 -> 5;
        $6 -> 4;
        $5 -> 3;
        $4 -> 2;
        $3 -> 1;
        $2 -> 0
    end.

second_ordering2([C1|CS1], [C2|CS2]) ->
    V1 = card_value2(C1),
    V2 = card_value2(C2),
    if
        V1 == V2 -> second_ordering2(CS1, CS2);
        V1 < V2 -> true;
        V1 > V2 -> false
    end.

part2() -> 
    lists:sum(
    lists:map(
        fun({I, {_, _, Bid}}) -> I * Bid end,
    lists:enumerate(
    lists:sort(
        fun({CS1, V1, _}, {CS2, V2, _}) ->
            if
                V1 < V2 -> true;
                V1 > V2 -> false;
                true -> second_ordering2(CS1, CS2)
            end
        end,
    lists:map(
        fun({Cards, Bid}) -> {Cards, score2(to_hand(Cards)), Bid} end,
    lists:map(
        fun(S) ->
            {L, R} = lists:split(5, S),
            [_|R2] = R,
            {B, _} = string:to_integer(R2),
            {L, B} end,
        readlines('input/day7.txt')
    )))))).
