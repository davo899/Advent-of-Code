% Advent of Code 2023 - Day 3
-module(day3).
-export([part1/0, part2/0]).

get_part_number(_, [$.|_], _) -> error;
get_part_number([], _, X) when X > 0 -> error;
get_part_number([C|F], R, X) when C >= 48 andalso C < 58 -> get_part_number(F, [C|R], X - 1);
get_part_number(_, R, X) ->
    case string:to_integer(R) of
        {error, _} -> error;
        {Int, Rest} -> {Int, Rest, X + (length(R) - length(Rest))}
    end.

get_part_numbers(G, Set) -> get_part_numbers([], G, Set, {0, 0}).
get_part_numbers(_, [], _, _) -> [];
get_part_numbers(F, [[]|G], Set, {_, Y}) -> get_part_numbers(F, G, Set, {0, Y + 1});
get_part_numbers(F, [[C|R]|G], Set, {X, Y}) ->
    IsElem = sets:is_element({X, Y}, Set),
    if
        IsElem ->
            if
                C >= 48 andalso C < 58 ->
                    {N, R2, X2} = get_part_number(F, [C|R], X),
                    [N|get_part_numbers([], [R2|G], Set, {X2, Y})];
                true -> 
                    get_part_numbers([C|F], [R|G], Set, {X + 1, Y})
            end;

        true -> get_part_numbers([C|F], [R|G], Set, {X + 1, Y})
    end.


get_search_set(G) -> get_search_set(sets:new(), G, {0, 0}).
get_search_set(Set, [], _) -> Set;
get_search_set(Set, [[]|G], {_, Y}) -> get_search_set(Set, G, {0, Y + 1});
get_search_set(Set, [[C|R]|G], {X, Y}) -> if
        C == $. orelse (C >= 48 andalso C < 58) -> get_search_set(Set, [R|G], {X + 1, Y});
        true -> get_search_set(
            sets:add_element({X + 1, Y + 1},
            sets:add_element({X + 1, Y},
            sets:add_element({X + 1, Y - 1},
            sets:add_element({X, Y + 1},
            sets:add_element({X, Y - 1},
            sets:add_element({X - 1, Y + 1},
            sets:add_element({X - 1, Y},
            sets:add_element({X - 1, Y - 1},
                Set
            )))))))), [R|G], {X + 1, Y})
    end.

part1() -> 
    G = lists:map(fun lists:droplast/1, reader:readlines('input/day3.txt')),
    lists:sum(get_part_numbers(G, get_search_set(G))).

get_part_number(error) -> error;
get_part_number({N, _, _}) -> N.
get_part_number(_, _, _, TX) when TX < 0 -> error;
get_part_number(F, R, X, X) -> get_part_number(get_part_number(F, R, X));
get_part_number(F, [C|R], X, TX) -> get_part_number([C|F], R, X + 1, TX).
get_part_number([], _) -> error;
get_part_number([R|_], {X, 0}) -> get_part_number([], R, 0, X);
get_part_number([_|G], {X, Y}) -> get_part_number(G, {X, Y - 1}).

get_gear_part_numbers(G) -> get_gear_part_numbers(G, {0, 0}, G).
get_gear_part_numbers([], _, _) -> [];
get_gear_part_numbers([[]|G], {_, Y}, SG) -> get_gear_part_numbers(G, {0, Y + 1}, SG);
get_gear_part_numbers([[$*|R]|G], {X, Y}, SG) ->
    Top = get_part_number(SG, {X, Y - 1}),
    Bot = get_part_number(SG, {X, Y + 1}),
    case Top of
        error -> NS = [get_part_number(SG, {X - 1, Y - 1})|[get_part_number(SG, {X + 1, Y - 1})]];
        N1 -> NS = [N1]
    end,
    case Bot of
        error -> NS2 = [get_part_number(SG, {X - 1, Y + 1})|[get_part_number(SG, {X + 1, Y + 1})|NS]];
        N2 -> NS2 = [N2|NS]
    end,
    NS3 = [get_part_number(SG, {X - 1, Y})|[get_part_number(SG, {X + 1, Y})|NS2]],
    NS4 = lists:filter(fun(N) -> N /= error end, NS3),
    [NS4|get_gear_part_numbers([R|G], {X + 1, Y}, SG)];
get_gear_part_numbers([[_|R]|G], {X, Y}, SG) -> get_gear_part_numbers([R|G], {X + 1, Y}, SG).

part2() ->
    lists:sum(
    lists:map(fun([M|[N]]) -> M * N end,
    lists:filter(fun(NS) -> length(NS) == 2 end,
        get_gear_part_numbers(lists:map(fun lists:droplast/1, reader:readlines('input/day3.txt')))
    ))).
