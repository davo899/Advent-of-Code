-module(reader).
-export([readlines/1, get_ints/1, get_coords_of/2, get_grid_map/2, get_coord_map/2]).

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try get_all_lines(Device)
      after file:close(Device)
    end.

get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> [Line | get_all_lines(Device)]
    end.

get_ints([]) -> [];
get_ints(S) ->
    case string:to_integer(S) of
        {error, _} -> [_|T] = S, get_ints(T);
        {Int, Rest} -> [Int|get_ints(Rest)]
    end.

get_coords_of(C, G) -> get_coords_of(C, G, 0, 0, sets:new()).
get_coords_of(_, [], _, _, Set) -> Set;
get_coords_of(C, [[]|G], _, Y, Set) -> get_coords_of(C, G, 0, Y + 1, Set);
get_coords_of(C, [[C|R]|G], X, Y, Set) ->
    get_coords_of(C, [R|G], X + 1, Y, sets:add_element({X, Y}, Set));
get_coords_of(C, [[_|R]|G], X, Y, Set) -> get_coords_of(C, [R|G], X + 1, Y, Set).

get_grid_map(CS, Grid) ->
    maps:from_list(lists:map(fun(C) -> {C, get_coords_of(C, Grid)} end, CS)).

get_coord_map(CS, Grid) ->
    maps:from_list(lists:flatmap(fun(C) ->
        lists:map(fun(P) -> {P, C} end,
            sets:to_list(get_coords_of(C, Grid))
        ) end,
    CS)).
