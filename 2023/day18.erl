% Advent of Code 2023 - Day 18
-module(day18).
-export([part1/0, part2/0]).

flood_fill(P, Boundary, Max) ->
    flood_fill(deque:appendleft(deque:new(), P), sets:new(), Boundary, Max).
flood_fill(Deque, Seen, Boundary, Max) ->
    Infinite = sets:size(Seen) > Max,
    Done = deque:is_empty(Deque),
    if
        Infinite -> infinite;
        Done -> Seen;
        true ->
            {{X, Y}, NDeque} = deque:popleft(Deque),
            IsSeen = sets:is_element({X, Y}, Seen),
            IsBoundary = sets:is_element({X, Y}, Boundary),
            if
                IsSeen orelse IsBoundary -> flood_fill(NDeque, Seen, Boundary, Max);
                true ->
                    NDeque1 = deque:appendright(NDeque, {X + 1, Y}),
                    NDeque2 = deque:appendright(NDeque1, {X - 1, Y}),
                    NDeque3 = deque:appendright(NDeque2, {X, Y + 1}),
                    NDeque4 = deque:appendright(NDeque3, {X, Y - 1}),
                    NSeen = sets:add_element({X, Y}, Seen),
                    flood_fill(NDeque4, NSeen, Boundary, Max)
            end
    end.

parse_line([D|[$ |Rest1]]) ->
    Direction = case D of
        $R -> right;
        $D -> down;
        $U -> up;
        $L -> left
    end,
    {N, _} = string:to_integer(Rest1),
    {Direction, N}.

get_boundary(_, [], Boundary) -> Boundary;
get_boundary({X, Y}, [{D, N}|Steps], Boundary) ->
    {DX, DY} = case D of
        left -> {-1, 0};
        right -> {1, 0};
        up -> {0, -1};
        down -> {0, 1}
    end,
    NBoundary = sets:union([
        Boundary,
        sets:from_list(
            lists:map(
                fun(I) -> {X + (I * DX), Y + (I * DY)} end,
                lists:seq(1, N)
            )
        )
    ]),
    get_boundary({X + (N * DX), Y + (N * DY)}, Steps, NBoundary).

get_interior(_, MaxY, _, _, _, MaxY) -> bad;
get_interior(MaxX, Y, Boundary, MinX, MaxX, MaxY) ->
    get_interior(MinX, Y + 1, Boundary, MinX, MaxX, MaxY);
get_interior(X, Y, Boundary, MinX, MaxX, MaxY) ->
    IsBoundary = sets:is_element({X, Y}, Boundary),
    if
        IsBoundary -> get_interior(X + 1, Y, Boundary, MinX, MaxX, MaxY);
        true ->
            Fill = flood_fill({X, Y}, Boundary, 100000),
            case Fill of
                infinite -> get_interior(X + 1, Y, Boundary, MinX, MaxX, MaxY);
                Set -> Set
            end
    end.

part1() ->
    Lines = 
        lists:map(fun parse_line/1,
        lists:map(fun lists:droplast/1,
            reader:readlines('input/day18.txt')
        )),
    Boundary = get_boundary({0, 0}, Lines, sets:new()),
    MinX = lists:min(lists:map(fun({X, _}) -> X end, sets:to_list(Boundary))) + 1,
    MaxX = lists:max(lists:map(fun({X, _}) -> X end, sets:to_list(Boundary))) - 1,
    MinY = lists:min(lists:map(fun({_, Y}) -> Y end, sets:to_list(Boundary))) + 1,
    MaxY = lists:max(lists:map(fun({_, Y}) -> Y end, sets:to_list(Boundary))) - 1,
    sets:size(Boundary) + sets:size(get_interior(MinX, MinY, Boundary, MinX, MaxX, MaxY)).

parse_line2([_|[$ |Rest1]]) ->
    {_, [$ |[$(|[$#|Colour]]]} = string:to_integer(Rest1),
    [$)|[D|RDist]] = lists:reverse(Colour),
    Direction = case D of
        $0 -> right;
        $1 -> down;
        $2 -> left;
        $3 -> up
    end,
    {Direction, list_to_integer(lists:reverse(RDist), 16)}.

get_vertices([], X, Y) -> [{X, Y}];
get_vertices([{D, N}|Steps], X, Y) ->
    {DX, DY} = case D of
        left -> {-1, 0};
        right -> {1, 0};
        up -> {0, -1};
        down -> {0, 1}
    end,
    [{X, Y}|get_vertices(Steps, X + (N * DX), Y + (N * DY))].

get_unaccounted([{D, N}|Steps]) -> get_unaccounted([{D, N}|Steps], D).
get_unaccounted([], _) -> 0;
get_unaccounted([{D, N}|Steps], FirstDir) ->
    NextDir = case Steps of
        [] -> FirstDir;
        [{NDir, _}|_] -> NDir
    end,
    IsLeftTurn = 
        (D == left andalso NextDir == down) orelse
        (D == down andalso NextDir == right) orelse
        (D == right andalso NextDir == up) orelse
        (D == up andalso NextDir == left),
    if
        IsLeftTurn -> 1/4;
        true -> 3/4
    end + ((N - 1) / 2) + get_unaccounted(Steps, FirstDir).

get_area([]) -> 0;
get_area([_]) -> 0;
get_area([{X1, Y1}|[{X2, Y2}|Vertices]]) ->
    (Y1 * X2) - (X1 * Y2) + get_area([{X2, Y2}|Vertices]).

part2() ->
    Lines = 
        lists:map(fun parse_line2/1,
        lists:map(fun lists:droplast/1,
            reader:readlines('input/day18.txt')
        )),
    Vertices = get_vertices(Lines, 0, 0),
    Unaccounted = get_unaccounted(Lines),
    Area = get_area(Vertices),
    trunc((abs(Area) / 2) + Unaccounted).
