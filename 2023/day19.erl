% Advent of Code 2023 - Day 19
-module(day19).
-export([part1/0, part2/0]).

get_workflows([[]|_]) -> [];
get_workflows([Line|Lines]) ->
    [Name|Rest] = string:split(Line, "{"),
    [[$}|RFinal]|RCheckStrs] = string:split(string:reverse(Rest), ",", all),
    Final = lists:reverse(RFinal),
    Checks = lists:reverse(lists:map(
        fun(RCheckStr) ->
            [[P|[C|N]]|[T]] = string:split(string:reverse(RCheckStr), ":"),
            Pred = case C of
                $> -> fun(X) -> X > list_to_integer(N) end;
                $< -> fun(X) -> X < list_to_integer(N) end
            end,
            {P, Pred, T} end,
        RCheckStrs
    )),
    [{Name, {Checks, Final}}|get_workflows(Lines)].

get_ratings([]) -> [];
get_ratings([Line|Lines]) ->
    [_|NS] = string:split(Line, "=", all),
    [maps:from_list(lists:zip(
        [$x, $m, $a, $s],
        lists:map(
            fun(NStr) ->
                {N, _} = string:to_integer(NStr),
                N end,
            NS
        )
    ))|get_ratings(Lines)].

get_ratings_start([[]|Lines]) -> get_ratings(Lines);
get_ratings_start([_|Lines]) -> get_ratings_start(Lines).

is_accepted(Rating, Workflow, Workflows) ->
    {Checks, Final} = maps:get(Workflow, Workflows),
    Targets = lists:filter(
        fun({P, Pred, _}) -> Pred(maps:get(P, Rating)) end,
        Checks
    ),
    Target = case Targets of
        [] -> Final;
        [{_, _, T}|_] -> T
    end,
    case Target of
        "A" -> true;
        "R" -> false;
        NWorkflow -> is_accepted(Rating, NWorkflow, Workflows)
    end.

part1() ->
    Lines = lists:map(fun lists:droplast/1, reader:readlines('input/day19.txt')),
    Workflows = maps:from_list(get_workflows(Lines)),
    Ratings = get_ratings_start(Lines),
    lists:sum(
    lists:flatmap(fun maps:values/1,
    lists:filter(
        fun(Rating) -> is_accepted(Rating, "in", Workflows) end,
        Ratings
    ))).

get_workflows2([[]|_]) -> [];
get_workflows2([Line|Lines]) ->
    [Name|Rest] = string:split(Line, "{"),
    [[$}|RFinal]|RCheckStrs] = string:split(string:reverse(Rest), ",", all),
    Final = lists:reverse(RFinal),
    Checks = lists:reverse(lists:map(
        fun(RCheckStr) ->
            [[P|[C|N]]|[T]] = string:split(string:reverse(RCheckStr), ":"),
            {P, {C, list_to_integer(N)}, T} end,
        RCheckStrs
    )),
    [{Name, {Checks, Final}}|get_workflows2(Lines)].

are_valid(Constraints) ->
    lists:all(
        fun({LB, UB}) -> UB - LB >= 2 end,
        maps:values(Constraints)
    ).

prod([]) -> 1;
prod([N|NS]) -> N * prod(NS).

get_combinations(_, "R", _) -> 0;
get_combinations(Constraints, "A", _) ->
    ConstraintsValid = are_valid(Constraints),
    if
        ConstraintsValid ->
            prod(lists:map(fun({LB, UB}) -> UB - LB - 1 end, maps:values(Constraints)));
        true -> 0
    end;
get_combinations(Constraints, Workflow, Workflows) ->
    ConstraintsValid = are_valid(Constraints),
    if
        ConstraintsValid ->
            {Checks, Final} = maps:get(Workflow, Workflows),
            {FinalConstraints, NCombos} = lists:foldl(
                fun({P, {C, N}, T}, {FConstraints, Combos}) ->
                    {LB, UB} = maps:get(P, FConstraints),
                    TakenConstraint = case C of
                        $> -> {max(LB, N), UB};
                        $< -> {LB, min(UB, N)}
                    end,
                    NotTakenConstraint = case C of
                        $> -> {LB, min(UB, N + 1)};
                        $< -> {max(LB, N - 1), UB}
                    end,
                    {
                        maps:put(P, NotTakenConstraint, FConstraints),
                        Combos + get_combinations(maps:put(P, TakenConstraint, FConstraints), T, Workflows)
                    } end,
                {Constraints, 0},
                Checks
            ),
            get_combinations(FinalConstraints, Final, Workflows) + NCombos;

        true -> 0
    end.

part2() ->
    Lines = lists:map(fun lists:droplast/1, reader:readlines('input/day19.txt')),
    Workflows = maps:from_list(get_workflows2(Lines)),
    get_combinations(
        maps:from_list([
            {$x, {0, 4001}},
            {$m, {0, 4001}},
            {$a, {0, 4001}},
            {$s, {0, 4001}}
        ]),
        "in",
        Workflows
    ).
