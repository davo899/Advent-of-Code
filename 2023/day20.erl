% Advent of Code 2023 - Day 20
-module(day20).
-export([part1/0, part2/0]).

get_nodes([], Map) -> Map;
get_nodes([[$%|Line]|Lines], Map) ->
    [Name|[Targets]] = string:split(Line, " -> "),
    get_nodes(Lines, maps:put(
        Name, {flip_flop, sets:from_list(string:split(Targets, ", ", all))}, Map
    ));
get_nodes([[$&|Line]|Lines], Map) ->
    [Name|[Targets]] = string:split(Line, " -> "),
    get_nodes(Lines, maps:put(
        Name, {conjunction, sets:from_list(string:split(Targets, ", ", all))}, Map
    ));
get_nodes([[$b|Line]|Lines], Map) ->
    [_|[Targets]] = string:split(Line, " -> "),
    get_nodes(Lines, maps:put(
        "broadcaster", {broadcaster, sets:from_list(string:split(Targets, ", ", all))}, Map
    )).

init_state(_, {flip_flop, Targets}, _) -> {flip_flop, Targets, low};
init_state(Name, {conjunction, Targets}, Map) ->
    State = maps:from_list(lists:map(
        fun({N, {_, _}}) -> {N, low} end,
        lists:filter(
            fun({_, {_, TS}}) -> sets:is_element(Name, TS) end,
            maps:to_list(Map)
        )
    )),
    {conjunction, Targets, State};
init_state(_, X, _) -> X.

handle_pulse(Deque, State, Lows, Highs) ->
    Done = deque:is_empty(Deque),
    if
        Done -> {State, Lows, Highs};
        true ->
            {{FromName, ToName, Level}, NDeque} = deque:popleft(Deque),
            {NLows, NHighs} = case Level of
                low -> {Lows + 1, Highs};
                high -> {Lows, Highs + 1}
            end,
            ToExists = maps:is_key(ToName, State),
            if
                ToExists ->
                    To = maps:get(ToName, State),
                    case {To, Level} of
                        {{flip_flop, Targets, L}, low} ->
                            NL = case L of
                                low -> high;
                                high -> low
                            end,
                            NNDeque = lists:foldl(
                                fun(T, FDeque) -> deque:appendright(FDeque, {ToName, T, NL}) end,
                                NDeque,
                                sets:to_list(Targets)
                            ),
                            handle_pulse(NNDeque, maps:put(ToName, {flip_flop, Targets, NL}, State), NLows, NHighs);

                        {{conjunction, Targets, Inputs}, Level} ->
                            NInputs = maps:put(FromName, Level, Inputs),
                            NState = maps:put(ToName, {conjunction, Targets, NInputs}, State),
                            ShouldSendLow = lists:all(fun(X) -> X == high end, maps:values(NInputs)),
                            SendLevel = if
                                ShouldSendLow -> low;
                                true -> high
                            end,
                            NNDeque = lists:foldl(
                                fun(T, FDeque) -> deque:appendright(FDeque, {ToName, T, SendLevel}) end,
                                NDeque,
                                sets:to_list(Targets)
                            ),
                            handle_pulse(NNDeque, NState, NLows, NHighs);

                        {{broadcaster, Targets}, Level} ->
                            NNDeque = lists:foldl(
                                fun(T, FDeque) -> deque:appendright(FDeque, {ToName, T, Level}) end,
                                NDeque,
                                sets:to_list(Targets)
                            ),
                            handle_pulse(NNDeque, State, NLows, NHighs);

                        _ -> handle_pulse(NDeque, State, NLows, NHighs)
                    end;

                true -> handle_pulse(NDeque, State, NLows, NHighs)
            end
    end.

part1() ->
    Lines = lists:map(fun lists:droplast/1,
        reader:readlines('input/day20.txt')
    ),
    LineMap = get_nodes(Lines, maps:new()),
    State = maps:map(
        fun(K, V) -> init_state(K, V, LineMap) end,
        LineMap
    ),
    {_, Lows, Highs} = lists:foldl(
        fun(_, {S, L, H}) -> handle_pulse(deque:from_list([{"button", "broadcaster", low}]), S, L, H) end,
        {State, 0, 0},
        lists:seq(1, 1000)
    ),
    Lows * Highs.

gcd(A,B) when A == 0; B == 0 -> 0;
gcd(A,B) when A == B -> A;
gcd(A,B) when A > B -> gcd(A-B, B);
gcd(A,B) -> gcd(A, B-A).

lcm(A,B) -> (A*B) div gcd(A, B).

handle_pulse2(Deque, State, Cycles, Cycle) ->
    Done = deque:is_empty(Deque),
    if
        Done -> {State, Cycles};
        true ->
            {{FromName, ToName, Level}, NDeque} = deque:popleft(Deque),
            GetCycle = Level == low andalso not maps:is_key(ToName, Cycles),
            NCycles = if
                GetCycle -> maps:put(ToName, Cycle, Cycles);
                true -> Cycles
            end,
            ToExists = maps:is_key(ToName, State),
            Completed = maps:size(NCycles) >= maps:size(State) - 1,
            if
                Completed -> {done, NCycles};
                ToExists ->
                    To = maps:get(ToName, State),
                    case {To, Level} of
                        {{flip_flop, Targets, L}, low} ->
                            NL = case L of
                                low -> high;
                                high -> low
                            end,
                            NNDeque = lists:foldl(
                                fun(T, FDeque) -> deque:appendright(FDeque, {ToName, T, NL}) end,
                                NDeque,
                                sets:to_list(Targets)
                            ),
                            handle_pulse2(NNDeque, maps:put(ToName, {flip_flop, Targets, NL}, State), NCycles, Cycle);

                        {{conjunction, Targets, Inputs}, Level} ->
                            NInputs = maps:put(FromName, Level, Inputs),
                            NState = maps:put(ToName, {conjunction, Targets, NInputs}, State),
                            ShouldSendLow = lists:all(fun(X) -> X == high end, maps:values(NInputs)),
                            SendLevel = if
                                ShouldSendLow -> low;
                                true -> high
                            end,
                            NNDeque = lists:foldl(
                                fun(T, FDeque) -> deque:appendright(FDeque, {ToName, T, SendLevel}) end,
                                NDeque,
                                sets:to_list(Targets)
                            ),
                            handle_pulse2(NNDeque, NState, NCycles, Cycle);

                        {{broadcaster, Targets}, Level} ->
                            NNDeque = lists:foldl(
                                fun(T, FDeque) -> deque:appendright(FDeque, {ToName, T, Level}) end,
                                NDeque,
                                sets:to_list(Targets)
                            ),
                            handle_pulse2(NNDeque, State, NCycles, Cycle);

                        _ -> handle_pulse2(NDeque, State, NCycles, Cycle)
                    end;

                true -> handle_pulse2(NDeque, State, NCycles, Cycle)
            end
    end.

%["qd", "bb", "dp", "dh"]
presses_needed(State, Cycles, Cycle) ->
    {NState, NCycles} = handle_pulse2(deque:from_list([{"button", "broadcaster", low}]), State, Cycles, Cycle),
    case NState of
        done -> NCycles;
        NState -> presses_needed(NState, NCycles, Cycle + 1)
    end.

part2() ->
    Lines = lists:map(fun lists:droplast/1,
        reader:readlines('input/day20.txt')
    ),
    LineMap = get_nodes(Lines, maps:new()),
    State = maps:put("output", output,
        maps:map(
            fun(K, V) -> init_state(K, V, LineMap) end,
            LineMap
        )
    ),
    Cycles = presses_needed(State, maps:new(), 1),
    lists:foldl(fun lcm/2, 1,
    lists:map(fun({Name, _}) -> maps:get(Name, Cycles) end,
    maps:to_list(
    maps:filter(
        fun(_, {_, Targets}) -> sets:is_element("rm", Targets) end,
        LineMap
    )))).
