-module(day25).
-export([part1/0]).

graph_size(Deque, Visited, Graph) ->
    Done = deque:is_empty(Deque),
    if
        Done -> sets:size(Visited);
        true ->
            {{Prev, Current}, NDeque} = deque:popleft(Deque),
            Seen = sets:is_element(Current, Visited),
            if
                Seen -> graph_size(NDeque, Visited, Graph);
                true ->
                    case get({Prev, Current}) of
                        undefined -> put({Prev, Current}, 1);
                        N -> put({Prev, Current}, N + 1)
                    end,
                    NVisited = sets:add_element(Current, Visited),
                    Nexts = shuffle(lists:filter(
                        fun(X) -> X /= discard end,
                        lists:map(
                            fun({F, T}) ->
                                VisitedT = sets:is_element(T, Visited),
                                VisitedF = sets:is_element(F, Visited),
                                if
                                    F == Current andalso not VisitedT -> T;
                                    T == Current andalso not VisitedF -> F;
                                    true -> discard
                                end
                            end,
                            Graph
                        )
                    )),
                    NNDeque = lists:foldl(
                        fun(Next, FDeque) -> deque:appendright(FDeque, {Current, Next}) end,
                        NDeque,
                        Nexts
                    ),
                    graph_size(NNDeque, NVisited, Graph)
            end
    end.

shuffle(L) -> [X||{_,X} <- lists:sort([ {rand:uniform(), N} || N <- L])].

part1() ->
    erase(),
    Lines = lists:map(fun lists:droplast/1, reader:readlines('input/day25.txt')),
    Graph = lists:flatmap(
        fun(Line) ->
            [K|[V]] = string:split(Line, ":"),
            lists:map(
                fun(X) -> {K, X} end,
                string:split(string:trim(V), " ", all)
            )
        end,
        Lines
    ),
    Points = shuffle(sets:to_list(sets:from_list(lists:map(fun({F, _}) -> F end, Graph)))),
    lists:foreach(
        fun(P) -> graph_size(deque:from_list([{none, P}]), sets:new(), Graph) end,
        lists:sublist(Points, 100)
    ),
    Cuts = 
        lists:map(fun({X, _}) -> X end,
        lists:sublist(
            lists:sort(fun({_, A}, {_, B}) -> B =< A end,
            lists:map(
                fun({F, T}) ->
                    N1 = case get({F, T}) of
                        undefined -> 0;
                        M -> M
                    end,
                    N2 = case get({T, F}) of
                        undefined -> 0;
                        N -> N
                    end,
                    {{F, T}, N1 + N2}
                end,
                Graph
            )),
            3
        )),
    CutGraph = lists:foldl(
        fun(C, FGraph) -> lists:delete(C, FGraph) end,
        Graph,
        Cuts
    ),
    [{L, R}|_] = Cuts,
    graph_size(deque:from_list([{none, L}]), sets:new(), CutGraph) *
    graph_size(deque:from_list([{none, R}]), sets:new(), CutGraph).
