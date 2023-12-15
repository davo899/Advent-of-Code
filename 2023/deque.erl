-module(deque).
-export([new/0, appendleft/2, appendright/2, popleft/1, popright/1, to_list/1]).

-record(deque, {front, back}).

new() -> #deque{front = [], back = []}.

appendleft(Deque, Elem) -> Deque#deque{front = [Elem|Deque#deque.front]}.

appendright(Deque, Elem) -> Deque#deque{back = [Elem|Deque#deque.back]}.

popleft(#deque{front = [], back = [Head]}) -> {Head, #deque{front = [], back = []}};
popleft(#deque{front = [], back = Back}) -> popleft(balance(#deque{front = [], back = Back}));
popleft(#deque{front = [Head|Front], back = Back}) -> {Head, balance(#deque{front = Front, back = Back})}.

popright(#deque{front = [Last], back = []}) -> {Last, #deque{front = [], back = []}};
popright(#deque{front = Front, back = []}) -> popright(balance(#deque{front = Front, back = []}));
popright(#deque{front = Front, back = [Last|Back]}) -> {Last, balance(#deque{front = Front, back = Back})}.

to_list(#deque{front = Front, back = Back}) -> Front ++ lists:reverse(Back).

balance(Deque) ->
    case Deque of
        #deque{front = [], back = [_|[_|_]]} ->
            {L, R} = lists:split(length(Deque#deque.back) div 2, Deque#deque.back),
            Deque#deque{front = lists:reverse(R), back = L};

        #deque{front = [_|[_|_]], back = []} ->
            {L, R} = lists:split(length(Deque#deque.front) div 2, Deque#deque.front),
            Deque#deque{front = L, back = lists:reverse(R)};

        _ -> Deque
    end.
