-module(reader).
-import(lists,[reverse/1]).
-import(string,[to_integer/1]).
-export([readlines/1, get_ints/1]).

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
    case to_integer(S) of
        {error, _} -> [_|T] = S, get_ints(T);
        {Int, Rest} -> [Int|get_ints(Rest)]
    end.
