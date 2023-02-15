-module(dllist).
-export([create_dllist/1, traverse/1, inverse/1]).
-export([listItem/1]).
% 2 Create a module that would implement a doubly linked list where each node of
% the list is an actor.

create_dllist(List) when is_list(List) ->
    [H|T] = List,
    CurrentPid = spawn(?MODULE, listItem, [H]),
    create_dllist(T, [], head, CurrentPid).

create_dllist([], Acc, PreviousPid, CurrentPid) -> 
    lists:reverse([{PreviousPid, CurrentPid, 'end'} | Acc]);

create_dllist(List, Acc, head, CurrentPid) ->
    [H|T] = List,
    NextPid = spawn(?MODULE, listItem, [H]),
    create_dllist(T, [{head, CurrentPid, NextPid} | Acc], CurrentPid, NextPid);

create_dllist(List, Acc, PreviousPid, CurrentPid) ->
    [H|T] = List,
    NextPid = spawn(?MODULE, listItem, [H]),
    create_dllist(T, [{PreviousPid, CurrentPid, NextPid} | Acc], CurrentPid, NextPid).
listItem(Value) ->
    receive
        {get, SenderPid} -> SenderPid ! Value
    end,
    listItem(Value).

traverse(DLList) ->
    [{head, _, _} = HeadNode | Rest] = DLList,
    traverse(HeadNode, Rest, []).

    traverse({_, CurrentPid, 'end'}, _, Acc) ->
        CurrentPid ! {get, self()},
        receive
            Value ->  lists:reverse([Value | Acc])
        end;
    traverse(CurrentNode, DLList, Acc) ->
        {_, CurrentPid, NextPid} = CurrentNode,
        CurrentPid ! {get, self()},
        receive
            Value -> 
                [{CurrentPid, NextPid, _ } = NextNode | Rest] = DLList, 
                traverse(NextNode, Rest, [Value | Acc])
        end.

inverse(DLList) ->
    [{_, _, 'end'} = EndNode | Rest] = lists:reverse(DLList),
    inverse(EndNode, Rest, []).

    inverse({'head', CurrentPid, _}, _, Acc) ->
        CurrentPid ! {get, self()},
        receive
            Value ->  lists:reverse([Value | Acc])
        end;
    inverse(CurrentNode, DLList, Acc) ->
        {PreviousPid, CurrentPid, _} = CurrentNode,
        CurrentPid ! {get, self()},
        receive
            Value -> 
                [{_, PreviousPid, CurrentPid } = PreviousNode | Rest] = DLList, 
                inverse(PreviousNode, Rest, [Value | Acc])
        end.
