-module(week3).
-export([registerSimpleActors/0, startMonitoringSimulation/0, new_queue/0, push/2, pop/1,
    startSemaphoreSimulation/0]).
-export([anyMessageActor/0, specificMessageActor/0, monitorActor/0, 
    monitoredActor/0, averager/1, queueActor/1, slowActor/1, 
    mediumActor/1, fastActor/1]).

registerSimpleActors() ->
    io:format("Actors:~n~n~w~n~w~n~w~n~n", [anyMessage, specificMessage, averager]),
    register(anyMessage, spawn(?MODULE, anyMessageActor, [])),
    register(specificMessage, spawn(?MODULE, specificMessageActor, [])),
    register(averager, spawn(?MODULE, averager, [0])).

% Minimal
% 1 Create an actor that prints on the screen any message it receives.
anyMessageActor() ->
    receive
        Anything -> io:format("~w~n", [Anything])
    end,
    anyMessageActor().

% 2 Create an actor that returns any message it receives, while modifying it.
specificMessageActor() ->
    receive
        N when is_integer(N) ->
            io:format("Received: ~p~n", [N + 1]);
        S when is_list(S) ->
            io:format("Received: ~s~n", [string:lowercase(S)]);
        _ ->
            io:format("I don't knwo how to HANDLE this!~n")
    end,
    specificMessageActor().

% 3 Create a two actors, actor one ”monitoring” the other. If the second actor stops, actor one gets notified via a message.
startMonitoringSimulation() ->
    Pid = spawn(?MODULE, monitorActor, []),
    io:format("Started monitorActor with Pid: ~w~n", [Pid]).

monitorActor() ->
    Pid = spawn(?MODULE, monitoredActor, []),
    monitor(process, Pid),
    io:format("Started monitoredActor with Pid: ~w~n", [Pid]),
    receive
        {'DOWN', _, _, MonitoredPid, Reason} -> error_logger:error_msg("monitoredActor stopped, Pid: ~w, Reason: ~w", [MonitoredPid, Reason])
    end.

monitoredActor() ->
    io:format("Start Sleep~n"),
    timer:sleep(2000),
    io:format("End Sleep~n").

% 4 Create an actor which receives numbers and with each request prints out the current average.
averager(CurrentAvg) -> 
    io:format("Current average is ~p~n", [CurrentAvg]), 
    receive
        N when is_number(N) -> 
            Avg = (CurrentAvg+N)/2, 
            averager(Avg)
    end.

% Main
% 1 Create an actor which maintains a simple FIFO queue. You should write helper functions to 
%   create an API for the user, which hides how the queue is implemented.
new_queue() ->
    spawn(?MODULE, queueActor, [[]]).

queueActor(Queue) ->
    receive
        {push, Value} ->  queueActor([Value | Queue]);
        {pop} when length(Queue) > 0 -> [H | T] = Queue, io:format("~w~n", [H]), queueActor(T);
        {pop} when length(Queue) =< 0 -> io:format("Queue is empty~n"), queueActor(Queue)
    end.

push(Pid, Value) when is_pid(Pid) ->
    Pid ! {push, Value},
    io:format("").

pop(Pid) when is_pid(Pid) ->
    Pid ! {pop},
    io:format("").

% 2 Create a module that would implement a semaphore.
startSemaphoreSimulation() ->
    Mutex = semaphore:create_semaphore(3),
    spawn(?MODULE, slowActor, [Mutex]),
    spawn(?MODULE, mediumActor, [Mutex]),
    spawn(?MODULE, fastActor, [Mutex]),
    spawn(?MODULE, slowActor, [Mutex]),
    spawn(?MODULE, slowActor, [Mutex]),
    spawn(?MODULE, slowActor, [Mutex]).



slowActor(Mutex) ->
    semaphore:acquire(Mutex),
    io:format("Start slow Actor~n"),
    timer:sleep(9000),
    semaphore:release(Mutex),
    io:format("End slow Actor~n").

mediumActor(Mutex) ->
    semaphore:acquire(Mutex),
    io:format("Start medium Actor~n"),
    timer:sleep(6000),
    semaphore:release(Mutex),
    io:format("End medium Actor~n").

fastActor(Mutex) ->
    semaphore:acquire(Mutex),
    io:format("Start fast Actor~n"),
    timer:sleep(3000),
    semaphore:release(Mutex),
    io:format("End fast Actor~n").

% Bonus
% 1 Create a module that would perform some risky business. Start by creating a
%   scheduler actor. When receiving a task to do, it will create a worker node that will perform the
%   task. Given the nature of the task, the worker node is prone to crashes (task completion rate
%   50%). If the scheduler detects a crash, it will log it and restart the worker node. If the worker
%   node finishes successfully, it should print the result.

% 2 Create a module that would implement a doubly linked list where each node of
% the list is an actor.