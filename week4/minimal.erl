-module(minimal).
-export([startIdenticalActors/1]).
-export([startActors/1, echoActor/0]).

% Create a supervised pool of identical worker actors. The number of actors
% is static, given at initialization. Workers should be individually addressable. Worker actors
% should echo any message they receive. If an actor dies (by receiving a “kill” message), it should
% be restarted by the supervisor. Logging is welcome.

startIdenticalActors(N) when is_integer(N) andalso N > 0 ->
    spawn(?MODULE, startActors, [N]).
startActors(0) -> 
    io:format("All actors spawned~n"),
    receive
        {'DOWN', _, _, KilledPid, Reason} -> 
            io:format("~w  died,  reason ~w~n", [KilledPid, Reason]),
            {Pid, _} = spawn_monitor(?MODULE, echoActor, []),
            io:format("~p actor respawned.~n",[Pid])
    end,
    startActors(0);
startActors(N) ->
    io:format("~w~n",[self()]),
    {Pid, _} = spawn_monitor(?MODULE, echoActor, []),
    io:format("~p actor spawned.~n",[Pid]),
    startActors(N-1).

echoActor() ->
    receive
        kill -> io:format("~w was killed ~n", [self()]), exit(killed);
        Anything -> io:format("~w~n", [Anything]), echoActor()
    end.