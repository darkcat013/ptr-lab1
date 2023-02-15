-module(semaphore).
-export([create_semaphore/1, acquire/1, release/1]).
-export([semaphoreActor/2]).
% 2 Create a module that would implement a semaphore.
create_semaphore(MaxValue) ->
    spawn(?MODULE, semaphoreActor, [MaxValue, MaxValue]).

acquire(Pid) ->
    Pid ! {acquire, self()},
    receive
        {ok} -> ok;
        {wait} -> timer:sleep(1000), acquire(Pid)
    end.

release(Pid) ->
    Pid ! {release}.

semaphoreActor(Value, MaxValue) ->
    receive
        {acquire, Pid} when Value > 0 andalso Value =< MaxValue -> Pid ! {ok}, semaphoreActor(Value-1, MaxValue);
        {acquire, Pid} when Value =< 0 -> Pid ! {wait}, semaphoreActor(Value, MaxValue);
        {release} -> semaphoreActor(Value + 1, MaxValue)
    end.
