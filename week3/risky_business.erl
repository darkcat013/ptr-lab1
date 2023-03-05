-module(risky_business).
-export([create_scheduler/0]).
-export([scheduler/1, task/1]).
% 1 Create a module that would perform some risky business. Start by creating a
%   scheduler actor. When receiving a task to do, it will create a worker node that will perform the
%   task. Given the nature of the task, the worker node is prone to crashes (task completion rate
%   50%). If the scheduler detects a crash, it will log it and restart the worker node. If the worker
%   node finishes successfully, it should print the result.

create_scheduler() ->
    Tasks = dict:new(),
    spawn(?MODULE, scheduler, [Tasks]).

scheduler(Tasks) ->
    receive
        {'DOWN',_,_,SuccessPid, normal} ->
            scheduler(dict:erase(SuccessPid, Tasks));
        {'DOWN',_,_,FailedPid,_} -> 
            {ok, Task} = dict:find(FailedPid, Tasks),
            io:format("Task failed: ~w~n", [Task]),
            self() ! {failed, Task},
            scheduler(dict:erase(FailedPid, Tasks));
        {failed, Task} ->
            Pid = spawn(?MODULE, task, [Task]),
            monitor(process, Pid),
            Pid ! rand:uniform(2),
            scheduler(dict:store(Pid, Task, Tasks));
        Anything -> 
            Pid = spawn(?MODULE, task, [Anything]),
            monitor(process, Pid),
            Pid ! rand:uniform(2),
            scheduler(dict:store(Pid, Anything, Tasks))
    end.

task(Task) ->
    receive
        1 -> io:format("Task succesful: ~w~n", [Task]);
        2 -> 1/0
    end.