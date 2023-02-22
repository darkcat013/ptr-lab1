-module(bonus).
-export([startCarSensors/0]).
-export([mainSensorSupervisor/0, cabinSensorRestarter/1, cabinSensor/0, 
    motorSensorRestarter/1, motorSensor/0, chassisSensorRestarter/1, 
    chassisSensor/0, wheelsSensorSupervisorRestarter/1, wheelsSensorSupervisor/0,
    wheelSensorRestarter/1, wheelSensor/0]).

% Write a supervised application that would simulate a sensor system in a car.
% There should be sensors for each wheel, the motor, the cabin and the chassis. If any sensor dies
% because of a random invalid measurement, it should be restarted. If, however, the main sensor
% supervisor system detects multiple crashes, it should deploy the airbags. 

startCarSensors() ->
    spawn(?MODULE, mainSensorSupervisor, []),
    ok.

mainSensorSupervisor() ->
    spawn_link(?MODULE, cabinSensorRestarter, [self()]),
    spawn_link(?MODULE, motorSensorRestarter, [self()]),
    spawn_link(?MODULE, chassisSensorRestarter, [self()]),
    spawn_link(?MODULE, wheelsSensorSupervisorRestarter, [self()]),

    mainSensorSupervisorLoop(0).

mainSensorSupervisorLoop(CurrentCrashes) -> 
    receive
        _ when CurrentCrashes > 1 -> io:format("================AIRBAGS================~n"), exit(airbag);
        crash -> mainSensorSupervisorLoop(CurrentCrashes+1);
        start ->  mainSensorSupervisorLoop(case CurrentCrashes == 0 of true-> 0; false -> CurrentCrashes-1 end)
    end.

cabinSensorRestarter(SupervisorPid) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, cabinSensor, []),
    timer:sleep(2000),
    io:format("Started Cabin Sensor: ~w~n", [Pid]),
    SupervisorPid ! start,
    receive
        {'EXIT', SupervisorPid, _} -> exit(airbag);
        {'EXIT', Pid, Reason} -> 
            SupervisorPid ! crash, 
            io:format("Cabin Sensor crashed, reason: ~w~n", [Reason]),
            cabinSensorRestarter(SupervisorPid)
    end.
cabinSensor() ->
    receive
        kill -> exit(killed)
    end,
    cabinSensor().

motorSensorRestarter(SupervisorPid) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, motorSensor, []),
    timer:sleep(2000),
    io:format("Started Motor Sensor: ~w~n", [Pid]),
    SupervisorPid ! start,
    receive
        {'EXIT', SupervisorPid, _} -> exit(airbag);
        {'EXIT', Pid, Reason} -> 
            SupervisorPid ! crash, 
            io:format("Motor Sensor crashed, reason: ~w~n", [Reason]),
            motorSensorRestarter(SupervisorPid)
    end.
motorSensor() ->
    receive
        kill -> exit(killed)
    end,
    motorSensor().

chassisSensorRestarter(SupervisorPid) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, chassisSensor, []),
    timer:sleep(2000),
    io:format("Started Chassis Sensor: ~w~n", [Pid]),
    SupervisorPid ! start,
    receive
        {'EXIT', SupervisorPid, _} -> exit(airbag);
        {'EXIT', Pid, Reason} -> 
            SupervisorPid ! crash, 
            io:format("Chassis Sensor crashed, reason: ~w~n", [Reason]),
            chassisSensorRestarter(SupervisorPid)
    end.

chassisSensor() ->
    receive
        kill -> exit(killed)
    end,
    chassisSensor().

wheelsSensorSupervisorRestarter(SupervisorPid) ->
    process_flag(trap_exit, true),
    timer:sleep(4000),
    Pid = spawn_link(?MODULE, wheelsSensorSupervisor, []),
    io:format("Started Wheel Sensor Supervisor~n"),
    SupervisorPid ! start,
    receive
        {'EXIT', SupervisorPid, _} -> exit(airbag);
        {'EXIT', Pid, Reason} -> 
            SupervisorPid ! crash, 
            io:format("Wheel Sensor Supervisor crashed, reason: ~w~n", [Reason]),
            wheelsSensorSupervisorRestarter(SupervisorPid)
    end.

wheelsSensorSupervisor() -> 
    spawn_link(?MODULE, wheelSensorRestarter, [self()]),
    spawn_link(?MODULE, wheelSensorRestarter, [self()]),
    spawn_link(?MODULE, wheelSensorRestarter, [self()]),
    spawn_link(?MODULE, wheelSensorRestarter, [self()]),
    wheelsSensorSupervisorLoop(0).

wheelsSensorSupervisorLoop(CurrentCrashes) ->
    receive
        _ when CurrentCrashes > 1 -> exit(wheels_crash);
        crash -> wheelsSensorSupervisorLoop(CurrentCrashes+1);
        start ->  wheelsSensorSupervisorLoop(case CurrentCrashes == 0 of true-> 0; false -> CurrentCrashes-1 end)
    end.

wheelSensorRestarter(SupervisorPid) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, wheelSensor, []),
    timer:sleep(2000),
    io:format("Started Wheel Sensor: ~w~n", [Pid]),
    SupervisorPid ! start,
    receive
        {'EXIT', SupervisorPid, _} -> exit(wheels_crash);
        {'EXIT', Pid, Reason} -> 
            SupervisorPid ! crash, 
            io:format("Wheel Sensor ~w crashed, reason: ~w~n", [Pid, Reason]),
            wheelSensorRestarter(SupervisorPid)
    end.


wheelSensor() ->
    receive
        kill -> exit(killed)
    end,
    wheelSensor().