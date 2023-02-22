-module(main).
-export([startProcessingLine/0]).
-export([processingLine/0, splitActor/1, lowerAndSwapActor/1, joinAndPrintActor/1]).

% Create a supervised processing line to clean messy strings. The first worker in
% the line would split the string by any white spaces (similar to Python’s str.split method).
% The second actor will lowercase all words and swap all m’s and n’s (you nomster!). The third
% actor will join back the sentence with one space between words (similar to Python’s str.join
% method). Each worker will receive as input the previous actor’s output, the last actor printing
% the result on screen. If any of the workers die because it encounters an error, the whole
% processing line needs to be restarted. Logging is welcome.

startProcessingLine() ->
    spawn(?MODULE, processingLine, []).

processingLine() ->
    {SplitPid, _} = spawn_monitor(?MODULE, splitActor, [self()]),
    {LowerPid, _} = spawn_monitor(?MODULE, lowerAndSwapActor, [self()]),
    {PrintPid, _} = spawn_monitor(?MODULE, joinAndPrintActor, [self()]),
    io:format("Processing line actors spawned"),
    processingLineLoop({SplitPid, LowerPid, PrintPid}, "").

processingLineLoop(LinePids, CurrentText) -> 
    {SplitPid, LowerPid, PrintPid} = LinePids,
    receive
        {'DOWN', _, _, SplitPid, Reason} -> 
            io:format("First actor died, reason: ~w~n Restarting.~n", [Reason]),
            {NewSplitPid, _} = spawn_monitor(?MODULE, splitActor, [self()]),
            NewSplitPid ! CurrentText,
            processingLineLoop({NewSplitPid, LowerPid, PrintPid}, CurrentText);
        {'DOWN', _, _, LowerPid, Reason} -> 
            io:format("Second actor died, reason: ~w~n Restarting.~n", [Reason]),
            {NewLowerPid, _} = spawn_monitor(?MODULE, lowerAndSwapActor, [self()]),
            SplitPid ! CurrentText,
            processingLineLoop({SplitPid, NewLowerPid, PrintPid}, CurrentText);
        {'DOWN', _, _, PrintPid, Reason} -> 
            io:format("Third actor died, reason: ~w~n Restarting.~n", [Reason]),
            {NewPrintPid, _} = spawn_monitor(?MODULE, joinAndPrintActor, [self()]),
            SplitPid ! CurrentText,
            processingLineLoop({SplitPid, LowerPid, NewPrintPid}, CurrentText);
        {SplitText, SplitPid} -> LowerPid ! SplitText, processingLineLoop(LinePids, CurrentText);
        {SplitAndLowerText, LowerPid} -> PrintPid ! SplitAndLowerText, processingLineLoop(LinePids, CurrentText);
        Text when is_list(Text)-> SplitPid ! Text, processingLineLoop(LinePids, Text)
    end.

splitActor(MonitorPid) ->
    receive
        Text -> 
            io:format("First actor received: ~s~n",[Text]),
            case rand:uniform(2) of
                1 -> SplitText = string:lexemes(Text, " "++[$\r, $\n]),
                    io:format("First actor output: ~w~n", [SplitText]),
                    MonitorPid ! {SplitText, self()};
                2 -> exit(failed)
            end
    end,
    splitActor(MonitorPid).
lowerAndSwapActor(MonitorPid) ->
    receive
        SplitText -> 
            io:format("Second actor received: ~w~n",[SplitText]),
            case rand:uniform(2) of
                1 -> 
                    SplitAndLowerText = lowercaseStringsSwapMN(SplitText),
                    io:format("Second actor output: ~w~n", [SplitAndLowerText]),
                    MonitorPid ! {SplitAndLowerText, self()};
                2 -> exit(failed)
            end
    end,
    lowerAndSwapActor(MonitorPid).

joinAndPrintActor(MonitorPid) ->
    receive
        SplitAndLowerText -> 
            io:format("Third actor received: ~w~n",[SplitAndLowerText]),
            case rand:uniform(2) of
                1 -> io:format("Third actor output: ~s~n",[string:join(SplitAndLowerText, " ")]);
                2 -> exit(failed)
            end
    end,
    joinAndPrintActor(MonitorPid).


lowercaseStringsSwapMN(ListOfStrings) ->
    lowercaseStringsSwapMN(ListOfStrings, []).

lowercaseStringsSwapMN([], Acc) -> lists:reverse(Acc);
lowercaseStringsSwapMN(ListOfStrings, Acc) ->
    [H | T] = ListOfStrings,
    lowercaseStringsSwapMN(T, [swapMN(string:lowercase(H)) | Acc]).

swapMN(String) ->
    swapMN(String, []).

swapMN([], StringAcc) -> lists:reverse(StringAcc);
swapMN(String, StringAcc) ->
    [H|T] = String,
    case H of
        $n -> swapMN(T, [$m | StringAcc]);
        $m -> swapMN(T, [$n | StringAcc]);
        _ -> swapMN(T, [H | StringAcc])
    end.