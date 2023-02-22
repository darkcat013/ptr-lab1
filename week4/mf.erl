-module(mf).
-export([startMf/0]).
-export([jules/0, whatCounter/1, questioning/1]).
% Write an application that, in the context of actor supervision. would mimic the
% exchange in that scene from the movie Pulp Fiction.

startMf() ->
    spawn_link(?MODULE, jules, []).

jules() ->
    process_flag(trap_exit, true),
    CounterPid = spawn_link(?MODULE, whatCounter, [1]),
    QuestionPid = spawn_link(?MODULE, questioning, [1]),
    io:format("What does Marsellus Wallace look like?"),
    julesQuestioning({CounterPid, QuestionPid}).

julesQuestioning(Pids) -> 
    {CounterPid, QuestionPid} = Pids,
    receive
        {'EXIT', _, counter_full} -> 
            io:format("Pray Motherf***er~n"),
            timer:sleep(1000),
            os:cmd("shutdown /s /t 11"),
            io:format("10~n"),
            timer:sleep(1000),
            io:format("9~n"),
            timer:sleep(1000),
            io:format("8~n"),
            timer:sleep(1000),
            io:format("7~n"),
            timer:sleep(1000),
            io:format("6~n"),
            timer:sleep(1000),
            io:format("5~n"),
            timer:sleep(1000),
            io:format("4~n"),
            timer:sleep(1000),
            io:format("3~n"),
            timer:sleep(1000),
            io:format("2~n"),
            timer:sleep(1000),
            io:format("1~n"),
            timer:sleep(1000),
            exit(goodbye);
        {'EXIT', _, question_full} -> 
            io:format("Then why you trying to f*** him like a bitch? You tried to f*** and Marsellus Wallace doesn't like to be f***ed by anybody except ms Wallace.~n"),
            timer:sleep(7000),
            os:cmd("shutdown /s /t 20"),
            io:format("The path of the righteous man is beset on all sides by the inequities of the selfish and the tyranny of evil men. Blessed is he who, in the name of charity and good will, shepherds the weak through the valley of the darkness. For he is truly his brother's keeper and the finder of lost children. And I will strike down upon thee with great vengeance and furious anger those who attempt to poison and destroy my brothers. And you will know I am the Lord when I lay my vengeance upon you.~n"),
            timer:sleep(18000),
            exit(goodbye);
        what -> CounterPid ! what;
        _ -> QuestionPid ! next
    end,
    julesQuestioning(Pids).

whatCounter(Count) ->
    receive
        what when Count > 3 -> exit(counter_full);
        what when Count == 3 -> io:format("Say what again, SAY WHAT AGAIN, I dare you I double dare you motherf***er, say what one more goddamn time.~n");
        what when Count == 2 -> io:format("ENGLISH motherf***er do you speak it?~n");
        what when Count == 1 -> io:format("What country you from? what ain't no country I ever heard of, you speak english or what?~n")
    end,
    whatCounter(Count+1). 

questioning(Question) ->
    receive
        next when Question > 4 -> exit(question_full);
        next when Question == 4 -> io:format("DOES HE LOOK LIKE A BITCH?~n");
        next when Question == 3 -> io:format("Does he look like a bitch?~n");
        next when Question == 2 -> io:format("Go on~n");
        next when Question == 1 -> io:format("DESCRIBE WHAT Marsellus Wallace looks like~n")
    end,
    questioning(Question+1).