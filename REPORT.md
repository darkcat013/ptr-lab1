# FAF.PTR16.1 -- Project 0

> **Performed by:** Viorel Noroc, group FAF-203
> **Verified by:** asist. univ. Alexandru Osadcenco

## P0W1

**Task 1** -- **Minimal Task** Follow an installation guide to install the language / development environment of your choice.

I have installed erlang on Windows 11 using chocolatey.

``` ps
choco install erlang
```

**Task 2** -- **Minimal Task** Write a script that would print the message “Hello PTR” on the screen.
Execute it.

```erlang
hello_world() ->
    io:format("Hello PTR\n").
```

I wrote a function hello_world that prints Hello PTR using the built-in function io:format() (the alternative for this function is io:fwrite()).

**Task 3** -- **Main Task** Initialize a VCS repository for your project. Push your project to a remote repo.

I initialized a repository on github, [link](https://github.com/darkcat013/ptr-lab1).

**Task 4** -- **Bonus Task** Write a comprehensive readme for your repository.

In the README.md of the repository I wrote how to install erlang, how to build a module and how to run functions from it. Also for the week 5 I wrote another README inside the folder.

**Task 5** -- **Bonus Task** Create a unit test for your project. Execute it.

```erlang
get_hello() ->
    "Hello PTR".

unit_test_() -> 
    ?_test(?assert(get_hello() =:= "Hello PTR")).
```

I wrote a function get_hello() that return the string "Hello PTR" and an unit test that checks if the function returns "Hello PTR".

## P0W2

**Task 1** -- **Minimal Task** Write a function that determines whether an input integer is prime.

```erlang
isPrime(N) when N < 2 -> false;
isPrime(N) when is_integer(N) -> checkPrime(N, 2, N div 2).

    checkPrime(N, I, Max) ->
        if
            I > Max -> true;
            N rem I =:= 0 -> false;
            true -> checkPrime(N, I + 1, Max)
        end.
```

I wrote a function isPrime/1 that checks if an integer is prime. Numbers smaller than 2 are not prime. For the rest of the numbers, I checked all the numbers until N/2 if they divide by N. If at leas one does then the number is not prime.

**Task 2** -- **Minimal Task** Write a function to calculate the area of a cylinder, given it’s height and radius.

```erlang
cylinderArea(Height, Radius) when is_number(Height) and is_number(Radius) -> 2 * math:pi() * Radius * (Height + Radius).

```

I wrote a function cylinderArea/2 that returns the cylinder area given the height and radius using the formula for it: $2\pi r(h + r)$

**Task 3** -- **Minimal Task** Write a function to reverse a list.

```erlang
reverse(List) when is_list(List) -> lists:reverse(List).
```

I wrote a function reverse/1 that gets a list and reverses it using the reverse/1 function from the lists module.

**Task 4** -- **Minimal Task** Write a function to calculate the sum of unique elements in a list.

```erlang
uniqueSum(List) when is_list(List) -> lists:sum(lists:uniq(List)).
```

I wrote a function uniqueSum/1 that gets a list and returns the sum of its unique elements by using the functions uniq/1 and sum/1 from the lists module.

**Task 5** -- **Minimal Task** Write a function that extracts a given number of randomly selected elements from a list.

```erlang
extractRandomN(List, N) when is_list(List) and is_integer(N) -> getRandomN(List, N, []).

    getRandomN(_, N, ResultList) when N < 1 -> ResultList;
    getRandomN(List, N, ResultList) ->
        Elem = lists:nth(rand:uniform(length(List)), List),
        getRandomN(List, N - 1, ResultList ++ [Elem]).
```

I wrote a function extractRandomN/2 which gets as input a list and the amount of random numbers to be extracted and returns a list of randomly picked numbers from the input list. For this I used rand:uniform to get a random number from 1 to the length of input list and lists:nth to get the number at the random index, then continued until I got the needed amount of numbers.

**Task 6** -- **Minimal Task** Write a function that returns the first n elements of the Fibonacci sequence.

```erlang
firstFibonacciElements(N) when N < 1 -> [];
firstFibonacciElements(N) when N =:= 1 -> [1];
firstFibonacciElements(N) when is_integer(N) ->  getFibonacciElements([1, 1], N - 2).

    getFibonacciElements(List, 0) -> List;
    getFibonacciElements(List, N) -> getFibonacciElements(List ++ [lists:sum(lists:nthtail(length(List) - 2, List))], N - 1).
```

I wrote a function firstFibonacciElements/1 that takes as input an integer which represents the length of the Fibonacci sequence to return. If N is smaller than 1 then it will return an empty list, if it's 1 it will return [1]. If N is bigger than 1 then It will start to calculate the fibonacci numbers. For this I send an initial list [1, 1] to a recursive function and to calculate  the next fibonacci element I take the last 2 elements of this list, $[1, 1] \rightarrow [1, 1, (1+1)] \rightarrow [1, 1, 2, (1+2)] \rightarrow [1, 1, 2, 3, (2+3)]$.

**Task 7** -- **Minimal Task** Write a function that, given a dictionary, would translate a sentence. Words not found in the dictionary need not be translated.

```erlang
translator(Dictionary, OriginalString) when is_map(Dictionary) ->
    SplitString = string:split(OriginalString, " ", all),
    ResultString = lists:map(
        fun(Word) ->
            case maps:is_key(Word, Dictionary) of
                true -> maps:get(Word, Dictionary);
                false -> Word
            end
        end,
        SplitString),
    string:join(ResultString, " ").
```

I wrote a function translator/2 which takes as input a map of words to translate and a string of different words. To translate the words in the string I just split it by space into a list of strings then map each word with a word from dictionary then join back the list of strings into a string with spaces.

**Task 8** -- **Minimal Task** Write a function that receives as input three digits and arranges them in an order that would create the smallest possible number. Numbers cannot start with a 0.

```erlang
smallestNumber(A, B, C) when is_integer(A) and is_integer(B) and is_integer(C) ->
    SortedNumbers = [integer_to_list(I) || I <- lists:sort([A, B, C])],
    StringResult = case SortedNumbers of
            ["0", "0", "0"] -> "0";
            ["0", "0", _] -> [X, Y, Z] = SortedNumbers, string:join([Z, Y, X], "");
            ["0", _, _] ->  [X, Y, Z] = SortedNumbers, string:join([Y, X, Z], "");
            _ -> string:join(SortedNumbers, "")
        end,
    list_to_integer(StringResult).
```

I wrote a function smallestNumber/3 that takes as input 3 integers and returns the smalles number that can be made from them. For this I sort the numbers and convert them into a string, then I check if it has leading zeroes and join string numbers into a string then convert it to integer and return.

**Task 9** -- **Minimal Task** Write a function that would rotate a list n places to the left.

```erlang
rotateLeft(List, N) when is_list(List) and is_integer(N) ->
    RealN = N rem length(List),
    {List1, List2} = lists:split(RealN, List),
    List2 ++ List1.
```

I wrote a function rotateLeft/2 that takes as input a list and the amount of places to rotate it to the left. If N is bigger than the list length then it doesn't make sense to fully rotate it multiple times, it can be rotated by the remaining of the division of N and length of the list and the result will be the same. To actually rotate the list I just split it at the N index and switch the 2 resulting lists places.

**Task 10** -- **Minimal Task** Write a function that lists all tuples $a,\ b,\ c$ such that $a^2 +b^2 = c^2$ and $a, b \le 20$.

```erlang
listRightAngleTriangles() ->
    findTriangles(1, 1, 1, []).

findTriangles(21, _, _, ResultList) -> ResultList;
findTriangles(A, 21, C, ResultList) -> findTriangles(A+1, 1, C, ResultList);
findTriangles(A, B, 21, ResultList) -> findTriangles(A, B+1, 1, ResultList);
findTriangles(A, B, C, ResultList) ->
    if
        A * A + B * B =:= C * C -> findTriangles(A, B, C+1, ResultList ++ [{A, B, C}]);
        true -> findTriangles(A,B,C+1, ResultList)
    end.
```

I wrote a function listRightAngleTriangles/0 that returns the tuples that satisfy the condition from the task. this function calls a function that simulates a for in for in for that checks for the condition.

**Task 11** -- **Main Task** Write a function that eliminates consecutive duplicates in a list.

```erlang
removeConsecutiveDuplicates(List) when is_list(List) ->
    removeDuplicates(List).

    removeDuplicates([]) -> [];
    removeDuplicates([X]) -> [X];
    removeDuplicates([X, X | Rest]) -> removeDuplicates([X | Rest]);
    removeDuplicates([X, Y | Rest]) -> [X | removeDuplicates([Y | Rest])].
```

I wrote a function removeConsecutiveDuplicates/1 that returns a new list with removed consecutive duplicates. This function calls a recursive function that uses pattern matching to check if the next 2 elements are the same or not. If they are, one of them will be removed. If not, it will continue further until there is no elements to check.

**Task 12** -- **Main Task** Write a function that, given an array of strings, will return the words that can be typed using only one row of the letters on an English keyboard layout.

```erlang
lineWords(List) when is_list(List) ->
    Rows = ["qwertyuiop", "asdfghjkl", "zxcvbnm"],
    lists:filter(fun(Word) -> checkWordInRows(Word, Rows) end, List).
        
    checkWordInRows(Word, Rows) ->
        lists:any(fun(Row) -> checkWordInRow(Word, Row) end, Rows).

    checkWordInRow(Word, Row) -> 
        lists:all(fun(Char) -> lists:member(Char, Row) end, string:lowercase(Word)).
```

I wrote a function lineWords/1 that takes a list of strings as input and returns a list of strings from the input that can be written using only 1 keyboard row from the keyboard. For this I declared the keyboard rows, checked each letter in each word if they correspond to a row. If at least one letter is from a different row then it won't be included in the input.

**Task 13** -- **Main Task** Create a pair of functions to encode and decode strings using the Caesar cipher.

```erlang
encode(String, K) when is_integer(K) ->
    RealShift = K rem 26,
    LowerString = string:lowercase(String),
    lists:map(fun(Char) -> Char + RealShift end, LowerString).

decode(String, K) when is_integer(K) ->
    encode(String, -K).
```

I wrote two functions encode/2 and decode/2 that take a string and the key as input and return either an encoded or decoded string. Here I just used the formula for the Caesar cipher in the encode function and for the decode I used encode with a negative key to inverse the result.

**Task 14** -- **Main Task** Write a function that, given a string of digits from 2 to 9, would return all possible letter combinations that the number could represent (think phones with buttons).

```erlang
lettersCombinations(NumbersString) -> 
    Mapping = #{"2"=>"abc", "3"=>"def", "4"=>"ghi", "5"=>"jkl", "6"=>"mno", "7"=>"pqrs", "8"=>"tuv", "9"=>"wxyz"},
    
    getCombinations(NumbersString, length(NumbersString), Mapping, 0, [], []).

    getCombinations(_,MaxLength, _, MaxLength, CurrentString, FinalResult) -> FinalResult ++ [CurrentString];
    getCombinations(NumbersString, MaxLength, Mapping, CurrentLength, CurrentString, FinalResult) ->
        {CurrentNumber, RestNumbers} = lists:split(1, NumbersString),
        case maps:get(CurrentNumber, Mapping) of 
            [X, Y, Z] -> 
                getCombinations(RestNumbers, MaxLength, Mapping, CurrentLength+1, CurrentString ++ [Z], 
                    getCombinations(RestNumbers, MaxLength, Mapping, CurrentLength+1, CurrentString ++ [Y], 
                        getCombinations(RestNumbers, MaxLength, Mapping, CurrentLength+1, CurrentString ++ [X], FinalResult)));
            [X, Y, Z, Z1] -> 
                getCombinations(RestNumbers, MaxLength, Mapping, CurrentLength+1, CurrentString ++ [Z1], 
                    getCombinations(RestNumbers, MaxLength, Mapping, CurrentLength+1, CurrentString ++ [Z], 
                        getCombinations(RestNumbers, MaxLength, Mapping, CurrentLength+1, CurrentString ++ [Y], 
                            getCombinations(RestNumbers, MaxLength, Mapping, CurrentLength+1, CurrentString ++ [X], FinalResult))))
        end.
```

I wrote a function lettersCombinations/1 that takes a string of numbers as input and returns all the combinations that can be made of them using a keyboard from a mobile phone. First of all I declared the mappings of numbers and letters and then made the input string behave like a tree and then traversed it in Inorder to get all the possible combinations.

**Task 15** -- **Main Task** Write a function that, given an array of strings, would group the anagrams together.

```erlang
groupAnagrams(List) when is_list(List) ->
    StringsList = lists:map(fun(String) -> {lists:sort(String), String} end, List),
    lists:foldl(fun({Key, Value}, Map) ->
        case maps:is_key(Key, Map) of
            false -> maps:put(Key, [Value], Map);
            true -> maps:put(Key, lists:sort(maps:get(Key, Map) ++ [Value]), Map)
        end
    end, #{}, StringsList).
```

I wrote a function groupAnagrams/1 that takes as input a list of strings and returns a map, where the keys are the discovered anagrams and elements are the words that match the anagram. For this I made a list of tuples that include the anagram and the corresponding string by sorting each word in the input list. Then I grouped the tuples in a map using the lists:foldl function.

**Task 16** -- **Bonus Task** Write a function to find the longest common prefix string amongst a list of strings.

```erlang
commonPrefix(List) when is_list(List), length(List)=:=0 -> "";
commonPrefix(List) when is_list(List)->
    checkPrefix(lists:sublist(lists:nth(1, List), 1), List, 1, "").

    checkPrefix(Prefix, List, PrefixLength, Result) ->
        case lists:all(fun(Word) -> lists:sublist(Word, PrefixLength) =:= Prefix end, List) of 
            true -> checkPrefix(lists:sublist(lists:nth(1, List), PrefixLength+1), List, PrefixLength+1, Prefix);
            false -> Result
        end.
```

I wrote a function commonPrefix/1 that takes as input a list of strings. It calls a recursive function that gradually checks if the start of a string matches the start of all other strings. It starts with the first letter, then first two letters and so on.

**Task 17** -- **Bonus Task** Write a function to convert arabic numbers to roman numerals.

```erlang
toRoman(N) when is_integer(N), N >= 1, N =< 3999 -> 
    reduceN(N, "").

    reduceN(0, Result) -> Result;
    reduceN(N, Result) ->
        if
            N >= 1000 -> reduceN(N-1000, Result ++ "M");
            N >= 900 -> reduceN(N-900, Result ++ "CM");
            N >= 500 -> reduceN(N-500, Result ++ "D");
            N >= 400 -> reduceN(N-400, Result ++ "CD");
            N >= 100 -> reduceN(N-100, Result ++ "C");
            N >= 90 -> reduceN(N-90, Result ++ "XC");
            N >= 50 -> reduceN(N-50, Result ++ "L");
            N >= 40 -> reduceN(N-40, Result ++ "XL");
            N >= 10 -> reduceN(N-10, Result ++ "X");
            N >= 9 -> reduceN(N-9, Result ++ "IX");
            N >= 5 -> reduceN(N-5, Result ++ "V");
            N >= 4 -> reduceN(N-4, Result ++ "IV");
            N >= 1 -> reduceN(N-1, Result ++ "I")
        end.
```

I wrote a function toRoman/1 that takes as input a number and returns its roman representation. The biggest number in roman system is 3999 and smallest is 1. For this I matched all the roman notations to the arabic numbers and gradually subtracted the input number while adding to an accumulator the roman notation until the input number becomes 0.

**Task 18** -- **Bonus Task** Write a function to calculate the prime factorization of an integer.

```erlang
factorize(N) when N < 2 -> [];
factorize(N) when is_integer(N) -> 
    nextFactor(N, 2, []).

    nextFactor(1, _, ResultList) -> ResultList;
    nextFactor(N, I, ResultList) ->
        case isPrime(I) of 
            false -> nextFactor(N, I+1, ResultList);
            true -> if
                N rem I =:= 0 -> nextFactor(N div I, I, ResultList ++ [integer_to_list(I)]);
                true -> nextFactor(N, I+1, ResultList)
            end
        end.
```

I wrote a function factorize/1 that gets as input a number and returns a list of prime numbers representing its factorization. For this I used the isPrime function from the first task to check the current divisor. If the current divisor is prime and input can be divided by it, it is added to the accumulator and it is checked again. If the current divisiro is either not prime or cannot be divided, it is incremented.

## P0W3

**Task 1** -- **Minimal Task** Create an actor that prints on the screen any message it receives.

```erlang
register(anyMessage, spawn(?MODULE, anyMessageActor, [])).

anyMessageActor() ->
    receive
        Anything -> io:format("~w~n", [Anything])
    end,
    anyMessageActor().
```

I wrote a function anyMessageActor/0 that infinitely receives anything and prints it to the console. To pass messages to this actor I registered it using register function and the anyMessage atom.

**Task 2** -- **Minimal Task** Create an actor that returns any message it receives, while modifying it. Infer the modification from the following example:

```erl
> Pid ! 10. % Integers
  Received : 11
> Pid ! " Hello ". % Strings
  Received : hello
> Pid ! {10 , " Hello "}. % Anything else
  Received : I don ’ t know how to HANDLE this !
```

```erlang
register(specificMessage, spawn(?MODULE, specificMessageActor, [])).

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
```

I wrote a function specificMessageActor/0 that infinitely receives anything, formats it like the condition requires and prints the result to the console. To pass messages to this actir I registered it using register function and the specificMessage atom.

**Task 3** -- **Minimal Task** Create a two actors, actor one ”monitoring” the other. If the second actor stops, actor one gets notified via a message.

```erlang
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
```

I wrote a function startMonitoringSimulation/0 which spawns an actor that monitors another actor, a function monitorActor/0 that spawns a monitored actor and a function monitoredActor/0 which is the monitored actor that dies in 2 seconds. The monitorActor monitors the monitored actors using the monitor function which takes the monitor actor's Pid then waits for it to die and outputs an error report to the console.

**Task 4** -- **Minimal Task** Create an actor which receives numbers and with each request prints out the current average.

```erlang
register(averager, spawn(?MODULE, averager, [0])).

averager(CurrentAvg) -> 
    io:format("Current average is ~p~n", [CurrentAvg]), 
    receive
        N when is_number(N) -> 
            Avg = (CurrentAvg+N)/2, 
            averager(Avg)
    end.
```

I wrote a function averager/1 that prints to the console the current average of all the numbers it received. To pass messages to this actir I registered it using register function and the averager atom.

**Task 5** -- **Main Task** Create an actor which maintains a simple FIFO queue. You should write helper functions to create an API for the user, which hides how the queue is implemented.

```erlang
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
```

I wrote an API that contains the functions new_queue/0, push/2 and pop/1. The function new_queue/0 spawns an actor which represents a queue and prints its Pid. The function push/2 receives the queue Pid and a value to push to the queue. The function pop/1 receives the queue Pid and prints the popped value from the queue. The queueActor waits for messages from the push/2 and pop/1 functions and operates based on what atom it received.

**Task 6** -- **Main Task** Create a module that would implement a semaphore.

```erlang
% semaphore module
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
        {release} when Value < MaxValue ->  semaphoreActor(Value + 1, MaxValue)
    end.

% semaphore simulation 
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
```

I wrote a function startSemaphoreSimulation/0 that simulates an usage of a semaphore by spawning a bunch of actors with different sleep times. The semaphore itself is created using semaphore:create_semaphore/1 function which takes as input the maximum amount of processes that can run concurrently. To use the semaphore, it the semaphore:acquire/1 function needs to be called which takes as input the semaphore Pid and increments the current amount of concurrent processes. If the maximum amount is reached, it will wait in an infinite loop until it is free. To mark the end of the semaphore usage, the semaphore:release/1 function is used which takes as input the semaphore Pid and decrements the current amount of concurrent processes. Both acquire/1 and release/1 functions send messages to an actor which handles the checking for the current amount of concurrent processes and either allows the process to run or sends it in an infinite loop unti it is allowed to run.

**Task 7** -- **Bonus Task** Create a module that would perform some risky business. Start by creating a scheduler actor. When receiving a task to do, it will create a worker node that will perform the task. Given the nature of the task, the worker node is prone to crashes (task completion rate 50%). If the scheduler detects a crash, it will log it and restart the worker node. If the worker node finishes successfully, it should print the result.

```erlang
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
```

I wrote a function create_scheluder/0 that spawns a scheduler that handles the all the tasks. The tasks are sent to actors that handle them with 50% success rate. If the actor fails it tries to divide by 0 and dies because of it. This sends a message to the scheduler that the task failed and it spawns another actor to take care of the task.

**Task 8** -- **Bonus Task** Create a module that would implement a doubly linked list where each node of the list is an actor.

```erlang
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
```

I wrote a function create_dllist/1 which takes as input a list of values. This function proceeds to make a list of tuples, each tuples containing either 3 actors or 2 actors and head or end atoms. the first item has the {head, Pid, Pid} structure and the last item {Pid, Pid, end}. Each actors holds a value from the input list and returns it to the caller when it's called. To traverse this list of tuples I wrote 2 functions traverse/1 and inverse/1 which traverses the list of actors either in order or inverse order like a doubly linked list by calling the current actor of each tuple, putting it into a list and going to the next actor. When the head or end atom is reached, the output is shown to the console.

## P0W4

**Task 1** -- **Minimal Task** Create a supervised pool of identical worker actors. The number of actors is static, given at initialization. Workers should be individually addressable. Worker actors should echo any message they receive. If an actor dies (by receiving a “kill” message), it should be restarted by the supervisor. Logging is welcome.

```erlang
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
```

I wrote a function startIdenticalActors/1 which takes as input the amount of identical actors to spawn. It shows the Pid of each spawned actor. When it spawned all the actors, it waits for any actor to die. When a spawned actor receives the kill atom, it dies and its supervisor writes this to the console and spawns another and also shows its Pid.

**Task 2** -- **Main Task** Create a supervised processing line to clean messy strings. The first worker in the line would split the string by any white spaces (similar to Python’s str.split method). The second actor will lowercase all words and swap all m’s and n’s (you nomster!). The third actor will join back the sentence with one space between words (similar to Python’s str.join
method). Each worker will receive as input the previous actor’s output, the last actor printing the result on screen. If any of the workers die because it encounters an error, the whole processing line needs to be restarted. Logging is welcome.

```erlang
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
```

I wrote a function startProcessingLine/0 which starts the task by spawning a supervisor processingLine that also starts 3 actors that do what the taks requires. When any of the 3 actors die, the supervisor receives a message that the actor died, restarts it and restarts the string manipulation.

**Task 3** -- **Bonus Task** Write a supervised application that would simulate a sensor system in a car. There should be sensors for each wheel, the motor, the cabin and the chassis. If any sensor dies because of a random invalid measurement, it should be restarted. If, however, the main sensor supervisor system detects multiple crashes, it should deploy the airbags.

```erlang
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
```

I wrote a function startCarSensors/0 that spawns the main supervisor. The main supervisor spawns 4 more actors: cabinSensorRestarter, motorSensorRestarter, chassisSensorRestarter, wheelsSensorSupervisorRestarter and goes into a loop where it checks how many sensor are currently dead. Each of the 4 actors supervise an actor and restarts it after a certain amount of time, while also sending to the supervisor that the actor died. If 2 or more sensors died then the main supervisor dies which kills the whole application and prints AIRBAGS to the console. The wheelsSensorSupervisorRestarter spawns a supervisor much like the main supervisor but supervises 4 identical actors, wheelSensorRestarter. This actor spawns a wheel sensor and restarts it after a certain amount of time whil sending to the wheelsSensorSupervisor that a wheel sensor died. If 2 or more wheel sensors died, the wheelsSensorSupervisor also dies and wheelsSensorSupervisorRestarter tries to restart it, while also sending to the main supervisor that the sensor died.

**Task 4** -- **Bonus Task** Write an application that, in the context of actor supervision. would mimic the exchange in [that scene](https://www.youtube.com/watch?v=xwT60UbOZnI) from the movie Pulp Fiction.

```erlang
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
```

I wrote a function startMf/0 that starts an actor named jules that supervises 2 actors, whatCounter and questioning. Depending on what the supervisor gets as a message, one of these 2 actors is called. If the whatCounter reaches 4 sent messages or questioning reaches 5 messages, they die and depending on what actor dies, the jules actor prints some text then shuts down the pc by using the os:cmd("shutdown") function.

## P0W5

**Task 1** -- **Minimal Task** Write an application that would visit [this link](https://quotes.toscrape.com/). Print out the HTTP response status code, response headers and response body.

```erlang
visit_quotes() ->
    {ok, StatusCode, Headers, Body} = request_quotes(),
    
    io:format("Status Code: ~p~n", [StatusCode]),
    io:format("Response Headers: ~p~n", [Headers]),
    io:format("Response Body:~n~s~n", [replace_html_entities_with_text(Body)]).

request_quotes() ->
    inets:start(),
    ssl:start(),
    {ok, {{_, StatusCode, _}, Headers, Body}} = httpc:request("https://quotes.toscrape.com/"),
    {ok, StatusCode, Headers, Body}.

replace_html_entities_with_text(String) ->
    string:replace(String, "&#39;", "'", all).
```

I wrote a function visit_quotes/0 that calls the request_quotes/0 and prints its results. The request_quotes/0 function makes a call to the https://quotes.toscrape.com/ link and returns the status code, response header and response body. The replace_html_entities_with_text/1 takes a string as input and replaces some html entities with its coresponding, human readable, text.

**Task 2** -- **Minimal Task** Continue your previous application. Extract all quotes from the HTTP response body. Collect the author of the quote, the quote text and tags. Save the data into a list of maps, each map representing a single quote.

```erlang
extract_quotes() ->
    {ok,_ , _, Body} = request_quotes(),
    find_quotes(Body).

find_quotes(Body) ->
    find_quotes(Body, []).

find_quotes([], Acc) -> lists:reverse(Acc);
find_quotes(Body, Acc) ->
    case Body of
        [$c, $l, $a, $s, $s, $=, $", $q, $u, $o, $t, $e, $" | T] -> 
            {BodyRest, AccElem} = find_quote_text(T, #{}),
            find_quotes(BodyRest, [AccElem | Acc]);
        [_ | T] -> find_quotes(T, Acc)
    end.

find_quote_text(Body, Map) ->
    case Body of 
        [$t, $e, $x, $t, $", $> | T] -> 
            {BodyRest, Text} = get_quote_text(T, []),
            find_quote_author(BodyRest, maps:put(text, list_to_binary(replace_html_entities_with_text(Text)), Map));
        [_ | T] -> find_quote_text(T, Map)
    end.

get_quote_text(Body, Acc) ->
    case Body of 
        [$<, $/, $s, $p, $a, $n, $> | T] -> {T, lists:reverse(Acc)};
        [H | T] -> get_quote_text(T, [H | Acc])
    end.

find_quote_author(Body, Map) ->
    case Body of 
        [$a, $u, $t, $h, $o, $r, $", $> | T] -> 
            {BodyRest, Author} = get_quote_author(T, []),
            find_quote_tags(BodyRest, maps:put(author, list_to_binary(replace_html_entities_with_text(Author)), Map));
        [_ | T] -> find_quote_author(T, Map)
    end.

get_quote_author(Body, Acc) ->
    case Body of 
        [$<, $/, $s, $m, $a, $l, $l, $> | T] -> {T, lists:reverse(Acc)};
        [H | T] -> get_quote_author(T, [H | Acc])
    end.

find_quote_tags(Body, Map) ->
    case Body of 
        [$t, $a, $g, $s, $", $> | T] ->
            {BodyRest, TagList} = get_tag_list(T, []),
            {BodyRest, maps:put(tags, TagList, Map)};
        [$<, $/, $d, $i, $v, $> | T] -> {T, Map};
        [_ | T] -> find_quote_tags(T, Map)
    end.

get_tag_list(Body, Acc) ->
    case Body of 
        [$<, $/, $d, $i, $v, $> | T] -> {T, lists:reverse(Acc)};
        [$c, $l, $a, $s, $s, $=, $", $t, $a, $g, $" | T] -> 
            {BodyRest, Tag} = find_tag(T),
            get_tag_list(BodyRest, [list_to_binary(Tag) | Acc]);
        [_ | T] -> get_tag_list(T, Acc)
    end.

find_tag(Body) ->
    case Body of 
        [$> | T] -> get_tag(T, []);
        [_ | T] -> find_tag(T)
    end.

get_tag(Body, Acc) ->
    case Body of 
        [$<, $/, $a, $> | T] -> {T, lists:reverse(Acc)};
        [H| T] -> get_tag(T, [H|Acc])
    end.
```

I wrote a function extract_quotes/0 that calls the request_quotes/0 from the previous task and then returns the result of the find_quotes/1 function while sending it the response body of the request_quotes/0 function. I scrapped the quotes by looking to each html element class name or tag. All the functions used for this have a similar template, adapted for the quote text, quote author and the quote tags.

**Task 3** -- **Minimal Task** Continue your previous application. Persist the list of quotes into a file. Encode the data into JSON format. Name the file quotes.json.

```erlang
quotes_to_json() ->
    file:write_file("quotes.json", jsx:prettify(jsx:encode(extract_quotes()))).
```

I wrote a function quotes_to_json/0 which calls the extract_quotes/0 function from the previous task, encodes the result to json, prettifies it and writes it to the file quotes.json. For this I used the jsx library, which I imported using rebar3.

**Task 4** -- **Main Task** Write an application that would implement a Star Wars-themed RESTful API. The API should implement the following HTTP methods:

- GET /movies
- GET /movies/:id
- POST /movies
- PUT /movies/:id
- PATCH /movies/:id
- DELETE /movies/:id

Use a database to persist your data. Populate the database with the following information:

```json
[
  {
    "id": 1,
    "title": "Star Wars : Episode IV - A New Hope",
    "release_year": 1977,
    "director": "George Lucas"
  },
  {
    "id": 2,
    "title": "Star Wars : Episode V - The Empire Strikes Back",
    "release_year": 1980,
    "director": "Irvin Kershner"
  },
  {
    "id": 3,
    "title": "Star Wars : Episode VI - Return of the Jedi ",
    "release_year": 1983,
    "director": "Richard Marquand"
  },
  {
    "id": 4,
    "title": "Star Wars : Episode I - The Phantom Menace ",
    "release_year": 1999,
    "director": "George Lucas"
  },
  {
    "id": 5,
    "title": "Star Wars : Episode II - Attack of the Clones ",
    "release_year": 2002,
    "director": "George Lucas"
  },
  {
    "id": 6,
    "title": "Star Wars : Episode III - Revenge of the Sith ",
    "release_year": 2005,
    "director": "George Lucas"
  },
  {
    "id": 7,
    "title": "Star Wars : The Force Awakens ",
    "release_year": 2015,
    "director": "J. J. Abrams"
  },
  {
    "id": 8,
    "title": "Rogue One : A Star Wars Story ",
    "release_year": 2016,
    "director": "Gareth Edwards"
  },
  {
    "id": 9,
    "title": "Star Wars : The Last Jedi",
    "release_year": 2017,
    "director": "Rian Johnson"
  },
  {
    "id": 10,
    "title": "Solo : A Star Wars Story",
    "release_year": 2018,
    "director": "Ron Howard"
  },
  {
    "id": 11,
    "title": "Star Wars : The Rise of Skywalker",
    "release_year": 2019,
    "director": "J. J. Abrams"
  }
]
```

```erlang
-module(main_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    start_ets(),
    start_cowboy(),
    
    main_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

start_ets() ->
    ets:new(movies, [ordered_set, public, named_table]),
    {ok, Binary} = file:read_file("movies.json"),
    Movies = jsx:decode(Binary),
    populate_movies_table(Movies).

populate_movies_table([]) -> ok;
populate_movies_table(Movies) ->
    [H | T] = Movies,
    #{
        <<"id">> := Id,
        <<"title">> := Title,
        <<"release_year">> := ReleaseYear,
        <<"director">> := Director
    } = H,
    ets:insert_new(movies, {Id, Title, ReleaseYear, Director}),
    populate_movies_table(T).
    
start_cowboy() ->
    Port = 8080,
    Dispatch = cowboy_router:compile([
        {'_', 
            [
                {"/movies", main_handler, []},
                {"/movies/:id", main_handler, []},
                {"/callback", spotify_handler, []}
            ]
        }
    ]), 
    {ok, _} = cowboy:start_clear(http_server,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ).

-module(main_handler).
 
-export([init/2]).
 
init(#{method := Method} = Req, State) ->

    Id = cowboy_req:binding(id, Req),
    case Id of
        undefined -> handle_req(Method, {Req, State});
        _ -> handle_id_req(Method, {Req, State, Id})
    end.
 
handle_req(<<"GET">>, Params) ->
    MovieMapList = get_map_list(ets:tab2list(movies)),
    {Req, State} = Params,  
    return_OK_json(Req, State, to_json(MovieMapList));
handle_req(<<"POST">>, Params) ->
    {Req, State} = Params,  
    {ok, Body, NewReq} = cowboy_req:read_body(Req),
    true = ets:insert_new(movies, map_to_movie(jsx:decode(Body))),
    return_OK_json(NewReq, State, Body);
handle_req(_Method, Params) ->
    {Req, State} = Params,
    return_not_found(Req, State).

handle_id_req(<<"GET">>, Params) ->
    {Req, State, Id} = Params,
    MovieLookup = ets:lookup(movies, binary_to_integer(Id)),
    case MovieLookup of 
        [Movie] -> return_OK_json(Req, State, to_json(movie_to_map(Movie)));
        _ -> return_not_found(Req, State)
    end;
handle_id_req(<<"PUT">>, Params) ->
    {Req, State, Id} = Params,
    IntegerId = binary_to_integer(Id),
    MovieLookup = ets:lookup(movies, IntegerId),
    case MovieLookup of 
        [_Movie] -> 
            {ok, Body, NewReq} = cowboy_req:read_body(Req),
            PutMovie = put_map_to_movie(jsx:decode(Body)),
            {Title, ReleaseYear, Director} = PutMovie,
            true = ets:update_element(movies, IntegerId, [
                {2, Title},
                {3, ReleaseYear},
                {4, Director}
            ]),
            [Movie] = ets:lookup(movies, IntegerId),
            return_OK_json(NewReq, State, to_json(movie_to_map(Movie)));
        _ -> return_not_found(Req, State)
    end;
handle_id_req(<<"PATCH">>, Params) ->
    {Req, State, StringId} = Params,
    MovieLookup = ets:lookup(movies, binary_to_integer(StringId)),
    case MovieLookup of 
        [Movie] -> 
            {ok, Body, NewReq} = cowboy_req:read_body(Req),
            PatchMap = jsx:decode(Body),
            case patch_valid(PatchMap) of
                true ->
                     PatchMovie = patch_movie(movie_to_map(Movie), PatchMap),
                    {Id, Title, ReleaseYear, Director} = PatchMovie,
                    true = ets:update_element(movies, Id, [
                        {2, Title},
                        {3, ReleaseYear},
                        {4, Director}
                    ]),
                    [NewMovie] = ets:lookup(movies, Id),
                    return_OK_json(NewReq, State, to_json(movie_to_map(NewMovie)));
                _ -> return_bad_request(Req, State)
            end;
        _ -> return_not_found(Req, State)
    end;
handle_id_req(<<"DELETE">>, Params) ->
    {Req, State, Id} = Params,
    ets:delete(movies, binary_to_integer(Id)),
    return_OK(Req, State, "OK");
handle_id_req(_Method, Params) ->
    {Req, State, _Id} = Params,
    return_not_found(Req, State).

to_json(Value) ->
    jsx:prettify(jsx:encode(Value)).
return_OK_json(Req, State, Json) ->
    NewReq = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Json,
        Req),
    {ok, NewReq, State}.
return_OK(Req, State, Message) ->
    NewReq = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        Message,
        Req),
    {ok, NewReq, State}.
return_bad_request(Req, State) ->
    NewReq = cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, <<"Bad request.">>, Req),
    {ok, NewReq, State}.
return_not_found(Req, State) ->
    NewReq = cowboy_req:reply(404, #{<<"content-type">> => <<"text/plain">>}, <<"Not found.">>, Req),
    {ok, NewReq, State}.

get_map_list(List) ->
    get_map_list(List, []).

get_map_list([], Acc) -> lists:reverse(Acc);
get_map_list(List, Acc) ->
    [H | T] = List,
    get_map_list(T, [movie_to_map(H) | Acc]).

map_to_movie(MovieMap) ->
    #{
        <<"id">> := Id,
        <<"title">> := Title,
        <<"release_year">> := ReleaseYear,
        <<"director">> := Director
    } = MovieMap,
    {Id, Title, ReleaseYear, Director}.
movie_to_map(Movie) ->
    {Id, Title, ReleaseYear, Director} = Movie,
    #{
        <<"id">> => Id,
        <<"title">> => Title,
        <<"release_year">> => ReleaseYear,
        <<"director">> => Director
    }.

put_map_to_movie(MovieMap) ->
    #{
        <<"title">> := Title,
        <<"release_year">> := ReleaseYear,
        <<"director">> := Director
    } = MovieMap,
    {Title, ReleaseYear, Director}.

patch_valid(PatchMap) ->
    Keys = maps:keys(PatchMap),
    ValidKeys = [<<"title">>, <<"release_year">>, <<"director">>],
    AnyInvalidKey = lists:any(fun(V) -> not lists:member(V, Keys) end, ValidKeys),
    not AnyInvalidKey.

patch_movie(MovieMap, PatchMap) ->
    map_to_movie(maps:fold( fun(K, V, Acc) -> maps:update(K, V, Acc) end, MovieMap, PatchMap)).
```

To start the http server, I put the main_app in the startup of this rebar3 project. This calls the main_app:start/2 function which initializes the ets database with the start_ets/0 function and uses the cowboy library to start the http server with the start_cowboy/0 function. The start_ets/0 function reads the contents of the movies.json files and populates a table in the ets database named movies. The start_cowboy/0 function starts an http server at port 8080 and exposes the routes /movies and /movies/:id (the /callback route is used in the bonus task). The /movies route have a handler called main_handler. This module is responsible for handling the requests specified in the task. For the /movies route it is GET and POST requests and for the /movies/:id it is teh GET, PUT, PATCH and DELETE requests. When the cowboy starts the http server, the init/2 function of the main_handler is called which listens for the incoming request on the /movies routes and calls the handle_req/2 if it is the /movies route or the handle_id_req/2 if it is the /movies/:id rout.These functions just pattern match the Request method,manipulate the movies table in the ets database and return a http response using the return_OK_json/3, return_OK/3, return_bad_request/2 and return_not_found/2 functions.

**Task 5** -- **Bonus Task** Write an application that would use the Spotify API to manage user playlists. It should be able to create a new playlist, add songs to it and add custom playlist cover images. You will probably get to play with OAuth 2.0 and Base64 encoding.

```erlang
-module(spotify_handler).

-export([init/2]).
-export([get_set_tokens/2]).

%https://accounts.spotify.com/authorize?response_type=code&client_id={clientId}&scope=user-read-private%20user-read-email%20playlist-read-private%20playlist-modify-private%20playlist-modify-public%20ugc-image-upload&redirect_uri=http%3A%2F%2Flocalhost%3A8080%2Fcallback

init(Req, State) ->
    #{qs := Code} = Req,
    [$c, $o, $d, $e, $= | StringCode] = binary_to_list(Code),
    [Base, Redirect, _, _] = cowboy_req:uri(Req),
    RedirectLink = iolist_to_binary([Base, Redirect]),
    spawn(?MODULE, get_set_tokens, [lists:sublist(StringCode, 1, string:str(StringCode, "&state")-1), RedirectLink]),
    {ok, Req, State}.

get_set_tokens(Code, RedirectLink) ->
    {ok, Binary} = file:read_file("env.json"),
    #{<<"clientId">>:=ClientId, <<"clientSecret">>:=ClientSecret}=jsx:decode(Binary),
    Authorization = base64:encode(iolist_to_binary([ClientId, ":", ClientSecret])),
    Body = iolist_to_binary([
        "grant_type=authorization_code",
        "&code=", Code,
        "&redirect_uri=", RedirectLink
        ]),
    Request = {
        "https://accounts.spotify.com/api/token",
        [{"Authorization", "Basic " ++ Authorization}],
        "application/x-www-form-urlencoded",
        Body
        },
    {ok, {{_, 200, _}, Headers, ResponseBody}} = httpc:request(post,Request,[],[]),
    file:write_file("tokens.json", jsx:prettify(list_to_binary(ResponseBody))),
    {ok, Request, 200, Headers, ResponseBody}.

-module(bonus).
-export([new_playlist/1, add_song/1, change_image/0]).

% bonus:new_playlist("Erlang playlist").
new_playlist(Name) ->
    AccessToken = init_request(),
    {ok, EnvJson} = file:read_file("env.json"),
    #{<<"userId">>:=UserId}=jsx:decode(EnvJson),
    RequestBody = #{<<"name">> => list_to_binary(Name), <<"public">>=>false},
    Request = {
        "https://api.spotify.com/v1/users/"++binary_to_list(UserId)++"/playlists",
        [{"Authorization", "Bearer " ++ AccessToken}],
        "application/json",
        jsx:encode(RequestBody)
    },
    {ok, {{_, StatusCode, _}, Headers, ResponseBody}} = httpc:request(post, Request,[],[]),
    case StatusCode of 
        401 -> refresh_token(), new_playlist(Name);
        _  -> 
            file:write_file("playlist_response.json", ResponseBody),
            {ok, StatusCode, Headers, ResponseBody}
    end.
    
% bonus:add_song("Along came a spider").
add_song(Query) ->
    AccessToken = init_request(),
    SongId = search_song(Query),
    PlaylistId = get_playlistId(),
    Request = {
        "https://api.spotify.com/v1/playlists/"++ binary_to_list(PlaylistId) ++"/tracks",
        [{"Authorization", "Bearer " ++ AccessToken}],
        "application/json",
        jsx:encode([SongId])
    },
    {ok, {{_, StatusCode, _}, Headers, Body}} = httpc:request(post,Request,[],[]),
    case StatusCode of 
        401 -> refresh_token(), add_song(Query);
        _  -> {ok, StatusCode, Headers, Body}
    end.

% bonus:change_image().
change_image() ->
    AccessToken = init_request(),
    PlaylistId = get_playlistId(),
    {ok, ImgBin} = file:read_file("playlist_image.jpg"),
    Base64Img = base64:encode(ImgBin),
    Request = {
        "https://api.spotify.com/v1/playlists/"++ binary_to_list(PlaylistId) ++"/images",
        [{"Authorization", "Bearer " ++ AccessToken}],
        "image/jpeg",
        Base64Img
    },
    {ok, {{_, StatusCode, _}, Headers, Body}} = httpc:request(put,Request,[],[]),
    case StatusCode of 
        401 -> refresh_token(), change_image();
        _  -> {ok, StatusCode, Headers, Body}
    end.
    
% local functions

% bonus:search_song("Along came a spider").
search_song(Query) ->
    AccessToken = init_request(),
    Request = {
        "https://api.spotify.com/v1/search?include_external=audio&type=track&limit=1&q="++string:replace(Query, " ", "%20", all),
        [{"Authorization", "Bearer " ++ AccessToken}]
    },
    {ok, {{_, StatusCode, _}, _, Body}} = httpc:request(get,Request,[],[]),
    DecodedBody = jsx:decode(list_to_binary(Body)),
    #{<<"tracks">> := 
        #{<<"items">>:= 
            [#{<<"uri">>:= SongId}]
        }
    } = DecodedBody,
    case StatusCode of 
        401 -> refresh_token(), search_song(Query);
        _  -> SongId
    end.

refresh_token() ->
    {ok, TokensJson} = file:read_file("tokens.json"),
    #{<<"refresh_token">> := RefreshToken} = jsx:decode(TokensJson),
    {ok, EnvJson} = file:read_file("env.json"),
    #{<<"clientId">>:=ClientId, <<"clientSecret">>:=ClientSecret}=jsx:decode(EnvJson),
    Authorization = base64:encode(iolist_to_binary([ClientId, ":", ClientSecret])),
    RequestBody = iolist_to_binary([
        "grant_type=refresh_token",
        "&refresh_token=", RefreshToken
    ]),
    Request = {
        "https://accounts.spotify.com/api/token",
        [{"Authorization", "Basic " ++ Authorization}],
        "application/x-www-form-urlencoded",
        RequestBody
    },
    {ok, {{_, 200, _}, Headers, ResponseBody}} = httpc:request(post,Request,[],[]),
    ResponseMap = jsx:decode(list_to_binary(ResponseBody)),
    file:write_file("tokens.json", jsx:prettify(jsx:encode(maps:put(<<"refresh_token">>, RefreshToken, ResponseMap)))),
    {ok, 200, Headers, ResponseBody}.
     
init_request() ->
    inets:start(),
    ssl:start(),
    {ok, TokensJson} = file:read_file("tokens.json"),
    #{<<"access_token">> := AccessToken} = jsx:decode(TokensJson),
    AccessToken.

get_playlistId() ->
    {ok, PlaylistJson} = file:read_file("playlist_response.json"),
    #{<<"id">> := PlaylistId} = jsx:decode(PlaylistJson),
    PlaylistId.
```

As mentioned before, the /callback route is used for this task. The spotify_handler handles this route. It participates in the OAuth2 flow by getting the code when I access the commented link in the spotify_handler module and sends this code to the Spotify API to get the access and refresh tokens. These tokens are saved in the tokens.json file and used in the new_playlist/1, add_song/1 and change_image/0 functions that can be called from console. These functions just call the endpoints on the Spotify API. The add_song/1 gets as input some text then it searches a song with that text using the search_song/1 function and puts the first result in the playlist. Also when the access token is expired, the requests return a 401 status code. When this code is encountered, the access token is refreshed using the refresh_token/0 function and the previously called function is called again. The clientId, clientSecret and userId used in some of the functions are in the env.json file. This file is in .gitignore so I wouldn't expose my credentials.

## Conclusion

This project helped me to go from zero knowledge about erlang to a somewhat decent level of understanding it, the actor model and the functional programming.

The project is split in 5 week, each one increasing in difficulty.

On the first week I had to install the environment and the language I was going to use for the rest of the semester. I chose Erlang and I installed it using on Windows 11 using Chocolatey. After this I initialized a repository on github and pushed my code there.

On the second week I had to solve some simple problems using Erlang. This helped me a lot to get used to the language and the functional paradigm.

On the third week I had to play with actors and supervisors. This week helped me to learn on practice what is an actor and a supervision tree.

The fourth week was all about supervision trees. This week helped me solidify the knowledge about actors and supervision trees.

The fifth and final week was all about web. On this week I learned how to make http requests using Erlang and how to make a http server in Erlang. Also in this week I learned about rebar3 and I made the http server using the cowboy library and ran it using rebar3. On this week I also had to make an integration with Spotify API where I learned about OAuth2 and it was a fun experience in general.

This project was an interesting and fun one and I'm looking forward to the next projects on this course.

## Bibliography

### Books: 

"*Learn You Some Erlang for great good!*" by Fred Hébert

"*Building Web Applications with Erlang*" by Zachary Kessin

### Courses:

https://www.udemy.com/course/modern-erlang-for-beginners/

https://www.udemy.com/course/master-erlang-programming-in-just-4-hours/

### Links:

Erlang documentation:

https://www.erlang.org/doc/

https://www.erlang.org/doc/man/stdlib_app

https://www.erlang.org/doc/apps/inets/http_client.html

https://www.erlang.org/doc/apps/inets/http_server.html

Rebar3, Cowboy:

https://github.com/erlang/rebar3

https://ninenines.eu/docs/en/cowboy/2.6/guide/

https://ezoeryou.github.io/blog/article/2020-04-07-tutorial.html

https://habr.com/ru/post/319950/

https://www.davekuhlman.org/rebar3-cowboy-rest-template.html

https://www.youtube.com/watch?v=5sOBwY2bTLY&list=PLOjc9X-vV0SH45CCKZsRsm5b_JO-w24Jz

https://www.youtube.com/watch?v=aWpSgAAiZaE

https://www.youtube.com/watch?v=BO-8Hx8kPtA

https://www.youtube.com/watch?v=CQh-b5miMAo

https://github.com/cwmaguire/erl_ws

Spotify API:

https://developer.spotify.com/documentation/web-api/quick-start/

https://developer.spotify.com/documentation/web-api/reference/#/

https://developer.spotify.com/documentation/general/guides/authorization/code-flow/