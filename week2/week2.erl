-module(week2).
-include_lib("eunit/include/eunit.hrl").
-export([
    isPrime/1,
    cylinderArea/2,
    reverse/1,
    uniqueSum/1,
    extractRandomN/2,
    firstFibonacciElements/1,
    translator/2,
    smallestNumber/3,
    rotateLeft/2,
    listRightAngleTriangles/0,
    removeConsecutiveDuplicates/1,
    lineWords/1,
    encode/2,
    decode/2,
    lettersCombinations/1,
    groupAnagrams/1,
    commonPrefix/1,
    toRoman/1,
    factorize/1
]).

% Minimal tasks

% 1 Write a function that determines whether an input integer is prime.
isPrime(N) when N < 2 -> false;
isPrime(N) when is_integer(N) -> checkPrime(N, 2, N div 2).

    checkPrime(N, I, Max) ->
        if
            I > Max -> true;
            N rem I =:= 0 -> false;
            true -> checkPrime(N, I + 1, Max)
        end.

% 2 Write a function to calculate the area of a cylinder, given it’s height and radius.
cylinderArea(Height, Radius) when is_number(Height) and is_number(Radius) -> 2 * math:pi() * Radius * (Height + Radius).

% 3 Write a function to reverse a list.
reverse(List) when is_list(List) -> lists:reverse(List).

% 4 Write a function to calculate the sum of unique elements in a list.
uniqueSum(List) when is_list(List) -> lists:sum(lists:uniq(List)).

% 5 Write a function that extracts a given number of randomly selected elements from a list.
extractRandomN(List, N) when is_list(List) and is_integer(N) -> getRandomN(List, N, []).

    getRandomN(_, N, ResultList) when N < 1 -> ResultList;
    getRandomN(List, N, ResultList) ->
        Elem = lists:nth(rand:uniform(length(List)), List),
        getRandomN(List, N - 1, ResultList ++ [Elem]).

% 6 Write a function that returns the first n elements of the Fibonacci sequence.
firstFibonacciElements(N) when N < 1 -> [];
firstFibonacciElements(N) when N =:= 1 -> [1];
firstFibonacciElements(N) when is_integer(N) ->  getFibonacciElements([1, 1], N - 2).

    getFibonacciElements(List, 0) -> List;
    getFibonacciElements(List, N) -> getFibonacciElements(List ++ [lists:sum(lists:nthtail(length(List) - 2, List))], N - 1).

% 7 Write a function that, given a dictionary, would translate a sentence. Words not found in the dictionary need not be translated.
% Dictionary = #{"mama"=>"mother","papa"=>"father"}. OriginalString = "mama is with papa".
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

% 8 Write a function that receives as input three digits and arranges them in an order that would create the smallest possible number. Numbers cannot start with a 0.
smallestNumber(A, B, C) when is_integer(A) and is_integer(B) and is_integer(C) ->
    SortedNumbers = [integer_to_list(I) || I <- lists:sort([A, B, C])],
    StringResult = case SortedNumbers of
            ["0", "0", "0"] -> "0";
            ["0", "0", _] -> [X, Y, Z] = SortedNumbers, string:join([Z, Y, X], "");
            ["0", _, _] ->  [X, Y, Z] = SortedNumbers, string:join([Y, X, Z], "");
            _ -> string:join(SortedNumbers, "")
        end,
    list_to_integer(StringResult).

% 9 Write a function that would rotate a list n places to the left.
rotateLeft(List, N) when is_list(List) and is_integer(N) ->
    RealN = N rem length(List),
    {List1, List2} = lists:split(RealN, List),
    List2 ++ List1.

% 10 Write a function that lists all tuples a, b, c such that a^2+b^2 = c^2 and a, b <= 20.
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

% Main tasks
% 1 Write a function that eliminates consecutive duplicates in a list.
removeConsecutiveDuplicates(List) when is_list(List) ->
    removeDuplicates(List).

    removeDuplicates([]) -> [];
    removeDuplicates([X]) -> [X];
    removeDuplicates([X, X | Rest]) -> removeDuplicates([X | Rest]);
    removeDuplicates([X, Y | Rest]) -> [X | removeDuplicates([Y | Rest])].

% 2 Write a function that, given an array of strings, will return the words that can be typed using only one row of the letters on an English keyboard layout.
lineWords(List) when is_list(List) ->
    Rows = ["qwertyuiop", "asdfghjkl", "zxcvbnm"],
    lists:filter(fun(Word) -> checkWordInRows(Word, Rows) end, List).
        
    checkWordInRows(Word, Rows) ->
        lists:any(fun(Row) -> checkWordInRow(Word, Row) end, Rows).

    checkWordInRow(Word, Row) -> 
        lists:all(fun(Char) -> lists:member(Char, Row) end, string:lowercase(Word)).

% 3 Create a pair of functions to encode and decode strings using the Caesar cipher.
encode(String, K) when is_integer(K) ->
    RealShift = K rem 26,
    LowerString = string:lowercase(String),
    lists:map(fun(Char) -> Char + RealShift end, LowerString).

decode(String, K) when is_integer(K) ->
    encode(String, -K).

% 4 Write a function that, given a string of digits from 2 to 9, would return all possible letter combinations that the number could represent (think phones with buttons).
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

% 5 Write a function that, given an array of strings, would group the anagrams together.
groupAnagrams(List) when is_list(List) ->
    StringsList = lists:map(fun(String) -> {lists:sort(String), String} end, List),
    lists:foldl(fun({Key, Value}, Map) ->
        case maps:is_key(Key, Map) of
            false -> maps:put(Key, [Value], Map);
            true -> maps:put(Key, lists:sort(maps:get(Key, Map) ++ [Value]), Map)
        end
    end, #{}, StringsList).

% Bonus tasks
% 1 Write a function to find the longest common prefix string amongst a list of strings.
commonPrefix(List) when is_list(List), length(List)=:=0 -> "";
commonPrefix(List) when is_list(List)->
    checkPrefix(lists:sublist(lists:nth(1, List), 1), List, 1, "").

    checkPrefix(Prefix, List, PrefixLength, Result) ->
        case lists:all(fun(Word) -> lists:sublist(Word, PrefixLength) =:= Prefix end, List) of 
            true -> checkPrefix(lists:sublist(lists:nth(1, List), PrefixLength+1), List, PrefixLength+1, Prefix);
            false -> Result
        end.

% 2 Write a function to convert arabic numbers to roman numerals.
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


% 3 Write a function to calculate the prime factorization of an integer.
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