-module(minimal).
-export([visit_quotes/0, extract_quotes/0, quotes_to_json/0]).

% 1 Write an application that would visit https://quotes.toscrape.com/. Print out the HTTP response
% status code, response headers and response body.
% minimal:visit_quotes().
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

% Continue your previous application. Extract all quotes from the HTTP
% response body. Collect the author of the quote, the quote text and tags. Save the data
% into a list of maps, each map representing a single quote.
% minimal:extract_quotes().

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
% Continue your previous application. Persist the list of quotes into a file.
% Encode the data into JSON format. Name the file quotes.json.
% minimal:quotes_to_json().

quotes_to_json() ->
    file:write_file("quotes.json", jsx:prettify(jsx:encode(extract_quotes()))).