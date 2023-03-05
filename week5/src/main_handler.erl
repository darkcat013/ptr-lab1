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