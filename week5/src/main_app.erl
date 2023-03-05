%%%-------------------------------------------------------------------
%% @doc main public API
%% @end
%%%-------------------------------------------------------------------

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