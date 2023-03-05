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