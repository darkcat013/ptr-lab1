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