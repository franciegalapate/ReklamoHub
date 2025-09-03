% Maps URLs/paths to handlers

-module(reklamohub_router).
-export([init/2]).

init(Req0, State) ->
    Path = binary_to_list(cowboy_req:path(Req0)),
    File = resolve_file(Path),

    case file:read_file(File) of
        {ok, Bin} ->
            Mime = mime_type(File),
            Req = cowboy_req:reply(200,
                    #{<<"content-type">> => Mime}, Bin, Req0),
            {ok, Req, State};
        {error, _} ->
            Req = cowboy_req:reply(404,
                    #{<<"content-type">> => <<"text/plain">>},
                    <<"Not found">>, Req0),
            {ok, Req, State}
    end.

resolve_file("/") ->
    "priv/html/index.html";
resolve_file(Path) ->
    "priv" ++ Path.

mime_type(File) ->
    case filename:extension(File) of
        ".html" -> <<"text/html">>;
        ".css"  -> <<"text/css">>;
        ".js"   -> <<"application/javascript">>;
        ".png"  -> <<"image/png">>;
        ".jpg"  -> <<"image/jpeg">>;
        ".jpeg" -> <<"image/jpeg">>;
        ".gif"  -> <<"image/gif">>;
        _       -> <<"text/plain">>
    end.