% Maps URLs/paths to handlers

-module(reklamohub_router).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Path = binary_to_list(cowboy_req:path(Req0)),
    io:format("👉 Requested path: ~s~n", [Path]),

    File = resolve_file(Path),
    io:format("📂 Resolved to file: ~s~n", [File]),

    case file:read_file(File) of
        {ok, Bin} ->
            Mime = mime_type(File),
            io:format("✅ Served file: ~s (MIME: ~s)~n", [File, binary_to_list(Mime)]),
            Req = cowboy_req:reply(200,
                    #{<<"content-type">> => Mime}, Bin, Req0),
            {ok, Req, State};

        {error, Reason} ->
            io:format("❌ Could not read file: ~s (Reason: ~p)~n", [File, Reason]),
            Req = cowboy_req:reply(404,
                    #{<<"content-type">> => <<"text/plain">>},
                    <<"Not found">>, Req0),
            {ok, Req, State}
    end.

resolve_file("/") ->
    "priv/html/index.html";

resolve_file(Path) ->
    case Path of
        %% CSS
        [$/,$c,$s,$/ | _] ->
            "priv" ++ Path;

        %% JavaScript
        [$/,$j,$a,$v,$a,$s,$c,$r,$i,$p,$t,$/ | _] ->
            "priv" ++ Path;

        %% Assets (images, logos, etc.)
        [$/,$a,$s,$s,$e,$t,$s,$/ | _] ->
            "priv" ++ Path;

        %% Default: decide by extension
        _ ->
            case filename:extension(Path) of
                ".html" -> "priv/html" ++ Path;
                _       -> "priv" ++ Path
            end
    end.

%% Detect MIME types
mime_type(File) ->
    case filename:extension(File) of
        ".html" -> <<"text/html">>;
        ".css"  -> <<"text/css">>;
        ".js"   -> <<"application/javascript">>;
        ".png"  -> <<"image/png">>;
        ".jpg"  -> <<"image/jpeg">>;
        ".jpeg" -> <<"image/jpeg">>;
        ".gif"  -> <<"image/gif">>;
        ".svg"  -> <<"image/svg+xml">>;
        ".ico"  -> <<"image/x-icon">>;
        _       -> <<"text/plain">>
    end.