-module(reklamohub_router).
-export([script/3]).

script(_SessionID, Env, _Input) ->
    %% Get request path (default to "/")
    Path = proplists:get_value(request_uri, Env, "/"),

    %% Normalize: if "/" then serve index.html
    RealPath =
        case Path of
            "/" -> "frontend/html/index.html";
            _   -> "frontend" ++ Path
        end,

    Response = serve_file(RealPath),

    {Status, Headers, Body} = Response,
    {proceed, [{response, {Status, Headers, Body}}]}.

%% File loader
serve_file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            {200, [{"content-type", content_type(Path)}], Bin};
        {error, enoent} ->
            {404, [{"content-type", "text/plain"}], "Page not found"};
        {error, Reason} ->
            {500, [{"content-type", "text/plain"}],
             io_lib:format("Error loading ~s: ~p", [Path, Reason])}
    end.

%% Guess MIME type
content_type(Path) ->
    case filename:extension(Path) of
        ".html" -> "text/html";
        ".css"  -> "text/css";
        ".js"   -> "application/javascript";
        ".png"  -> "image/png";
        ".jpg"  -> "image/jpeg";
        ".jpeg" -> "image/jpeg";
        ".gif"  -> "image/gif";
        ".ico"  -> "image/x-icon";
        _       -> "text/plain"
    end.