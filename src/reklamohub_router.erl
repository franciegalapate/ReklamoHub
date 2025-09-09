% Maps URLs/paths to handlers

-module(reklamohub_router).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Path = cowboy_req:path(Req0),

    case Path of
        %% API endpoints
        <<"/admin_login">> ->
            admin_handler:init(Req0, State);

        <<"/update_status">> ->
            admin_handler:init(Req0, State);

        <<"/submit_complaint">> ->
            complaint_handler:init(Req0, State);

        <<"/track_complaint">> ->
            complaint_handler:init(Req0, State);

        <<"/upload_image">> ->
            image_handler:init(Req0, State);

        <<"/uploads/", _/binary>> ->
            serve_static(Req0, State);

        <<"/admin_dashboard">> ->
            {cowboy_websocket, admin_ws_handler, State};

        %% Otherwise → serve static files
        _ ->
            serve_static(Req0, State)
    end.


serve_static(Req0, State) ->
    Path = binary_to_list(cowboy_req:path(Req0)),
    File = resolve_file(Path),
    case file:read_file(File) of
        {ok, Bin} ->
            Mime = mime_type(File),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => Mime}, Bin, Req0),
            {ok, Req, State};
        {error, _Reason} ->
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

        %% Uploads
        [$/,$u,$p,$l,$o,$a,$d,$s,$/ | _] ->
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