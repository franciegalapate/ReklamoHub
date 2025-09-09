% Handler for image uploads and serving

-module(image_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),

    case {Method, Path} of
        {<<"POST">>, <<"/upload_image">>} ->
            handle_upload(Req0, State);
        {<<"GET">>, _} ->
            handle_serve(Req0, State);
        _ ->
            Req = cowboy_req:reply(404, #{}, <<"Not found">>, Req0),
            {ok, Req, State}
    end.

% Handle image upload
handle_upload(Req0, State) ->
    io:format("Handling image upload... ~p~n", [self()]),
    
    case cowboy_req:read_part(Req0) of
        {ok, Headers, Req1} ->
            case maps:get(<<"content-disposition">>, Headers, undefined) of
                undefined ->
                    Req = cowboy_req:reply(400, #{}, <<"No content-disposition header">>, Req1),
                    {ok, Req, State};
                DispositionHeader ->
                    case parse_disposition_header(DispositionHeader) of
                        {ok, FieldName, Filename} ->
                            {ok, Body, Req2} = cowboy_req:read_part_body(Req1),
                            case Filename of
                                undefined ->
                                    % This is regular form data
                                    handle_form_data(FieldName, Body, Req2, State);
                                _ ->
                                    % This is a file upload
                                    handle_file_upload(FieldName, Filename, <<"application/octet-stream">>, Body, Req2, State)
                            end;
                        {error, Reason} ->
                            io:format("❌ ERROR parsing disposition header: ~p~n", [Reason]),
                            Req = cowboy_req:reply(400, #{}, <<"Invalid form data">>, Req1),
                            {ok, Req, State}
                    end
            end;
        {done, Req1} ->
            Req = cowboy_req:reply(400, #{}, <<"No form data received">>, Req1),
            {ok, Req, State}
    end.

% Handle file upload
handle_file_upload(_FieldName, Filename, _ContentType, Body, Req, State) ->
    io:format("Uploading file: ~p (~p bytes)~n", [Filename, byte_size(Body)]),
    
    % Generate unique filename
    Timestamp = integer_to_binary(erlang:system_time(second)),
    UniqueFilename = <<Timestamp/binary, "_", Filename/binary>>,
    
    % Save file to uploads directory
    UploadPath = filename:join(["priv", "uploads", UniqueFilename]),
    
    case file:write_file(UploadPath, Body) of
        ok ->
            % Return the filename for database storage
            Response = jsx:encode(#{
                <<"success">> => true,
                <<"filename">> => UniqueFilename,
                <<"original_name">> => Filename
            }),
            Req1 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Response, Req),
            {ok, Req1, State};
        {error, Reason} ->
            io:format("❌ ERROR saving file: ~p~n", [Reason]),
            Response = jsx:encode(#{
                <<"success">> => false,
                <<"error">> => <<"Failed to save file">>
            }),
            Req1 = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                Response, Req),
            {ok, Req1, State}
    end.

% Handle form data (non-file fields)
handle_form_data(FieldName, Body, Req, State) ->
    io:format("Form data field: ~p = ~p~n", [FieldName, Body]),
    % Continue reading parts
    case cowboy_req:read_part(Req) of
        {ok, Headers, Req1} ->
            case maps:get(<<"content-disposition">>, Headers, undefined) of
                undefined ->
                    Req = cowboy_req:reply(400, #{}, <<"No content-disposition header">>, Req1),
                    {ok, Req, State};
                DispositionHeader ->
                    case parse_disposition_header(DispositionHeader) of
                        {ok, NextFieldName, Filename} ->
                            {ok, NextBody, Req2} = cowboy_req:read_part_body(Req1),
                            case Filename of
                                undefined ->
                                    % This is regular form data
                                    handle_form_data(NextFieldName, NextBody, Req2, State);
                                _ ->
                                    % This is a file upload
                                    handle_file_upload(NextFieldName, Filename, <<"application/octet-stream">>, NextBody, Req2, State)
                            end;
                        {error, Reason} ->
                            io:format("❌ ERROR parsing disposition header: ~p~n", [Reason]),
                            Req = cowboy_req:reply(400, #{}, <<"Invalid form data">>, Req1),
                            {ok, Req, State}
                    end
            end;
        {done, Req1} ->
            % All parts processed, return success
            Response = jsx:encode(#{
                <<"success">> => true,
                <<"message">> => <<"Form data processed">>
            }),
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Response, Req1),
            {ok, Req2, State}
    end.

% Parse content-disposition header manually
parse_disposition_header(Header) ->
    % Header format: form-data; name="fieldname"; filename="filename.jpg"
    case binary:split(Header, <<";">>, [global]) of
        [<<"form-data">> | Rest] ->
            parse_disposition_params(Rest, undefined, undefined);
        _ ->
            {error, not_form_data}
    end.

parse_disposition_params([], FieldName, Filename) ->
    {ok, FieldName, Filename};
parse_disposition_params([Param | Rest], FieldName, Filename) ->
    TrimmedParam = binary:replace(Param, <<" ">>, <<>>, [global]),
    case binary:split(TrimmedParam, <<"=">>) of
        [<<"name">>, QuotedName] ->
            % Remove quotes
            Name = binary:replace(QuotedName, [<<"\"">>], <<>>, [global]),
            parse_disposition_params(Rest, Name, Filename);
        [<<"filename">>, QuotedFilename] ->
            % Remove quotes
            CleanFilename = binary:replace(QuotedFilename, [<<"\"">>], <<>>, [global]),
            parse_disposition_params(Rest, FieldName, CleanFilename);
        _ ->
            parse_disposition_params(Rest, FieldName, Filename)
    end.

% Handle serving uploaded images
handle_serve(Req0, State) ->
    Path = cowboy_req:path(Req0),
    
    % Extract filename from path like /uploads/filename.jpg
    case binary:split(Path, <<"/uploads/">>) of
        [_, Filename] ->
            serve_image(Filename, Req0, State);
        _ ->
            Req = cowboy_req:reply(404, #{}, <<"Invalid upload path">>, Req0),
            {ok, Req, State}
    end.

% Serve image file
serve_image(Filename, Req0, State) ->
    FilePath = filename:join(["priv", "uploads", Filename]),
    
    case file:read_file(FilePath) of
        {ok, Bin} ->
            MimeType = mime_type(Filename),
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => MimeType},
                Bin, Req0),
            {ok, Req, State};
        {error, _Reason} ->
            Req = cowboy_req:reply(404,
                #{<<"content-type">> => <<"text/plain">>},
                <<"Image not found">>, Req0),
            {ok, Req, State}
    end.

% Detect MIME type for images
mime_type(Filename) ->
    case filename:extension(Filename) of
        <<".jpg">> -> <<"image/jpeg">>;
        <<".jpeg">> -> <<"image/jpeg">>;
        <<".png">> -> <<"image/png">>;
        <<".gif">> -> <<"image/gif">>;
        <<".webp">> -> <<"image/webp">>;
        _ -> <<"application/octet-stream">>
    end.
