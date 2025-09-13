% The business logic for handling complaints (submitting, tracking, retrieving status).

-module(complaint_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path   = cowboy_req:path(Req0),

    case {Method, Path} of
        {<<"POST">>, <<"/submit_complaint">>} ->
            handle_submit(Req0, State);

        {<<"GET">>, <<"/track_complaint">>} ->
            handle_track(Req0, State);

        _ ->
            Req = cowboy_req:reply(404, #{}, <<"Not found">>, Req0),
            {ok, Req, State}
    end.

% Submit a new complaint
handle_submit(Req0, State) ->
    io:format("Handling submitted complaint... ~p~n", [self()]),
    
    % Check content type to determine how to handle the request
    ContentType = cowboy_req:header(<<"content-type">>, Req0),
    io:format("Content-Type: ~p~n", [ContentType]),
    
    case binary:split(ContentType, <<"multipart/form-data">>) of
        [_, _] ->
            % This is multipart form data - handle file upload
            io:format("Detected multipart form data~n"),
            handle_multipart_submit(Req0, State);
        _ ->
            % This is JSON data - handle normally
            io:format("Detected JSON data~n"),
            handle_json_submit(Req0, State)
    end.

% Handle multipart form submission with file upload
handle_multipart_submit(Req0, State) ->
    io:format("Starting multipart parsing...~n"),
    case parse_multipart_data_with_file(Req0, #{}, undefined) of
        {ok, FormData, TempFile, Req1} ->
            io:format("Multipart parsing successful. FormData: ~p~n", [FormData]),
            % Extract fields
            Resident = maps:get(<<"resident">>, FormData, null),
            Category = maps:get(<<"category">>, FormData),
            Address  = maps:get(<<"address">>, FormData),
            Details  = maps:get(<<"details">>, FormData),
            
            io:format("Extracted fields - Resident: ~p, Category: ~p, Address: ~p, Details: ~p~n", 
                     [Resident, Category, Address, Details]),
            
            process_complaint_with_file(Resident, Category, Address, Details, TempFile, Req1, State);
        {error, Reason, Req1} ->
            io:format("❌ ERROR parsing multipart data: ~p~n", [Reason]),
            Req = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                <<"{\"error\":\"Invalid form data\"}">>, Req1),
            {ok, Req, State}
    end.

% Handle JSON submission
handle_json_submit(Req0, State) ->
    try
        {ok, Body, Req1} = cowboy_req:read_body(Req0),
        Map = jsx:decode(Body, [return_maps]),
        
        % Extract fields from JSON
        Resident = maps:get(<<"resident">>, Map, null),
        Category = maps:get(<<"category">>, Map),
        Address  = maps:get(<<"address">>, Map),
        Details  = maps:get(<<"details">>, Map),
        Img      = maps:get(<<"img">>, Map, null),
        
        process_complaint_data(Resident, Category, Address, Details, Img, Req1, State)
    catch
        _:_ ->
            io:format("❌ ERROR parsing JSON data~n"),
            Req = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                <<"{\"error\":\"Invalid request data\"}">>, Req0),
            {ok, Req, State}
    end.

% Parse multipart data and save file temporarily
parse_multipart_data_with_file(Req0, Acc, TempFile) ->
    case cowboy_req:read_part(Req0) of
        {ok, Headers, Req1} ->
            case maps:get(<<"content-disposition">>, Headers, undefined) of
                undefined ->
                    {error, no_content_disposition, Req1};
                DispositionHeader ->
                    case extract_field_info(DispositionHeader) of
                        {ok, FieldName, Filename} ->
                            {ok, Body, Req2} = cowboy_req:read_part_body(Req1),
                            case Filename of
                                undefined ->
                                    % Regular form field
                                    NewAcc = Acc#{FieldName => Body},
                                    parse_multipart_data_with_file(Req2, NewAcc, TempFile);
                                _ ->
                                    % File upload - save temporarily
                                    case save_temp_file(Filename, Body) of
                                        {ok, TempFilename} ->
                                            NewAcc = Acc#{FieldName => TempFilename},
                                            parse_multipart_data_with_file(Req2, NewAcc, TempFilename);
                                        {error, Reason} ->
                                            {error, {file_save_failed, Reason}, Req2}
                                    end
                            end;
                        {error, Reason} ->
                            {error, {parse_error, Reason}, Req1}
                    end
            end;
        {done, Req1} ->
            {ok, Acc, TempFile, Req1}
    end.


% Extract field information from content-disposition header
extract_field_info(Header) ->
    HeaderStr = binary_to_list(Header),
    
    % Extract field name
    case re:run(HeaderStr, "name=\"([^\"]+)\"", [{capture, all_but_first, list}]) of
        {match, [FieldName]} ->
            % Check if there's a filename
            case re:run(HeaderStr, "filename=\"([^\"]+)\"", [{capture, all_but_first, list}]) of
                {match, [Filename]} ->
                    {ok, list_to_binary(FieldName), list_to_binary(Filename)};
                nomatch ->
                    {ok, list_to_binary(FieldName), undefined}
            end;
        nomatch ->
            {error, no_field_name}
    end.


% Save file temporarily (will be renamed later with complaint ID)
save_temp_file(Filename, Body) ->
    % Generate temporary filename
    Timestamp = integer_to_binary(erlang:system_time(second)),
    TempFilename = <<"temp_", Timestamp/binary, "_", Filename/binary>>,
    
    % Save file to uploads directory
    UploadPath = filename:join(["priv", "uploads", TempFilename]),
    
    case file:write_file(UploadPath, Body) of
        ok ->
            io:format("✅ Temp file saved: ~p~n", [TempFilename]),
            {ok, TempFilename};
        {error, Reason} ->
            io:format("❌ ERROR saving temp file: ~p~n", [Reason]),
            {error, Reason}
    end.

% Rename file with complaint ID
rename_file_with_complaint_id(TempFilename, ComplaintID) ->
    % Get file extension from original filename
    Extension = filename:extension(TempFilename),
    
    % Create new filename with complaint ID
    NewFilename = <<"CMP-", (integer_to_binary(ComplaintID))/binary, Extension/binary>>,
    
    % Full paths
    TempPath = filename:join(["priv", "uploads", TempFilename]),
    NewPath = filename:join(["priv", "uploads", NewFilename]),
    
    case file:rename(TempPath, NewPath) of
        ok ->
            io:format("✅ File renamed: ~p -> ~p~n", [TempFilename, NewFilename]),
            {ok, NewFilename};
        {error, Reason} ->
            io:format("❌ ERROR renaming file: ~p~n", [Reason]),
            {error, Reason}
    end.

% Process complaint with file upload
process_complaint_with_file(Resident, Category, Address, Details, TempFile, Req1, State) ->
    % Crash trigger for demo
    case Details of
        <<"no water">> ->
            io:format("Submitted complaint has crashed ~p~n", [self()]),
            erlang:error(simulated_crash);
        _ ->
            ok
    end,

    % Save complaint to DB first (without image filename)
    case reklamohub_db:save_complaint(Resident, Category, Address, Details, null) of
        {ok, #{last_insert_id := ID}} ->
            % Rename the temporary file with complaint ID
            case TempFile of
                undefined ->
                    % No file uploaded
                    FinalImg = null;
                _ ->
                    case rename_file_with_complaint_id(TempFile, ID) of
                        {ok, NewFilename} ->
                            FinalImg = NewFilename;
                        {error, _Reason} ->
                            % If rename fails, keep the temp filename
                            FinalImg = TempFile
                    end
            end,
            
            % Update the complaint with the final image filename
            case FinalImg of
                null ->
                    % No image, proceed with current data
                    ok;
                _ ->
                    % Update the complaint record with the final image filename
                    case reklamohub_db:update_complaint_image(ID, FinalImg) of
                        ok ->
                            io:format("✅ Updated complaint ~p with image: ~p~n", [ID, FinalImg]);
                        {error, Reason} ->
                            io:format("❌ ERROR updating complaint image: ~p~n", [Reason])
                    end
            end,
            
            %% Fetch full complaint row
            case reklamohub_db:get_complaint_by_id(ID) of
                {ok, #{columns := Cols, rows := [Row]}} ->
                    Complaint = normalize_complaint(maps:from_list(lists:zip(Cols, Row))),
                    dashboard_manager:new_complaint(Complaint),
                    reply_with_complaint(Cols, Row, Req1, State);

                {ok, Cols, [Row]} ->
                    Complaint = normalize_complaint(maps:from_list(lists:zip(Cols, Row))),
                    dashboard_manager:new_complaint(Complaint),
                    reply_with_complaint(Cols, Row, Req1, State);

                FetchError ->
                    io:format("❌ ERROR fetching complaint after insert: ~p~n", [FetchError]),
                    Req = cowboy_req:reply(500,
                        #{<<"content-type">> => <<"application/json">>},
                        <<"{\"error\":\"Complaint saved, but could not retrieve details\"}">>, Req1),
                    {ok, Req, State}
            end;

        SaveError ->
            io:format("❌ ERROR in save_complaint: ~p~n", [SaveError]),
            % Clean up temporary file if it exists
            case TempFile of
                undefined -> ok;
                _ ->
                    TempPath = filename:join(["priv", "uploads", TempFile]),
                    file:delete(TempPath)
            end,
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                <<"{\"error\":\"Could not save complaint\"}">>, Req1),
            {ok, Req, State}
    end.


% Process complaint data (shared between multipart and JSON)
process_complaint_data(Resident, Category, Address, Details, Img, Req1, State) ->

    % Crash trigger for demo
    case Details of
        <<"CRASH">> ->
            io:format("Submitted complaint has crashed ~p~n", [self()]),
            erlang:error(simulated_crash);
        _ ->
            ok
    end,

    % Save complaint to DB first to get the complaint ID
    case reklamohub_db:save_complaint(Resident, Category, Address, Details, Img) of
        {ok, #{last_insert_id := ID}} ->
            %% Fetch full complaint row
            case reklamohub_db:get_complaint_by_id(ID) of
                {ok, #{columns := Cols, rows := [Row]}} ->
                    Complaint = normalize_complaint(maps:from_list(lists:zip(Cols, Row))),
                    dashboard_manager:new_complaint(Complaint),
                    reply_with_complaint(Cols, Row, Req1, State);

                {ok, Cols, [Row]} ->
                    Complaint = normalize_complaint(maps:from_list(lists:zip(Cols, Row))),
                    dashboard_manager:new_complaint(Complaint),
                    reply_with_complaint(Cols, Row, Req1, State);

                FetchError ->
                    io:format("❌ ERROR fetching complaint after insert: ~p~n", [FetchError]),
                    Req = cowboy_req:reply(500,
                        #{<<"content-type">> => <<"application/json">>},
                        <<"{\"error\":\"Complaint saved, but could not retrieve details\"}">>, Req1),
                    {ok, Req, State}
            end;

        SaveError ->
            io:format("❌ ERROR in save_complaint: ~p~n", [SaveError]),
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                <<"{\"error\":\"Could not save complaint\"}">>, Req1),
            {ok, Req, State}
    end.

% Track complaint by ID
handle_track(Req0, State) ->
    io:format("Fetching complaint... ~p~n", [self()]),
    Qs = cowboy_req:parse_qs(Req0),
    ComplaintIDStr = proplists:get_value(<<"id">>, Qs),
    Req1 = Req0,

    % Normalize input: allows "CMP-0026" or "26"
    ComplaintID =
    case string:to_upper(binary_to_list(ComplaintIDStr)) of
        "CMP-" ++ Digits ->
            list_to_integer(Digits);
        Digits ->
            list_to_integer(Digits)
    end,

    case reklamohub_db:get_complaint_by_id(ComplaintID) of
        {ok, #{rows := []}} ->
            Req = cowboy_req:reply(404,
                #{<<"content-type">> => <<"application/json">>},
                <<"{\"error\":\"Complaint not found\"}">>, Req1),
            {ok, Req, State};

        {ok, #{columns := Cols, rows := [Row]}} ->
            reply_with_complaint(Cols, Row, Req1, State);
        {ok, Cols, [Row]} ->
            reply_with_complaint(Cols, Row, Req1, State);

        Error ->
            io:format("❌ ERROR in handle_track: ~p~n", [Error]),
            Req = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                <<"{\"error\":\"Database error\"}">>, Req1),
            {ok, Req, State}
    end.

% Helper to standardize the complaint response
reply_with_complaint(Cols, Row, Req, State) ->
    Obj0 = maps:from_list(lists:zip(Cols, Row)),
    Obj  = normalize_complaint(Obj0),
    Json = jsx:encode(Obj),
    Resp = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Json, Req),
    {ok, Resp, State}.

normalize_complaint(Obj0) ->
    #{
        complaint_id => maps:get(<<"complaint_id">>, Obj0),
        resident     => maps:get(<<"resident">>, Obj0),
        category     => maps:get(<<"category">>, Obj0),
        status       => maps:get(<<"status">>, Obj0),
        date         => maps:get(<<"filed_date">>, Obj0),
        img          => maps:get(<<"img">>, Obj0),
        address      => maps:get(<<"address">>, Obj0),
        details      => maps:get(<<"details">>, Obj0)
    }.