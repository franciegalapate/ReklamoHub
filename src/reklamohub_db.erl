% DB queries (insert, list, update)

-module(reklamohub_db).
-export([save_complaint/5, get_complaint_by_id/1, get_admin_password/1, get_all_complaints/0 ]).

% Insert complaint into DB
save_complaint(Resident, Category, Address, Details, Img) ->
    Sql = "INSERT INTO complaints (resident, category, address, details, img) VALUES (?,?,?,?,?)",
    case db_manager:query(Sql, [Resident, Category, Address, Details, Img]) of
        ok ->
            %% Handle driver that only returns 'ok'
            case db_manager:query("SELECT LAST_INSERT_ID();") of
                {ok, [_Col], [[ID]]} ->
                    {ok, #{last_insert_id => ID}};
                Other ->
                    io:format("❌ ERROR fetching LAST_INSERT_ID(): ~p~n", [Other]),
                    {error, no_insert_id}
            end;
        {ok, Result} ->
            %% Handle driver that returns insert_id directly
            ID = case maps:get(insert_id, Result, undefined) of
                undefined -> maps:get(last_insert_id, Result, 0);
                Val -> Val
            end,
            {ok, #{last_insert_id => ID}};
        Error ->
            io:format("❌ ERROR in db_manager:query: ~p~n", [Error]),
            Error
    end.

% Fetch complaint by ID (fixed to match schema: complaint_id)
get_complaint_by_id(ID) ->
    Sql = "SELECT concat('CMP-', lpad(c.complaint_id,4,'0')) AS complaint_id, \
                  c.resident, \
                  c.category, \
                  c.status, \
                  DATE_FORMAT(c.date, '%Y-%m-%d %H:%i:%s') AS filed_date, \
                  c.img, \
                  c.address, \
                  c.details \
           FROM complaints c \
           WHERE c.complaint_id = ?",
    db_manager:query(Sql, [ID]).

%% --- Admin helpers ---

% Get the stored password for an admin username (plaintext per seed data)
get_admin_password(Username) ->
    Sql = "SELECT admin_password FROM admin WHERE admin_username = ? LIMIT 1",
    db_manager:query(Sql, [Username]).

% Fetch all complaints (for admin dashboard)
get_all_complaints() ->
    Sql = "SELECT complaint_id, resident, category, status, date, img, address, details FROM complaints ORDER BY date DESC",
    db_manager:query(Sql).