% DB queries (insert, list, update)

-module(reklamohub_db).
-export([save_complaint/5, get_complaint_by_id/1, get_admin_password/1, get_all_complaints/0 ]).

% Insert complaint into DB
save_complaint(Resident, Category, Address, Details, Img) ->
    Sql = "INSERT INTO complaints (resident, category, address, details, img) VALUES (?, ?, ?, ?, ?)",
    db_manager:query(Sql, [Resident, Category, Address, Details, Img]).

% Fetch complaint by ID (fixed to match schema: complaint_id)
get_complaint_by_id(Id) ->
    Sql = "SELECT complaint_id, resident, category, address, details, img, status, date \
           FROM complaints WHERE complaint_id = ?",
    db_manager:query(Sql, [Id]).

%% --- Admin helpers ---

% Get the stored password for an admin username (plaintext per seed data)
get_admin_password(Username) ->
    Sql = "SELECT admin_password FROM admin WHERE admin_username = ? LIMIT 1",
    db_manager:query(Sql, [Username]).

% Fetch all complaints (for admin dashboard)
get_all_complaints() ->
    Sql = "SELECT complaint_id, resident, category, status, date, img, address, details FROM complaints ORDER BY date DESC",
    db_manager:query(Sql).