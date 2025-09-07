% DB queries (insert, list, update)

-module(reklamohub_db).
-export([save_complaint/5, get_complaint_by_id/1]).

%% Insert complaint into DB
save_complaint(Resident, Category, Address, Details, Img) ->
    Sql = "INSERT INTO complaints (resident, category, address, details, img) VALUES (?, ?, ?, ?, ?)",
    db_manager:query(Sql, [Resident, Category, Address, Details, Img]).

%% Fetch complaint by ID
get_complaint_by_id(Id) ->
    Sql = "SELECT id, resident, category, address, details, img, status FROM complaints WHERE id = ?",
    db_manager:query(Sql, [Id]).