open Ahrefs_types.Types
open Ahrefs_json.Types_j

let () = Random.self_init ()

let data_dir =
  let home = Sys.getenv_opt "HOME" |> Option.value ~default:"/tmp" in
  Filename.concat home ".ahrefs-recruit"

let ensure_dir () =
  if not (Sys.file_exists data_dir) then
    Unix.mkdir data_dir 0o755

let ensure_tenants_dir () =
  ensure_dir ();
  let d = Filename.concat data_dir "tenants" in
  if not (Sys.file_exists d) then Unix.mkdir d 0o755

let tenant_dir company_id =
  ensure_tenants_dir ();
  let d = Filename.concat data_dir (Filename.concat "tenants" company_id) in
  (try Unix.mkdir d 0o700 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  d

(* ── Per-tenant file paths ── *)

let pool_file cid = Filename.concat (tenant_dir cid) "pool.json"
let sourcing_sessions_file cid = Filename.concat (tenant_dir cid) "sourcing_sessions.json"
let integration_settings_file cid = Filename.concat (tenant_dir cid) "integration_settings.json"
let company_profile_file cid = Filename.concat (tenant_dir cid) "company_profile.json"
let greenhouse_sync_state_file cid = Filename.concat (tenant_dir cid) "greenhouse_sync_state.json"

(* ── Global auth file paths ── *)

let users_file    = Filename.concat data_dir "users.json"
let companies_file = Filename.concat data_dir "companies.json"
let sessions_file  = Filename.concat data_dir "sessions.json"

(* ── Utilities ── *)

let generate_id () =
  let t = int_of_float (Unix.gettimeofday () *. 1000.0) in
  Printf.sprintf "c-%d-%06d" t (Random.int 999999)

let now_iso () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let save_json_file path json =
  let tmp = path ^ ".tmp" in
  Yojson.Safe.to_file tmp json;
  Sys.rename tmp path

(* ── Candidate Pool ── *)

let load_pool ~company_id () : candidate_record list =
  let f = pool_file company_id in
  if not (Sys.file_exists f) then []
  else
    try
      let json = Yojson.Safe.from_file f in
      match json with
      | `List items ->
        List.filter_map (fun item ->
          match candidate_record_of_yojson item with
          | Ok c -> Some c
          | Error _ -> None
        ) items
      | _ -> []
    with _ -> []

let save_pool ~company_id (candidates : candidate_record list) : unit =
  let json = `List (List.map candidate_record_to_yojson candidates) in
  save_json_file (pool_file company_id) json

let get_all ~company_id () = load_pool ~company_id ()

let get_by_id ~company_id id =
  List.find_opt (fun (c : candidate_record) -> c.id = id) (load_pool ~company_id ())

let upsert ~company_id (c : candidate_record) : unit =
  let pool = load_pool ~company_id () in
  let exists = List.exists (fun (x : candidate_record) -> x.id = c.id) pool in
  let updated =
    if exists then List.map (fun (x : candidate_record) -> if x.id = c.id then c else x) pool
    else pool @ [c]
  in
  save_pool ~company_id updated

let update_stage ~company_id id (stage : ats_stage) =
  match get_by_id ~company_id id with
  | None -> Error "not found"
  | Some c ->
    let updated = { c with ats_stage = stage; updated_at = now_iso () } in
    upsert ~company_id updated;
    Ok updated

let best_score_for scores =
  List.fold_left (fun (best : candidate_score option) (s : candidate_score) ->
    match best with
    | None -> Some s
    | Some b -> if s.overall_score > b.overall_score then Some s else best
  ) None scores

let summary_of_record (c : candidate_record) : candidate_summary =
  let (top_role_id, top_role_name, top_score, top_rec) =
    match best_score_for c.scores with
    | None -> ("", "", 0.0, Reject)
    | Some s -> (s.role_id, s.role_name, s.overall_score, s.recommendation)
  in
  let distinct_roles =
    List.sort_uniq String.compare
      (List.map (fun (s : candidate_score) -> s.role_id) c.scores)
  in
  {
    id = c.id; name = c.name; ats_stage = c.ats_stage;
    top_role_id; top_role_name; top_score;
    top_recommendation = top_rec;
    score_count = List.length c.scores;
    role_count = List.length distinct_roles;
    created_at = c.created_at;
  }

let contains_sub s sub =
  let n = String.length s and m = String.length sub in
  if m = 0 then true
  else
    let found = ref false in
    for i = 0 to n - m do
      if String.sub s i m = sub then found := true
    done;
    !found

let get_summaries ~company_id ?(role_id = "") ?(stage = None) ?(q = "") () =
  let pool = load_pool ~company_id () in
  let filtered = List.filter (fun (c : candidate_record) ->
    (String.length role_id = 0 ||
     List.exists (fun (s : candidate_score) -> s.role_id = role_id) c.scores)
    && (match stage with None -> true | Some st -> c.ats_stage = st)
    && (String.length q = 0 ||
        contains_sub (String.lowercase_ascii c.name) (String.lowercase_ascii q))
  ) pool in
  List.sort (fun (a : candidate_record) (b : candidate_record) ->
    String.compare b.created_at a.created_at) filtered
  |> List.map summary_of_record

let get_pool_stats ~company_id () : pool_stats =
  let pool = load_pool ~company_id () in
  let hired = List.filter (fun (c : candidate_record) -> c.ats_stage = Hired) pool in
  let active = List.filter (fun (c : candidate_record) ->
    match c.ats_stage with Hired | Rejected | Withdrawn -> false | _ -> true
  ) pool in
  let roles =
    List.concat_map (fun (c : candidate_record) ->
      List.map (fun (s : candidate_score) -> s.role_id) c.scores
    ) pool
    |> List.sort_uniq String.compare
  in
  {
    total_candidates = List.length pool;
    roles_covered = List.length roles;
    hired_count = List.length hired;
    active_count = List.length active;
  }

(* ── AI Sourcing Sessions ── *)

let load_sourcing_sessions ~company_id () : ai_sourcing_result list =
  let f = sourcing_sessions_file company_id in
  if not (Sys.file_exists f) then []
  else
    try
      let json = Yojson.Safe.from_file f in
      match json with
      | `List items ->
        List.filter_map (fun item ->
          match ai_sourcing_result_of_yojson item with
          | Ok s -> Some s
          | Error _ -> None
        ) items
      | _ -> []
    with _ -> []

let save_sourcing_sessions ~company_id (sessions : ai_sourcing_result list) : unit =
  let json = `List (List.map ai_sourcing_result_to_yojson sessions) in
  save_json_file (sourcing_sessions_file company_id) json

let append_sourcing_session ~company_id (s : ai_sourcing_result) : unit =
  let existing = load_sourcing_sessions ~company_id () in
  let updated = s :: existing in
  let trimmed = if List.length updated > 50 then
    let rec take_n n lst = match lst, n with
      | _, 0 | [], _ -> []
      | x :: rest, k -> x :: take_n (k - 1) rest
    in
    take_n 50 updated
  else updated in
  save_sourcing_sessions ~company_id trimmed

let get_sourcing_sessions ~company_id () = load_sourcing_sessions ~company_id ()

(* ── Integration Settings ── *)

let empty_integration_settings =
  { greenhouse_subdomain = ""; greenhouse_api_key = ""; ai_provider = "anthropic"; ai_api_key = "" }

let load_integration_settings ~company_id () : integration_settings =
  let f = integration_settings_file company_id in
  if not (Sys.file_exists f) then empty_integration_settings
  else
    try
      let json = Yojson.Safe.from_file f in
      match integration_settings_of_yojson json with
      | Ok s -> s
      | Error _ -> empty_integration_settings
    with _ -> empty_integration_settings

let save_integration_settings ~company_id (s : integration_settings) : unit =
  let json = integration_settings_to_yojson s in
  save_json_file (integration_settings_file company_id) json

(* ── Company Profile ── *)

let load_company_profile ~company_id () : company_profile option =
  let f = company_profile_file company_id in
  if not (Sys.file_exists f) then None
  else
    try
      let json = Yojson.Safe.from_file f in
      match company_profile_of_yojson json with
      | Ok p -> Some p
      | Error _ -> None
    with _ -> None

let save_company_profile ~company_id (p : company_profile) : unit =
  let json = company_profile_to_yojson p in
  save_json_file (company_profile_file company_id) json

(* ── Greenhouse Sync State ── *)

let default_sync_state : greenhouse_sync_state =
  { last_synced_at = None; total_synced = 0; last_error = None }

let load_greenhouse_sync_state ~company_id () : greenhouse_sync_state =
  let f = greenhouse_sync_state_file company_id in
  if not (Sys.file_exists f) then default_sync_state
  else
    try
      let json = Yojson.Safe.from_file f in
      match greenhouse_sync_state_of_yojson json with
      | Ok s -> s
      | Error _ -> default_sync_state
    with _ -> default_sync_state

let save_greenhouse_sync_state ~company_id (s : greenhouse_sync_state) : unit =
  let json = greenhouse_sync_state_to_yojson s in
  save_json_file (greenhouse_sync_state_file company_id) json

let get_by_greenhouse_id ~company_id gh_app_id =
  List.find_opt (fun (c : candidate_record) ->
    c.greenhouse_application_id = Some gh_app_id
  ) (load_pool ~company_id ())

let get_by_skill ~company_id (criterion_text : string) : skill_match list =
  let pool = load_pool ~company_id () in
  let q = String.lowercase_ascii criterion_text in
  List.concat_map (fun (c : candidate_record) ->
    List.filter_map (fun (cs : candidate_score) ->
      let met = List.exists (fun (cr : criterion_result) ->
        cr.met &&
        contains_sub (String.lowercase_ascii cr.criterion.text) q
      ) cs.criterion_results in
      if met then
        Some {
          candidate_id = c.id; candidate_name = c.name;
          ats_stage = c.ats_stage; role_id = cs.role_id;
          role_name = cs.role_name; overall_score = cs.overall_score;
          scored_at = cs.scored_at;
        }
      else None
    ) c.scores
  ) pool
  |> List.sort (fun a b -> Float.compare b.overall_score a.overall_score)

(* ── Auth: Users ── *)

let load_users () : user_record list =
  ensure_dir ();
  if not (Sys.file_exists users_file) then []
  else
    try
      let json = Yojson.Safe.from_file users_file in
      match json with
      | `List items ->
        List.filter_map (fun item ->
          match user_record_of_yojson item with
          | Ok u -> Some u | Error _ -> None
        ) items
      | _ -> []
    with _ -> []

let save_users (users : user_record list) : unit =
  ensure_dir ();
  save_json_file users_file (`List (List.map user_record_to_yojson users))

let find_user_by_email email =
  List.find_opt (fun (u : user_record) ->
    String.equal (String.lowercase_ascii u.email) (String.lowercase_ascii email)
  ) (load_users ())

let upsert_user (u : user_record) : unit =
  let users = load_users () in
  let exists = List.exists (fun (x : user_record) -> x.id = u.id) users in
  let updated =
    if exists then List.map (fun (x : user_record) -> if x.id = u.id then u else x) users
    else users @ [u]
  in
  save_users updated

(* ── Auth: Companies ── *)

let load_companies () : company_record list =
  ensure_dir ();
  if not (Sys.file_exists companies_file) then []
  else
    try
      let json = Yojson.Safe.from_file companies_file in
      match json with
      | `List items ->
        List.filter_map (fun item ->
          match company_record_of_yojson item with
          | Ok c -> Some c | Error _ -> None
        ) items
      | _ -> []
    with _ -> []

let save_companies (companies : company_record list) : unit =
  ensure_dir ();
  save_json_file companies_file (`List (List.map company_record_to_yojson companies))

let find_company_by_domain domain =
  List.find_opt (fun (c : company_record) ->
    String.equal (String.lowercase_ascii c.domain) (String.lowercase_ascii domain)
  ) (load_companies ())

let find_company_by_id id =
  List.find_opt (fun (c : company_record) -> c.id = id) (load_companies ())

let upsert_company (c : company_record) : unit =
  let companies = load_companies () in
  let exists = List.exists (fun (x : company_record) -> x.id = c.id) companies in
  let updated =
    if exists then List.map (fun (x : company_record) -> if x.id = c.id then c else x) companies
    else companies @ [c]
  in
  save_companies updated

(* ── Auth: Sessions ── *)

let load_auth_sessions () : session_record list =
  ensure_dir ();
  if not (Sys.file_exists sessions_file) then []
  else
    try
      let json = Yojson.Safe.from_file sessions_file in
      match json with
      | `List items ->
        List.filter_map (fun item ->
          match session_record_of_yojson item with
          | Ok s -> Some s | Error _ -> None
        ) items
      | _ -> []
    with _ -> []

let save_auth_sessions (sessions : session_record list) : unit =
  ensure_dir ();
  save_json_file sessions_file (`List (List.map session_record_to_yojson sessions))

let find_session token =
  List.find_opt (fun (s : session_record) -> s.token = token) (load_auth_sessions ())

let upsert_session (s : session_record) : unit =
  let sessions = load_auth_sessions () in
  let exists = List.exists (fun (x : session_record) -> x.token = s.token) sessions in
  let updated =
    if exists then List.map (fun (x : session_record) -> if x.token = s.token then s else x) sessions
    else sessions @ [s]
  in
  save_auth_sessions updated

let delete_session token =
  let sessions = load_auth_sessions () in
  save_auth_sessions (List.filter (fun (s : session_record) -> s.token <> token) sessions)
