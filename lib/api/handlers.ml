open Roladeck_types.Types
open Roladeck_json.Types_j
open Roladeck_skills_data.Skills_registry
open Roladeck_scoring.Scoring
open Roladeck_sourcing.Sourcing
module Store = Roladeck_storage.Storage
module Auth = Roladeck_auth.Auth
module AiSourcing = Roladeck_ai.Ai_sourcing
module CompanyResearch = Roladeck_ai.Company_research
module Classify = Roladeck_ai.Classify
module SkillParser = Roladeck_skill_parser.Skill_parser
module Verify = Roladeck_ai.Verify
module GhSync = Roladeck_sync.Greenhouse_sync

let json_response ?(status = `OK) body =
  Lwt.return (Dream.response ~status ~headers:[("Content-Type", "application/json")] body)

let skill_to_summary (s : skill_record) : skill_summary =
  {
    id = s.id;
    name = s.discipline.name;
    category = s.discipline.category;
    description = s.discipline.description;
    criteria_count = List.length s.criteria;
  }

let find_skill_for_company ~company_id id =
  match find_by_id id with
  | Some s -> Some s
  | None -> Store.find_custom_skill ~company_id id

let require_session req =
  match Auth.get_session req with
  | None -> None
  | Some s -> Some s

(* ── Existing skill endpoints ── *)

let handle_ping _req =
  Dream.json {|{"status":"ok","service":"roladeck"}|}

let handle_skills _req =
  let summaries = List.map skill_to_summary all_skills in
  let json = `List (List.map skill_summary_to_yojson summaries) in
  Dream.json (Yojson.Safe.to_string json)

let handle_skill req =
  let id = Dream.param req "id" in
  match find_by_id id with
  | Some skill ->
    Dream.json (Yojson.Safe.to_string (skill_record_to_yojson skill))
  | None ->
    match require_session req with
    | None -> json_response ~status:`Not_Found {|{"error":"skill not found"}|}
    | Some session ->
      match Store.find_custom_skill ~company_id:session.company_id id with
      | None -> json_response ~status:`Not_Found {|{"error":"skill not found"}|}
      | Some skill ->
        Dream.json (Yojson.Safe.to_string (skill_record_to_yojson skill))

let handle_search req =
  let q = Dream.query req "q" |> Option.value ~default:"" in
  let category_filter = Dream.query req "category" in
  let results =
    if String.length q = 0 then all_skills
    else search q
  in
  let results =
    match category_filter with
    | None -> results
    | Some cat_str ->
      let cat_opt = match String.lowercase_ascii cat_str with
        | "tech" -> Some Tech
        | "marketing" -> Some Marketing
        | "sales" -> Some Sales
        | _ -> None
      in
      (match cat_opt with
       | None -> results
       | Some cat -> List.filter (fun s -> s.discipline.category = cat) results)
  in
  let summaries = List.map skill_to_summary results in
  let json = `List (List.map skill_summary_to_yojson summaries) in
  Dream.json (Yojson.Safe.to_string json)

(* Keep old /api/score for backward compat *)
let handle_score req =
  let%lwt body = Dream.body req in
  (match Yojson.Safe.from_string body |> scoring_request_of_yojson with
   | Error e ->
     json_response ~status:`Bad_Request
       (Printf.sprintf {|{"error":"invalid request: %s"}|} e)
   | Ok score_req ->
     let skill_opt = match require_session req with
       | None -> find_by_id score_req.discipline_id
       | Some session -> find_skill_for_company ~company_id:session.company_id score_req.discipline_id
     in
     match skill_opt with
     | None ->
       json_response ~status:`Not_Found
         {|{"error":"discipline not found"}|}
     | Some skill ->
       let result = score_candidate skill score_req in
       Dream.json (Yojson.Safe.to_string (scoring_result_to_yojson result)))

let handle_source req =
  let%lwt body = Dream.body req in
  (match Yojson.Safe.from_string body |> sourcing_query_of_yojson with
   | Error e ->
     json_response ~status:`Bad_Request
       (Printf.sprintf {|{"error":"invalid request: %s"}|} e)
   | Ok query ->
     let skill_opt = match require_session req with
       | None -> find_by_id query.discipline_id
       | Some session -> find_skill_for_company ~company_id:session.company_id query.discipline_id
     in
     match skill_opt with
     | None ->
       json_response ~status:`Not_Found
         {|{"error":"discipline not found"}|}
     | Some skill ->
       let result = execute_query skill query in
       Dream.json (Yojson.Safe.to_string (sourcing_result_to_yojson result)))

(* ── Auth endpoints ── *)

(* POST /api/auth/signup *)
let handle_signup req =
  let%lwt body = Dream.body req in
  let json = Yojson.Safe.from_string body in
  let get_str key = match Yojson.Safe.Util.member key json with
    | `String s -> s | _ -> "" in
  let email        = get_str "email" in
  let password     = get_str "password" in
  let company_name = get_str "company_name" in
  if String.length email = 0 || String.length password = 0 then
    Dream.respond ~status:`Bad_Request {|{"error":"Email and password required"}|}
  else
    let domain = Auth.domain_of_email email in
    if String.length domain = 0 then
      Dream.respond ~status:`Bad_Request {|{"error":"Invalid email address"}|}
    else if Option.is_some (Store.find_user_by_email email) then
      Dream.respond ~status:`Bad_Request {|{"error":"An account with this email already exists"}|}
    else begin
      (* Find or create company *)
      let company = match Store.find_company_by_domain domain with
        | Some c -> c
        | None ->
          let t = int_of_float (Unix.gettimeofday () *. 1000.0) in
          let cid = Printf.sprintf "co-%s-%d"
            (String.map (fun c -> if c = '.' then '-' else c) domain) t in
          let c : company_record = {
            id = cid; domain; name = company_name; created_at = Store.now_iso ()
          } in
          Store.upsert_company c;
          c
      in
      let t = int_of_float (Unix.gettimeofday () *. 1000.0) in
      let uid = Printf.sprintf "u-%d-%06d" t (Random.int 999999) in
      let user : user_record = {
        id = uid;
        email;
        password_hash = Auth.hash_password password;
        company_id = company.id;
        created_at = Store.now_iso ();
      } in
      Store.upsert_user user;
      Store.cleanup_expired_sessions ();
      let token = Auth.generate_token () in
      let session : session_record = {
        token; user_id = user.id; company_id = company.id; created_at = Store.now_iso ()
      } in
      Store.upsert_session session;
      let%lwt resp = Dream.json (Printf.sprintf
        {|{"user_id":"%s","email":"%s","company_id":"%s","company_name":"%s"}|}
        user.id user.email company.id company.name) in
      Dream.set_cookie resp req "rd_session" token
        ~http_only:true ~same_site:(Some `Lax) ~path:(Some "/");
      Lwt.return resp
    end

(* POST /api/auth/login *)
let handle_login req =
  let%lwt body = Dream.body req in
  let json = Yojson.Safe.from_string body in
  let get_str key = match Yojson.Safe.Util.member key json with
    | `String s -> s | _ -> "" in
  let email    = get_str "email" in
  let password = get_str "password" in
  match Store.find_user_by_email email with
  | None ->
    Dream.respond ~status:`Unauthorized {|{"error":"Invalid email or password"}|}
  | Some user ->
    if not (Auth.verify_password password user.password_hash) then
      Dream.respond ~status:`Unauthorized {|{"error":"Invalid email or password"}|}
    else begin
      let company_name = match Store.find_company_by_id user.company_id with
        | Some c -> c.name | None -> "" in
      Store.cleanup_expired_sessions ();
      let token = Auth.generate_token () in
      let session : session_record = {
        token; user_id = user.id; company_id = user.company_id; created_at = Store.now_iso ()
      } in
      Store.upsert_session session;
      let%lwt resp = Dream.json (Printf.sprintf
        {|{"user_id":"%s","email":"%s","company_id":"%s","company_name":"%s"}|}
        user.id user.email user.company_id company_name) in
      Dream.set_cookie resp req "rd_session" token
        ~http_only:true ~same_site:(Some `Lax) ~path:(Some "/");
      Lwt.return resp
    end

(* POST /api/auth/logout *)
let handle_logout req =
  (match Dream.cookie req "rd_session" with
   | Some token -> Store.delete_session token
   | None -> ());
  let%lwt resp = Dream.json {|{"ok":true}|} in
  Dream.set_cookie resp req "rd_session" ""
    ~http_only:true ~same_site:(Some `Lax) ~path:(Some "/") ~max_age:0.0;
  Lwt.return resp

(* GET /api/auth/me *)
let handle_me req =
  match Auth.get_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let users = Store.load_users () in
    let user_email = match List.find_opt (fun (u : user_record) -> u.id = session.user_id) users with
      | Some u -> u.email | None -> "" in
    let company_name = match Store.find_company_by_id session.company_id with
      | Some c -> c.name | None -> "" in
    Dream.json (Printf.sprintf
      {|{"user_id":"%s","email":"%s","company_id":"%s","company_name":"%s"}|}
      session.user_id user_email session.company_id company_name)

(* ── Pool endpoints ── *)

let handle_pool_list req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    let role_id  = Dream.query req "role"  |> Option.value ~default:"" in
    let q        = Dream.query req "q"     |> Option.value ~default:"" in
    let stage    = Dream.query req "stage" |> Option.map (fun s ->
      match Yojson.Safe.from_string (Printf.sprintf {|"%s"|} s) |> ats_stage_of_yojson with
      | Ok st -> st | Error _ -> Screening
    ) in
    let lookback =
      match Store.load_company_profile ~company_id () with
      | None -> None
      | Some p -> p.pool_lookback
    in
    let summaries = Store.get_summaries ~company_id ~role_id ~stage ~q ~lookback () in
    let json = `List (List.map candidate_summary_to_yojson summaries) in
    Dream.json (Yojson.Safe.to_string json)

let handle_pool_stats req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    let stats = Store.get_pool_stats ~company_id () in
    Dream.json (Yojson.Safe.to_string (pool_stats_to_yojson stats))

let handle_pool_by_skill req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    let criterion = Dream.query req "criterion" |> Option.value ~default:"" in
    let matches = Store.get_by_skill ~company_id criterion in
    let json = `List (List.map skill_match_to_yojson matches) in
    Dream.json (Yojson.Safe.to_string json)

let handle_pool_save req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    let%lwt body = Dream.body req in
    (match Yojson.Safe.from_string body |> pool_save_request_of_yojson with
     | Error e ->
       json_response ~status:`Bad_Request
         (Printf.sprintf {|{"error":"invalid request: %s"}|} e)
     | Ok pr ->
       match find_by_id pr.discipline_id with
       | None ->
         json_response ~status:`Not_Found {|{"error":"discipline not found"}|}
       | Some skill ->
         let score_req : scoring_request = {
           candidate_id = pr.candidate_name;
           discipline_id = pr.discipline_id;
           seniority = pr.seniority;
           candidate_notes = pr.candidate_notes;
         } in
         let result = score_candidate skill score_req in
         let now = Store.now_iso () in
         let cs : candidate_score = {
           role_id          = pr.discipline_id;
           role_name        = skill.discipline.name;
           seniority        = pr.seniority;
           overall_score    = result.overall_score;
           recommendation   = result.recommendation;
           tier_scores      = result.tier_scores;
           red_flags_hit    = result.red_flags_hit;
           criterion_results= result.criterion_results;
           scored_at        = now;
         } in
         let id = Store.generate_id () in
         let record : candidate_record = {
           id;
           name        = (if String.length pr.candidate_name = 0 then "Candidate" else pr.candidate_name);
           ats_stage   = pr.ats_stage;
           scores      = [cs];
           source_text = pr.candidate_notes;
           created_at  = now;
           updated_at  = now;
           greenhouse_url = None;
           greenhouse_application_id = None;
           trust_check = None;
         } in
         Store.upsert ~company_id record;
         Dream.json (Yojson.Safe.to_string (candidate_record_to_yojson record)))

let handle_pool_get req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    let id = Dream.param req "id" in
    (match Store.get_by_id ~company_id id with
     | None ->
       json_response ~status:`Not_Found {|{"error":"candidate not found"}|}
     | Some c ->
       Dream.json (Yojson.Safe.to_string (candidate_record_to_yojson c)))

let handle_pool_stage req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    let id = Dream.param req "id" in
    let%lwt body = Dream.body req in
    (match Yojson.Safe.from_string body |> update_stage_request_of_yojson with
     | Error e ->
       json_response ~status:`Bad_Request
         (Printf.sprintf {|{"error":"invalid request: %s"}|} e)
     | Ok r ->
       match Store.update_stage ~company_id id r.stage with
       | Error msg ->
         json_response ~status:`Not_Found
           (Printf.sprintf {|{"error":"%s"}|} msg)
       | Ok updated ->
         Dream.json (Yojson.Safe.to_string (candidate_record_to_yojson updated)))

let handle_pool_score_existing req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    let candidate_id = Dream.param req "id" in
    (match Store.get_by_id ~company_id candidate_id with
     | None -> json_response ~status:`Not_Found {|{"error":"candidate not found"}|}
     | Some candidate ->
       let%lwt body = Dream.body req in
       let json = Yojson.Safe.from_string body in
       let get_str key = match Yojson.Safe.Util.member key json with `String s -> s | _ -> "" in
       let role_id  = get_str "role_id" in
       let seniority_str = get_str "seniority" in
       let seniority =
         match Yojson.Safe.from_string (Printf.sprintf {|"%s"|} seniority_str) |> seniority_level_of_yojson with
         | Ok s -> s | Error _ -> Senior
       in
       if String.length role_id = 0 then
         json_response ~status:`Bad_Request {|{"error":"role_id required"}|}
       else
         match find_skill_for_company ~company_id role_id with
         | None -> json_response ~status:`Not_Found {|{"error":"role not found"}|}
         | Some skill ->
           let scoring_text =
             let st = String.trim candidate.source_text in
             if String.length st > 0 then st else candidate.name
           in
           let score_req : scoring_request = {
             candidate_id  = candidate.id;
             discipline_id = role_id;
             seniority;
             candidate_notes = scoring_text;
           } in
           let result = score_candidate skill score_req in
           let now = Store.now_iso () in
           let cs : candidate_score = {
             role_id       = skill.id;
             role_name     = skill.discipline.name;
             seniority;
             overall_score = result.overall_score;
             recommendation= result.recommendation;
             tier_scores   = result.tier_scores;
             red_flags_hit = result.red_flags_hit;
             criterion_results = result.criterion_results;
             scored_at     = now;
           } in
           (* Replace any existing score for this role, append otherwise *)
           let existing_scores =
             List.filter (fun (s : candidate_score) -> s.role_id <> role_id) candidate.scores
           in
           let updated_candidate = {
             candidate with
             scores     = existing_scores @ [cs];
             updated_at = now;
           } in
           Store.upsert ~company_id updated_candidate;
           Dream.json (Yojson.Safe.to_string (candidate_record_to_yojson updated_candidate)))

let handle_verify_candidate req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    let candidate_id = Dream.param req "id" in
    (match Store.get_by_id ~company_id candidate_id with
     | None -> json_response ~status:`Not_Found {|{"error":"Not found"}|}
     | Some candidate ->
       let%lwt tc = Verify.verify_candidate ~company_id ~candidate_notes:candidate.source_text in
       let updated = { candidate with trust_check = Some tc; updated_at = Store.now_iso () } in
       Store.upsert ~company_id updated;
       let summary = Store.summary_of_record updated in
       Dream.json (Yojson.Safe.to_string (candidate_summary_to_yojson summary)))

let handle_calibration req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    let role_id = Dream.param req "role_id" in
    let pool = Store.get_all ~company_id () in
    let cal = compute_calibration pool role_id in
    Dream.json (Yojson.Safe.to_string (role_calibration_to_yojson cal))

(* ── AI Classification endpoint ── *)

(* POST /api/classify *)
let handle_classify req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    Lwt.catch
      (fun () ->
        let%lwt body = Dream.body req in
        let candidate_notes =
          try
            Yojson.Safe.from_string body
            |> Yojson.Safe.Util.member "candidate_notes"
            |> Yojson.Safe.Util.to_string
          with _ -> ""
        in
        if String.length (String.trim candidate_notes) < 30 then
          Dream.json {|{"matches":[]}|}
        else
          let%lwt result = Classify.classify_candidate ~company_id ~candidate_notes in
          Dream.json (Yojson.Safe.to_string (classify_result_to_yojson result)))
      (fun exn ->
        json_response ~status:`Internal_Server_Error
          (Printf.sprintf {|{"error":"%s"}|} (String.escaped (Printexc.to_string exn))))

(* ── AI Sourcing endpoints ── *)

let handle_ai_source req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    Lwt.catch
      (fun () ->
        let%lwt body = Dream.body req in
        match Yojson.Safe.from_string body |> ai_sourcing_request_of_yojson with
        | Error e ->
          json_response ~status:`Bad_Request
            (Printf.sprintf {|{"error":"invalid request: %s"}|} e)
        | Ok ai_req ->
          let%lwt result = AiSourcing.run_sourcing ~company_id ai_req in
          (match result with
           | Error e ->
             json_response ~status:`Internal_Server_Error
               (Printf.sprintf {|{"error":"%s"}|} (String.escaped e))
           | Ok sess ->
             Store.append_sourcing_session ~company_id sess;
             Dream.json (Yojson.Safe.to_string (ai_sourcing_result_to_yojson sess))))
      (fun exn ->
        json_response ~status:`Internal_Server_Error
          (Printf.sprintf {|{"error":"%s"}|} (String.escaped (Printexc.to_string exn))))

let handle_ai_sessions req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    let sessions = Store.get_sourcing_sessions ~company_id () in
    let json = `List (List.map ai_sourcing_result_to_yojson sessions) in
    Dream.json (Yojson.Safe.to_string json)

(* ── Settings endpoints ── *)

let handle_settings_status req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    let settings = Store.load_integration_settings ~company_id () in
    let anthropic_key_set =
      (match Sys.getenv_opt "ANTHROPIC_API_KEY" with
       | Some k when String.length k > 0 -> true | _ -> false) ||
      (settings.ai_provider = "anthropic" && String.length settings.ai_api_key > 0)
    in
    let greenhouse_configured =
      String.length settings.greenhouse_subdomain > 0 &&
      String.length settings.greenhouse_api_key > 0
    in
    let status : settings_status = { anthropic_key_set; greenhouse_configured } in
    Dream.json (Yojson.Safe.to_string (settings_status_to_yojson status))

let mask_key k =
  if String.length k > 8 then
    String.sub k 0 4 ^ "..." ^ String.sub k (String.length k - 4) 4
  else if String.length k > 0 then "***"
  else ""

let handle_get_integration_settings req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    let s = Store.load_integration_settings ~company_id () in
    let ai_key_set =
      String.length s.ai_api_key > 0 ||
      (match s.ai_provider with
       | "openai"    -> (match Sys.getenv_opt "OPENAI_API_KEY" with Some k -> String.length k > 0 | None -> false)
       | "perplexity" -> (match Sys.getenv_opt "PERPLEXITY_API_KEY" with Some k -> String.length k > 0 | None -> false)
       | _           -> (match Sys.getenv_opt "ANTHROPIC_API_KEY" with Some k -> String.length k > 0 | None -> false))
    in
    Dream.json (Yojson.Safe.to_string (`Assoc [
      "greenhouse_subdomain",   `String s.greenhouse_subdomain;
      "greenhouse_api_key_hint",`String (mask_key s.greenhouse_api_key);
      "greenhouse_configured",  `Bool (String.length s.greenhouse_subdomain > 0 &&
                                       String.length s.greenhouse_api_key > 0);
      "ai_provider",            `String s.ai_provider;
      "ai_api_key_hint",        `String (mask_key s.ai_api_key);
      "ai_key_set",             `Bool ai_key_set;
    ]))

let handle_save_integration_settings req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    let%lwt body = Dream.body req in
    let parse j =
      let open Roladeck_json.Types_j in
      let open Result in
      str_field j "greenhouse_subdomain" >>= fun greenhouse_subdomain ->
      str_field j "greenhouse_api_key"   >>= fun greenhouse_api_key ->
      let ai_provider = match str_field j "ai_provider" with Ok s -> s | Error _ -> "" in
      let ai_api_key  = match str_field j "ai_api_key"  with Ok s -> s | Error _ -> "" in
      Ok (greenhouse_subdomain, greenhouse_api_key, ai_provider, ai_api_key)
    in
    (match Yojson.Safe.from_string body |> parse with
     | Error e ->
       json_response ~status:`Bad_Request
         (Printf.sprintf {|{"error":"invalid request: %s"}|} e)
     | Ok (greenhouse_subdomain, greenhouse_api_key, ai_provider, ai_api_key) ->
       let existing = Store.load_integration_settings ~company_id () in
       let to_save : integration_settings = {
         greenhouse_subdomain;
         greenhouse_api_key =
           if String.length greenhouse_api_key = 0 then existing.greenhouse_api_key
           else greenhouse_api_key;
         ai_provider =
           if String.length ai_provider = 0 then existing.ai_provider
           else ai_provider;
         ai_api_key =
           if String.length ai_api_key = 0 then existing.ai_api_key
           else ai_api_key;
       } in
       Store.save_integration_settings ~company_id to_save;
       json_response (`Assoc [
         "ok", `Bool true;
         "greenhouse_configured", `Bool (
           String.length to_save.greenhouse_subdomain > 0 &&
           String.length to_save.greenhouse_api_key > 0);
         "ai_provider", `String to_save.ai_provider;
       ] |> Yojson.Safe.to_string))

(* ── Company Profile endpoints ── *)

let handle_get_company_profile req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    (match Store.load_company_profile ~company_id () with
     | None ->
       Dream.json (Yojson.Safe.to_string (company_profile_to_yojson {
         company_name = ""; company_urls = []; company_brief = ""; brief_generated_at = None; pool_lookback = None;
       }))
     | Some p ->
       Dream.json (Yojson.Safe.to_string (company_profile_to_yojson p)))

let handle_save_company_profile req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    let%lwt body = Dream.body req in
    let parse j =
      let open Roladeck_json.Types_j in
      let open Result in
      str_field j "company_name" >>= fun company_name ->
      list_field j "company_urls"
        (function `String s -> Ok s | v -> Error (Yojson.Safe.to_string v))
      >>= fun company_urls ->
      Ok (company_name, company_urls)
    in
    (match Yojson.Safe.from_string body |> parse with
     | Error e ->
       json_response ~status:`Bad_Request
         (Printf.sprintf {|{"error":"invalid request: %s"}|} e)
     | Ok (company_name, company_urls) ->
       let existing = Store.load_company_profile ~company_id ()
         |> Option.value ~default:{ company_name = ""; company_urls = []; company_brief = ""; brief_generated_at = None; pool_lookback = None } in
       let profile = { existing with company_name; company_urls } in
       Store.save_company_profile ~company_id profile;
       (* Fire async research job — does not block *)
       Lwt.async (fun () ->
         CompanyResearch.run_company_research ~company_id profile
         |> Lwt.map (function
           | Ok updated -> Store.save_company_profile ~company_id updated
           | Error e -> Printf.eprintf "Company research error: %s\n%!" e)
       );
       Dream.json {|{"status":"researching"}|})

let handle_save_pool_settings req =
  match require_session req with
  | None -> json_response ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let%lwt body = Dream.body req in
    let json = Yojson.Safe.from_string body in
    let lookback = match Yojson.Safe.Util.member "pool_lookback" json with
      | `String s -> Some s | `Null -> None | _ -> None
    in
    let existing = Store.load_company_profile ~company_id:session.company_id ()
      |> Option.value ~default:{ company_name = ""; company_urls = []; company_brief = "";
                                  brief_generated_at = None; pool_lookback = None }
    in
    Store.save_company_profile ~company_id:session.company_id { existing with pool_lookback = lookback };
    Dream.json {|{"status":"ok"}|}

(* Simple base64 for Greenhouse basic auth *)
let base64_encode s =
  let tbl = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let n = String.length s in
  let buf = Buffer.create ((n / 3 + 1) * 4) in
  let i = ref 0 in
  while !i + 2 < n do
    let b0 = Char.code s.[!i] and b1 = Char.code s.[!i+1] and b2 = Char.code s.[!i+2] in
    Buffer.add_char buf tbl.[b0 lsr 2];
    Buffer.add_char buf tbl.[((b0 land 3) lsl 4) lor (b1 lsr 4)];
    Buffer.add_char buf tbl.[((b1 land 15) lsl 2) lor (b2 lsr 6)];
    Buffer.add_char buf tbl.[b2 land 63];
    i := !i + 3
  done;
  if !i + 1 = n then begin
    let b0 = Char.code s.[!i] in
    Buffer.add_char buf tbl.[b0 lsr 2];
    Buffer.add_char buf tbl.[(b0 land 3) lsl 4];
    Buffer.add_string buf "=="
  end else if !i + 2 = n then begin
    let b0 = Char.code s.[!i] and b1 = Char.code s.[!i+1] in
    Buffer.add_char buf tbl.[b0 lsr 2];
    Buffer.add_char buf tbl.[((b0 land 3) lsl 4) lor (b1 lsr 4)];
    Buffer.add_char buf tbl.[(b1 land 15) lsl 2];
    Buffer.add_char buf '='
  end;
  Buffer.contents buf

let json_string_escape s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '"'  -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c when Char.code c < 0x20 -> ()
    | c    -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let strip_html html =
  let n = String.length html in
  let buf = Buffer.create n in
  let i = ref 0 in
  let starts_with_ci prefix =
    let pl = String.length prefix in
    !i + pl <= n &&
    String.lowercase_ascii (String.sub html !i pl) = String.lowercase_ascii prefix
  in
  let skip_to_gt () =
    while !i < n && html.[!i] <> '>' do incr i done;
    if !i < n then incr i
  in
  let skip_until close_tag =
    let ct = String.lowercase_ascii close_tag in
    let cl = String.length ct in
    let found = ref false in
    while !i < n && not !found do
      if !i + cl <= n &&
         String.lowercase_ascii (String.sub html !i cl) = ct
      then (i := !i + cl; found := true)
      else incr i
    done
  in
  while !i < n do
    if html.[!i] = '<' then begin
      if starts_with_ci "<script" || starts_with_ci "<style" then begin
        let is_script = starts_with_ci "<script" in
        skip_to_gt ();
        skip_until (if is_script then "</script>" else "</style>")
      end else
        skip_to_gt ()
    end else begin
      Buffer.add_char buf html.[!i];
      incr i
    end
  done;
  Buffer.contents buf

let collapse_whitespace s =
  let buf = Buffer.create (String.length s) in
  let at_ws = ref true in
  String.iter (fun c ->
    if c = ' ' || c = '\t' || c = '\r' then begin
      if not !at_ws then (Buffer.add_char buf ' '; at_ws := true)
    end else if c = '\n' then begin
      if not !at_ws then (Buffer.add_char buf '\n'; at_ws := true)
    end else begin
      Buffer.add_char buf c;
      at_ws := false
    end
  ) s;
  Buffer.contents buf

let handle_fetch_url req =
  let%lwt body = Dream.body req in
  let json = try Some (Yojson.Safe.from_string body) with _ -> None in
  match json with
  | None -> json_response ~status:`Bad_Request {|{"error":"Invalid JSON"}|}
  | Some (`Assoc fields) -> begin
    let url = match List.assoc_opt "url" fields with
      | Some (`String s) -> s
      | _ -> "" in
    if String.length url = 0 then
      json_response ~status:`Bad_Request {|{"error":"Missing url field"}|}
    else
      let uri = Uri.of_string url in
      let headers = Cohttp.Header.of_list [
        ("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36");
        ("Accept", "text/html,text/plain,*/*");
        ("Accept-Language", "en-US,en;q=0.9");
      ] in
      Lwt.catch
        (fun () ->
          let%lwt (resp, resp_body) = Cohttp_lwt_unix.Client.get ~headers uri in
          let code = Cohttp.Code.code_of_status (Cohttp.Response.status resp) in
          if code >= 200 && code < 300 then begin
            let%lwt raw = Cohttp_lwt.Body.to_string resp_body in
            let raw = if String.length raw > 200_000 then String.sub raw 0 200_000 else raw in
            let text = collapse_whitespace (strip_html raw) in
            let text = if String.length text > 50_000 then String.sub text 0 50_000 else text in
            Dream.json (Printf.sprintf {|{"text":"%s"}|} (json_string_escape text))
          end else
            json_response ~status:`Bad_Gateway
              (Printf.sprintf {|{"error":"Remote returned HTTP %d"}|} code))
        (fun exn ->
          json_response ~status:`Internal_Server_Error
            (Printf.sprintf {|{"error":"%s"}|}
              (json_string_escape (Printexc.to_string exn))))
  end
  | Some _ -> json_response ~status:`Bad_Request {|{"error":"Invalid JSON"}|}

(* ── Greenhouse Sync endpoints ── *)

let handle_greenhouse_sync_status req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    let state = Store.load_greenhouse_sync_state ~company_id () in
    let settings = Store.load_integration_settings ~company_id () in
    let configured =
      String.length settings.greenhouse_subdomain > 0 &&
      String.length settings.greenhouse_api_key > 0
    in
    Dream.json (Yojson.Safe.to_string (`Assoc [
      "configured",    `Bool configured;
      "last_synced_at", (match state.last_synced_at with None -> `Null | Some t -> `String t);
      "total_synced",  `Int state.total_synced;
      "last_error",    (match state.last_error with None -> `Null | Some e -> `String e);
    ]))

let handle_greenhouse_sync_now req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    let settings = Store.load_integration_settings ~company_id () in
    if String.length settings.greenhouse_api_key = 0 then
      json_response ~status:`Bad_Request
        {|{"error":"Greenhouse credentials not configured"}|}
    else begin
      Lwt.async (fun () ->
        Lwt.catch
          (fun () ->
            let%lwt result = GhSync.sync_once ~company_id ~api_key:settings.greenhouse_api_key () in
            (match result with
             | Ok n ->
               if n > 0 then Printf.printf "Manual sync: imported %d candidate(s)\n%!" n
             | Error e -> Printf.eprintf "Manual sync error: %s\n%!" e);
            Lwt.return ())
          (fun exn ->
            Printf.eprintf "Manual sync exception: %s\n%!" (Printexc.to_string exn);
            Lwt.return ())
      );
      Dream.json {|{"status":"syncing"}|}
    end

let handle_test_greenhouse req =
  match require_session req with
  | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let company_id = session.company_id in
    let settings = Store.load_integration_settings ~company_id () in
    if String.length settings.greenhouse_subdomain = 0 ||
       String.length settings.greenhouse_api_key = 0 then
      json_response ~status:`Bad_Request
        {|{"ok":false,"error":"Greenhouse credentials not configured"}|}
    else
      let url = Printf.sprintf "https://%s.greenhouse.io/v1/users?per_page=1"
        settings.greenhouse_subdomain in
      let auth = "Basic " ^ base64_encode (settings.greenhouse_api_key ^ ":") in
      let uri = Uri.of_string url in
      let headers = Cohttp.Header.of_list [("Authorization", auth)] in
      Lwt.catch
        (fun () ->
          let%lwt (resp, _body) = Cohttp_lwt_unix.Client.get ~headers uri in
          let code = Cohttp.Code.code_of_status (Cohttp.Response.status resp) in
          if code = 200 then Dream.json {|{"ok":true}|}
          else Dream.json
            (Printf.sprintf {|{"ok":false,"error":"Greenhouse returned HTTP %d"}|} code))
        (fun exn ->
          json_response ~status:`Internal_Server_Error
            (Printf.sprintf {|{"ok":false,"error":"%s"}|}
              (String.escaped (Printexc.to_string exn))))

(* ── Custom Skill CRUD ── *)

let handle_custom_skills_list req =
  match require_session req with
  | None -> json_response ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let skills = Store.load_custom_skills ~company_id:session.company_id () in
    let summaries = List.map skill_to_summary skills in
    let json = `List (List.map skill_summary_to_yojson summaries) in
    Dream.json (Yojson.Safe.to_string json)

let handle_custom_skill_create req =
  match require_session req with
  | None -> json_response ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let%lwt body = Dream.body req in
    (match Yojson.Safe.from_string body |> skill_record_of_yojson with
     | Error e -> json_response ~status:`Bad_Request
         (Printf.sprintf {|{"error":"invalid request: %s"}|} e)
     | Ok skill ->
       Store.upsert_custom_skill ~company_id:session.company_id skill;
       Dream.json (Yojson.Safe.to_string (skill_record_to_yojson skill)))

let handle_custom_skill_update req =
  match require_session req with
  | None -> json_response ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let id = Dream.param req "id" in
    (match Store.find_custom_skill ~company_id:session.company_id id with
     | None -> json_response ~status:`Not_Found {|{"error":"not found"}|}
     | Some _ ->
       let%lwt body = Dream.body req in
       (match Yojson.Safe.from_string body |> skill_record_of_yojson with
        | Error e -> json_response ~status:`Bad_Request
            (Printf.sprintf {|{"error":"invalid request: %s"}|} e)
        | Ok skill ->
          Store.upsert_custom_skill ~company_id:session.company_id skill;
          Dream.json (Yojson.Safe.to_string (skill_record_to_yojson skill))))

let handle_custom_skill_delete req =
  match require_session req with
  | None -> json_response ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let id = Dream.param req "id" in
    (match Store.find_custom_skill ~company_id:session.company_id id with
     | None -> json_response ~status:`Not_Found {|{"error":"not found"}|}
     | Some _ ->
       Store.delete_custom_skill ~company_id:session.company_id id;
       Dream.json {|{"ok":true}|})

(* ── Custom Skill Import (markdown) ── *)

let normalise_github_url url =
  let prefix = "https://github.com/" in
  let plen = String.length prefix in
  if String.length url >= plen && String.sub url 0 plen = prefix then
    let rest = String.sub url plen (String.length url - plen) in
    (try
      let i = Str.search_forward (Str.regexp "/blob/") rest 0 in
      let before = String.sub rest 0 i in
      let after = String.sub rest (i + 6) (String.length rest - i - 6) in
      "https://raw.githubusercontent.com/" ^ before ^ "/" ^ after
    with Not_found -> url)
  else url

let handle_import_playbook req =
  match require_session req with
  | None -> json_response ~status:`Unauthorized {|{"error":"Unauthorized"}|}
  | Some session ->
    let%lwt body = Dream.body req in
    let json = Yojson.Safe.from_string body in
    let get_str key = match Yojson.Safe.Util.member key json with
      | `String s -> s | _ -> "" in
    let content_param = get_str "content" in
    let url_param     = get_str "url" in
    let id_param      = get_str "id" in
    if String.length content_param = 0 && String.length url_param = 0 then
      json_response ~status:`Bad_Request {|{"error":"provide content or url"}|}
    else
      let%lwt content_result =
        if String.length content_param > 0 then Lwt.return (Ok content_param)
        else
          let url = normalise_github_url url_param in
          let uri = Uri.of_string url in
          Lwt.catch
            (fun () ->
              let%lwt (_resp, body_cohttp) = Cohttp_lwt_unix.Client.get uri in
              let%lwt text = Cohttp_lwt.Body.to_string body_cohttp in
              Lwt.return (Ok text))
            (fun e -> Lwt.return (Error (Printexc.to_string e)))
      in
      match content_result with
      | Error e ->
        json_response ~status:`Bad_Request
          (Printf.sprintf {|{"error":"fetch failed: %s"}|} (String.escaped e))
      | Ok text ->
        let skill_id =
          if String.length id_param > 0 then id_param
          else
            let t = int_of_float (Unix.gettimeofday () *. 1000.0) in
            Printf.sprintf "custom-hiring-import-%d" t
        in
        (match SkillParser.parse_skill_content ~content:text ~skill_id with
         | None ->
           json_response ~status:`Bad_Request
             {|{"error":"could not parse markdown — check the file matches the template format"}|}
         | Some ps ->
           let skill = SkillParser.parsed_skill_to_record ps in
           Store.upsert_custom_skill ~company_id:session.company_id skill;
           Dream.json (Yojson.Safe.to_string (skill_record_to_yojson skill)))
