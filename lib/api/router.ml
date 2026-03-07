open Handlers

let cors_middleware inner_handler req =
  let%lwt response = inner_handler req in
  Dream.add_header response "Access-Control-Allow-Origin" "*";
  Dream.add_header response "Access-Control-Allow-Headers" "Content-Type";
  Dream.add_header response "Access-Control-Allow-Methods" "GET, POST, PUT, OPTIONS";
  Lwt.return response

let auth_middleware inner_handler req =
  let path = Dream.target req in
  let starts s p =
    String.length s >= String.length p &&
    String.sub s 0 (String.length p) = p
  in
  let is_public =
    starts path "/api/auth/" ||
    starts path "/api/skills" ||
    starts path "/api/search" ||
    path = "/api/ping"
  in
  if is_public then inner_handler req
  else
    match Ahrefs_auth.Auth.get_session req with
    | None -> Dream.respond ~status:`Unauthorized {|{"error":"Unauthorized"}|}
    | Some _ -> inner_handler req

let routes = Dream.router [
  Dream.get  "/api/ping"                handle_ping;
  Dream.get  "/api/skills"              handle_skills;
  Dream.get  "/api/skills/:id"          handle_skill;
  Dream.get  "/api/search"              handle_search;
  Dream.post "/api/score"               handle_score;
  Dream.post "/api/source"              handle_source;
  (* Auth *)
  Dream.post "/api/auth/signup" handle_signup;
  Dream.post "/api/auth/login"  handle_login;
  Dream.post "/api/auth/logout" handle_logout;
  Dream.get  "/api/auth/me"     handle_me;
  (* Pool — define literal routes before parameterised *)
  Dream.get  "/api/pool/stats"          handle_pool_stats;
  Dream.get  "/api/pool/by-skill"       handle_pool_by_skill;
  Dream.get  "/api/pool"                handle_pool_list;
  Dream.post "/api/pool"                handle_pool_save;
  Dream.get  "/api/pool/:id"            handle_pool_get;
  Dream.put  "/api/pool/:id/stage"      handle_pool_stage;
  Dream.get  "/api/calibration/:role_id" handle_calibration;
  (* AI Sourcing *)
  Dream.post "/api/ai/source"   handle_ai_source;
  Dream.get  "/api/ai/sessions" handle_ai_sessions;
  (* Company Profile *)
  Dream.get  "/api/company/profile" handle_get_company_profile;
  Dream.post "/api/company/profile" handle_save_company_profile;
  (* URL fetcher *)
  Dream.post "/api/fetch-url" handle_fetch_url;
  (* Settings *)
  Dream.get  "/api/settings/status"       handle_settings_status;
  Dream.get  "/api/settings/integrations" handle_get_integration_settings;
  Dream.post "/api/settings/integrations" handle_save_integration_settings;
  Dream.post "/api/settings/test-greenhouse" handle_test_greenhouse;
  (* Greenhouse Sync *)
  Dream.get  "/api/greenhouse/sync/status" handle_greenhouse_sync_status;
  Dream.post "/api/greenhouse/sync/now"    handle_greenhouse_sync_now;
  (* OPTIONS preflight *)
  Dream.options "/api/**" (fun _ -> Lwt.return (Dream.response ~headers:[
    ("Access-Control-Allow-Origin", "*");
    ("Access-Control-Allow-Headers", "Content-Type");
    ("Access-Control-Allow-Methods", "GET, POST, PUT, OPTIONS");
  ] ""));
]

let app = cors_middleware @@ auth_middleware @@ routes
