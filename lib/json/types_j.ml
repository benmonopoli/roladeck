open Roladeck_types.Types

let ( >>= ) = Result.bind

(* ---- Yojson encoders / decoders (native only) ---- *)

(* discipline_category *)
let discipline_category_to_yojson = function
  | Tech -> `String "Tech"
  | Marketing -> `String "Marketing"
  | Sales -> `String "Sales"

let discipline_category_of_yojson = function
  | `String "Tech" -> Ok Tech
  | `String "Marketing" -> Ok Marketing
  | `String "Sales" -> Ok Sales
  | j -> Error ("expected discipline_category, got: " ^ Yojson.Safe.to_string j)

(* seniority_level *)
let seniority_level_to_yojson = function
  | Junior -> `String "Junior"
  | Mid -> `String "Mid"
  | Senior -> `String "Senior"
  | Staff -> `String "Staff"
  | Principal -> `String "Principal"
  | Coordinator -> `String "Coordinator"
  | Manager -> `String "Manager"
  | SeniorManager -> `String "SeniorManager"
  | Director -> `String "Director"
  | VP -> `String "VP"
  | CMO -> `String "CMO"
  | CRO -> `String "CRO"

let seniority_level_of_yojson = function
  | `String "Junior" -> Ok Junior
  | `String "Mid" -> Ok Mid
  | `String "Senior" -> Ok Senior
  | `String "Staff" -> Ok Staff
  | `String "Principal" -> Ok Principal
  | `String "Coordinator" -> Ok Coordinator
  | `String "Manager" -> Ok Manager
  | `String "SeniorManager" -> Ok SeniorManager
  | `String "Director" -> Ok Director
  | `String "VP" -> Ok VP
  | `String "CMO" -> Ok CMO
  | `String "CRO" -> Ok CRO
  | j -> Error ("expected seniority_level, got: " ^ Yojson.Safe.to_string j)

(* skill_tier *)
let skill_tier_to_yojson = function
  | T1_MustHave -> `String "T1_MustHave"
  | T2_Differentiator -> `String "T2_Differentiator"
  | T3_RareUpside -> `String "T3_RareUpside"

let skill_tier_of_yojson = function
  | `String "T1_MustHave" -> Ok T1_MustHave
  | `String "T2_Differentiator" -> Ok T2_Differentiator
  | `String "T3_RareUpside" -> Ok T3_RareUpside
  | j -> Error ("expected skill_tier, got: " ^ Yojson.Safe.to_string j)

(* sourcing_platform *)
let sourcing_platform_to_yojson = function
  | LinkedIn_XRay -> `String "LinkedIn_XRay"
  | GitHub_XRay -> `String "GitHub_XRay"
  | GitHub_Search -> `String "GitHub_Search"
  | ArXiv -> `String "ArXiv"
  | Custom s -> `List [`String "Custom"; `String s]

let sourcing_platform_of_yojson = function
  | `String "LinkedIn_XRay" -> Ok LinkedIn_XRay
  | `String "GitHub_XRay" -> Ok GitHub_XRay
  | `String "GitHub_Search" -> Ok GitHub_Search
  | `String "ArXiv" -> Ok ArXiv
  | `List [`String "Custom"; `String s] -> Ok (Custom s)
  | j -> Error ("expected sourcing_platform, got: " ^ Yojson.Safe.to_string j)

let field j key =
  match j with
  | `Assoc kvs -> (match List.assoc_opt key kvs with
      | Some v -> Ok v
      | None -> Error ("missing field: " ^ key))
  | _ -> Error "expected object"

let str_field j key =
  match field j key with
  | Ok (`String s) -> Ok s
  | Ok v -> Error (key ^ ": expected string, got " ^ Yojson.Safe.to_string v)
  | Error e -> Error e

let int_field j key =
  match field j key with
  | Ok (`Int n) -> Ok n
  | Ok v -> Error (key ^ ": expected int, got " ^ Yojson.Safe.to_string v)
  | Error e -> Error e

let list_field j key of_item =
  match field j key with
  | Ok (`List lst) ->
    let results = List.map of_item lst in
    let errors = List.filter_map (function Error e -> Some e | Ok _ -> None) results in
    if errors <> [] then Error (String.concat "; " errors)
    else Ok (List.filter_map (function Ok v -> Some v | Error _ -> None) results)
  | Ok v -> Error (key ^ ": expected array, got " ^ Yojson.Safe.to_string v)
  | Error e -> Error e

(* skill_criterion *)
let skill_criterion_to_yojson (c : skill_criterion) =
  `Assoc [
    "text", `String c.text;
    "tier", skill_tier_to_yojson c.tier;
  ]

let skill_criterion_of_yojson j =
  let open Result in
  str_field j "text" >>= fun text ->
  field j "tier" >>= skill_tier_of_yojson >>= fun tier ->
  Ok { text; tier }

(* sourcing_string *)
let sourcing_string_to_yojson (s : sourcing_string) =
  `Assoc [
    "platform", sourcing_platform_to_yojson s.platform;
    "label", `String s.label;
    "query", `String s.query;
  ]

let sourcing_string_of_yojson j =
  let open Result in
  field j "platform" >>= sourcing_platform_of_yojson >>= fun platform ->
  str_field j "label" >>= fun label ->
  str_field j "query" >>= fun query ->
  Ok ({ platform; label; query } : sourcing_string)

(* seniority_signal *)
let seniority_signal_to_yojson s =
  `Assoc [
    "from_level", seniority_level_to_yojson s.from_level;
    "to_level", seniority_level_to_yojson s.to_level;
    "signal_text", `String s.signal_text;
  ]

(* interview_stage *)
let interview_stage_to_yojson s =
  `Assoc [
    "stage_name", `String s.stage_name;
    "format", `String s.format;
    "assessing", `String s.assessing;
  ]

(* comp_range *)
let comp_range_to_yojson c =
  `Assoc [
    "level", seniority_level_to_yojson c.level;
    "base_min", `Int c.base_min;
    "base_max", `Int c.base_max;
  ]

(* discipline *)
let discipline_to_yojson (d : discipline) =
  `Assoc [
    "id", `String d.id;
    "name", `String d.name;
    "category", discipline_category_to_yojson d.category;
    "description", `String d.description;
  ]

(* skill_record *)
let skill_record_to_yojson (s : skill_record) =
  `Assoc [
    "id", `String s.id;
    "discipline", discipline_to_yojson s.discipline;
    "criteria", `List (List.map skill_criterion_to_yojson s.criteria);
    "sourcing_strings", `List (List.map sourcing_string_to_yojson s.sourcing_strings);
    "title_synonyms", `List (List.map (fun (lvl, titles) ->
      `Assoc [
        "level", seniority_level_to_yojson lvl;
        "titles", `List (List.map (fun t -> `String t) titles);
      ]
    ) s.title_synonyms);
    "seniority_signals", `List (List.map seniority_signal_to_yojson s.seniority_signals);
    "interview_stages", `List (List.map interview_stage_to_yojson s.interview_stages);
    "red_flags", `List (List.map (fun s -> `String s) s.red_flags);
    "comp_ranges", `List (List.map comp_range_to_yojson s.comp_ranges);
  ]

(* skill_record decoders *)
let discipline_of_yojson j =
  let open Result in
  str_field j "id" >>= fun id ->
  str_field j "name" >>= fun name ->
  field j "category" >>= discipline_category_of_yojson >>= fun category ->
  str_field j "description" >>= fun description ->
  Ok { id; name; category; description }

let seniority_signal_of_yojson j =
  let open Result in
  field j "from_level" >>= seniority_level_of_yojson >>= fun from_level ->
  field j "to_level" >>= seniority_level_of_yojson >>= fun to_level ->
  str_field j "signal_text" >>= fun signal_text ->
  Ok { from_level; to_level; signal_text }

let interview_stage_of_yojson j =
  let open Result in
  str_field j "stage_name" >>= fun stage_name ->
  str_field j "format" >>= fun format ->
  str_field j "assessing" >>= fun assessing ->
  Ok { stage_name; format; assessing }

let comp_range_of_yojson j =
  let open Result in
  field j "level" >>= seniority_level_of_yojson >>= fun level ->
  int_field j "base_min" >>= fun base_min ->
  int_field j "base_max" >>= fun base_max ->
  Ok { level; base_min; base_max }

let title_synonyms_of_yojson = function
  | `List items ->
    let pairs = List.filter_map (fun item ->
      let lv = match field item "level" with Ok v -> v | Error _ -> `Null in
      match seniority_level_of_yojson lv with
      | Error _ -> None
      | Ok level ->
        let titles = match field item "titles" with
          | Ok (`List ts) -> List.filter_map (function `String s -> Some s | _ -> None) ts
          | _ -> [] in
        Some (level, titles)
    ) items in
    Ok pairs
  | j -> Error ("expected array for title_synonyms, got: " ^ Yojson.Safe.to_string j)

let skill_record_of_yojson j =
  let open Result in
  str_field j "id" >>= fun id ->
  field j "discipline" >>= discipline_of_yojson >>= fun discipline ->
  list_field j "criteria" skill_criterion_of_yojson >>= fun criteria ->
  let sourcing_strings = match list_field j "sourcing_strings" sourcing_string_of_yojson with
    | Ok ss -> ss | Error _ -> [] in
  let title_synonyms = match field j "title_synonyms" with
    | Ok ts -> (match title_synonyms_of_yojson ts with Ok p -> p | Error _ -> [])
    | Error _ -> [] in
  let seniority_signals = match list_field j "seniority_signals" seniority_signal_of_yojson with
    | Ok ss -> ss | Error _ -> [] in
  let interview_stages = match list_field j "interview_stages" interview_stage_of_yojson with
    | Ok ss -> ss | Error _ -> [] in
  let red_flags = match field j "red_flags" with
    | Ok (`List fs) -> List.filter_map (function `String s -> Some s | _ -> None) fs
    | _ -> [] in
  let comp_ranges = match list_field j "comp_ranges" comp_range_of_yojson with
    | Ok cr -> cr | Error _ -> [] in
  Ok { id; discipline; criteria; sourcing_strings; title_synonyms;
       seniority_signals; interview_stages; red_flags; comp_ranges }

(* skill_summary *)
let skill_summary_to_yojson (s : skill_summary) =
  `Assoc [
    "id", `String s.id;
    "name", `String s.name;
    "category", discipline_category_to_yojson s.category;
    "description", `String s.description;
    "criteria_count", `Int s.criteria_count;
  ]

(* recommendation_tier *)
let recommendation_tier_to_yojson = function
  | StrongProgress -> `String "StrongProgress"
  | Progress -> `String "Progress"
  | Maybe -> `String "Maybe"
  | Reject -> `String "Reject"

(* tier_score *)
let tier_score_to_yojson (ts : tier_score) =
  `Assoc [
    "tier", skill_tier_to_yojson ts.tier;
    "met", `Int ts.met;
    "total", `Int ts.total;
    "score", `Float ts.score;
  ]

(* criterion_result *)
let criterion_result_to_yojson (cr : criterion_result) =
  `Assoc [
    "criterion", skill_criterion_to_yojson cr.criterion;
    "met", `Bool cr.met;
    "matched_keywords", `List (List.map (fun s -> `String s) cr.matched_keywords);
  ]

(* scoring_result — explicit annotation to avoid candidate_score inference *)
let scoring_result_to_yojson (r : scoring_result) =
  `Assoc [
    "candidate_id", `String r.candidate_id;
    "discipline_id", `String r.discipline_id;
    "seniority_assessed", seniority_level_to_yojson r.seniority_assessed;
    "tier_scores", `List (List.map tier_score_to_yojson r.tier_scores);
    "overall_score", `Float r.overall_score;
    "recommendation", recommendation_tier_to_yojson r.recommendation;
    "red_flags_hit", `List (List.map (fun s -> `String s) r.red_flags_hit);
    "criterion_results", `List (List.map criterion_result_to_yojson r.criterion_results);
  ]

(* scoring_request *)
let scoring_request_of_yojson j =
  let open Result in
  str_field j "candidate_id" >>= fun candidate_id ->
  str_field j "discipline_id" >>= fun discipline_id ->
  field j "seniority" >>= seniority_level_of_yojson >>= fun seniority ->
  str_field j "candidate_notes" >>= fun candidate_notes ->
  Ok { candidate_id; discipline_id; seniority; candidate_notes }

(* sourcing_query *)
let sourcing_query_of_yojson j =
  let open Result in
  str_field j "discipline_id" >>= fun discipline_id ->
  field j "seniority" >>= seniority_level_of_yojson >>= fun seniority ->
  let platform =
    match field j "platform" with
    | Ok `Null | Error _ -> Ok None
    | Ok v -> sourcing_platform_of_yojson v |> Result.map (fun p -> Some p)
  in
  platform >>= fun platform ->
  Ok { discipline_id; seniority; platform }

(* sourcing_result *)
let sourcing_result_to_yojson (r : sourcing_result) =
  `Assoc [
    "discipline_id", `String r.discipline_id;
    "seniority", seniority_level_to_yojson r.seniority;
    "strings", `List (List.map sourcing_string_to_yojson r.strings);
  ]

(* ── Trust codecs ── *)

let trust_status_to_yojson = function
  | TrustPending    -> `String "pending"
  | TrustClean      -> `String "clean"
  | TrustSuspicious -> `String "suspicious"

let trust_status_of_string = function
  | "clean"      -> TrustClean
  | "suspicious" -> TrustSuspicious
  | _            -> TrustPending

let trust_check_to_yojson (tc : trust_check) =
  `Assoc [
    "trust_status", trust_status_to_yojson tc.trust_status;
    "trust_flags",  `List (List.map (fun s -> `String s) tc.trust_flags);
    "checked_at",   `String tc.checked_at;
  ]

let trust_check_of_yojson j =
  let open Result in
  str_field j "trust_status" >>= fun st ->
  let trust_status = trust_status_of_string st in
  let trust_flags = match field j "trust_flags" with
    | Ok (`List flags) -> List.filter_map (function `String s -> Some s | _ -> None) flags
    | _ -> [] in
  str_field j "checked_at" >>= fun checked_at ->
  Ok { trust_status; trust_flags; checked_at }

(* ── Pool codecs ── *)

let ats_stage_to_yojson = function
  | Screening -> `String "Screening"
  | Interview -> `String "Interview"
  | FinalRound -> `String "FinalRound"
  | Offer -> `String "Offer"
  | Hired -> `String "Hired"
  | Rejected -> `String "Rejected"
  | Withdrawn -> `String "Withdrawn"

let ats_stage_of_yojson = function
  | `String "Screening" -> Ok Screening
  | `String "Interview" -> Ok Interview
  | `String "FinalRound" -> Ok FinalRound
  | `String "Offer" -> Ok Offer
  | `String "Hired" -> Ok Hired
  | `String "Rejected" -> Ok Rejected
  | `String "Withdrawn" -> Ok Withdrawn
  | j -> Error ("expected ats_stage, got: " ^ Yojson.Safe.to_string j)

let float_field j key =
  match field j key with
  | Ok (`Float f) -> Ok f
  | Ok (`Int n) -> Ok (float_of_int n)
  | Ok v -> Error (key ^ ": expected float, got " ^ Yojson.Safe.to_string v)
  | Error e -> Error e

let tier_score_of_yojson j =
  let open Result in
  field j "tier" >>= skill_tier_of_yojson >>= fun tier ->
  int_field j "met" >>= fun met ->
  int_field j "total" >>= fun total ->
  float_field j "score" >>= fun score ->
  Ok { tier; met; total; score }

let criterion_result_of_yojson j =
  let open Result in
  field j "criterion" >>= skill_criterion_of_yojson >>= fun criterion ->
  let met = match field j "met" with Ok (`Bool b) -> b | _ -> false in
  list_field j "matched_keywords"
    (function `String s -> Ok s | v -> Error ("expected string, got " ^ Yojson.Safe.to_string v))
  >>= fun matched_keywords ->
  Ok { criterion; met; matched_keywords }

let recommendation_tier_of_yojson = function
  | `String "StrongProgress" -> Ok StrongProgress
  | `String "Progress"       -> Ok Progress
  | `String "Maybe"          -> Ok Maybe
  | `String "Reject"         -> Ok Reject
  | j -> Error ("expected recommendation_tier, got: " ^ Yojson.Safe.to_string j)

let candidate_score_to_yojson (cs : candidate_score) =
  `Assoc [
    "role_id",          `String cs.role_id;
    "role_name",        `String cs.role_name;
    "seniority",        seniority_level_to_yojson cs.seniority;
    "overall_score",    `Float cs.overall_score;
    "recommendation",   recommendation_tier_to_yojson cs.recommendation;
    "tier_scores",      `List (List.map tier_score_to_yojson cs.tier_scores);
    "red_flags_hit",    `List (List.map (fun s -> `String s) cs.red_flags_hit);
    "criterion_results",`List (List.map criterion_result_to_yojson cs.criterion_results);
    "scored_at",        `String cs.scored_at;
  ]

let candidate_score_of_yojson j =
  let open Result in
  str_field j "role_id" >>= fun role_id ->
  str_field j "role_name" >>= fun role_name ->
  field j "seniority" >>= seniority_level_of_yojson >>= fun seniority ->
  float_field j "overall_score" >>= fun overall_score ->
  field j "recommendation" >>= recommendation_tier_of_yojson >>= fun recommendation ->
  list_field j "tier_scores" tier_score_of_yojson >>= fun tier_scores ->
  list_field j "red_flags_hit"
    (function `String s -> Ok s | v -> Error (Yojson.Safe.to_string v))
  >>= fun red_flags_hit ->
  list_field j "criterion_results" criterion_result_of_yojson >>= fun criterion_results ->
  str_field j "scored_at" >>= fun scored_at ->
  Ok { role_id; role_name; seniority; overall_score; recommendation;
       tier_scores; red_flags_hit; criterion_results; scored_at }

let candidate_record_to_yojson (c : candidate_record) =
  `Assoc [
    "id",          `String c.id;
    "name",        `String c.name;
    "ats_stage",   ats_stage_to_yojson c.ats_stage;
    "scores",      `List (List.map candidate_score_to_yojson c.scores);
    "source_text", `String c.source_text;
    "created_at",  `String c.created_at;
    "updated_at",  `String c.updated_at;
    "greenhouse_url", (match c.greenhouse_url with None -> `Null | Some s -> `String s);
    "greenhouse_application_id", (match c.greenhouse_application_id with None -> `Null | Some s -> `String s);
    "trust_check", (match c.trust_check with None -> `Null | Some tc -> trust_check_to_yojson tc);
  ]

let candidate_record_of_yojson j =
  let open Result in
  str_field j "id" >>= fun id ->
  str_field j "name" >>= fun name ->
  field j "ats_stage" >>= ats_stage_of_yojson >>= fun ats_stage ->
  list_field j "scores" candidate_score_of_yojson >>= fun scores ->
  str_field j "source_text" >>= fun source_text ->
  str_field j "created_at" >>= fun created_at ->
  str_field j "updated_at" >>= fun updated_at ->
  let greenhouse_url = match field j "greenhouse_url" with
    | Ok (`String s) -> Some s | _ -> None in
  let greenhouse_application_id = match field j "greenhouse_application_id" with
    | Ok (`String s) -> Some s | _ -> None in
  let trust_check = match field j "trust_check" with
    | Ok (`Assoc _ as tc_j) -> (match trust_check_of_yojson tc_j with Ok tc -> Some tc | _ -> None)
    | _ -> None in
  Ok { id; name; ats_stage; scores; source_text; created_at; updated_at;
       greenhouse_url; greenhouse_application_id; trust_check }

let candidate_summary_to_yojson (cs : candidate_summary) =
  `Assoc [
    "id",               `String cs.id;
    "name",             `String cs.name;
    "ats_stage",        ats_stage_to_yojson cs.ats_stage;
    "top_role_id",      `String cs.top_role_id;
    "top_role_name",    `String cs.top_role_name;
    "top_score",        `Float cs.top_score;
    "top_recommendation", recommendation_tier_to_yojson cs.top_recommendation;
    "score_count",      `Int cs.score_count;
    "role_count",       `Int cs.role_count;
    "scored_role_ids",  `List (List.map (fun s -> `String s) cs.scored_role_ids);
    "created_at",       `String cs.created_at;
    "trust_status",     trust_status_to_yojson cs.trust_status;
    "trust_flags",      `List (List.map (fun s -> `String s) cs.trust_flags);
  ]

let playbook_match_to_yojson (m : playbook_match) =
  `Assoc [
    "playbook_id",   `String m.playbook_id;
    "playbook_name", `String m.playbook_name;
    "confidence",    `Float m.confidence;
    "rationale",     `String m.rationale;
  ]

let classify_result_to_yojson (r : classify_result) =
  `Assoc [
    "matches", `List (List.map playbook_match_to_yojson r.matches);
  ]

let pool_save_request_of_yojson j =
  let open Result in
  str_field j "candidate_name" >>= fun candidate_name ->
  str_field j "discipline_id" >>= fun discipline_id ->
  field j "seniority" >>= seniority_level_of_yojson >>= fun seniority ->
  str_field j "candidate_notes" >>= fun candidate_notes ->
  field j "ats_stage" >>= ats_stage_of_yojson >>= fun ats_stage ->
  Ok { candidate_name; discipline_id; seniority; candidate_notes; ats_stage }

let update_stage_request_of_yojson j =
  let open Result in
  field j "stage" >>= ats_stage_of_yojson >>= fun stage ->
  Ok { stage }

let criterion_signal_to_yojson (s : criterion_signal) =
  `Assoc [
    "criterion_text",  `String s.criterion_text;
    "tier",            skill_tier_to_yojson s.tier;
    "hired_hit_rate",  `Float s.hired_hit_rate;
    "all_hit_rate",    `Float s.all_hit_rate;
    "signal_strength", `Float s.signal_strength;
    "sample_count",    `Int s.sample_count;
  ]

let role_calibration_to_yojson (rc : role_calibration) =
  `Assoc [
    "role_id",      `String rc.role_id;
    "total_scored", `Int rc.total_scored;
    "hired_count",  `Int rc.hired_count;
    "signals",      `List (List.map criterion_signal_to_yojson rc.signals);
  ]

let skill_match_to_yojson (sm : skill_match) =
  `Assoc [
    "candidate_id",   `String sm.candidate_id;
    "candidate_name", `String sm.candidate_name;
    "ats_stage",      ats_stage_to_yojson sm.ats_stage;
    "role_id",        `String sm.role_id;
    "role_name",      `String sm.role_name;
    "overall_score",  `Float sm.overall_score;
    "scored_at",      `String sm.scored_at;
  ]

let pool_stats_to_yojson (ps : pool_stats) =
  `Assoc [
    "total_candidates", `Int ps.total_candidates;
    "roles_covered",    `Int ps.roles_covered;
    "hired_count",      `Int ps.hired_count;
    "active_count",     `Int ps.active_count;
  ]

(* ── AI Sourcing codecs ── *)

let sourcing_candidate_to_yojson (c : sourcing_candidate) =
  `Assoc [
    "name",        `String c.name;
    "profile_url", `String c.profile_url;
    "rationale",   `String c.rationale;
  ]

let sourcing_candidate_of_yojson j =
  let open Result in
  str_field j "name"        >>= fun name ->
  str_field j "profile_url" >>= fun profile_url ->
  str_field j "rationale"   >>= fun rationale ->
  Ok { name; profile_url; rationale }

let ai_boolean_string_to_yojson (b : ai_boolean_string) =
  `Assoc [
    "platform", `String b.platform;
    "label",    `String b.label;
    "query",    `String b.query;
  ]

let ai_boolean_string_of_yojson j =
  let open Result in
  str_field j "platform" >>= fun platform ->
  str_field j "label"    >>= fun label ->
  str_field j "query"    >>= fun query ->
  Ok { platform; label; query }

let ai_sourcing_result_to_yojson (r : ai_sourcing_result) =
  `Assoc [
    "session_id",        `String r.session_id;
    "role_id",           `String r.role_id;
    "role_name",         `String r.role_name;
    "seniority",         seniority_level_to_yojson r.seniority;
    "candidates",        `List (List.map sourcing_candidate_to_yojson r.candidates);
    "boolean_strings",   `List (List.map ai_boolean_string_to_yojson r.boolean_strings);
    "target_companies",  `List (List.map (fun s -> `String s) r.target_companies);
    "outreach_template", `String r.outreach_template;
    "ran_at",            `String r.ran_at;
  ]

let ai_sourcing_result_of_yojson j =
  let open Result in
  str_field j "session_id"        >>= fun session_id ->
  str_field j "role_id"           >>= fun role_id ->
  str_field j "role_name"         >>= fun role_name ->
  field j "seniority" >>= seniority_level_of_yojson >>= fun seniority ->
  list_field j "candidates" sourcing_candidate_of_yojson >>= fun candidates ->
  list_field j "boolean_strings" ai_boolean_string_of_yojson >>= fun boolean_strings ->
  list_field j "target_companies"
    (function `String s -> Ok s | v -> Error (Yojson.Safe.to_string v))
  >>= fun target_companies ->
  str_field j "outreach_template" >>= fun outreach_template ->
  str_field j "ran_at"            >>= fun ran_at ->
  Ok { session_id; role_id; role_name; seniority; candidates;
       boolean_strings; target_companies; outreach_template; ran_at }

(* ── Integration Settings codecs ── *)

let integration_settings_to_yojson (s : integration_settings) =
  `Assoc [
    "greenhouse_subdomain", `String s.greenhouse_subdomain;
    "greenhouse_api_key",   `String s.greenhouse_api_key;
    "ai_provider",          `String s.ai_provider;
    "ai_api_key",           `String s.ai_api_key;
  ]

let integration_settings_of_yojson j =
  let open Result in
  str_field j "greenhouse_subdomain" >>= fun greenhouse_subdomain ->
  str_field j "greenhouse_api_key"   >>= fun greenhouse_api_key ->
  let ai_provider = match str_field j "ai_provider" with Ok s -> s | Error _ -> "anthropic" in
  let ai_api_key  = match str_field j "ai_api_key"  with Ok s -> s | Error _ -> "" in
  Ok { greenhouse_subdomain; greenhouse_api_key; ai_provider; ai_api_key }

let settings_status_to_yojson (s : settings_status) =
  `Assoc [
    "anthropic_key_set",     `Bool s.anthropic_key_set;
    "greenhouse_configured", `Bool s.greenhouse_configured;
  ]

(* ── Company Profile codecs ── *)

let company_profile_to_yojson (p : company_profile) =
  `Assoc [
    "company_name",       `String p.company_name;
    "company_urls",       `List (List.map (fun s -> `String s) p.company_urls);
    "company_brief",      `String p.company_brief;
    "brief_generated_at", (match p.brief_generated_at with
                           | None -> `Null
                           | Some s -> `String s);
  ]

let company_profile_of_yojson j =
  let open Result in
  str_field j "company_name" >>= fun company_name ->
  list_field j "company_urls"
    (function `String s -> Ok s | v -> Error (Yojson.Safe.to_string v))
  >>= fun company_urls ->
  str_field j "company_brief" >>= fun company_brief ->
  let brief_generated_at =
    match field j "brief_generated_at" with
    | Ok (`String s) -> Some s
    | _ -> None
  in
  Ok { company_name; company_urls; company_brief; brief_generated_at }

let ai_sourcing_request_of_yojson j =
  let open Result in
  str_field j "role_id"           >>= fun role_id ->
  field j "seniority" >>= seniority_level_of_yojson >>= fun seniority ->
  str_field j "context"           >>= fun context ->
  Ok { role_id; seniority; context }

(* ── Greenhouse Sync State codecs ── *)

let greenhouse_sync_state_to_yojson (s : greenhouse_sync_state) =
  `Assoc [
    "last_synced_at", (match s.last_synced_at with None -> `Null | Some t -> `String t);
    "total_synced",   `Int s.total_synced;
    "last_error",     (match s.last_error with None -> `Null | Some e -> `String e);
  ]

let greenhouse_sync_state_of_yojson j =
  let last_synced_at = match field j "last_synced_at" with
    | Ok (`String s) -> Some s | _ -> None in
  let total_synced = match int_field j "total_synced" with
    | Ok n -> n | Error _ -> 0 in
  let last_error = match field j "last_error" with
    | Ok (`String s) -> Some s | _ -> None in
  Ok { last_synced_at; total_synced; last_error }

let user_record_to_yojson (u : user_record) =
  `Assoc [
    "id",            `String u.id;
    "email",         `String u.email;
    "password_hash", `String u.password_hash;
    "company_id",    `String u.company_id;
    "created_at",    `String u.created_at;
  ]

let user_record_of_yojson j =
  match str_field j "id", str_field j "email", str_field j "password_hash",
        str_field j "company_id", str_field j "created_at" with
  | Ok id, Ok email, Ok password_hash, Ok company_id, Ok created_at ->
    Ok { id; email; password_hash; company_id; created_at }
  | _ -> Error "user_record_of_yojson"

let company_record_to_yojson (c : company_record) =
  `Assoc [
    "id",         `String c.id;
    "domain",     `String c.domain;
    "name",       `String c.name;
    "created_at", `String c.created_at;
  ]

let company_record_of_yojson j =
  match str_field j "id", str_field j "domain",
        str_field j "name", str_field j "created_at" with
  | Ok id, Ok domain, Ok name, Ok created_at ->
    Ok { id; domain; name; created_at }
  | _ -> Error "company_record_of_yojson"

let session_record_to_yojson (s : session_record) =
  `Assoc [
    "token",      `String s.token;
    "user_id",    `String s.user_id;
    "company_id", `String s.company_id;
    "created_at", `String s.created_at;
  ]

let session_record_of_yojson j =
  match str_field j "token", str_field j "user_id",
        str_field j "company_id", str_field j "created_at" with
  | Ok token, Ok user_id, Ok company_id, Ok created_at ->
    Ok { token; user_id; company_id; created_at }
  | _ -> Error "session_record_of_yojson"
