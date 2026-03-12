open Lwt.Infix
open Roladeck_types.Types

module Store    = Roladeck_storage.Storage
module GH       = Roladeck_greenhouse.Greenhouse_client
module Skills   = Roladeck_skills_data.Skills_registry
module Sc       = Roladeck_scoring.Scoring
module Classify = Roladeck_ai.Classify

let poll_interval_s = 300.0  (* 5 minutes *)

let is_playbook (s : skill_record) =
  let id = s.id in
  let n = String.length id and m = String.length "-hiring-" in
  let found = ref false in
  for i = 0 to n - m do
    if String.sub id i m = "-hiring-" then found := true
  done;
  !found

let contains_sub s sub =
  let n = String.length s and m = String.length sub in
  if m = 0 then true
  else
    let found = ref false in
    for i = 0 to n - m do
      if String.sub s i m = sub then found := true
    done;
    !found

(* Guess seniority from a job title string *)
let detect_seniority title =
  let t = String.lowercase_ascii title in
  if contains_sub t "principal"     then Principal
  else if contains_sub t "staff"    then Staff
  else if contains_sub t "senior manager" || contains_sub t "sr. manager" then SeniorManager
  else if contains_sub t "director" then Director
  else if contains_sub t " vp "     then VP
  else if contains_sub t "manager"  then Manager
  else if contains_sub t "coordinator" then Coordinator
  else if contains_sub t "senior"   || contains_sub t "sr." then Senior
  else if contains_sub t "junior"   || contains_sub t "jr." then Junior
  else if contains_sub t " mid "    then Mid
  else Senior

(* Build source text for scoring from Greenhouse candidate data *)
let build_source_text (c : GH.gh_candidate) (app : GH.gh_application) =
  let buf = Buffer.create 512 in
  let add s =
    let s = String.trim s in
    if String.length s > 0 then (Buffer.add_string buf s; Buffer.add_char buf '\n')
  in
  let full_name = String.trim (c.first_name ^ " " ^ c.last_name) in
  if String.length full_name > 0 then add full_name;
  if String.length c.headline > 0 then add c.headline;
  if String.length c.company > 0 then add ("Current company: " ^ c.company);
  List.iter (fun job_name ->
    add ("Applied for: " ^ job_name)
  ) app.job_names;
  List.iter (fun (e : GH.gh_employment) ->
    let line = e.title ^ (if String.length e.company_name > 0 then " at " ^ e.company_name else "") in
    let line =
      if String.length e.start_date > 0 then
        line ^ " (" ^ e.start_date ^
        (if String.length e.end_date > 0 then " - " ^ e.end_date else " - present") ^ ")"
      else line
    in
    add line
  ) c.employments;
  List.iter (fun (e : GH.gh_education) ->
    let line = e.school_name ^
      (if String.length e.degree > 0 then " - " ^ e.degree else "") in
    add line
  ) c.educations;
  Buffer.contents buf

(* Process one Greenhouse application: score and save to pool *)
let process_application ~company_id ~api_key (app : GH.gh_application) =
  let app_id_str = string_of_int app.id in
  match Store.get_by_greenhouse_id ~company_id app_id_str with
  | Some _ -> Lwt.return 0  (* already in pool *)
  | None ->
    GH.get_candidate ~api_key app.candidate_id >>= function
    | Error _ -> Lwt.return 0
    | Ok cand ->
      let source_text = build_source_text cand app in
      let all_title   = String.concat " " app.job_names ^ " " ^ cand.headline in
      let seniority   = detect_seniority all_title in
      let name =
        let n = String.trim (cand.first_name ^ " " ^ cand.last_name) in
        if String.length n = 0 then "Candidate" else n
      in
      let%lwt cl = Classify.classify_candidate ~company_id ~candidate_notes:source_text in
      let now = Store.now_iso () in
      let scores =
        List.filter_map (fun (m : playbook_match) ->
          match List.find_opt (fun (s : skill_record) -> s.id = m.playbook_id) Skills.all_skills with
          | None -> None
          | Some skill ->
            let score_req : scoring_request = {
              candidate_id    = name;
              discipline_id   = skill.id;
              seniority;
              candidate_notes = source_text;
            } in
            let result = Sc.score_candidate skill score_req in
            let cs : candidate_score = {
              role_id          = skill.id;
              role_name        = skill.discipline.name;
              seniority;
              overall_score    = result.overall_score;
              recommendation   = result.recommendation;
              tier_scores      = result.tier_scores;
              red_flags_hit    = result.red_flags_hit;
              criterion_results= result.criterion_results;
              scored_at        = now;
            } in
            Some cs
        ) cl.matches
      in
      let id = Store.generate_id () in
      let gh_url = Printf.sprintf "https://app.greenhouse.io/people/%d?application_id=%d"
        cand.id app.id in
      let record : candidate_record = {
        id;
        name;
        ats_stage = Screening;
        scores;
        source_text;
        created_at = now;
        updated_at = now;
        greenhouse_url = Some gh_url;
        greenhouse_application_id = Some app_id_str;
        trust_check = None;
      } in
      Store.upsert ~company_id record;
      Lwt.return 1

(* Run one full sync pass for a specific company *)
let sync_once ~company_id ~api_key () =
  let state = Store.load_greenhouse_sync_state ~company_id () in
  GH.get_applications ~api_key ?since:state.last_synced_at () >>= function
  | Error e ->
    Store.save_greenhouse_sync_state ~company_id { state with last_error = Some e };
    Lwt.return (Error e)
  | Ok apps ->
    let now = Store.now_iso () in
    (* Process all applications sequentially to avoid hammering Greenhouse API *)
    let counts = List.map (fun app ->
      process_application ~company_id ~api_key app
    ) apps in
    let%lwt total =
      Lwt_list.fold_left_s (fun acc t ->
        t >>= fun n -> Lwt.return (acc + n)
      ) 0 counts
    in
    let new_state : greenhouse_sync_state = {
      last_synced_at = Some now;
      total_synced   = state.total_synced + total;
      last_error     = None;
    } in
    Store.save_greenhouse_sync_state ~company_id new_state;
    Lwt.return (Ok total)

(* Run sync for all companies that have Greenhouse configured *)
let sync_all_companies () =
  let companies = Store.load_companies () in
  Lwt_list.iter_s (fun (c : company_record) ->
    let settings = Store.load_integration_settings ~company_id:c.id () in
    if String.length settings.greenhouse_api_key > 0 then
      Lwt.catch
        (fun () ->
          sync_once ~company_id:c.id ~api_key:settings.greenhouse_api_key () >>= function
          | Ok n ->
            if n > 0 then
              Printf.printf "Greenhouse sync [%s]: imported %d candidate(s)\n%!" c.id n;
            Lwt.return ()
          | Error e ->
            Printf.eprintf "Greenhouse sync error [%s]: %s\n%!" c.id e;
            Lwt.return ())
        (fun exn ->
          Printf.eprintf "Greenhouse sync exception [%s]: %s\n%!" c.id (Printexc.to_string exn);
          Lwt.return ())
    else Lwt.return ()
  ) companies

(* Background polling loop — call once at startup *)
let start_sync_loop () =
  let rec loop () =
    Lwt.catch
      (fun () -> sync_all_companies ())
      (fun exn ->
        Printf.eprintf "Greenhouse sync loop exception: %s\n%!" (Printexc.to_string exn);
        Lwt.return ()) >>= fun () ->
    Lwt_unix.sleep poll_interval_s >>= fun () ->
    loop ()
  in
  Lwt.async loop
