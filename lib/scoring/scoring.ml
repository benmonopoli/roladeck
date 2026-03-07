open Ahrefs_types.Types

let tier_weight = function
  | T1_MustHave -> 1.0
  | T2_Differentiator -> 0.6
  | T3_RareUpside -> 0.2

(** Ahrefs-specific enrichment: extra criteria added at score time *)
let ahrefs_enrichments = [
  ("tech-hiring-backend-engineering",
   [{ text = "Functional programming (OCaml, Haskell, F#)"; tier = T2_Differentiator }]);
  ("tech-hiring-search-crawler-infrastructure",
   [{ text = "OCaml production experience"; tier = T3_RareUpside }]);
]

let apply_enrichment discipline_id base_criteria =
  match List.assoc_opt discipline_id ahrefs_enrichments with
  | None -> base_criteria
  | Some extra -> base_criteria @ extra

(** Tokenize text: lowercase, split on non-alphanumeric *)
let tokenize s =
  let s = String.lowercase_ascii s in
  let words = ref [] in
  let buf = Buffer.create 16 in
  String.iter (fun c ->
    if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then
      Buffer.add_char buf c
    else begin
      let w = Buffer.contents buf in
      if String.length w >= 3 then words := w :: !words;
      Buffer.clear buf
    end
  ) s;
  let w = Buffer.contents buf in
  if String.length w >= 3 then words := w :: !words;
  List.rev !words

(** Keyword overlap between criterion text and candidate notes *)
let keyword_overlap criterion_text notes =
  let criterion_tokens = tokenize criterion_text in
  let notes_tokens = tokenize notes in
  let notes_set = Hashtbl.create 32 in
  List.iter (fun t -> Hashtbl.replace notes_set t true) notes_tokens;
  List.filter (fun t -> Hashtbl.mem notes_set t) criterion_tokens

(** Criterion is "met" if >= 2 keywords overlap *)
let criterion_met criterion notes =
  let overlap = keyword_overlap criterion.text notes in
  List.length overlap >= 2, overlap

(** Check if a red flag text appears in candidate notes *)
let red_flag_hit flag_text notes =
  let fl = String.lowercase_ascii flag_text in
  let nl = String.lowercase_ascii notes in
  (* Check for any 3+ char word from flag appearing in notes *)
  let flag_tokens = tokenize fl in
  let notes_tokens = tokenize nl in
  let notes_set = Hashtbl.create 32 in
  List.iter (fun t -> Hashtbl.replace notes_set t true) notes_tokens;
  let hits = List.filter (fun t -> Hashtbl.mem notes_set t) flag_tokens in
  List.length hits >= 2

(** Compute per-tier scores *)
let compute_tier_scores criterion_results =
  let tier_data = Hashtbl.create 4 in
  List.iter (fun { criterion; met; _ } ->
    let t = criterion.tier in
    let (m, tot) = try Hashtbl.find tier_data t with Not_found -> (0, 0) in
    Hashtbl.replace tier_data t (m + (if met then 1 else 0), tot + 1)
  ) criterion_results;
  List.map (fun tier ->
    let (m, tot) = try Hashtbl.find tier_data tier with Not_found -> (0, 0) in
    let score = if tot = 0 then 1.0 else float_of_int m /. float_of_int tot in
    { tier; met = m; total = tot; score }
  ) [T1_MustHave; T2_Differentiator; T3_RareUpside]

(** Weighted overall score *)
let compute_overall_score tier_scores =
  let total_weight = ref 0.0 in
  let weighted_sum = ref 0.0 in
  List.iter (fun ts ->
    if ts.total > 0 then begin
      let w = tier_weight ts.tier in
      total_weight := !total_weight +. w;
      weighted_sum := !weighted_sum +. (w *. ts.score)
    end
  ) tier_scores;
  if !total_weight = 0.0 then 0.0
  else !weighted_sum /. !total_weight

(** T1 gap = any T1 criterion unmet *)
let has_t1_gap criterion_results =
  List.exists (fun { criterion; met; _ } ->
    criterion.tier = T1_MustHave && not met
  ) criterion_results

(** Derive recommendation from score + red flags *)
let derive_recommendation overall_score t1_gap red_flags_hit =
  let downgrade tier =
    match tier with
    | StrongProgress -> Progress
    | Progress -> Maybe
    | Maybe -> Reject
    | Reject -> Reject
  in
  let base =
    if overall_score >= 0.75 && not t1_gap then StrongProgress
    else if overall_score >= 0.55 then Progress
    else if overall_score >= 0.35 then Maybe
    else Reject
  in
  if red_flags_hit <> [] then downgrade base
  else base

(** Compute calibration: which criteria predict hiring for this role *)
let compute_calibration (pool : candidate_record list) (role_id : string) : role_calibration =
  let scored = List.filter_map (fun (c : candidate_record) ->
    let role_scores = List.filter (fun (s : candidate_score) -> s.role_id = role_id) c.scores in
    match role_scores with
    | [] -> None
    | _ ->
      let best = List.fold_left
        (fun (b : candidate_score) (s : candidate_score) -> if s.overall_score > b.overall_score then s else b)
        (List.hd role_scores) (List.tl role_scores) in
      Some (c.ats_stage, best)
  ) pool in
  let total = List.length scored in
  let hired_count = List.length (List.filter (fun (st, _) -> st = Hired) scored) in
  let signals =
    if total = 0 then []
    else
      let all_criteria =
        match scored with
        | [] -> []
        | (_, first) :: _ -> List.map (fun cr -> cr.criterion) first.criterion_results
      in
      if hired_count = 0 then
        List.map (fun (c : skill_criterion) -> {
          criterion_text = c.text; tier = c.tier;
          hired_hit_rate = 0.0; all_hit_rate = 0.0;
          signal_strength = 0.0; sample_count = total;
        }) all_criteria
      else
        List.map (fun (c : skill_criterion) ->
          let count_met filter =
            let relevant = List.filter (fun (st, _) -> filter st) scored in
            let n = List.length relevant in
            if n = 0 then 0.0
            else
              let m = List.length (List.filter (fun (_, score) ->
                List.exists (fun cr -> cr.criterion.text = c.text && cr.met)
                  score.criterion_results
              ) relevant) in
              float_of_int m /. float_of_int n
          in
          let hired_hit_rate = count_met (fun st -> st = Hired) in
          let all_hit_rate   = count_met (fun _ -> true) in
          { criterion_text = c.text; tier = c.tier;
            hired_hit_rate; all_hit_rate;
            signal_strength = hired_hit_rate -. all_hit_rate;
            sample_count = total; }
        ) all_criteria
  in
  { role_id; total_scored = total; hired_count; signals }

(** Main scoring entry point *)
let score_candidate (skill : skill_record) (req : scoring_request) =
  let criteria = apply_enrichment req.discipline_id skill.criteria in
  let criterion_results =
    List.map (fun criterion ->
      let met, matched_keywords = criterion_met criterion req.candidate_notes in
      { criterion; met; matched_keywords }
    ) criteria
  in
  let tier_scores = compute_tier_scores criterion_results in
  let overall_score = compute_overall_score tier_scores in
  let t1_gap = has_t1_gap criterion_results in
  let red_flags_hit =
    List.filter (fun flag -> red_flag_hit flag req.candidate_notes) skill.red_flags
  in
  let recommendation = derive_recommendation overall_score t1_gap red_flags_hit in
  {
    candidate_id = req.candidate_id;
    discipline_id = req.discipline_id;
    seniority_assessed = req.seniority;
    tier_scores;
    overall_score;
    recommendation;
    red_flags_hit;
    criterion_results;
  }
