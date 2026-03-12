open Roladeck_types.Types

let tier_weight = function
  | T1_MustHave -> 1.0
  | T2_Differentiator -> 0.6
  | T3_RareUpside -> 0.2

(** Ahrefs-specific enrichment: extra criteria added at score time *)
let roladeck_enrichments = [
  ("tech-hiring-backend-engineering",
   [{ text = "Functional programming (OCaml, Haskell, F#)"; tier = T2_Differentiator }]);
  ("tech-hiring-search-crawler-infrastructure",
   [{ text = "OCaml production experience"; tier = T3_RareUpside }]);
]

let apply_enrichment discipline_id base_criteria =
  match List.assoc_opt discipline_id roladeck_enrichments with
  | None -> base_criteria
  | Some extra -> base_criteria @ extra

(** Tokenize text: lowercase, split on non-alphanumeric, keep words >= 3 chars *)
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

(** Filler words common in hiring criteria that carry no evidential signal.
    Filtering these prevents "experience" or "knowledge" in candidate notes
    from contributing to a criterion match when the real term is absent. *)
let stop_words =
  let tbl = Hashtbl.create 64 in
  List.iter (fun w -> Hashtbl.replace tbl w ()) [
    (* Generic competency filler *)
    "experience"; "knowledge"; "understanding"; "ability"; "skills"; "skill";
    "familiarity"; "familiars"; "proficiency"; "proficient"; "competency";
    (* Qualifiers that mean nothing without the adjacent noun *)
    "strong"; "solid"; "proven"; "excellent"; "demonstrated"; "clear";
    "good"; "great"; "deep"; "broad"; "relevant"; "related"; "practical";
    "hands"; "handson"; "working"; "effective"; "comfortable";
    (* Career-arc filler *)
    "background"; "track"; "record"; "exposure"; "awareness"; "history";
    (* Time filler *)
    "years"; "year";
    (* Function words that survive the 3-char tokeniser filter *)
    "the"; "and"; "for"; "with"; "can"; "has"; "its"; "are"; "was";
    "all"; "any"; "not"; "our"; "use"; "used"; "able"; "will"; "well";
    "high"; "some"; "very"; "plus"; "via"; "etc"; "e.g"; "i.e";
  ];
  tbl

let is_stop w = Hashtbl.mem stop_words w

let significant_tokens tokens = List.filter (fun t -> not (is_stop t)) tokens

(** Build consecutive bigrams from a token list *)
let bigrams tokens =
  let rec go acc = function
    | a :: (b :: _ as rest) -> go ((a ^ " " ^ b) :: acc) rest
    | _ -> List.rev acc
  in
  go [] tokens

(** Normalise for phrase search: lowercase + replace punctuation with spaces
    so "machine-learning" matches the phrase "machine learning". *)
let normalise s =
  String.lowercase_ascii s
  |> String.map (fun c ->
       match c with
       | '-' | '_' | '/' | '\\' | '(' | ')' | ',' | '.' | '+' | ':' | ';' -> ' '
       | _ -> c)

let contains_phrase haystack phrase =
  let hlen = String.length haystack and plen = String.length phrase in
  if plen = 0 || plen > hlen then false
  else begin
    let found = ref false in
    for i = 0 to hlen - plen do
      if String.sub haystack i plen = phrase then found := true
    done;
    !found
  end

(** Keyword overlap between criterion text and candidate notes (all tokens) *)
let keyword_overlap criterion_text notes =
  let criterion_tokens = tokenize criterion_text in
  let notes_tokens = tokenize notes in
  let notes_set = Hashtbl.create 32 in
  List.iter (fun t -> Hashtbl.replace notes_set t true) notes_tokens;
  List.filter (fun t -> Hashtbl.mem notes_set t) criterion_tokens

(** Criterion is met using stop-word-aware matching with a phrase bonus.

    1. Extract significant (non-stop) tokens from the criterion.
    2. Phrase check: if any consecutive bigram of significant tokens appears
       verbatim in the candidate notes (after normalisation), the criterion
       is met. This catches "distributed systems" appearing as a phrase,
       which is strong evidence even if just one token matched elsewhere.
    3. Token fallback: count significant token overlap.
       - 0 significant tokens → fall back to old 2-token rule
       - 1 significant token  → need that 1 token to appear in notes
       - 2+ significant tokens → need at least 2 to appear

    The matched_keywords returned are the significant tokens that matched
    (or the raw overlap if no significant tokens matched), used for UI display. *)
let criterion_met criterion notes =
  let ct_sig = significant_tokens (tokenize criterion.text) in
  let n_sig = List.length ct_sig in
  let notes_norm = normalise notes in
  (* Phrase check: any sig bigram appearing verbatim is strong evidence *)
  let phrase_hit =
    bigrams ct_sig |> List.exists (fun phrase -> contains_phrase notes_norm phrase)
  in
  if phrase_hit then
    true, ct_sig
  else begin
    let overlap = keyword_overlap criterion.text notes in
    let sig_overlap = significant_tokens overlap in
    let n_sig_hit = List.length sig_overlap in
    let threshold =
      if n_sig = 0 then 2        (* no sig tokens: preserve old behaviour *)
      else if n_sig = 1 then 1   (* single meaningful term: require it *)
      else 2                     (* 2+ meaningful terms: require 2 to appear *)
    in
    let met = n_sig_hit >= threshold in
    met, (if sig_overlap <> [] then sig_overlap else overlap)
  end

(** Check if a red flag text appears in candidate notes *)
let red_flag_hit flag_text notes =
  let flag_tokens = tokenize (String.lowercase_ascii flag_text) in
  let notes_tokens = tokenize (String.lowercase_ascii notes) in
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
