open Ahrefs_types.Types

let contains_sub s sub =
  let n = String.length s and m = String.length sub in
  if m = 0 then true
  else
    let found = ref false in
    for i = 0 to n - m do
      if String.sub s i m = sub then found := true
    done;
    !found

let is_playbook (s : skill_record) = contains_sub s.id "-hiring-"

let cat_label = function
  | Tech -> "Tech" | Marketing -> "Marketing" | Sales -> "Sales"

let criteria_sample (criteria : skill_criterion list) tier n =
  criteria
  |> List.filter (fun (c : skill_criterion) -> c.tier = tier)
  |> (fun cs -> let rec take acc k = function
    | [] -> List.rev acc
    | _ when k = 0 -> List.rev acc
    | x :: rest -> take (x :: acc) (k-1) rest
    in take [] n cs)
  |> List.map (fun (c : skill_criterion) -> "    - " ^ c.text)
  |> String.concat "\n"

let build_prompt (candidate_notes : string) =
  let playbooks =
    Ahrefs_skills_data.Skills_registry.all_skills
    |> List.filter is_playbook
  in
  let entries =
    playbooks
    |> List.map (fun (s : skill_record) ->
        let desc =
          if String.length s.discipline.description > 120
          then String.sub s.discipline.description 0 120 ^ "..."
          else s.discipline.description
        in
        let t1 = criteria_sample s.criteria T1_MustHave 3 in
        let t1_block = if String.length t1 > 0 then "\n" ^ t1 else "" in
        Printf.sprintf "  id:%s | %s (%s)\n  %s%s"
          s.id s.discipline.name (cat_label s.discipline.category) desc t1_block
      )
    |> String.concat "\n"
  in
  let system =
    "You are a talent intelligence system. Your job is to identify what a \
candidate genuinely specialises in based on their real experience and career \
history — not the job they happened to apply for.\n\n\
A candidate can match multiple playbooks. For example, someone with 10 years \
in product marketing but who started in content marketing may score highly \
against both.\n\n\
Given the candidate profile, identify which playbooks from the library below \
best describe what this person IS. Focus on their actual skills, experience \
patterns, and career trajectory.\n\n\
Return a JSON object with a \"matches\" array of 1-3 playbooks ordered by \
confidence (highest first). Only include matches with confidence >= 0.5.\n\n\
Each match must have:\n\
  playbook_id: exact id from the list\n\
  playbook_name: exact name from the list\n\
  confidence: float 0.0-1.0 (0.9+ clear fit, 0.7-0.9 strong, 0.5-0.7 plausible)\n\
  rationale: one sentence citing specific evidence from their profile\n\n\
If nothing fits clearly, return {\"matches\": []}.\n\n\
Playbook library:\n" ^ entries
  in
  let user = "Candidate profile:\n\n" ^ candidate_notes ^ "\n\nClassify this candidate." in
  (system, user)

let extract_json text =
  let len = String.length text in
  let rec find_open i =
    if i >= len then None
    else if text.[i] = '{' then Some i
    else find_open (i + 1)
  in
  match find_open 0 with
  | None -> None
  | Some start ->
    let rec find_close i depth =
      if i >= len then None
      else match text.[i] with
        | '{' -> find_close (i + 1) (depth + 1)
        | '}' -> if depth = 1 then Some i else find_close (i + 1) (depth - 1)
        | _ -> find_close (i + 1) depth
    in
    (match find_close (start + 1) 1 with
    | None -> None
    | Some end_ -> Some (String.sub text start (end_ - start + 1)))

(* Validate that a playbook_id exists in the registry *)
let valid_ids =
  lazy (
    Ahrefs_skills_data.Skills_registry.all_skills
    |> List.filter is_playbook
    |> List.map (fun (s : skill_record) -> s.id)
  )

let parse_response text =
  match extract_json text with
  | None -> { matches = [] }
  | Some json_str ->
    (try
      let j = Yojson.Safe.from_string json_str in
      let open Yojson.Safe.Util in
      let ids = Lazy.force valid_ids in
      let matches =
        j |> member "matches" |> to_list
        |> List.filter_map (fun m ->
            try
              let playbook_id = m |> member "playbook_id" |> to_string in
              let confidence  = m |> member "confidence"  |> to_number in
              if confidence < 0.5 || not (List.mem playbook_id ids) then None
              else Some {
                playbook_id;
                playbook_name = m |> member "playbook_name" |> to_string;
                confidence;
                rationale     = m |> member "rationale"     |> to_string;
              }
            with _ -> None)
      in
      { matches }
    with _ -> { matches = [] })

let classify_candidate ~company_id ~candidate_notes =
  if String.length (String.trim candidate_notes) < 30
  then Lwt.return { matches = [] }
  else
    let (system, user_prompt) = build_prompt candidate_notes in
    Anthropic_client.run_ai ~company_id ~system ~user_prompt ()
    |> Lwt.map (function
      | Error _ -> { matches = [] }
      | Ok text -> parse_response text)
