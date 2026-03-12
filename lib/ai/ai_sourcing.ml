open Roladeck_types.Types
open Roladeck_json.Types_j
open Roladeck_skills_data.Skills_registry

(* Generate a session id *)
let gen_session_id () =
  let t = int_of_float (Unix.gettimeofday () *. 1000.0) in
  Printf.sprintf "ai-%d-%06d" t (Random.int 999999)

let now_iso () =
  let t = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let seniority_to_string = function
  | Junior -> "Junior" | Mid -> "Mid" | Senior -> "Senior"
  | Staff -> "Staff" | Principal -> "Principal"
  | Coordinator -> "Coordinator" | Manager -> "Manager"
  | SeniorManager -> "Senior Manager" | Director -> "Director"
  | VP -> "VP" | CMO -> "CMO" | CRO -> "CRO"

(* Build criteria bullet list for a given tier *)
let criteria_bullets (criteria : skill_criterion list) (tier : skill_tier) =
  criteria
  |> List.filter (fun (c : skill_criterion) -> c.tier = tier)
  |> List.map (fun (c : skill_criterion) -> "  - " ^ c.text)
  |> String.concat "\n"

(* First N elements of a list *)
let take n lst =
  let rec aux acc k = function
    | [] -> List.rev acc
    | _ when k = 0 -> List.rev acc
    | x :: rest -> aux (x :: acc) (k - 1) rest
  in
  aux [] n lst

(* Get title synonyms for a specific seniority level *)
let titles_for_level (title_synonyms : (seniority_level * string list) list) level =
  match List.assoc_opt level title_synonyms with
  | None | Some [] -> []
  | Some ts -> ts

(* Build the sourcing-examples section *)
let sourcing_examples_section (strings : sourcing_string list) =
  strings
  |> take 5
  |> List.map (fun (s : sourcing_string) -> Printf.sprintf "  [%s] %s\n  %s" s.label s.label s.query)
  |> String.concat "\n\n"

let build_system_prompt (skill : skill_record) (seniority : seniority_level) =
  let role_name = skill.discipline.name in
  let seniority_str = seniority_to_string seniority in
  let description = skill.discipline.description in

  let t1_bullets = criteria_bullets skill.criteria T1_MustHave in
  let t2_bullets = criteria_bullets skill.criteria T2_Differentiator in

  let sourcing_ex = sourcing_examples_section skill.sourcing_strings in

  let titles = titles_for_level skill.title_synonyms seniority in
  let titles_str =
    if titles = [] then seniority_str ^ " " ^ role_name
    else String.concat ", " titles
  in

  let red_flags = take 8 skill.red_flags in
  let red_flags_str =
    red_flags
    |> List.map (fun f -> "  - " ^ f)
    |> String.concat "\n"
  in

  Printf.sprintf {|You are an expert technical recruiter. Your task is to source high-quality candidates for the following role.

ROLE: %s (%s level)
DESCRIPTION: %s

TIER 1 - MUST HAVE criteria:
%s

TIER 2 - DIFFERENTIATOR criteria:
%s

TITLE SYNONYMS at this level: %s

RED FLAGS to avoid:
%s

EXISTING SOURCING STRINGS (examples):
%s

SEARCH INSTRUCTIONS:
1. Search GitHub for active contributors to relevant open-source projects, maintainers with public repos showing relevant expertise.
2. Search conference speaker lists: StrangeLoop, ICFP, SREcon, QCon, CraftConf, dotScale, and similar for %s topics.
3. Search for technical blog authors and writers publishing about %s.
4. Use Google X-Ray searches for LinkedIn profiles: site:linkedin.com/in "<title>" "<skill>" location.
5. Look for candidates at companies known for excellence in this domain.

Find at least 5 real candidate profiles. For each, provide their actual name, a direct profile URL (GitHub, LinkedIn, or personal site), and a brief rationale explaining why they fit the T1/T2 criteria.

OUTPUT: Return ONLY valid JSON (no markdown fences, no code blocks, ASCII characters only):
{"candidates":[{"name":"Full Name","profile_url":"https://...","rationale":"Why they fit"}],"boolean_strings":[{"platform":"LinkedIn","label":"Senior Engineer Boolean","query":"(\"senior\" OR \"staff\") AND (\"skill1\" OR \"skill2\")"}],"target_companies":["Company1","Company2"],"outreach_template":"Hi [Name],\n\nI came across your work on [specific project] and was impressed by [specific thing].\n\nWe are hiring a %s %s at Ahrefs...\n\nWould you be open to a quick chat?\n\nBest,\n[Your name]"}|}
    role_name seniority_str description
    t1_bullets t2_bullets
    titles_str
    red_flags_str
    sourcing_ex
    description description
    seniority_str role_name

let build_user_prompt (req : ai_sourcing_request) (skill : skill_record) =
  let extra =
    if String.length req.context > 0 then
      "\n\nADDITIONAL CONTEXT FROM RECRUITER:\n" ^ req.context
    else ""
  in
  Printf.sprintf "Please source %s-level %s candidates for Ahrefs. Use web search to find real people with public profiles.%s"
    (seniority_to_string req.seniority) skill.discipline.name extra

(* Extract JSON object from text using brace-depth tracking.
   Finds the first '{' then walks forward counting opens/closes to find
   the matching '}'. This handles trailing content (code fences, prose)
   that would cause a first-to-last approach to return malformed JSON. *)
let extract_json_from_text text =
  let n = String.length text in
  let rec find_open i =
    if i >= n then None
    else if text.[i] = '{' then Some i
    else find_open (i + 1)
  in
  match find_open 0 with
  | None -> None
  | Some start ->
    let rec find_close i depth =
      if i >= n then None
      else match text.[i] with
        | '{' -> find_close (i + 1) (depth + 1)
        | '}' -> if depth = 1 then Some i else find_close (i + 1) (depth - 1)
        | _   -> find_close (i + 1) depth
    in
    (match find_close (start + 1) 1 with
     | None -> None
     | Some stop -> Some (String.sub text start (stop - start + 1)))

let parse_response ~session_id ~role_id ~role_name ~seniority text =
  match extract_json_from_text text with
  | None -> Error ("Could not find JSON in response: " ^ String.sub text 0 (min 200 (String.length text)))
  | Some json_str ->
    (match (try Ok (Yojson.Safe.from_string json_str) with e -> Error (Printexc.to_string e)) with
     | Error e -> Error ("JSON parse failed: " ^ e)
     | Ok json ->
       let get_str_list key =
         match Yojson.Safe.Util.member key json with
         | `List items -> List.filter_map (function `String s -> Some s | _ -> None) items
         | _ -> []
       in
       let candidates =
         match Yojson.Safe.Util.member "candidates" json with
         | `List items ->
           List.filter_map (fun item ->
             match sourcing_candidate_of_yojson item with
             | Ok c -> Some c
             | Error _ -> None
           ) items
         | _ -> []
       in
       let boolean_strings =
         match Yojson.Safe.Util.member "boolean_strings" json with
         | `List items ->
           List.filter_map (fun item ->
             match ai_boolean_string_of_yojson item with
             | Ok b -> Some b
             | Error _ -> None
           ) items
         | _ -> []
       in
       let target_companies = get_str_list "target_companies" in
       let outreach_template =
         match Yojson.Safe.Util.member "outreach_template" json with
         | `String s -> s
         | _ -> ""
       in
       Ok {
         session_id;
         role_id;
         role_name;
         seniority;
         candidates;
         boolean_strings;
         target_companies;
         outreach_template;
         ran_at = now_iso ();
       }
    )

let run_sourcing ?(company_id = "") (req : ai_sourcing_request) =
  let () = Random.self_init () in
  match find_by_id req.role_id with
  | None -> Lwt.return (Error (Printf.sprintf "Role not found: %s" req.role_id))
  | Some skill ->
    let system = build_system_prompt skill req.seniority in
    let user_prompt = build_user_prompt req skill in
    Anthropic_client.run_ai ~company_id ~system ~user_prompt ()
    |> Lwt.map (function
      | Error e -> Error e
      | Ok text ->
        let session_id = gen_session_id () in
        parse_response
          ~session_id
          ~role_id:req.role_id
          ~role_name:skill.discipline.name
          ~seniority:req.seniority
          text
    )
