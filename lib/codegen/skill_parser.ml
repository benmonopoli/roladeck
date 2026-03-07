open Ahrefs_types.Types

(** Intermediate representation during parsing *)
type parsed_skill = {
  ps_id : string;
  ps_name : string;
  ps_category : discipline_category;
  ps_description : string;
  ps_criteria : skill_criterion list;
  ps_sourcing : sourcing_string list;
  ps_title_synonyms : (seniority_level * string list) list;
  ps_seniority_signals : seniority_signal list;
  ps_interview_stages : interview_stage list;
  ps_red_flags : string list;
  ps_comp_ranges : comp_range list;
}

let default_parsed id name cat desc = {
  ps_id = id;
  ps_name = name;
  ps_category = cat;
  ps_description = desc;
  ps_criteria = [];
  ps_sourcing = [];
  ps_title_synonyms = [];
  ps_seniority_signals = [];
  ps_interview_stages = [];
  ps_red_flags = [];
  ps_comp_ranges = [];
}

(** Infer discipline category from skill id *)
let infer_category id =
  if String.length id >= 4 && String.sub id 0 4 = "tech" then Tech
  else if String.length id >= 9 && String.sub id 0 9 = "marketing" then Marketing
  else if String.length id >= 5 && String.sub id 0 5 = "sales" then Sales
  else Tech

(** Extract frontmatter field from YAML-like header *)
let extract_frontmatter_field field content =
  let re = Str.regexp (Printf.sprintf "%s: \"\\([^\"]*\\)\"" field) in
  try
    ignore (Str.search_forward re content 0);
    Some (Str.matched_group 1 content)
  with Not_found ->
    let re2 = Str.regexp (Printf.sprintf "%s: \\([^\n]*\\)" field) in
    try
      ignore (Str.search_forward re2 content 0);
      let v = String.trim (Str.matched_group 1 content) in
      Some v
    with Not_found -> None

(** Parse seniority level from string *)
let parse_seniority_level s =
  let s = String.lowercase_ascii (String.trim s) in
  match s with
  | "junior" -> Some Junior
  | "mid" -> Some Mid
  | "senior" -> Some Senior
  | "staff" | "staff+" -> Some Staff
  | "principal" | "principal / distinguished" -> Some Principal
  | "coordinator" -> Some Coordinator
  | "manager" -> Some Manager
  | "senior manager" -> Some SeniorManager
  | "director" -> Some Director
  | "vp" -> Some VP
  | "cmo" -> Some CMO
  | "cro" -> Some CRO
  | _ -> None

(** Parse a tier bullet point like "- Python for data pipelines..." *)
let parse_criterion_line tier line =
  let line = String.trim line in
  if String.length line = 0 then None
  else
    let text =
      if String.length line > 2 && line.[0] = '-' then
        String.trim (String.sub line 1 (String.length line - 1))
      else line
    in
    if String.length text = 0 then None
    else Some { text; tier }

(** Split string by newline *)
let split_lines s =
  String.split_on_char '\n' s

(** Trim whitespace from both ends *)
let trim s = String.trim s

(** Check if line starts with a given prefix (case-insensitive) *)
let starts_with_ci prefix line =
  let n = String.length prefix in
  String.length line >= n &&
  String.lowercase_ascii (String.sub line 0 n) = String.lowercase_ascii prefix

(** Parse skill tiers section from markdown text *)
let parse_skill_tiers text =
  let lines = split_lines text in
  let criteria = ref [] in
  let current_tier = ref None in
  List.iter (fun line ->
    let tl = String.lowercase_ascii (trim line) in
    if starts_with_ci "**tier 1" tl || starts_with_ci "**t1" tl ||
       starts_with_ci "tier 1" tl || tl = "**tier 1 — must-have:**" ||
       (String.length tl > 10 && starts_with_ci "**tier 1" tl) then
      current_tier := Some T1_MustHave
    else if starts_with_ci "**tier 2" tl || starts_with_ci "**t2" tl ||
            starts_with_ci "tier 2" tl then
      current_tier := Some T2_Differentiator
    else if starts_with_ci "**tier 3" tl || starts_with_ci "**t3" tl ||
            starts_with_ci "tier 3" tl then
      current_tier := Some T3_RareUpside
    else begin
      let trimmed = trim line in
      (* Stop at any new section heading *)
      if String.length trimmed > 0 && trimmed.[0] = '#' then
        current_tier := None
      else
      match !current_tier with
      | Some tier ->
        if String.length trimmed > 0 && trimmed.[0] = '-' then begin
          match parse_criterion_line tier trimmed with
          | Some c -> criteria := c :: !criteria
          | None -> ()
        end
      | None -> ()
    end
  ) lines;
  List.rev !criteria

(** Parse sourcing strings from a code block in markdown *)
let parse_sourcing_block label query_text =
  (* Detect platform from label or query *)
  let platform =
    let ql = String.lowercase_ascii query_text in
    let ll = String.lowercase_ascii label in
    if String.length ll >= 6 && String.sub ll 0 6 = "github" then
      if String.length ll > 12 && String.sub ll 7 5 = "searc" then GitHub_Search
      else GitHub_XRay
    else if String.length ll >= 8 && String.sub ll 0 8 = "linkedin" then LinkedIn_XRay
    else if String.length ll >= 5 && String.sub ll 0 5 = "arxiv" then ArXiv
    else if Str.string_match (Str.regexp ".*site:linkedin\\.com.*") ql 0 then LinkedIn_XRay
    else if Str.string_match (Str.regexp ".*site:github\\.com.*") ql 0 then GitHub_XRay
    else if Str.string_match (Str.regexp ".*arxiv\\.org.*") ql 0 then ArXiv
    else Custom label
  in
  ({ platform; label; query = String.trim query_text } : sourcing_string)

(** Extract sourcing strings from markdown content *)
let parse_sourcing_strings text =
  (* Find "### Sourcing Strings" section *)
  let lines = split_lines text in
  let in_sourcing = ref false in
  let in_code_block = ref false in
  let code_label = ref "Search" in
  let code_lines = ref [] in
  let results = ref [] in

  let flush_code () =
    if !code_lines <> [] then begin
      (* Split by comment lines starting with # to get labeled blocks *)
      let all = List.rev !code_lines in
      let current_label = ref !code_label in
      let current_block = ref [] in
      let flush_block () =
        if !current_block <> [] then begin
          let q = String.concat "\n" (List.rev !current_block) in
          let q = String.trim q in
          if String.length q > 0 then
            results := (parse_sourcing_block !current_label q) :: !results;
          current_block := []
        end
      in
      List.iter (fun line ->
        let tl = String.trim line in
        if String.length tl > 1 && tl.[0] = '#' then begin
          flush_block ();
          current_label := String.trim (String.sub tl 1 (String.length tl - 1))
        end else if String.length tl > 0 then
          current_block := line :: !current_block
      ) all;
      flush_block ();
      code_lines := []
    end
  in

  List.iter (fun line ->
    let tl = String.lowercase_ascii (trim line) in
    if not !in_sourcing then begin
      if starts_with_ci "### sourcing" tl || starts_with_ci "## sourcing" tl then
        in_sourcing := true
    end else begin
      (* Stop at next major section — but not if inside a code block *)
      if not !in_code_block && String.length tl > 0 && tl.[0] = '#' &&
         not (starts_with_ci "### sourcing" tl || starts_with_ci "## sourcing" tl) then
        in_sourcing := false
      else if starts_with_ci "```" (trim line) then begin
        if !in_code_block then begin
          flush_code ();
          in_code_block := false
        end else begin
          in_code_block := true;
          (* Extract language hint as label *)
          let rest = String.sub (trim line) 3 (String.length (trim line) - 3) in
          let rest = String.trim rest in
          code_label := if String.length rest > 0 then rest else "Search"
        end
      end else if !in_code_block then
        code_lines := line :: !code_lines
    end
  ) lines;
  flush_code ();
  List.rev !results

(** Parse red flags from markdown *)
let parse_red_flags text =
  let lines = split_lines text in
  let in_section = ref false in
  let flags = ref [] in
  List.iter (fun line ->
    let tl = String.lowercase_ascii (trim line) in
    if starts_with_ci "### red flag" tl || starts_with_ci "## red flag" tl then
      in_section := true
    else if !in_section then begin
      if String.length tl > 0 && tl.[0] = '#' then
        in_section := false
      else begin
        let t = trim line in
        if String.length t > 1 && t.[0] = '-' then
          flags := String.trim (String.sub t 1 (String.length t - 1)) :: !flags
      end
    end
  ) lines;
  List.rev !flags

(** Parse compensation table from markdown *)
let parse_comp_ranges text =
  let lines = split_lines text in
  let in_section = ref false in
  let ranges = ref [] in
  List.iter (fun line ->
    let tl = String.lowercase_ascii (trim line) in
    if starts_with_ci "### compensation" tl || starts_with_ci "## compensation" tl then
      in_section := true
    else if !in_section then begin
      if String.length tl > 0 && tl.[0] = '#' then
        in_section := false
      else begin
        (* Look for table rows: | Junior | $95k – $135k | *)
        let t = trim line in
        if String.length t > 0 && t.[0] = '|' then begin
          let parts = String.split_on_char '|' t in
          let parts = List.map trim parts in
          let parts = List.filter (fun s -> String.length s > 0) parts in
          match parts with
          | level_str :: range_str :: _ -> begin
              let level_str = trim level_str in
              (* Remove markdown bold markers *)
              let level_str = Str.global_replace (Str.regexp "\\*\\*") "" level_str in
              let level_str = trim level_str in
              match parse_seniority_level level_str with
              | Some level ->
                (* Parse range like "$95k – $135k" or "$95k - $135k" *)
                let range_str = trim range_str in
                let re = Str.regexp "\\$\\([0-9]+\\)k[^0-9]*\\([0-9]+\\)k" in
                if Str.string_match re range_str 0 then begin
                  let lo = int_of_string (Str.matched_group 1 range_str) * 1000 in
                  let hi = int_of_string (Str.matched_group 2 range_str) * 1000 in
                  ranges := { level; base_min = lo; base_max = hi } :: !ranges
                end
              | None -> ()
            end
          | _ -> ()
        end
      end
    end
  ) lines;
  List.rev !ranges

(** Parse title synonyms table *)
let parse_title_synonyms text =
  let lines = split_lines text in
  let in_section = ref false in
  let synonyms = ref [] in
  List.iter (fun line ->
    let tl = String.lowercase_ascii (trim line) in
    if starts_with_ci "### titles" tl || starts_with_ci "## titles" tl ||
       starts_with_ci "### title" tl then
      in_section := true
    else if !in_section then begin
      if String.length tl > 0 && tl.[0] = '#' then
        in_section := false
      else begin
        let t = trim line in
        if String.length t > 0 && t.[0] = '|' then begin
          let parts = String.split_on_char '|' t in
          let parts = List.map trim parts in
          let parts = List.filter (fun s -> String.length s > 0) parts in
          match parts with
          | level_str :: titles_str :: _ -> begin
              let level_str = trim level_str in
              let level_str = Str.global_replace (Str.regexp "\\*\\*") "" level_str in
              let level_str = trim level_str in
              match parse_seniority_level level_str with
              | Some level ->
                let titles = String.split_on_char ',' titles_str in
                let titles = List.map trim titles in
                let titles = List.filter (fun s -> String.length s > 0) titles in
                if titles <> [] then
                  synonyms := (level, titles) :: !synonyms
              | None -> ()
            end
          | _ -> ()
        end
      end
    end
  ) lines;
  List.rev !synonyms

(** Parse seniority signals *)
let parse_seniority_signals text =
  let lines = split_lines text in
  let in_section = ref false in
  let signals = ref [] in
  List.iter (fun line ->
    let tl = String.lowercase_ascii (trim line) in
    if starts_with_ci "### seniority signal" tl then
      in_section := true
    else if !in_section then begin
      if String.length tl > 0 && tl.[0] = '#' then
        in_section := false
      else begin
        let t = trim line in
        (* Lines like "- **Junior → Mid:** signal text" *)
        if String.length t > 1 && t.[0] = '-' then begin
          let body = String.trim (String.sub t 1 (String.length t - 1)) in
          (* Try to find "Level → Level:" pattern *)
          let re = Str.regexp "\\*\\*\\([^→]+\\)→ *\\([^:]+\\):\\*\\* *\\(.*\\)" in
          if Str.string_match re body 0 then begin
            let from_s = trim (Str.matched_group 1 body) in
            let to_s = trim (Str.matched_group 2 body) in
            let signal_text = trim (Str.matched_group 3 body) in
            match parse_seniority_level from_s, parse_seniority_level to_s with
            | Some from_level, Some to_level ->
              signals := { from_level; to_level; signal_text } :: !signals
            | _ -> ()
          end
        end
      end
    end
  ) lines;
  List.rev !signals

(** Parse interview stages from table *)
let parse_interview_stages text =
  let lines = split_lines text in
  let in_section = ref false in
  let stages = ref [] in
  let in_table = ref false in
  List.iter (fun line ->
    let tl = String.lowercase_ascii (trim line) in
    if starts_with_ci "### interview" tl || starts_with_ci "## interview" tl then begin
      in_section := true;
      in_table := false
    end else if !in_section then begin
      if String.length tl > 0 && tl.[0] = '#' then begin
        in_section := false;
        in_table := false
      end else begin
        let t = trim line in
        if String.length t > 0 && t.[0] = '|' then begin
          let parts = String.split_on_char '|' t in
          let parts = List.map trim parts in
          let parts = List.filter (fun s -> String.length s > 0) parts in
          match parts with
          | stage :: fmt :: assessing :: _ ->
            let stage = Str.global_replace (Str.regexp "\\*\\*") "" stage in
            let stage = trim stage in
            (* Skip header/separator rows *)
            if stage <> "---" && not (String.lowercase_ascii stage = "stage") then begin
              in_table := true;
              stages := { stage_name = stage; format = trim fmt; assessing = trim assessing } :: !stages
            end
          | _ -> ()
        end else if !in_table && String.length tl > 0 && tl.[0] <> '|' then
          in_table := false
      end
    end
  ) lines;
  List.rev !stages

(** Extract display name from the H1 heading in the markdown body *)
let extract_display_name content =
  let lines = split_lines content in
  let rec find = function
    | [] -> None
    | line :: rest ->
      let t = trim line in
      if String.length t > 2 && t.[0] = '#' && t.[1] <> '#' then begin
        (* Strip leading # characters and spaces *)
        let i = ref 0 in
        while !i < String.length t && (t.[!i] = '#' || t.[!i] = ' ') do incr i done;
        let text = String.sub t !i (String.length t - !i) in
        (* Strip " — Hiring Guide" and " Skill" suffixes *)
        let text =
          let suf = " — Hiring Guide" in
          if String.length text > String.length suf &&
             String.sub text (String.length text - String.length suf) (String.length suf) = suf
          then String.sub text 0 (String.length text - String.length suf)
          else text
        in
        let text =
          let suf = " Skill" in
          if String.length text > String.length suf &&
             String.sub text (String.length text - String.length suf) (String.length suf) = suf
          then String.sub text 0 (String.length text - String.length suf)
          else text
        in
        let text = trim text in
        if String.length text > 0 then Some text
        else find rest
      end else
        find rest
  in
  find lines

(** Main parse function: path → skill_id → parsed_skill option *)
let parse_skill_file path skill_id =
  try
    let ic = open_in path in
    let n = in_channel_length ic in
    let content = Bytes.create n in
    really_input ic content 0 n;
    close_in ic;
    let content = Bytes.to_string content in

    (* Extract frontmatter — prefer H1 heading as display name *)
    let name =
      match extract_display_name content with
      | Some n -> n
      | None ->
        match extract_frontmatter_field "name" content with
        | Some n -> n
        | None -> skill_id
    in
    let description =
      match extract_frontmatter_field "description" content with
      | Some d ->
        (* Truncate long descriptions *)
        if String.length d > 300 then String.sub d 0 300 ^ "..."
        else d
      | None -> ""
    in
    let category = infer_category skill_id in

    let criteria = parse_skill_tiers content in
    let sourcing = parse_sourcing_strings content in
    let red_flags = parse_red_flags content in
    let comp_ranges = parse_comp_ranges content in
    let title_synonyms = parse_title_synonyms content in
    let seniority_signals = parse_seniority_signals content in
    let interview_stages = parse_interview_stages content in

    Some {
      ps_id = skill_id;
      ps_name = name;
      ps_category = category;
      ps_description = description;
      ps_criteria = criteria;
      ps_sourcing = sourcing;
      ps_title_synonyms = title_synonyms;
      ps_seniority_signals = seniority_signals;
      ps_interview_stages = interview_stages;
      ps_red_flags = red_flags;
      ps_comp_ranges = comp_ranges;
    }
  with e ->
    Printf.eprintf "Warning: failed to parse %s: %s\n%!" path (Printexc.to_string e);
    None
