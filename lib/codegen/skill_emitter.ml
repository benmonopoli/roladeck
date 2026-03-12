open Roladeck_types.Types
open Roladeck_skill_parser.Skill_parser

(** OCaml string literal escaping *)
let escape_string s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | '"' -> Buffer.add_string buf "\\\""
    | '\\' -> Buffer.add_string buf "\\\\"
    | '\n' -> Buffer.add_string buf "\\n"
    | '\r' -> Buffer.add_string buf "\\r"
    | '\t' -> Buffer.add_string buf "\\t"
    | c -> Buffer.add_char buf c
  ) s;
  Buffer.contents buf

let emit_string buf s =
  Buffer.add_char buf '"';
  Buffer.add_string buf (escape_string s);
  Buffer.add_char buf '"'

let emit_category buf = function
  | Tech -> Buffer.add_string buf "Tech"
  | Marketing -> Buffer.add_string buf "Marketing"
  | Sales -> Buffer.add_string buf "Sales"

let emit_seniority buf = function
  | Junior -> Buffer.add_string buf "Junior"
  | Mid -> Buffer.add_string buf "Mid"
  | Senior -> Buffer.add_string buf "Senior"
  | Staff -> Buffer.add_string buf "Staff"
  | Principal -> Buffer.add_string buf "Principal"
  | Coordinator -> Buffer.add_string buf "Coordinator"
  | Manager -> Buffer.add_string buf "Manager"
  | SeniorManager -> Buffer.add_string buf "SeniorManager"
  | Director -> Buffer.add_string buf "Director"
  | VP -> Buffer.add_string buf "VP"
  | CMO -> Buffer.add_string buf "CMO"
  | CRO -> Buffer.add_string buf "CRO"

let emit_tier buf = function
  | T1_MustHave -> Buffer.add_string buf "T1_MustHave"
  | T2_Differentiator -> Buffer.add_string buf "T2_Differentiator"
  | T3_RareUpside -> Buffer.add_string buf "T3_RareUpside"

let emit_platform buf = function
  | LinkedIn_XRay -> Buffer.add_string buf "LinkedIn_XRay"
  | GitHub_XRay -> Buffer.add_string buf "GitHub_XRay"
  | GitHub_Search -> Buffer.add_string buf "GitHub_Search"
  | ArXiv -> Buffer.add_string buf "ArXiv"
  | Custom s ->
    Buffer.add_string buf "(Custom ";
    emit_string buf s;
    Buffer.add_char buf ')'

let emit_criterion buf { text; tier } =
  Buffer.add_string buf "{ text = ";
  emit_string buf text;
  Buffer.add_string buf "; tier = ";
  emit_tier buf tier;
  Buffer.add_string buf " }"

let emit_sourcing buf ({ platform; label; query } : sourcing_string) =
  Buffer.add_string buf "{ platform = ";
  emit_platform buf platform;
  Buffer.add_string buf "; label = ";
  emit_string buf label;
  Buffer.add_string buf "; query = ";
  emit_string buf query;
  Buffer.add_string buf " }"

let emit_seniority_signal buf { from_level; to_level; signal_text } =
  Buffer.add_string buf "{ from_level = ";
  emit_seniority buf from_level;
  Buffer.add_string buf "; to_level = ";
  emit_seniority buf to_level;
  Buffer.add_string buf "; signal_text = ";
  emit_string buf signal_text;
  Buffer.add_string buf " }"

let emit_interview_stage buf { stage_name; format; assessing } =
  Buffer.add_string buf "{ stage_name = ";
  emit_string buf stage_name;
  Buffer.add_string buf "; format = ";
  emit_string buf format;
  Buffer.add_string buf "; assessing = ";
  emit_string buf assessing;
  Buffer.add_string buf " }"

let emit_comp_range buf { level; base_min; base_max } =
  Buffer.add_string buf "{ level = ";
  emit_seniority buf level;
  Buffer.add_string buf "; base_min = ";
  Buffer.add_string buf (string_of_int base_min);
  Buffer.add_string buf "; base_max = ";
  Buffer.add_string buf (string_of_int base_max);
  Buffer.add_string buf " }"

let emit_list emit_item buf lst =
  Buffer.add_char buf '[';
  List.iteri (fun i item ->
    if i > 0 then Buffer.add_string buf "; ";
    emit_item buf item
  ) lst;
  Buffer.add_char buf ']'

let emit_string_list = emit_list emit_string

let emit_title_synonyms buf lst =
  Buffer.add_char buf '[';
  List.iteri (fun i (level, titles) ->
    if i > 0 then Buffer.add_string buf "; ";
    Buffer.add_char buf '(';
    emit_seniority buf level;
    Buffer.add_string buf ", ";
    emit_string_list buf titles;
    Buffer.add_char buf ')'
  ) lst;
  Buffer.add_char buf ']'

(** Emit a single skill_record as an OCaml expression *)
let emit_skill buf (ps : parsed_skill) =
  let d_id = ps.ps_id in
  Buffer.add_string buf "{\n";
  Buffer.add_string buf "  id = "; emit_string buf ps.ps_id; Buffer.add_string buf ";\n";
  Buffer.add_string buf "  discipline = {\n";
  Buffer.add_string buf "    id = "; emit_string buf d_id; Buffer.add_string buf ";\n";
  Buffer.add_string buf "    name = "; emit_string buf ps.ps_name; Buffer.add_string buf ";\n";
  Buffer.add_string buf "    category = "; emit_category buf ps.ps_category; Buffer.add_string buf ";\n";
  Buffer.add_string buf "    description = "; emit_string buf ps.ps_description; Buffer.add_string buf ";\n";
  Buffer.add_string buf "  };\n";
  Buffer.add_string buf "  criteria = ";
  emit_list emit_criterion buf ps.ps_criteria;
  Buffer.add_string buf ";\n";
  Buffer.add_string buf "  sourcing_strings = ";
  emit_list emit_sourcing buf ps.ps_sourcing;
  Buffer.add_string buf ";\n";
  Buffer.add_string buf "  title_synonyms = ";
  emit_title_synonyms buf ps.ps_title_synonyms;
  Buffer.add_string buf ";\n";
  Buffer.add_string buf "  seniority_signals = ";
  emit_list emit_seniority_signal buf ps.ps_seniority_signals;
  Buffer.add_string buf ";\n";
  Buffer.add_string buf "  interview_stages = ";
  emit_list emit_interview_stage buf ps.ps_interview_stages;
  Buffer.add_string buf ";\n";
  Buffer.add_string buf "  red_flags = ";
  emit_string_list buf ps.ps_red_flags;
  Buffer.add_string buf ";\n";
  Buffer.add_string buf "  comp_ranges = ";
  emit_list emit_comp_range buf ps.ps_comp_ranges;
  Buffer.add_string buf ";\n";
  Buffer.add_string buf "}"

(** Emit the full skills_registry.ml file *)
let emit_registry buf (skills : parsed_skill list) =
  Buffer.add_string buf "(* AUTO-GENERATED by lib/codegen/gen_skills.ml — do not edit *)\n";
  Buffer.add_string buf "open Roladeck_types.Types\n\n";
  Buffer.add_string buf "let all_skills : skill_record list = [\n";
  List.iteri (fun i ps ->
    if i > 0 then Buffer.add_string buf ";\n";
    Buffer.add_string buf "  ";
    emit_skill buf ps
  ) skills;
  Buffer.add_string buf "\n]\n\n";
  Buffer.add_string buf "let find_by_id id =\n";
  Buffer.add_string buf "  List.find_opt (fun (s : skill_record) -> s.id = id) all_skills\n\n";
  Buffer.add_string buf "let contains_substr hay needle =\n";
  Buffer.add_string buf "  let hlen = String.length hay and nlen = String.length needle in\n";
  Buffer.add_string buf "  if nlen = 0 then true\n";
  Buffer.add_string buf "  else if nlen > hlen then false\n";
  Buffer.add_string buf "  else\n";
  Buffer.add_string buf "    let found = ref false in\n";
  Buffer.add_string buf "    for i = 0 to hlen - nlen do\n";
  Buffer.add_string buf "      if String.sub hay i nlen = needle then found := true\n";
  Buffer.add_string buf "    done;\n";
  Buffer.add_string buf "    !found\n\n";
  Buffer.add_string buf "let search query =\n";
  Buffer.add_string buf "  let q = String.lowercase_ascii query in\n";
  Buffer.add_string buf "  List.filter (fun (s : skill_record) ->\n";
  Buffer.add_string buf "    let name_l = String.lowercase_ascii s.discipline.name in\n";
  Buffer.add_string buf "    let desc_l = String.lowercase_ascii s.discipline.description in\n";
  Buffer.add_string buf "    let id_l = String.lowercase_ascii s.id in\n";
  Buffer.add_string buf "    contains_substr name_l q || contains_substr desc_l q || contains_substr id_l q\n";
  Buffer.add_string buf "  ) all_skills\n"
