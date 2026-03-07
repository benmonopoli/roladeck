open Ahrefs_types.Types

(** Filter sourcing strings by platform preference *)
let filter_by_platform (strings : sourcing_string list) platform_opt =
  match platform_opt with
  | None -> strings
  | Some p ->
    let matches = List.filter (fun (s : sourcing_string) -> s.platform = p) strings in
    if matches = [] then strings (* fallback to all if none match *)
    else matches

(** Execute a sourcing query: returns relevant strings from a skill *)
let execute_query (skill : skill_record) (query : sourcing_query) =
  let filtered = filter_by_platform skill.sourcing_strings query.platform in
  (* Annotate with seniority level titles where relevant *)
  let seniority_titles =
    match List.assoc_opt query.seniority skill.title_synonyms with
    | None -> []
    | Some titles -> titles
  in
  (* If we have title synonyms for this seniority, inject as a note sourcing string *)
  let title_hint =
    if seniority_titles = [] then []
    else
      let label = Printf.sprintf "Title synonyms — %s"
        (match query.seniority with
         | Junior -> "Junior" | Mid -> "Mid" | Senior -> "Senior"
         | Staff -> "Staff" | Principal -> "Principal"
         | Coordinator -> "Coordinator" | Manager -> "Manager"
         | SeniorManager -> "Senior Manager" | Director -> "Director"
         | VP -> "VP" | CMO -> "CMO" | CRO -> "CRO") in
      [({ platform = Custom "Reference";
         label;
         query = String.concat ", " seniority_titles } : sourcing_string)]
  in
  {
    discipline_id = query.discipline_id;
    seniority = query.seniority;
    strings = title_hint @ filtered;
  }
