(**
  gen_skills.ml — walks the open-recruit skills directory,
  parses each SKILL.md, and emits skills_registry.ml
*)

let () =
  let skills_dir = ref "" in
  let output = ref "" in

  let spec = [
    ("--skills-dir", Arg.Set_string skills_dir, "Path to open-recruit/skills");
    ("--output", Arg.Set_string output, "Output .ml file path");
  ] in
  Arg.parse spec (fun _ -> ()) "gen_skills [--skills-dir DIR] [--output FILE]";

  if !skills_dir = "" then begin
    Printf.eprintf "Error: --skills-dir required\n%!";
    exit 1
  end;
  if !output = "" then begin
    Printf.eprintf "Error: --output required\n%!";
    exit 1
  end;

  (* Walk directory recursively for SKILL.md files *)
  let rec find_skill_files dir acc =
    let entries =
      try Array.to_list (Sys.readdir dir)
      with _ -> []
    in
    List.fold_left (fun acc entry ->
      let path = Filename.concat dir entry in
      if Sys.is_directory path then
        find_skill_files path acc
      else if entry = "SKILL.md" then
        path :: acc
      else acc
    ) acc entries
  in

  let skill_files = find_skill_files !skills_dir [] in
  Printf.printf "Found %d SKILL.md files\n%!" (List.length skill_files);

  (* Parse each file — derive skill_id from directory name *)
  let parsed_skills =
    List.filter_map (fun path ->
      (* skill_id = immediate parent directory name *)
      let dir = Filename.dirname path in
      let skill_id = Filename.basename dir in
      Printf.printf "  Parsing %s\n%!" skill_id;
      Skill_parser.parse_skill_file path skill_id
    ) (List.sort String.compare skill_files)
  in

  Printf.printf "Successfully parsed %d skills\n%!" (List.length parsed_skills);

  (* Emit *)
  let buf = Buffer.create (256 * 1024) in
  Skill_emitter.emit_registry buf parsed_skills;

  let oc = open_out !output in
  Buffer.output_buffer oc buf;
  close_out oc;
  Printf.printf "Written to %s\n%!" !output
