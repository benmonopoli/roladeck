open Ahrefs_types.Types

let system_prompt = {|You are a company researcher. Your job is to build a comprehensive brief about a company to help a recruiter personalise their hiring outreach and AI sourcing.

You will be given one or more URLs as starting points. Use the web_search tool to:
1. Fetch those URLs to understand the company
2. Find the company's engineering/tech blog (if any)
3. Find key people in engineering, product, and leadership (names + roles)
4. Understand the tech stack and product areas
5. Find recent job postings to understand hiring priorities
6. Understand the company culture and values
7. Find any notable projects, open source work, or technical achievements

After your research, return ONLY a valid JSON object with exactly this schema (no markdown, no explanation, ASCII characters only):
{"company_name":"...","summary":"1-2 sentence overview","tech_stack":["..."],"key_people":[{"name":"...","role":"..."}],"culture_notes":"...","open_roles":["..."],"sourcing_notes":"..."}

The sourcing_notes field should be a 2-3 sentence paragraph explaining how to best source and approach candidates for this company — what signals to look for, what the culture is like, what candidates would find appealing about working there.|}

let run_company_research ?(company_id = "") (profile : company_profile) : (company_profile, string) result Lwt.t =
  let urls_text = String.concat "\n" (List.mapi (fun i u ->
    Printf.sprintf "%d. %s" (i+1) u
  ) profile.company_urls) in
  let user_prompt =
    Printf.sprintf "Research the company: %s\n\nStarting URLs:\n%s\n\nSearch these URLs and dig deeper to build a comprehensive company brief."
      profile.company_name
      urls_text
  in
  Lwt.catch
    (fun () ->
      let%lwt result = Anthropic_client.run_ai ~company_id ~system:system_prompt ~user_prompt () in
      match result with
      | Error e -> Lwt.return (Error e)
      | Ok text ->
        (* Find first { and last } to extract JSON *)
        let n = String.length text in
        let start =
          let i = ref 0 in
          while !i < n && text.[!i] <> '{' do incr i done;
          if !i < n then !i else (-1)
        in
        let stop =
          let i = ref (n - 1) in
          while !i >= 0 && text.[!i] <> '}' do decr i done;
          !i
        in
        if start < 0 || stop <= start then
          Lwt.return (Error "No JSON object found in AI response")
        else
          let json_str = String.sub text start (stop - start + 1) in
          (try
            let _json = Yojson.Safe.from_string json_str in
            let t = Unix.gettimeofday () in
            let tm = Unix.gmtime t in
            let now = Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
              (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
              tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
            in
            let updated = {
              profile with
              company_brief      = json_str;
              brief_generated_at = Some now;
            } in
            Lwt.return (Ok updated)
          with e ->
            Lwt.return (Error (Printf.sprintf "JSON parse error: %s" (Printexc.to_string e)))))
    (fun exn ->
      Lwt.return (Error (Printexc.to_string exn)))
