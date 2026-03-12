open Lwt.Infix

let api_url = "https://api.anthropic.com/v1/messages"
let model = "claude-sonnet-4-6"
let max_tokens = 8096

let get_api_key ?(api_key = "") () =
  if String.length api_key > 0 then Ok api_key
  else
    match Sys.getenv_opt "ANTHROPIC_API_KEY" with
    | Some k when String.length k > 0 -> Ok k
    | _ -> Error "Anthropic API key not set. Add it in Settings → AI Configuration."

(* Build the HTTP headers *)
let make_headers key =
  Cohttp.Header.of_list [
    ("x-api-key",         key);
    ("anthropic-version", "2023-06-01");
    ("content-type",      "application/json");
  ]

(* Serialise a messages-API request body *)
let build_body ~(messages : Yojson.Safe.t list) ~system =
  `Assoc [
    "model",      `String model;
    "max_tokens", `Int max_tokens;
    "system",     `String system;
    "tools",      `List [
      `Assoc [
        "type", `String "web_search_20250305";
        "name", `String "web_search";
      ]
    ];
    "messages",   `List messages;
  ]
  |> Yojson.Safe.to_string

(* Extract all text blocks from a content array *)
let extract_text_blocks content =
  match content with
  | `List blocks ->
    List.filter_map (fun b ->
      match b with
      | `Assoc kvs ->
        (match List.assoc_opt "type" kvs, List.assoc_opt "text" kvs with
         | Some (`String "text"), Some (`String t) -> Some t
         | _ -> None)
      | _ -> None
    ) blocks
  | _ -> []

(* Extract tool_use blocks from a content array, return their ids and names *)
let extract_tool_uses content =
  match content with
  | `List blocks ->
    List.filter_map (fun b ->
      match b with
      | `Assoc kvs ->
        (match List.assoc_opt "type" kvs, List.assoc_opt "id" kvs with
         | Some (`String "tool_use"), Some (`String id) ->
           Some id
         | _ -> None)
      | _ -> None
    ) blocks
  | _ -> []

(* Build a user message with tool_result blocks (empty content for server-side tools) *)
let make_tool_result_message tool_ids =
  let results = List.map (fun id ->
    `Assoc [
      "type",        `String "tool_result";
      "tool_use_id", `String id;
      "content",     `String "";
    ]
  ) tool_ids in
  `Assoc [
    "role",    `String "user";
    "content", `List results;
  ]

(* POST to the Anthropic API and return parsed response body *)
let post_request key body_str =
  let uri = Uri.of_string api_url in
  let headers = make_headers key in
  let body = Cohttp_lwt.Body.of_string body_str in
  Lwt.catch
    (fun () ->
      Cohttp_lwt_unix.Client.post ~headers ~body uri >>= fun (resp, body) ->
      let status = Cohttp.Response.status resp in
      Cohttp_lwt.Body.to_string body >>= fun body_str ->
      if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then
        Lwt.return (Ok body_str)
      else
        Lwt.return (Error (Printf.sprintf "HTTP %d: %s"
          (Cohttp.Code.code_of_status status) body_str)))
    (fun exn ->
      Lwt.return (Error (Printf.sprintf "Network error: %s" (Printexc.to_string exn))))

(* The agentic loop: POST → if tool_use, loop; if end_turn, return text *)
let rec agentic_loop key system messages =
  let body_str = build_body ~messages ~system in
  post_request key body_str >>= function
  | Error e -> Lwt.return (Error e)
  | Ok resp_str ->
    let json =
      try Ok (Yojson.Safe.from_string resp_str)
      with e -> Error ("JSON parse error: " ^ Printexc.to_string e)
    in
    match json with
    | Error e -> Lwt.return (Error e)
    | Ok json ->
      let stop_reason =
        match json with
        | `Assoc kvs ->
          (match List.assoc_opt "stop_reason" kvs with
           | Some (`String s) -> s
           | _ -> "unknown")
        | _ -> "unknown"
      in
      let content =
        match json with
        | `Assoc kvs ->
          (match List.assoc_opt "content" kvs with
           | Some c -> c
           | None -> `List [])
        | _ -> `List []
      in
      if stop_reason = "tool_use" then begin
        (* Append assistant message with this content, then tool_result user message *)
        let tool_ids = extract_tool_uses content in
        let assistant_msg = `Assoc [
          "role",    `String "assistant";
          "content", content;
        ] in
        let tool_result_msg = make_tool_result_message tool_ids in
        let messages' = messages @ [assistant_msg; tool_result_msg] in
        agentic_loop key system messages'
      end else begin
        (* end_turn — collect all text *)
        let texts = extract_text_blocks content in
        Lwt.return (Ok (String.concat "\n" texts))
      end

let run_agentic_loop ~system ~user_prompt ?(api_key = "") () =
  match get_api_key ~api_key () with
  | Error e -> Lwt.return (Error e)
  | Ok key ->
    let messages = [
      `Assoc [
        "role",    `String "user";
        "content", `String user_prompt;
      ]
    ] in
    agentic_loop key system messages

(* Dispatcher: pick client based on configured provider *)
let run_ai ?(company_id = "") ~system ~user_prompt () =
  let provider =
    if String.length company_id > 0 then
      let s = Roladeck_storage.Storage.load_integration_settings ~company_id () in
      s.Roladeck_types.Types.ai_provider
    else
      Sys.getenv_opt "AI_PROVIDER" |> Option.value ~default:"anthropic"
  in
  let api_key =
    if String.length company_id > 0 then
      let s = Roladeck_storage.Storage.load_integration_settings ~company_id () in
      s.Roladeck_types.Types.ai_api_key
    else ""
  in
  match provider with
  | "openai" -> Openai_client.run_completion ~system ~user_prompt ~api_key ()
  | "perplexity" -> Openai_client.run_completion ~system ~user_prompt ~api_key ()
  | _ -> run_agentic_loop ~system ~user_prompt ~api_key ()
