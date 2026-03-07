open Lwt.Infix

(* Provider configs *)
type provider_config = {
  api_url : string;
  model   : string;
  auth_header : string -> string;  (* api_key -> header value *)
}

let providers = [
  "openai", {
    api_url     = "https://api.openai.com/v1/chat/completions";
    model       = "gpt-4o";
    auth_header = (fun k -> "Bearer " ^ k);
  };
  "perplexity", {
    api_url     = "https://api.perplexity.ai/chat/completions";
    model       = "sonar-pro";
    auth_header = (fun k -> "Bearer " ^ k);
  };
]

let get_config provider =
  match List.assoc_opt provider providers with
  | Some c -> c
  | None   -> List.assoc "openai" providers

let get_api_key ?(api_key = "") provider =
  if String.length api_key > 0 then Ok api_key
  else
    let env_var = match provider with
      | "perplexity" -> "PERPLEXITY_API_KEY"
      | _            -> "OPENAI_API_KEY"
    in
    match Sys.getenv_opt env_var with
    | Some k when String.length k > 0 -> Ok k
    | _ -> Error (Printf.sprintf
        "API key not configured for %s. Add it in Settings → AI Configuration." provider)

let post_json ~url ~auth_header ~body_str =
  let uri = Uri.of_string url in
  let headers = Cohttp.Header.of_list [
    ("Authorization", auth_header);
    ("Content-Type",  "application/json");
  ] in
  let body = Cohttp_lwt.Body.of_string body_str in
  Lwt.catch
    (fun () ->
      Cohttp_lwt_unix.Client.post ~headers ~body uri >>= fun (resp, body) ->
      let code = Cohttp.Code.code_of_status (Cohttp.Response.status resp) in
      Cohttp_lwt.Body.to_string body >>= fun body_str ->
      if Cohttp.Code.is_success code then Lwt.return (Ok body_str)
      else Lwt.return (Error (Printf.sprintf "HTTP %d: %s" code body_str)))
    (fun exn ->
      Lwt.return (Error (Printf.sprintf "Network error: %s" (Printexc.to_string exn))))

let extract_text resp_str =
  try
    let json = Yojson.Safe.from_string resp_str in
    match json with
    | `Assoc kvs ->
      (match List.assoc_opt "choices" kvs with
       | Some (`List (choice :: _)) ->
         (match choice with
          | `Assoc ck ->
            (match List.assoc_opt "message" ck with
             | Some (`Assoc mk) ->
               (match List.assoc_opt "content" mk with
                | Some (`String t) -> Ok t
                | _ -> Error "No content in message")
             | _ -> Error "No message in choice")
          | _ -> Error "Invalid choice")
       | _ -> Error "No choices in response")
    | _ -> Error "Invalid response format"
  with e -> Error (Printf.sprintf "JSON parse error: %s" (Printexc.to_string e))

let run_completion ~system ~user_prompt ?(api_key = "") () =
  let provider = Sys.getenv_opt "AI_PROVIDER" |> Option.value ~default:"openai" in
  let cfg = get_config provider in
  match get_api_key ~api_key provider with
  | Error e -> Lwt.return (Error e)
  | Ok key ->
    let body = `Assoc [
      "model",      `String cfg.model;
      "max_tokens", `Int 4096;
      "messages",   `List [
        `Assoc ["role", `String "system"; "content", `String system];
        `Assoc ["role", `String "user";   "content", `String user_prompt];
      ];
    ] |> Yojson.Safe.to_string in
    post_json
      ~url:cfg.api_url
      ~auth_header:(cfg.auth_header key)
      ~body_str:body
    >>= function
    | Error e -> Lwt.return (Error e)
    | Ok resp  -> Lwt.return (extract_text resp)
