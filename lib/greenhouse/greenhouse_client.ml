open Lwt.Infix

(* Base64 encode for Basic auth *)
let base64_encode s =
  let tbl = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" in
  let n = String.length s in
  let buf = Buffer.create ((n / 3 + 1) * 4) in
  let i = ref 0 in
  while !i + 2 < n do
    let b0 = Char.code s.[!i] and b1 = Char.code s.[!i+1] and b2 = Char.code s.[!i+2] in
    Buffer.add_char buf tbl.[b0 lsr 2];
    Buffer.add_char buf tbl.[((b0 land 3) lsl 4) lor (b1 lsr 4)];
    Buffer.add_char buf tbl.[((b1 land 15) lsl 2) lor (b2 lsr 6)];
    Buffer.add_char buf tbl.[b2 land 63];
    i := !i + 3
  done;
  if !i + 1 = n then begin
    let b0 = Char.code s.[!i] in
    Buffer.add_char buf tbl.[b0 lsr 2];
    Buffer.add_char buf tbl.[(b0 land 3) lsl 4];
    Buffer.add_string buf "=="
  end else if !i + 2 = n then begin
    let b0 = Char.code s.[!i] and b1 = Char.code s.[!i+1] in
    Buffer.add_char buf tbl.[b0 lsr 2];
    Buffer.add_char buf tbl.[((b0 land 3) lsl 4) lor (b1 lsr 4)];
    Buffer.add_char buf tbl.[(b1 land 15) lsl 2];
    Buffer.add_char buf '='
  end;
  Buffer.contents buf

type gh_employment = {
  company_name : string;
  title        : string;
  start_date   : string;
  end_date     : string;
}

type gh_education = {
  school_name : string;
  degree      : string;
}

type gh_candidate = {
  id          : int;
  first_name  : string;
  last_name   : string;
  headline    : string;
  company     : string;
  employments : gh_employment list;
  educations  : gh_education list;
}

type gh_application = {
  id           : int;
  candidate_id : int;
  job_names    : string list;
  applied_at   : string;
}

let str_of = function `String s -> s | _ -> ""

let get_f json key =
  match json with
  | `Assoc kvs -> (match List.assoc_opt key kvs with Some v -> v | None -> `Null)
  | _ -> `Null

let list_of f = function
  | `List items -> List.filter_map f items
  | _ -> []

let decode_employment json =
  let company_name = str_of (get_f json "company_name") in
  let title        = str_of (get_f json "title") in
  let start_date   = str_of (get_f json "start_date") in
  let end_date     = str_of (get_f json "end_date") in
  if String.length company_name > 0 || String.length title > 0
  then Some { company_name; title; start_date; end_date }
  else None

let decode_education json =
  let school_name = str_of (get_f json "school_name") in
  let degree      = str_of (get_f json "degree") in
  if String.length school_name > 0
  then Some { school_name; degree }
  else None

let decode_job json =
  match get_f json "name" with `String s -> Some s | _ -> None

let decode_application json =
  match get_f json "id", get_f json "candidate_id" with
  | `Int id, `Int cid ->
    let job_names  = list_of decode_job (get_f json "jobs") in
    let applied_at = str_of (get_f json "applied_at") in
    Some { id; candidate_id = cid; job_names; applied_at }
  | _ -> None

let decode_candidate json =
  match get_f json "id" with
  | `Int id ->
    let first_name  = str_of (get_f json "first_name") in
    let last_name   = str_of (get_f json "last_name") in
    let headline    = str_of (get_f json "headline") in
    let company     = str_of (get_f json "company") in
    let employments = list_of decode_employment (get_f json "employments") in
    let educations  = list_of decode_education  (get_f json "educations") in
    Some { id; first_name; last_name; headline; company; employments; educations }
  | _ -> None

let make_auth api_key =
  "Basic " ^ base64_encode (api_key ^ ":")

let get_json ~api_key path =
  let url = "https://harvest.greenhouse.io" ^ path in
  let uri = Uri.of_string url in
  let headers = Cohttp.Header.of_list [
    ("Authorization", make_auth api_key);
    ("Accept", "application/json");
  ] in
  Lwt.catch
    (fun () ->
      Cohttp_lwt_unix.Client.get ~headers uri >>= fun (resp, body) ->
      let code = Cohttp.Code.code_of_status (Cohttp.Response.status resp) in
      Cohttp_lwt.Body.to_string body >>= fun body_str ->
      if code = 200 then
        (try Lwt.return (Ok (Yojson.Safe.from_string body_str))
         with e -> Lwt.return (Error (Printf.sprintf "JSON parse: %s" (Printexc.to_string e))))
      else
        Lwt.return (Error (Printf.sprintf "HTTP %d" code)))
    (fun exn ->
      Lwt.return (Error (Printf.sprintf "Network: %s" (Printexc.to_string exn))))

let get_applications ~api_key ?since () =
  let since_param = match since with
    | None -> ""
    | Some t -> "&last_activity_after=" ^ (Uri.pct_encode t)
  in
  let path = Printf.sprintf "/v1/applications?per_page=500%s" since_param in
  get_json ~api_key path >>= function
  | Error e -> Lwt.return (Error e)
  | Ok json ->
    let apps = match json with
      | `List _ -> list_of decode_application json
      | _ -> []
    in
    Lwt.return (Ok apps)

let get_candidate ~api_key id =
  let path = Printf.sprintf "/v1/candidates/%d" id in
  get_json ~api_key path >>= function
  | Error e -> Lwt.return (Error e)
  | Ok json ->
    match decode_candidate json with
    | None -> Lwt.return (Error "could not decode candidate")
    | Some c -> Lwt.return (Ok c)
