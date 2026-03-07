module PBKDF2 = Pbkdf.Make(Digestif.SHA256)

let to_hex s =
  let buf = Buffer.create (String.length s * 2) in
  String.iter (fun c ->
    Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))
  ) s;
  Buffer.contents buf

let hex_to_string h =
  let n = String.length h / 2 in
  Bytes.init n (fun i ->
    Char.chr (int_of_string ("0x" ^ String.sub h (i * 2) 2))
  ) |> Bytes.to_string

let generate_token () =
  to_hex (Mirage_crypto_rng.generate 32)

let generate_id prefix =
  let t = int_of_float (Unix.gettimeofday () *. 1000.0) in
  Printf.sprintf "%s-%d-%06d" prefix t (Random.int 999999)

let hash_password password =
  let salt = Mirage_crypto_rng.generate 16 in
  let salt_hex = to_hex salt in
  let key = PBKDF2.pbkdf2 ~password ~salt ~count:100_000 ~dk_len:32l in
  let key_hex = to_hex key in
  salt_hex ^ "$" ^ key_hex

let verify_password password stored =
  match String.split_on_char '$' stored with
  | [salt_hex; key_hex] ->
    let salt = hex_to_string salt_hex in
    let key = PBKDF2.pbkdf2 ~password ~salt ~count:100_000 ~dk_len:32l in
    String.equal (to_hex key) key_hex
  | _ -> false

let domain_of_email email =
  match String.split_on_char '@' email with
  | [_; domain] -> String.lowercase_ascii (String.trim domain)
  | _ -> ""

let get_session req =
  match Dream.cookie req "rd_session" with
  | None -> None
  | Some token -> Ahrefs_storage.Storage.find_session token
