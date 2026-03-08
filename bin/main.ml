let () =
  Mirage_crypto_rng_unix.use_default ();
  Printf.printf "ahrefs-recruit server starting on port 4000\n%!";
  Printf.printf "Skills loaded: %d\n%!"
    (List.length Ahrefs_skills_data.Skills_registry.all_skills);
  Ahrefs_sync.Greenhouse_sync.start_sync_loop ();
  Dream.run ~port:4000 ~interface:"127.0.0.1"
  @@ Dream.logger
  @@ Ahrefs_api.Router.app
