open Roladeck_types.Types
open Roladeck_scoring.Scoring

(** Build a minimal skill_record for testing *)
let make_skill id criteria red_flags =
  {
    id;
    discipline = { id; name = id; category = Tech; description = "" };
    criteria;
    sourcing_strings = [];
    title_synonyms = [];
    seniority_signals = [];
    interview_stages = [];
    red_flags;
    comp_ranges = [];
  }

let make_req candidate_id discipline_id notes =
  { candidate_id; discipline_id; seniority = Senior; candidate_notes = notes }

(** --- Tests --- *)

let test_all_t1_met () =
  let skill = make_skill "test-backend" [
    { text = "Python OCaml programming language"; tier = T1_MustHave };
    { text = "PostgreSQL database SQL queries"; tier = T1_MustHave };
  ] [] in
  let req = make_req "alice" "test-backend"
    "python ocaml postgresql database sql queries programming language" in
  let result = score_candidate skill req in
  Alcotest.(check bool) "all T1 met → StrongProgress or Progress"
    true
    (result.recommendation = StrongProgress || result.recommendation = Progress);
  Alcotest.(check bool) "overall_score > 0.5"
    true (result.overall_score > 0.5)

let test_no_t1_met () =
  let skill = make_skill "test-backend" [
    { text = "Python OCaml programming language"; tier = T1_MustHave };
    { text = "PostgreSQL database SQL queries"; tier = T1_MustHave };
  ] [] in
  let req = make_req "bob" "test-backend"
    "javascript react frontend css html design" in
  let result = score_candidate skill req in
  Alcotest.(check bool) "no T1 met → Maybe or Reject"
    true
    (result.recommendation = Maybe || result.recommendation = Reject)

let test_red_flag_downgrade () =
  let skill = make_skill "test" [
    { text = "Python OCaml programming"; tier = T1_MustHave };
    { text = "PostgreSQL database SQL"; tier = T1_MustHave };
    { text = "Spark Hadoop distributed"; tier = T2_Differentiator };
    { text = "Kubernetes Docker cloud"; tier = T2_Differentiator };
  ] ["no SQL no database experience"] in
  (* Notes: strong T1 match but also hits red flag *)
  let req = make_req "charlie" "test"
    "python ocaml programming postgresql database sql no sql no database experience spark hadoop kubernetes docker cloud distributed" in
  let result = score_candidate skill req in
  Alcotest.(check bool) "red flag hit"
    true (result.red_flags_hit <> []);
  (* Recommendation should be downgraded vs what it'd be without red flag *)
  Alcotest.(check bool) "recommendation present"
    true (result.recommendation = Progress || result.recommendation = Maybe)

let test_reject_score () =
  let skill = make_skill "niche" [
    { text = "Haskell monad functor typeclass"; tier = T1_MustHave };
    { text = "Erlang distributed concurrent actor"; tier = T1_MustHave };
    { text = "Prolog logic programming constraint"; tier = T2_Differentiator };
  ] [] in
  let req = make_req "dana" "niche"
    "javascript react css html frontend bootstrap tailwind" in
  let result = score_candidate skill req in
  Alcotest.(check pass) "reject score" () (
    if result.recommendation = Reject || result.recommendation = Maybe then ()
    else Alcotest.fail
      (Printf.sprintf "expected Reject/Maybe, got overall_score=%.2f" result.overall_score)
  )

let test_t2_t3_only () =
  let skill = make_skill "bonus" [
    { text = "GraphQL schema resolver mutation"; tier = T2_Differentiator };
    { text = "WebAssembly WASM Rust compile"; tier = T3_RareUpside };
  ] [] in
  let req = make_req "eve" "bonus"
    "graphql schema resolver mutation webassembly wasm rust compile" in
  let result = score_candidate skill req in
  Alcotest.(check bool) "no T1 criteria → no T1 gap → can progress"
    true
    (result.recommendation = StrongProgress || result.recommendation = Progress)

let test_roladeck_enrichment () =
  (* Backend engineering gets OCaml bonus *)
  let skill = make_skill "tech-hiring-backend-engineering" [
    { text = "REST API HTTP endpoints JSON"; tier = T1_MustHave };
  ] [] in
  let req = make_req "frank" "tech-hiring-backend-engineering"
    "rest api http endpoints json functional programming ocaml haskell" in
  let result = score_candidate skill req in
  (* Enrichment adds T2 criterion for OCaml — should appear in results *)
  let contains_substr hay needle =
    let hlen = String.length hay and nlen = String.length needle in
    if nlen = 0 then true
    else if nlen > hlen then false
    else begin
      let found = ref false in
      for i = 0 to hlen - nlen do
        if String.sub hay i nlen = needle then found := true
      done;
      !found
    end
  in
  let has_ocaml_criterion =
    List.exists (fun cr ->
      let t = String.lowercase_ascii cr.criterion.text in
      contains_substr t "ocaml"
    ) result.criterion_results
  in
  Alcotest.(check bool) "ahrefs enrichment adds OCaml criterion" true has_ocaml_criterion

let test_criterion_results_complete () =
  let skill = make_skill "complete" [
    { text = "Python data pipelines automation"; tier = T1_MustHave };
    { text = "Kafka streaming real-time events"; tier = T2_Differentiator };
    { text = "Iceberg Delta Lake table format"; tier = T3_RareUpside };
  ] [] in
  let req = make_req "grace" "complete"
    "python data pipelines automation kafka streaming real-time events" in
  let result = score_candidate skill req in
  (* All criteria should be in results *)
  Alcotest.(check int) "criterion_results count" 3
    (List.length result.criterion_results);
  (* T3 not met *)
  let t3_result = List.find (fun cr -> cr.criterion.tier = T3_RareUpside)
    result.criterion_results in
  Alcotest.(check bool) "T3 not met (no iceberg/delta)" false t3_result.met

let () =
  let open Alcotest in
  run "Scoring" [
    "basic", [
      test_case "all T1 met → progress" `Quick test_all_t1_met;
      test_case "no T1 met → reject" `Quick test_no_t1_met;
      test_case "red flag downgrade" `Quick test_red_flag_downgrade;
      test_case "reject score" `Quick test_reject_score;
      test_case "T2/T3 only, no gap" `Quick test_t2_t3_only;
    ];
    "enrichment", [
      test_case "ahrefs OCaml enrichment" `Quick test_roladeck_enrichment;
    ];
    "criterion_results", [
      test_case "complete criterion list" `Quick test_criterion_results_complete;
    ];
  ]
