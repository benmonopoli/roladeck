/* Api.re  -  fetch wrappers using shared types */
open Roladeck_types.Types;

let base = "";  /* same origin  -  Vite proxies /api to :4000 */

let fetchJson = url =>
  Fetch.fetch(url)
  |> Js.Promise.then_(Fetch.Response.json);

let postJson = (url, body) => {
  let bodyStr = Js.Json.stringify(body);
  Fetch.fetchWithInit(
    url,
    Fetch.RequestInit.make(
      ~method_=Post,
      ~body=Fetch.BodyInit.make(bodyStr),
      ~headers=Fetch.HeadersInit.makeWithArray([|
        ("Content-Type", "application/json")
      |]),
      ()
    )
  )
  |> Js.Promise.then_(Fetch.Response.json);
};

let putJson = (url, body) => {
  let bodyStr = Js.Json.stringify(body);
  Fetch.fetchWithInit(
    url,
    Fetch.RequestInit.make(
      ~method_=Put,
      ~body=Fetch.BodyInit.make(bodyStr),
      ~headers=Fetch.HeadersInit.makeWithArray([|
        ("Content-Type", "application/json")
      |]),
      ()
    )
  )
  |> Js.Promise.then_(Fetch.Response.json);
};

/* --- JSON helpers --- */

let decodeString = json =>
  Js.Json.decodeString(json) |> Option.value(~default="");

let decodeInt = json =>
  switch (Js.Json.decodeNumber(json)) {
  | Some(n) => int_of_float(n)
  | None => 0
  };

let decodeFloat = json =>
  switch (Js.Json.decodeNumber(json)) {
  | Some(n) => n
  | None => 0.0
  };

let decodeBool = json =>
  Js.Json.decodeBoolean(json) |> Option.value(~default=false);

let getField = (json, key) =>
  switch (Js.Json.decodeObject(json)) {
  | None => None
  | Some(obj) => Js.Dict.get(obj, key)
  };

let getFieldExn = (json, key) =>
  switch (getField(json, key)) {
  | Some(v) => v
  | None => Js.Json.parseExn("{}")
  };

let decodeCategory = json =>
  switch (Js.Json.decodeString(json)) {
  | Some("Tech") => Tech
  | Some("Marketing") => Marketing
  | Some("Sales") => Sales
  | _ => Tech
  };

let decodeSeniority = json =>
  switch (Js.Json.decodeString(json)) {
  | Some("Junior") => Junior
  | Some("Mid") => Mid
  | Some("Senior") => Senior
  | Some("Staff") => Staff
  | Some("Principal") => Principal
  | Some("Coordinator") => Coordinator
  | Some("Manager") => Manager
  | Some("SeniorManager") => SeniorManager
  | Some("Director") => Director
  | Some("VP") => VP
  | Some("CMO") => CMO
  | Some("CRO") => CRO
  | _ => Senior
  };

let decodeTier = json =>
  switch (Js.Json.decodeString(json)) {
  | Some("T1_MustHave") => T1_MustHave
  | Some("T2_Differentiator") => T2_Differentiator
  | Some("T3_RareUpside") => T3_RareUpside
  | _ => T1_MustHave
  };

let decodePlatform = json =>
  switch (Js.Json.classify(json)) {
  | JSONString("LinkedIn_XRay") => LinkedIn_XRay
  | JSONString("GitHub_XRay") => GitHub_XRay
  | JSONString("GitHub_Search") => GitHub_Search
  | JSONString("ArXiv") => ArXiv
  | JSONArray([|_tag, label|]) => Custom(decodeString(label))
  | _ => Custom("Unknown")
  };

let decodeRecommendation = json =>
  switch (Js.Json.decodeString(json)) {
  | Some("StrongProgress") => StrongProgress
  | Some("Progress") => Progress
  | Some("Maybe") => Maybe
  | Some("Reject") => Reject
  | _ => Reject
  };

let decodeAtsStage = json =>
  switch (Js.Json.decodeString(json)) {
  | Some("Screening") => Screening
  | Some("Interview") => Interview
  | Some("FinalRound") => FinalRound
  | Some("Offer") => Offer
  | Some("Hired") => Hired
  | Some("Rejected") => Rejected
  | Some("Withdrawn") => Withdrawn
  | _ => Screening
  };

let encodeAtsStage = s =>
  Js.Json.string(switch (s) {
  | Screening => "Screening"
  | Interview => "Interview"
  | FinalRound => "FinalRound"
  | Offer => "Offer"
  | Hired => "Hired"
  | Rejected => "Rejected"
  | Withdrawn => "Withdrawn"
  });

let decodeStringList = json =>
  switch (Js.Json.decodeArray(json)) {
  | None => []
  | Some(arr) => arr |> Array.to_list |> List.map(decodeString)
  };

let decodeList = (decoder, json) =>
  switch (Js.Json.decodeArray(json)) {
  | None => []
  | Some(arr) => arr |> Array.to_list |> List.map(decoder)
  };

let decodeSkillSummary = json => {
  id: decodeString(getFieldExn(json, "id")),
  name: decodeString(getFieldExn(json, "name")),
  category: decodeCategory(getFieldExn(json, "category")),
  description: decodeString(getFieldExn(json, "description")),
  criteria_count: decodeInt(getFieldExn(json, "criteria_count")),
};

let decodeCriterion = json => {
  text: decodeString(getFieldExn(json, "text")),
  tier: decodeTier(getFieldExn(json, "tier")),
};

let decodeSourcingString = (json) : sourcing_string => {
  platform: decodePlatform(getFieldExn(json, "platform")),
  label: decodeString(getFieldExn(json, "label")),
  query: decodeString(getFieldExn(json, "query")),
};

let decodeCompRange = json => {
  level: decodeSeniority(getFieldExn(json, "level")),
  base_min: decodeInt(getFieldExn(json, "base_min")),
  base_max: decodeInt(getFieldExn(json, "base_max")),
};

let decodeDiscipline = json => {
  id: decodeString(getFieldExn(json, "id")),
  name: decodeString(getFieldExn(json, "name")),
  category: decodeCategory(getFieldExn(json, "category")),
  description: decodeString(getFieldExn(json, "description")),
};

let decodeTitleSynonyms = json =>
  switch (Js.Json.decodeArray(json)) {
  | None => []
  | Some(arr) =>
    arr |> Array.to_list |> List.map(item => (
      decodeSeniority(getFieldExn(item, "level")),
      decodeStringList(getFieldExn(item, "titles"))
    ))
  };

let decodeSkillRecord = json => {
  id: decodeString(getFieldExn(json, "id")),
  discipline: decodeDiscipline(getFieldExn(json, "discipline")),
  criteria: decodeList(decodeCriterion, getFieldExn(json, "criteria")),
  sourcing_strings: decodeList(decodeSourcingString, getFieldExn(json, "sourcing_strings")),
  title_synonyms: decodeTitleSynonyms(getFieldExn(json, "title_synonyms")),
  seniority_signals: [],
  interview_stages: [],
  red_flags: decodeStringList(getFieldExn(json, "red_flags")),
  comp_ranges: decodeList(decodeCompRange, getFieldExn(json, "comp_ranges")),
};

let decodeTierScore = json => {
  tier: decodeTier(getFieldExn(json, "tier")),
  met: decodeInt(getFieldExn(json, "met")),
  total: decodeInt(getFieldExn(json, "total")),
  score: decodeFloat(getFieldExn(json, "score")),
};

let decodeCriterionResult = json => {
  criterion: decodeCriterion(getFieldExn(json, "criterion")),
  met: decodeBool(getFieldExn(json, "met")),
  matched_keywords: decodeStringList(getFieldExn(json, "matched_keywords")),
};

let decodeTrustStatus = json =>
  switch (Js.Json.decodeString(json)) {
  | Some("clean")      => TrustClean
  | Some("suspicious") => TrustSuspicious
  | _                  => TrustPending
  };

let decodeTrustCheck = json =>
  switch (Js.Json.decodeObject(json)) {
  | None => None
  | Some(d) => Some({
      trust_status: switch (Js.Dict.get(d, "trust_status")) {
        | Some(j) => decodeTrustStatus(j)
        | None => TrustPending
      },
      trust_flags: switch (Js.Dict.get(d, "trust_flags")) {
        | Some(j) => decodeStringList(j)
        | None => []
      },
      checked_at: switch (Js.Dict.get(d, "checked_at")) {
        | Some(j) => decodeString(j)
        | None => ""
      },
    })
  };

/* --- Pool types --- */

let decodeCandidateScore = json => {
  role_id: decodeString(getFieldExn(json, "role_id")),
  role_name: decodeString(getFieldExn(json, "role_name")),
  seniority: decodeSeniority(getFieldExn(json, "seniority")),
  overall_score: decodeFloat(getFieldExn(json, "overall_score")),
  recommendation: decodeRecommendation(getFieldExn(json, "recommendation")),
  tier_scores: decodeList(decodeTierScore, getFieldExn(json, "tier_scores")),
  red_flags_hit: decodeStringList(getFieldExn(json, "red_flags_hit")),
  criterion_results: decodeList(decodeCriterionResult, getFieldExn(json, "criterion_results")),
  scored_at: decodeString(getFieldExn(json, "scored_at")),
};

let decodeCandidateRecord = json => {
  id: decodeString(getFieldExn(json, "id")),
  name: decodeString(getFieldExn(json, "name")),
  ats_stage: decodeAtsStage(getFieldExn(json, "ats_stage")),
  scores: decodeList(decodeCandidateScore, getFieldExn(json, "scores")),
  source_text: decodeString(getFieldExn(json, "source_text")),
  created_at: decodeString(getFieldExn(json, "created_at")),
  updated_at: decodeString(getFieldExn(json, "updated_at")),
  greenhouse_url: switch (getField(json, "greenhouse_url")) {
    | Some(j) => Js.Json.decodeString(j)
    | None => None
  },
  greenhouse_application_id: switch (getField(json, "greenhouse_application_id")) {
    | Some(j) => Js.Json.decodeString(j)
    | None => None
  },
  trust_check: switch (getField(json, "trust_check")) {
    | Some(j) => decodeTrustCheck(j)
    | None => None
  },
};

let decodeCandidateSummary = json => {
  id: decodeString(getFieldExn(json, "id")),
  name: decodeString(getFieldExn(json, "name")),
  ats_stage: decodeAtsStage(getFieldExn(json, "ats_stage")),
  top_role_id: decodeString(getFieldExn(json, "top_role_id")),
  top_role_name: decodeString(getFieldExn(json, "top_role_name")),
  top_score: decodeFloat(getFieldExn(json, "top_score")),
  top_recommendation: decodeRecommendation(getFieldExn(json, "top_recommendation")),
  score_count: decodeInt(getFieldExn(json, "score_count")),
  role_count: decodeInt(getFieldExn(json, "role_count")),
  scored_role_ids: switch (getField(json, "scored_role_ids")) {
    | Some(j) => decodeStringList(j)
    | None => []
  },
  created_at: decodeString(getFieldExn(json, "created_at")),
  trust_status: switch (getField(json, "trust_status")) {
    | Some(j) => decodeTrustStatus(j)
    | None => TrustPending
  },
  trust_flags: switch (getField(json, "trust_flags")) {
    | Some(j) => decodeStringList(j)
    | None => []
  },
};

let decodePoolStats = json => {
  total_candidates: decodeInt(getFieldExn(json, "total_candidates")),
  roles_covered: decodeInt(getFieldExn(json, "roles_covered")),
  hired_count: decodeInt(getFieldExn(json, "hired_count")),
  active_count: decodeInt(getFieldExn(json, "active_count")),
};

let decodeCriterionSignal = json => {
  criterion_text: decodeString(getFieldExn(json, "criterion_text")),
  tier: decodeTier(getFieldExn(json, "tier")),
  hired_hit_rate: decodeFloat(getFieldExn(json, "hired_hit_rate")),
  all_hit_rate: decodeFloat(getFieldExn(json, "all_hit_rate")),
  signal_strength: decodeFloat(getFieldExn(json, "signal_strength")),
  sample_count: decodeInt(getFieldExn(json, "sample_count")),
};

let decodeRoleCalibration = json => {
  role_id: decodeString(getFieldExn(json, "role_id")),
  total_scored: decodeInt(getFieldExn(json, "total_scored")),
  hired_count: decodeInt(getFieldExn(json, "hired_count")),
  signals: decodeList(decodeCriterionSignal, getFieldExn(json, "signals")),
};

let decodeSkillMatch = json => {
  candidate_id: decodeString(getFieldExn(json, "candidate_id")),
  candidate_name: decodeString(getFieldExn(json, "candidate_name")),
  ats_stage: decodeAtsStage(getFieldExn(json, "ats_stage")),
  role_id: decodeString(getFieldExn(json, "role_id")),
  role_name: decodeString(getFieldExn(json, "role_name")),
  overall_score: decodeFloat(getFieldExn(json, "overall_score")),
  scored_at: decodeString(getFieldExn(json, "scored_at")),
};

/* --- Encoders --- */

let encodeSeniority = s =>
  Js.Json.string(switch (s) {
  | Junior => "Junior" | Mid => "Mid" | Senior => "Senior"
  | Staff => "Staff" | Principal => "Principal"
  | Coordinator => "Coordinator" | Manager => "Manager"
  | SeniorManager => "SeniorManager" | Director => "Director"
  | VP => "VP" | CMO => "CMO" | CRO => "CRO"
  });

let encodePlatformOpt = p =>
  switch (p) {
  | None => Js.Json.null
  | Some(LinkedIn_XRay) => Js.Json.string("LinkedIn_XRay")
  | Some(GitHub_XRay) => Js.Json.string("GitHub_XRay")
  | Some(GitHub_Search) => Js.Json.string("GitHub_Search")
  | Some(ArXiv) => Js.Json.string("ArXiv")
  | Some(Custom(s)) => Js.Json.array([|Js.Json.string("Custom"), Js.Json.string(s)|])
  };

let encodeTier = t =>
  Js.Json.string(switch (t) {
  | T1_MustHave => "T1_MustHave"
  | T2_Differentiator => "T2_Differentiator"
  | T3_RareUpside => "T3_RareUpside"
  });

let encodeCriterion = (c: skill_criterion) =>
  Js.Json.object_(Js.Dict.fromArray([|
    ("text", Js.Json.string(c.text)),
    ("tier", encodeTier(c.tier)),
  |]));

let encodeCategory = cat =>
  Js.Json.string(switch (cat) {
  | Tech => "Tech" | Marketing => "Marketing" | Sales => "Sales"
  });

let encodeSkillRecord = (s: skill_record) =>
  Js.Json.object_(Js.Dict.fromArray([|
    ("id", Js.Json.string(s.id)),
    ("discipline", Js.Json.object_(Js.Dict.fromArray([|
      ("id", Js.Json.string(s.discipline.id)),
      ("name", Js.Json.string(s.discipline.name)),
      ("category", encodeCategory(s.discipline.category)),
      ("description", Js.Json.string(s.discipline.description)),
    |]))),
    ("criteria", Js.Json.array(Array.of_list(List.map(encodeCriterion, s.criteria)))),
    ("sourcing_strings", Js.Json.array([||])),
    ("title_synonyms", Js.Json.array([||])),
    ("seniority_signals", Js.Json.array([||])),
    ("interview_stages", Js.Json.array([||])),
    ("red_flags", Js.Json.array(Array.of_list(List.map(Js.Json.string, s.red_flags)))),
    ("comp_ranges", Js.Json.array([||])),
  |]));

/* --- API functions --- */

let getSkills = () =>
  fetchJson(base ++ "/api/skills")
  |> Js.Promise.then_(json => {
    let skills =
      json
      |> Js.Json.decodeArray
      |> Option.value(~default=[||])
      |> Array.to_list
      |> List.map(decodeSkillSummary);
    Js.Promise.resolve(skills);
  });

let getSkill = id =>
  fetchJson(base ++ "/api/skills/" ++ id)
  |> Js.Promise.then_(json =>
    Js.Promise.resolve(Some(decodeSkillRecord(json)))
  );

let searchSkills = (~q="", ~category=?, ()) => {
  let params = ref([]);
  if (String.length(q) > 0) {
    params := [("q", q), ...(params^)];
  };
  switch (category) {
  | Some(cat) =>
    let cat_str = switch (cat) {
    | Tech => "tech"
    | Marketing => "marketing"
    | Sales => "sales"
    };
    params := [("category", cat_str), ...(params^)];
  | None => ()
  };
  let qs = List.map(((k, v)) => k ++ "=" ++ Js.Global.encodeURIComponent(v), params^)
           |> String.concat("&");
  let url = String.length(qs) > 0
    ? base ++ "/api/search?" ++ qs
    : base ++ "/api/search";
  fetchJson(url)
  |> Js.Promise.then_(json => {
    let skills =
      json
      |> Js.Json.decodeArray
      |> Option.value(~default=[||])
      |> Array.to_list
      |> List.map(decodeSkillSummary);
    Js.Promise.resolve(skills);
  });
};

let sourcingQueryToJson = (query: sourcing_query) =>
  Js.Json.object_(Js.Dict.fromArray([|
    ("discipline_id", Js.Json.string(query.discipline_id)),
    ("seniority", encodeSeniority(query.seniority)),
    ("platform", encodePlatformOpt(query.platform)),
  |]));

let decodeSourcingResult = json => {
  discipline_id: decodeString(getFieldExn(json, "discipline_id")),
  seniority: decodeSeniority(getFieldExn(json, "seniority")),
  strings: decodeList(decodeSourcingString, getFieldExn(json, "strings")),
};

let getSourceStrings = (query: sourcing_query) =>
  postJson(base ++ "/api/source", sourcingQueryToJson(query))
  |> Js.Promise.then_(json =>
    Js.Promise.resolve(Some(decodeSourcingResult(json)))
  );

/* --- Custom Skills API --- */

let getCustomSkills = () =>
  fetchJson(base ++ "/api/custom-skills")
  |> Js.Promise.then_(json => {
    let skills =
      json
      |> Js.Json.decodeArray
      |> Option.value(~default=[||])
      |> Array.to_list
      |> List.map(decodeSkillSummary);
    Js.Promise.resolve(skills);
  });

let createCustomSkill = (skill: skill_record) =>
  postJson(base ++ "/api/custom-skills", encodeSkillRecord(skill))
  |> Js.Promise.then_(json => Js.Promise.resolve(decodeSkillRecord(json)));

let updateCustomSkill = (id: string, skill: skill_record) =>
  putJson(base ++ "/api/custom-skills/" ++ id, encodeSkillRecord(skill))
  |> Js.Promise.then_(json => Js.Promise.resolve(decodeSkillRecord(json)));

let deleteCustomSkill = (id: string) =>
  Fetch.fetchWithInit(
    base ++ "/api/custom-skills/" ++ id,
    Fetch.RequestInit.make(~method_=Delete, ())
  )
  |> Js.Promise.then_(Fetch.Response.json)
  |> Js.Promise.then_(_ => Js.Promise.resolve());

/* --- Pool API --- */

/* isRolePlaybook: IDs like tech-hiring-X, marketing-hiring-X, sales-hiring-X */
let isRolePlaybook = id => {
  let n = String.length(id);
  let m = String.length("-hiring-");
  let found = ref(false);
  for (i in 0 to n - m) {
    if (String.sub(id, i, m) == "-hiring-") {
      found := true;
    };
  };
  found^;
};

let getPoolStats = () =>
  fetchJson(base ++ "/api/pool/stats")
  |> Js.Promise.then_(json =>
    Js.Promise.resolve(decodePoolStats(json))
  );

let getPool = (~role="", ~stage=?, ~q="", ()) => {
  let params = ref([]);
  if (String.length(role) > 0) { params := [("role", role), ...(params^)]; };
  if (String.length(q) > 0) { params := [("q", q), ...(params^)]; };
  switch (stage) {
  | Some(st) =>
    let s = switch (st) {
    | Screening => "Screening" | Interview => "Interview"
    | FinalRound => "FinalRound" | Offer => "Offer"
    | Hired => "Hired" | Rejected => "Rejected" | Withdrawn => "Withdrawn"
    };
    params := [("stage", s), ...params^];
  | None => ()
  };
  let qs = List.map(((k, v)) => k ++ "=" ++ Js.Global.encodeURIComponent(v), params^)
           |> String.concat("&");
  let url = String.length(qs) > 0
    ? base ++ "/api/pool?" ++ qs
    : base ++ "/api/pool";
  fetchJson(url)
  |> Js.Promise.then_(json => {
    let candidates =
      json
      |> Js.Json.decodeArray
      |> Option.value(~default=[||])
      |> Array.to_list
      |> List.map(decodeCandidateSummary);
    Js.Promise.resolve(candidates);
  });
};

let getCandidate = id =>
  fetchJson(base ++ "/api/pool/" ++ id)
  |> Js.Promise.then_(json =>
    Js.Promise.resolve(Some(decodeCandidateRecord(json)))
  );

let poolSaveToJson = (~name, ~disciplineId, ~seniority, ~notes, ~stage) =>
  Js.Json.object_(Js.Dict.fromArray([|
    ("candidate_name",  Js.Json.string(name)),
    ("discipline_id",   Js.Json.string(disciplineId)),
    ("seniority",       encodeSeniority(seniority)),
    ("candidate_notes", Js.Json.string(notes)),
    ("ats_stage",       encodeAtsStage(stage)),
  |]));

let saveToPool = (~name, ~disciplineId, ~seniority, ~notes, ~stage) =>
  postJson(
    base ++ "/api/pool",
    poolSaveToJson(~name, ~disciplineId, ~seniority, ~notes, ~stage)
  )
  |> Js.Promise.then_(json =>
    Js.Promise.resolve(Some(decodeCandidateRecord(json)))
  );

let updateStage = (id, stage) =>
  putJson(
    base ++ "/api/pool/" ++ id ++ "/stage",
    Js.Json.object_(Js.Dict.fromArray([|
      ("stage", encodeAtsStage(stage))
    |]))
  )
  |> Js.Promise.then_(json =>
    Js.Promise.resolve(Some(decodeCandidateRecord(json)))
  );

let verifyCandidate = (id: string) =>
  Fetch.fetchWithInit(
    base ++ "/api/candidates/" ++ id ++ "/verify",
    Fetch.RequestInit.make(~method_=Post, ())
  )
  |> Js.Promise.then_(Fetch.Response.json)
  |> Js.Promise.then_(json => Js.Promise.resolve(decodeCandidateSummary(json)));

let getCalibration = roleId =>
  fetchJson(base ++ "/api/calibration/" ++ roleId)
  |> Js.Promise.then_(json =>
    Js.Promise.resolve(Some(decodeRoleCalibration(json)))
  );

let getBySkill = criterion =>
  fetchJson(
    base ++ "/api/pool/by-skill?criterion=" ++ Js.Global.encodeURIComponent(criterion)
  )
  |> Js.Promise.then_(json => {
    let matches =
      json
      |> Js.Json.decodeArray
      |> Option.value(~default=[||])
      |> Array.to_list
      |> List.map(decodeSkillMatch);
    Js.Promise.resolve(matches);
  });

module Clipboard = {
  let writeText : string => Js.Promise.t(unit) = [%raw {|
    function(text) {
      return navigator.clipboard.writeText(text);
    }
  |}];
};

module LocalStorage = {
  let get : string => option(string) = [%raw {|
    function(key) {
      var v = localStorage.getItem(key);
      return (v === null || v === undefined) ? undefined : v;
    }
  |}];
  let set : (string, string) => unit = [%raw {|
    function(key, value) {
      localStorage.setItem(key, value);
    }
  |}];
};

/* --- Settings API --- */

let getSettingsStatus = () =>
  fetchJson(base ++ "/api/settings/status")
  |> Js.Promise.then_(json =>
    Js.Promise.resolve({
      anthropic_key_set:     decodeBool(getFieldExn(json, "anthropic_key_set")),
      greenhouse_configured: decodeBool(getFieldExn(json, "greenhouse_configured")),
    })
  );

let getIntegrationSettings = () =>
  fetchJson(base ++ "/api/settings/integrations")
  |> Js.Promise.then_(json =>
    Js.Promise.resolve({
      greenhouse_subdomain:    decodeString(getFieldExn(json, "greenhouse_subdomain")),
      greenhouse_api_key_hint: decodeString(getFieldExn(json, "greenhouse_api_key_hint")),
      greenhouse_configured:   decodeBool(getFieldExn(json, "greenhouse_configured")),
      ai_provider:             decodeString(getFieldExn(json, "ai_provider")),
      ai_api_key_hint:         decodeString(getFieldExn(json, "ai_api_key_hint")),
      ai_key_set:              decodeBool(getFieldExn(json, "ai_key_set")),
    })
  );

let saveIntegrationSettings = (~subdomain, ~apiKey, ~aiProvider, ~aiApiKey) =>
  postJson(
    base ++ "/api/settings/integrations",
    Js.Json.object_(Js.Dict.fromArray([|
      ("greenhouse_subdomain", Js.Json.string(subdomain)),
      ("greenhouse_api_key",   Js.Json.string(apiKey)),
      ("ai_provider",          Js.Json.string(aiProvider)),
      ("ai_api_key",           Js.Json.string(aiApiKey)),
    |]))
  )
  |> Js.Promise.then_(json =>
    Js.Promise.resolve(decodeBool(getFieldExn(json, "ok")))
  );

let testGreenhouseConnection = () =>
  postJson(base ++ "/api/settings/test-greenhouse", Js.Json.object_(Js.Dict.fromArray([||])))
  |> Js.Promise.then_(json => {
    let ok  = decodeBool(getFieldExn(json, "ok"));
    let err = switch (getField(json, "error")) {
      | Some(j) => Some(decodeString(j))
      | None => None
    };
    Js.Promise.resolve((ok, err));
  });

/* --- AI Sourcing --- */

let decodeAiCandidate = json => {
  name:        decodeString(getFieldExn(json, "name")),
  profile_url: decodeString(getFieldExn(json, "profile_url")),
  rationale:   decodeString(getFieldExn(json, "rationale")),
};

let decodeAiBooleanString = json => {
  platform: decodeString(getFieldExn(json, "platform")),
  label:    decodeString(getFieldExn(json, "label")),
  query:    decodeString(getFieldExn(json, "query")),
};

let decodeAiSourcingResult = json => {
  session_id:        decodeString(getFieldExn(json, "session_id")),
  role_id:           decodeString(getFieldExn(json, "role_id")),
  role_name:         decodeString(getFieldExn(json, "role_name")),
  seniority:         decodeSeniority(getFieldExn(json, "seniority")),
  candidates:        decodeList(decodeAiCandidate, getFieldExn(json, "candidates")),
  boolean_strings:   decodeList(decodeAiBooleanString, getFieldExn(json, "boolean_strings")),
  target_companies:  decodeStringList(getFieldExn(json, "target_companies")),
  outreach_template: decodeString(getFieldExn(json, "outreach_template")),
  ran_at:            decodeString(getFieldExn(json, "ran_at")),
};

let encodeAiSourcingRequest = (~roleId, ~seniority, ~context) =>
  Js.Json.object_(Js.Dict.fromArray([|
    ("role_id",   Js.Json.string(roleId)),
    ("seniority", encodeSeniority(seniority)),
    ("context",   Js.Json.string(context)),
  |]));

let postAiSource = (~roleId, ~seniority, ~context) => {
  let bodyStr = Js.Json.stringify(
    encodeAiSourcingRequest(~roleId, ~seniority, ~context)
  );
  Fetch.fetchWithInit(
    base ++ "/api/ai/source",
    Fetch.RequestInit.make(
      ~method_=Post,
      ~body=Fetch.BodyInit.make(bodyStr),
      ~headers=Fetch.HeadersInit.makeWithArray([|
        ("Content-Type", "application/json")
      |]),
      ()
    )
  )
  |> Js.Promise.then_(Fetch.Response.text)
  |> Js.Promise.then_(text => {
    if (String.length(text) == 0) {
      Js.Promise.reject(Js.Exn.raiseError("Server returned an empty response"))
    } else {
      let json = try(Some(Js.Json.parseExn(text))) { | _ => None };
      switch (json) {
      | None =>
        Js.Promise.reject(Js.Exn.raiseError("Server returned non-JSON: " ++ String.sub(text, 0, min(200, String.length(text)))))
      | Some(j) =>
        switch (getField(j, "error")) {
        | Some(errJson) =>
          let msg = Js.Json.decodeString(errJson) |> Option.value(~default="Unknown error");
          Js.Promise.reject(Js.Exn.raiseError(msg))
        | None =>
          Js.Promise.resolve(decodeAiSourcingResult(j))
        }
      }
    }
  });
};

/* --- Auth --- */

type userInfo = {
  user_id:      string,
  email:        string,
  company_id:   string,
  company_name: string,
};

let decodeUserInfo = (json) : userInfo => {
  user_id:      decodeString(getFieldExn(json, "user_id")),
  email:        decodeString(getFieldExn(json, "email")),
  company_id:   decodeString(getFieldExn(json, "company_id")),
  company_name: decodeString(getFieldExn(json, "company_name")),
};

let signup = (~email, ~password, ~companyName) =>
  postJson(
    base ++ "/api/auth/signup",
    Js.Json.object_(Js.Dict.fromArray([|
      ("email",        Js.Json.string(email)),
      ("password",     Js.Json.string(password)),
      ("company_name", Js.Json.string(companyName)),
    |]))
  )
  |> Js.Promise.then_(json => Js.Promise.resolve(decodeUserInfo(json)));

let login = (~email, ~password) =>
  postJson(
    base ++ "/api/auth/login",
    Js.Json.object_(Js.Dict.fromArray([|
      ("email",    Js.Json.string(email)),
      ("password", Js.Json.string(password)),
    |]))
  )
  |> Js.Promise.then_(json => Js.Promise.resolve(decodeUserInfo(json)));

let logout = () =>
  postJson(base ++ "/api/auth/logout", Js.Json.object_(Js.Dict.fromArray([||])))
  |> Js.Promise.then_(_ => Js.Promise.resolve());

let getMe = () =>
  Fetch.fetch(base ++ "/api/auth/me")
  |> Js.Promise.then_(resp => {
    let status = Fetch.Response.status(resp);
    if (status == 401 || status == 403) {
      Js.Promise.resolve(None)
    } else {
      Fetch.Response.json(resp)
      |> Js.Promise.then_(json =>
        Js.Promise.resolve(Some(decodeUserInfo(json)))
      )
    }
  })
  |> Js.Promise.catch(_ => Js.Promise.resolve(None));

/* --- Company Profile --- */

type companyProfile = {
  company_name:       string,
  company_urls:       list(string),
  company_brief:      string,
  brief_generated_at: option(string),
};

let decodeCompanyProfile = (json) : companyProfile => {
  company_name:       decodeString(getFieldExn(json, "company_name")),
  company_urls:       decodeStringList(getFieldExn(json, "company_urls")),
  company_brief:      decodeString(getFieldExn(json, "company_brief")),
  brief_generated_at: switch (getField(json, "brief_generated_at")) {
    | Some(j) => switch (Js.Json.decodeString(j)) { | Some(s) => Some(s) | None => None }
    | None => None
  },
};

let getCompanyProfile = () : Js.Promise.t(companyProfile) =>
  fetchJson(base ++ "/api/company/profile")
  |> Js.Promise.then_(json =>
    Js.Promise.resolve(decodeCompanyProfile(json))
  );

let saveAndResearchCompany = (~name, ~urls: list(string)) =>
  postJson(
    base ++ "/api/company/profile",
    Js.Json.object_(Js.Dict.fromArray([|
      ("company_name", Js.Json.string(name)),
      ("company_urls", Js.Json.array(
        Array.of_list(List.map(Js.Json.string, urls))
      )),
    |]))
  )
  |> Js.Promise.then_(_ => Js.Promise.resolve());

let fetchUrl = url =>
  postJson(
    base ++ "/api/fetch-url",
    Js.Json.object_(Js.Dict.fromArray([|
      ("url", Js.Json.string(url)),
    |]))
  )
  |> Js.Promise.then_(json =>
    Js.Promise.resolve(decodeString(getFieldExn(json, "text")))
  );

let getSourcingSessions = () =>
  fetchJson(base ++ "/api/ai/sessions")
  |> Js.Promise.then_(json => {
    let sessions =
      json
      |> Js.Json.decodeArray
      |> Option.value(~default=[||])
      |> Array.to_list
      |> List.map(decodeAiSourcingResult);
    Js.Promise.resolve(sessions);
  });

/* --- Greenhouse Sync --- */

type greenhouseSyncStatus = {
  configured:     bool,
  last_synced_at: option(string),
  total_synced:   int,
  last_error:     option(string),
};

let getGreenhouseSyncStatus = () =>
  fetchJson(base ++ "/api/greenhouse/sync/status")
  |> Js.Promise.then_(json =>
    Js.Promise.resolve({
      configured:     decodeBool(getFieldExn(json, "configured")),
      last_synced_at: switch (getField(json, "last_synced_at")) {
        | Some(j) => Js.Json.decodeString(j)
        | None => None
      },
      total_synced:   decodeInt(getFieldExn(json, "total_synced")),
      last_error:     switch (getField(json, "last_error")) {
        | Some(j) => Js.Json.decodeString(j)
        | None => None
      },
    })
  );

let triggerGreenhouseSync = () =>
  postJson(base ++ "/api/greenhouse/sync/now", Js.Json.object_(Js.Dict.fromArray([||])))
  |> Js.Promise.then_(_ => Js.Promise.resolve());

/* --- AI Classify --- */

type playbookMatch = {
  playbook_id:   string,
  playbook_name: string,
  confidence:    float,
  rationale:     string,
};

let decodePlaybookMatch = json => {
  playbook_id:   decodeString(getFieldExn(json, "playbook_id")),
  playbook_name: decodeString(getFieldExn(json, "playbook_name")),
  confidence:    decodeFloat(getFieldExn(json, "confidence")),
  rationale:     decodeString(getFieldExn(json, "rationale")),
};

let classifyCandidate = (~candidateNotes) =>
  postJson(
    base ++ "/api/classify",
    Js.Json.object_(Js.Dict.fromArray([|
      ("candidate_notes", Js.Json.string(candidateNotes)),
    |]))
  )
  |> Js.Promise.then_(json => {
    let matches = switch (getField(json, "matches")) {
      | Some(j) => decodeList(decodePlaybookMatch, j)
      | None => []
    };
    Js.Promise.resolve(matches);
  });

