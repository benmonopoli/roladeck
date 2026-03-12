type discipline_category = Tech | Marketing | Sales

type seniority_level =
  | Junior
  | Mid
  | Senior
  | Staff
  | Principal
  | Coordinator
  | Manager
  | SeniorManager
  | Director
  | VP
  | CMO
  | CRO

type skill_tier = T1_MustHave | T2_Differentiator | T3_RareUpside

type skill_criterion = {
  text : string;
  tier : skill_tier;
}

type sourcing_platform =
  | LinkedIn_XRay
  | GitHub_XRay
  | GitHub_Search
  | ArXiv
  | Custom of string

type sourcing_string = {
  platform : sourcing_platform;
  label : string;
  query : string;
}

type seniority_signal = {
  from_level : seniority_level;
  to_level : seniority_level;
  signal_text : string;
}

type interview_stage = {
  stage_name : string;
  format : string;
  assessing : string;
}

type comp_range = {
  level : seniority_level;
  base_min : int;
  base_max : int;
}

type discipline = {
  id : string;
  name : string;
  category : discipline_category;
  description : string;
}

type skill_record = {
  id : string;
  discipline : discipline;
  criteria : skill_criterion list;
  sourcing_strings : sourcing_string list;
  title_synonyms : (seniority_level * string list) list;
  seniority_signals : seniority_signal list;
  interview_stages : interview_stage list;
  red_flags : string list;
  comp_ranges : comp_range list;
}

type skill_summary = {
  id : string;
  name : string;
  category : discipline_category;
  description : string;
  criteria_count : int;
}

type recommendation_tier = StrongProgress | Progress | Maybe | Reject

type tier_score = {
  tier : skill_tier;
  met : int;
  total : int;
  score : float;
}

type criterion_result = {
  criterion : skill_criterion;
  met : bool;
  matched_keywords : string list;
}

type scoring_result = {
  candidate_id : string;
  discipline_id : string;
  seniority_assessed : seniority_level;
  tier_scores : tier_score list;
  overall_score : float;
  recommendation : recommendation_tier;
  red_flags_hit : string list;
  criterion_results : criterion_result list;
}

type scoring_request = {
  candidate_id : string;
  discipline_id : string;
  seniority : seniority_level;
  candidate_notes : string;
}

type sourcing_query = {
  discipline_id : string;
  seniority : seniority_level;
  platform : sourcing_platform option;
}

type sourcing_result = {
  discipline_id : string;
  seniority : seniority_level;
  strings : sourcing_string list;
}

(* ── Trust Verification ── *)

type trust_status = TrustPending | TrustClean | TrustSuspicious

type trust_check = {
  trust_status : trust_status;
  trust_flags  : string list;
  checked_at   : string;
}

(* ── Talent Pool ── *)

type ats_stage =
  | Screening | Interview | FinalRound | Offer | Hired | Rejected | Withdrawn

(** One scoring event saved against a candidate *)
type candidate_score = {
  role_id : string;
  role_name : string;
  seniority : seniority_level;
  overall_score : float;
  recommendation : recommendation_tier;
  tier_scores : tier_score list;
  red_flags_hit : string list;
  criterion_results : criterion_result list;
  scored_at : string;
}

type candidate_record = {
  id : string;
  name : string;
  ats_stage : ats_stage;
  scores : candidate_score list;
  source_text : string;
  created_at : string;
  updated_at : string;
  greenhouse_url : string option;
  greenhouse_application_id : string option;
  trust_check : trust_check option;
}

type candidate_summary = {
  id : string;
  name : string;
  ats_stage : ats_stage;
  top_role_id : string;
  top_role_name : string;
  top_score : float;
  top_recommendation : recommendation_tier;
  score_count : int;
  role_count : int;
  scored_role_ids : string list;
  created_at : string;
  trust_status : trust_status;
  trust_flags  : string list;
}

type pool_save_request = {
  candidate_name : string;
  discipline_id : string;
  seniority : seniority_level;
  candidate_notes : string;
  ats_stage : ats_stage;
}

type update_stage_request = {
  stage : ats_stage;
}

(* ── Calibration ── *)

type criterion_signal = {
  criterion_text : string;
  tier : skill_tier;
  hired_hit_rate : float;
  all_hit_rate : float;
  signal_strength : float;
  sample_count : int;
}

type role_calibration = {
  role_id : string;
  total_scored : int;
  hired_count : int;
  signals : criterion_signal list;
}

(* ── Skill drill-down ── *)

type skill_match = {
  candidate_id : string;
  candidate_name : string;
  ats_stage : ats_stage;
  role_id : string;
  role_name : string;
  overall_score : float;
  scored_at : string;
}

(* ── Home stats ── *)

type pool_stats = {
  total_candidates : int;
  roles_covered : int;
  hired_count : int;
  active_count : int;
}

(* ── AI Sourcing ── *)

type sourcing_candidate = {
  name        : string;
  profile_url : string;
  rationale   : string;
}

type ai_boolean_string = {
  platform : string;
  label    : string;
  query    : string;
}

type ai_sourcing_result = {
  session_id        : string;
  role_id           : string;
  role_name         : string;
  seniority         : seniority_level;
  candidates        : sourcing_candidate list;
  boolean_strings   : ai_boolean_string list;
  target_companies  : string list;
  outreach_template : string;
  ran_at            : string;
}

type playbook_match = {
  playbook_id   : string;
  playbook_name : string;
  confidence    : float;
  rationale     : string;
}

type classify_result = {
  matches : playbook_match list;
}

type ai_sourcing_request = {
  role_id   : string;
  seniority : seniority_level;
  context   : string;
}

(* ── Company Profile ── *)

type company_profile = {
  company_name       : string;
  company_urls       : string list;
  company_brief      : string;
  brief_generated_at : string option;
  pool_lookback      : string option;
}

(* ── Greenhouse Sync ── *)

type greenhouse_sync_state = {
  last_synced_at : string option;
  total_synced   : int;
  last_error     : string option;
}

(* ── Integration Settings ── *)

type integration_settings = {
  greenhouse_subdomain : string;
  greenhouse_api_key   : string;
  ai_provider          : string;  (* "anthropic" | "openai" | "groq" *)
  ai_api_key           : string;
}

type settings_status = {
  anthropic_key_set       : bool;
  greenhouse_configured   : bool;
}

type greenhouse_settings_view = {
  greenhouse_subdomain    : string;
  greenhouse_api_key_hint : string;
  greenhouse_configured   : bool;
  ai_provider             : string;
  ai_api_key_hint         : string;
  ai_key_set              : bool;
}

type user_record = {
  id            : string;
  email         : string;
  password_hash : string;
  company_id    : string;
  created_at    : string;
}

type company_record = {
  id         : string;
  domain     : string;
  name       : string;
  created_at : string;
}

type session_record = {
  token      : string;
  user_id    : string;
  company_id : string;
  created_at : string;
}
