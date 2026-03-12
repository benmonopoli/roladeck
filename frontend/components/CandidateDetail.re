open Roladeck_types.Types;

[@react.component]
let make = (~candidateId: string, ~onSelectRole: string => unit) => {
  let (candidate, setCandidate) = React.useState(() => None);
  let (loading, setLoading) = React.useState(() => true);
  let (stageUpdating, setStageUpdating) = React.useState(() => false);
  let (copied, setCopied) = React.useState(() => false);
  let (verifying, setVerifying) = React.useState(() => false);
  let (trustStatus, setTrustStatus) = React.useState(() => TrustPending);
  let (trustFlags, setTrustFlags) = React.useState(() => []);
  let (allSkills, setAllSkills) = React.useState(() => []);
  let (scoreRoleId, setScoreRoleId) = React.useState(() => "");
  let (scoreSeniority, setScoreSeniority) = React.useState(() => "Senior");
  let (scoring, setScoring) = React.useState(() => false);
  let (scoreMsg, setScoreMsg) = React.useState(() => None);

  React.useEffect0(() => {
    Roladeck_frontend_api.Api.getSkills()
    |> Js.Promise.then_(s => {
      setAllSkills(_ => s);
      Js.Promise.resolve();
    })
    |> ignore;
    None;
  });

  React.useEffect1(() => {
    setLoading(_ => true);
    Roladeck_frontend_api.Api.getCandidate(candidateId)
    |> Js.Promise.then_(r => {
      setCandidate(_ => r);
      setLoading(_ => false);
      (switch (r) {
       | Some(c) =>
         let (st, fl) = switch (c.trust_check) {
           | None => (TrustPending, [])
           | Some(tc) => (tc.trust_status, tc.trust_flags)
         };
         setTrustStatus(_ => st);
         setTrustFlags(_ => fl);
       | None => ()
       });
      Js.Promise.resolve();
    })
    |> ignore;
    None;
  }, [|candidateId|]);

  let stageLabel = s => switch (s) {
    | Screening => "Screening" | Interview => "Interview"
    | FinalRound => "Final Round" | Offer => "Offer"
    | Hired => "Hired" | Rejected => "Rejected" | Withdrawn => "Withdrawn"
  };

  let stageClass = s => switch (s) {
    | Screening => "stage-screening" | Interview => "stage-interview"
    | FinalRound => "stage-final" | Offer => "stage-offer"
    | Hired => "stage-hired" | Rejected => "stage-rejected" | Withdrawn => "stage-withdrawn"
  };

  let parseStage = s => switch (s) {
    | "Interview" => Interview | "Final Round" => FinalRound
    | "Offer" => Offer | "Hired" => Hired
    | "Rejected" => Rejected | "Withdrawn" => Withdrawn
    | _ => Screening
  };

  let recLabel = r => switch (r) {
    | StrongProgress => "Strong Progress" | Progress => "Progress"
    | Maybe => "Maybe" | Reject => "Reject"
  };

  let recClass = r => switch (r) {
    | StrongProgress => "rec-green" | Progress => "rec-blue"
    | Maybe => "rec-yellow" | Reject => "rec-red"
  };

  let seniorityLabel = s => switch (s) {
    | Junior => "Junior" | Mid => "Mid" | Senior => "Senior"
    | Staff => "Staff" | Principal => "Principal"
    | Coordinator => "Coordinator" | Manager => "Manager"
    | SeniorManager => "Senior Manager" | Director => "Director"
    | VP => "VP" | CMO => "CMO" | CRO => "CRO"
  };

  let tierShort = t => switch (t) {
    | T1_MustHave => "T1" | T2_Differentiator => "T2" | T3_RareUpside => "T3"
  };

  let tierClass = t => switch (t) {
    | T1_MustHave => "t1" | T2_Differentiator => "t2" | T3_RareUpside => "t3"
  };

  let handleStageChange = (c: candidate_record, newStageStr) => {
    let newStage = parseStage(newStageStr);
    setStageUpdating(_ => true);
    Roladeck_frontend_api.Api.updateStage(c.id, newStage)
    |> Js.Promise.then_(r => {
      setCandidate(_ => r);
      setStageUpdating(_ => false);
      Js.Promise.resolve();
    })
    |> ignore;
  };

  let parseSeniority = s => switch (s) {
    | "Junior" => Junior | "Mid" => Mid | "Staff" => Staff
    | "Principal" => Principal | _ => Senior
  };

  let handleScoreAgainstRole = (c: candidate_record) => {
    if (scoreRoleId != "") {
      setScoring(_ => true);
      setScoreMsg(_ => None);
      Roladeck_frontend_api.Api.scoreExistingCandidate(
        ~candidateId=c.id,
        ~roleId=scoreRoleId,
        ~seniority=parseSeniority(scoreSeniority)
      )
      |> Js.Promise.then_(updated => {
        setCandidate(_ => Some(updated));
        setScoring(_ => false);
        setScoreMsg(_ => Some("Scored."));
        Js.Promise.resolve();
      })
      |> Js.Promise.catch(_ => {
        setScoring(_ => false);
        setScoreMsg(_ => Some("Score failed."));
        Js.Promise.resolve();
      })
      |> ignore;
    };
  };

  let exportMarkdown = (c: candidate_record) => {
    let lines = ref(["# Candidate Report\n"]);
    lines := lines^ @ ["**Name:** " ++ c.name];
    List.iter((cs: candidate_score) => {
      lines := lines^ @ [
        "\n## " ++ cs.role_name ++ " - " ++ seniorityLabel(cs.seniority),
        "**Score:** " ++ string_of_int(int_of_float(cs.overall_score *. 100.0)) ++ "%",
        "**Result:** " ++ recLabel(cs.recommendation),
      ];
      if (cs.red_flags_hit != []) {
        lines := lines^ @ ["\n**Red flags:** " ++ String.concat(", ", cs.red_flags_hit)];
      };
      lines := lines^ @ ["\n**Criteria:**"];
      List.iter((cr: criterion_result) => {
        lines := lines^ @ [
          (cr.met ? "- [x] " : "- [ ] ") ++
          "[" ++ tierShort(cr.criterion.tier) ++ "] " ++
          cr.criterion.text
        ];
      }, cs.criterion_results);
    }, c.scores);
    let _ = Roladeck_frontend_api.Api.Clipboard.writeText(String.concat("\n", lines^));
    setCopied(_ => true);
    let _ = Js.Global.setTimeout(~f=() => setCopied(_ => false), 2000);
    ();
  };

  if (loading) {
    <div className="loading-state">{"Loading..." |> React.string}</div>
  } else {
    switch (candidate) {
    | None => <div className="error">{"Candidate not found." |> React.string}</div>
    | Some(c) =>
      <div className="candidate-detail">
        <div className="candidate-header">
          {let isExceptional = List.exists(
              (cs: candidate_score) => cs.recommendation == StrongProgress,
              c.scores
            );
           switch (trustStatus, isExceptional) {
           | (TrustSuspicious, true) =>
             <div className="trust-badge trust-badge-circle">{React.string({js|⚠|js})}</div>
           | (TrustSuspicious, false) =>
             <div className="trust-badge trust-badge-sticker">{React.string({js|⚠ Suspicious|js})}</div>
           | (_, true) =>
             <div className="trust-badge trust-badge-star">{React.string({js|★ Exceptional|js})}</div>
           | _ => React.null
           }}
          <div className="candidate-header-left">
            <h1 className="candidate-name">{React.string(c.name)}</h1>
            <span className="candidate-meta">
              {React.string(
                string_of_int(List.length(c.scores)) ++ " score" ++
                (List.length(c.scores) == 1 ? "" : "s") ++
                " - Added " ++
                String.sub(c.created_at, 0, min(10, String.length(c.created_at)))
              )}
            </span>
            {switch (c.greenhouse_url) {
             | None => React.null
             | Some(url) =>
               <a
                 className="candidate-gh-link"
                 href=url
                 target="_blank"
                 rel="noopener noreferrer">
                 {React.string("View in Greenhouse")}
               </a>
             }}
          </div>
          <div className="candidate-header-right">
            <label className="stage-label">
              {React.string("Stage")}
              <select
                value={stageLabel(c.ats_stage)}
                onChange={e => handleStageChange(c, React.Event.Form.target(e)##value)}
                className={"stage-select " ++ stageClass(c.ats_stage)}
                disabled=stageUpdating>
                {[Screening, Interview, FinalRound, Offer, Hired, Rejected, Withdrawn]
                 |> List.map((s: ats_stage) =>
                      <option key={stageLabel(s)} value={stageLabel(s)}>
                        {React.string(stageLabel(s))}
                      </option>
                    )
                 |> Array.of_list
                 |> React.array}
              </select>
            </label>
            <button
              className="btn-secondary btn-sm"
              disabled=verifying
              onClick={_ => {
                setVerifying(_ => true);
                Roladeck_frontend_api.Api.verifyCandidate(candidateId)
                |> Js.Promise.then_(summary => {
                  setTrustStatus(_ => summary.trust_status);
                  setTrustFlags(_ => summary.trust_flags);
                  setVerifying(_ => false);
                  Js.Promise.resolve();
                })
                |> ignore;
              }}>
              {React.string(verifying ? "Checking..." : "Verify profile")}
            </button>
            <button
              className={"btn-ghost " ++ (copied ? "copied" : "")}
              onClick={_ => exportMarkdown(c)}>
              {React.string(copied ? "Copied!" : "Copy report")}
            </button>
          </div>
        </div>

        {trustStatus == TrustSuspicious && trustFlags != []
          ? <div className="trust-flags">
              <div className="trust-flags-title">{React.string({js|⚠ Suspicious claims detected|js})}</div>
              {trustFlags
               |> List.mapi((i, flag) =>
                    <div key={string_of_int(i)} className="trust-flag-item">{React.string(flag)}</div>
                  )
               |> Array.of_list
               |> React.array}
            </div>
          : React.null}

        {c.scores == []
          ? <div className="empty-state">
              <p className="empty-msg">{"No scores recorded." |> React.string}</p>
            </div>
          : <div className="score-cards">
              {c.scores
               |> List.map((cs: candidate_score) =>
                    <div key={cs.role_id ++ cs.scored_at} className="score-card-full">
                      <div className="score-card-head">
                        <div className="score-card-role-wrap">
                          <button
                            className="score-card-role-link"
                            onClick={_ => onSelectRole(cs.role_id)}>
                            {React.string(cs.role_name)}
                          </button>
                          <span className="score-card-seniority">
                            {React.string(seniorityLabel(cs.seniority))}
                          </span>
                        </div>
                        <div className="score-card-head-right">
                          <span className={"rec-badge " ++ recClass(cs.recommendation)}>
                            {React.string(recLabel(cs.recommendation))}
                          </span>
                          <span className="score-big-inline">
                            {React.string(string_of_int(int_of_float(cs.overall_score *. 100.0)) ++ "%")}
                          </span>
                        </div>
                      </div>

                      <div className="tier-scores">
                        {cs.tier_scores
                         |> List.map((ts: tier_score) =>
                              <div key={tierShort(ts.tier)} className="tier-score-row">
                                <span className={"tier-score-label tier-score-" ++ tierClass(ts.tier)}>
                                  {React.string(tierShort(ts.tier))}
                                </span>
                                <div className="tier-bar-track">
                                  <div
                                    className={"tier-bar-fill tier-bar-" ++ tierClass(ts.tier)}
                                    style={ReactDOM.Style.make(
                                      ~width=string_of_int(int_of_float(ts.score *. 100.0)) ++ "%",
                                      ()
                                    )}
                                  />
                                </div>
                                <span className="tier-score-count">
                                  {React.string(string_of_int(ts.met) ++ "/" ++ string_of_int(ts.total))}
                                </span>
                              </div>
                            )
                         |> Array.of_list
                         |> React.array}
                      </div>

                      {cs.red_flags_hit != []
                        ? <div className="red-flags-hit">
                            <strong>{React.string("Red flags: ")}</strong>
                            {React.string(String.concat(", ", cs.red_flags_hit))}
                          </div>
                        : React.null}

                      <div className="checklist">
                        {cs.criterion_results
                         |> List.map((cr: criterion_result) =>
                              <div
                                key={cr.criterion.text}
                                className={"check-row " ++ (cr.met ? "met" : "unmet")}>
                                <span className="check-icon">{React.string(cr.met ? "v" : "x")}</span>
                                <span className={"check-tier " ++ tierClass(cr.criterion.tier)}>
                                  {React.string(tierShort(cr.criterion.tier))}
                                </span>
                                <span className="check-text">{React.string(cr.criterion.text)}</span>
                              </div>
                            )
                         |> Array.of_list
                         |> React.array}
                      </div>

                      <span className="score-card-date">
                        {React.string("Scored " ++
                          String.sub(cs.scored_at, 0, min(10, String.length(cs.scored_at))))}
                      </span>
                    </div>
                  )
               |> Array.of_list
               |> React.array}
            </div>}

        <div className="score-against-panel">
          <h3 className="score-against-title">{"Score against a role" |> React.string}</h3>
          <p className="score-against-desc">
            {"Uses the stored profile to score this candidate against any playbook." |> React.string}
          </p>
          <div className="score-against-row">
            <select
              className="field-input score-against-select"
              value=scoreRoleId
              onChange={e => setScoreRoleId(_ => React.Event.Form.target(e)##value)}>
              <option value="">{"Select a role..." |> React.string}</option>
              {allSkills
               |> List.map((s: skill_summary) =>
                    <option key={s.id} value={s.id}>{React.string(s.name)}</option>
                  )
               |> Array.of_list
               |> React.array}
            </select>
            <select
              className="field-input score-against-seniority"
              value=scoreSeniority
              onChange={e => setScoreSeniority(_ => React.Event.Form.target(e)##value)}>
              {["Junior", "Mid", "Senior", "Staff", "Principal"]
               |> List.map(level =>
                    <option key=level value=level>{React.string(level)}</option>
                  )
               |> Array.of_list
               |> React.array}
            </select>
            <button
              className={"btn-primary " ++ (scoring ? "btn-loading" : "")}
              disabled={scoring || scoreRoleId == ""}
              onClick={_ => handleScoreAgainstRole(c)}>
              {React.string(scoring ? "Scoring..." : "Score")}
            </button>
            {switch(scoreMsg) {
              | None => React.null
              | Some(msg) => <span className="save-msg">{React.string(msg)}</span>
            }}
          </div>
        </div>
      </div>
    };
  };
};
