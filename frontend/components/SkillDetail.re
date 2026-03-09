open Ahrefs_types.Types;

type tab = Guide | Score | Source | Pool;

[@react.component]
let make = (~skillId: string, ~onBack: unit => unit, ~onScore as _onScore: string => unit, ~onSelectCandidate: string => unit, ~isAnonymous: bool) => {
  /* ── Skill data ── */
  let (skill, setSkill) = React.useState(() => None);
  let (loading, setLoading) = React.useState(() => true);
  let (tab, setTab) = React.useState(() => Guide);

  /* ── Score panel state ── */
  let (candidateName, setCandidateName) = React.useState(() => "");
  let (scoreSeniority, setScoreSeniority) = React.useState(() => Senior);
  let (notes, setNotes) = React.useState(() => "");
  let (scoreResult, setScoreResult) = React.useState(() => None);
  let (scoreLoading, setScoreLoading) = React.useState(() => false);
  let (savedId, setSavedId) = React.useState(() => None);

  /* ── Source panel state ── */
  let (sourceSeniority, setSourceSeniority) = React.useState(() => Senior);
  let (platform, setPlatform) = React.useState(() => None);
  let (sourceResult, setSourceResult) = React.useState(() => None);
  let (sourceLoading, setSourceLoading) = React.useState(() => false);
  let (copied, setCopied) = React.useState(() => None);

  /* ── Pool panel state ── */
  let (matchedCandidates, setMatchedCandidates) = React.useState(() => []);
  let (poolLoading, setPoolLoading) = React.useState(() => false);

  React.useEffect1(() => {
    setLoading(_ => true);
    Ahrefs_frontend_api.Api.getSkill(skillId)
    |> Js.Promise.then_(result => {
      setSkill(_ => result);
      setLoading(_ => false);
      Js.Promise.resolve();
    })
    |> ignore;
    None;
  }, [|skillId|]);

  /* ── Load matched candidates when Pool tab is active ── */
  let poolKey = (tab == Pool ? "pool-" : "noop-") ++ skillId;
  React.useEffect1(() => {
    if (tab == Pool) {
      setPoolLoading(_ => true);
      setMatchedCandidates(_ => []);
      Ahrefs_frontend_api.Api.getPool(~role=skillId, ())
      |> Js.Promise.then_(candidates => {
        setMatchedCandidates(_ => candidates);
        setPoolLoading(_ => false);
        Js.Promise.resolve();
      })
      |> ignore;
    };
    None;
  }, [|poolKey|]);

  /* ── Shared helpers ── */
  let seniorityLabel = s => switch (s) {
    | Junior => "Junior" | Mid => "Mid" | Senior => "Senior"
    | Staff => "Staff" | Principal => "Principal"
    | Coordinator => "Coordinator" | Manager => "Manager"
    | SeniorManager => "Senior Manager" | Director => "Director"
    | VP => "VP" | CMO => "CMO" | CRO => "CRO"
  };

  let parseSeniority = s => switch (s) {
    | "Junior" => Junior | "Mid" => Mid | "Staff" => Staff
    | "Principal" => Principal | "Coordinator" => Coordinator
    | "Manager" => Manager | "Senior Manager" => SeniorManager
    | "Director" => Director | "VP" => VP | "CMO" => CMO | "CRO" => CRO
    | _ => Senior
  };

  let tierClass = tier => switch (tier) {
    | T1_MustHave => "t1" | T2_Differentiator => "t2" | T3_RareUpside => "t3"
  };

  let tierShort = tier => switch (tier) {
    | T1_MustHave => "T1" | T2_Differentiator => "T2" | T3_RareUpside => "T3"
  };

  let tierTitle = tier => switch (tier) {
    | T1_MustHave => "Must-have" | T2_Differentiator => "Differentiator" | T3_RareUpside => "Rare upside"
  };

  let platformLabel = p => switch (p) {
    | LinkedIn_XRay => "LinkedIn X-Ray" | GitHub_XRay => "GitHub X-Ray"
    | GitHub_Search => "GitHub Search" | ArXiv => "ArXiv"
    | Custom(s) => s
  };

  let recLabel = r => switch (r) {
    | StrongProgress => "Strong Progress" | Progress => "Progress"
    | Maybe => "Maybe" | Reject => "Reject"
  };

  let recClass = r => switch (r) {
    | StrongProgress => "rec-green" | Progress => "rec-blue"
    | Maybe => "rec-yellow" | Reject => "rec-red"
  };

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

  let seniorityOptions = [Junior, Mid, Senior, Staff, Principal, Manager, Director, VP];

  /* ── Score handler  -  saves to pool ── */
  let handleScore = (e, skillRecord: skill_record) => {
    React.Event.Form.preventDefault(e);
    setScoreLoading(_ => true);
    setScoreResult(_ => None);
    setSavedId(_ => None);
    Ahrefs_frontend_api.Api.saveToPool(
      ~name=candidateName,
      ~disciplineId=skillRecord.discipline.id,
      ~seniority=scoreSeniority,
      ~notes,
      ~stage=Screening
    )
    |> Js.Promise.then_(r => {
      switch (r) {
      | None => ()
      | Some(record) =>
        let fakeResult: scoring_result = switch (record.scores) {
          | [] => {
              candidate_id: record.name,
              discipline_id: skillRecord.discipline.id,
              seniority_assessed: scoreSeniority,
              tier_scores: [],
              overall_score: 0.0,
              recommendation: Reject,
              red_flags_hit: [],
              criterion_results: [],
            }
          | [cs, ..._] => {
              candidate_id: record.name,
              discipline_id: cs.role_id,
              seniority_assessed: cs.seniority,
              tier_scores: cs.tier_scores,
              overall_score: cs.overall_score,
              recommendation: cs.recommendation,
              red_flags_hit: cs.red_flags_hit,
              criterion_results: cs.criterion_results,
            }
        };
        setScoreResult(_ => Some(fakeResult));
        setSavedId(_ => Some(record.id));
      };
      setScoreLoading(_ => false);
      Js.Promise.resolve();
    })
    |> ignore;
  };

  /* ── Source handler ── */
  let handleSource = (e, skillRecord) => {
    React.Event.Form.preventDefault(e);
    setSourceLoading(_ => true);
    setSourceResult(_ => None);
    let q: sourcing_query = {
      discipline_id: skillRecord.discipline.id,
      seniority: sourceSeniority,
      platform,
    };
    Ahrefs_frontend_api.Api.getSourceStrings(q)
    |> Js.Promise.then_(r => {
      setSourceResult(_ => r);
      setSourceLoading(_ => false);
      Js.Promise.resolve();
    })
    |> ignore;
  };

  let copyQuery = (label, query) => {
    let _ = Ahrefs_frontend_api.Clipboard.writeText(query);
    setCopied(_ => Some(label));
    let _ = Js.Global.setTimeout(~f=() => setCopied(_ => None), 2000);
    ();
  };

  let exportMarkdown = (r: scoring_result) => {
    let lines = ref(["# Candidate Scoring Report"]);
    lines := (lines^) @ [
      "\n**Candidate:** " ++ r.candidate_id,
      "**Role:** " ++ r.discipline_id,
      "**Seniority:** " ++ seniorityLabel(r.seniority_assessed),
      "**Score:** " ++ Js.Float.toPrecision(~digits=1, r.overall_score *. 100.0) ++ "%",
      "**Recommendation:** " ++ recLabel(r.recommendation),
    ];
    if (r.red_flags_hit != []) {
      lines := (lines^) @ ["\n## Red Flags"];
      List.iter(f => lines := (lines^) @ ["- [!] " ++ f], r.red_flags_hit);
    };
    lines := (lines^) @ ["\n## Criteria"];
    List.iter(cr =>
      lines := (lines^) @ [
        (cr.met ? "- [x] " : "- [ ] ") ++
        "[" ++ tierShort(cr.criterion.tier) ++ "] " ++
        cr.criterion.text
      ],
      r.criterion_results
    );
    let _ = Ahrefs_frontend_api.Clipboard.writeText(String.concat("\n", lines^));
    ();
  };

  /* ── Guide tab ── */
  let renderGuide = (s: skill_record) => {
    let t1 = List.filter((c: skill_criterion) => c.tier == T1_MustHave, s.criteria);
    let t2 = List.filter((c: skill_criterion) => c.tier == T2_Differentiator, s.criteria);
    let t3 = List.filter((c: skill_criterion) => c.tier == T3_RareUpside, s.criteria);
    <div className="guide-content">
      {[|(T1_MustHave, t1, "Dealbreaker if missing. Every strong candidate must have all of these."),
         (T2_Differentiator, t2, "Separates good from great. Nice-to-have but not required."),
         (T3_RareUpside, t3, "Exceptional depth. Rare - most candidates won't have these.")|]
       |> Array.map(((tier, criteria, hint)) =>
            criteria == [] ? React.null :
            <div key={tierShort(tier)} className={"tier-block tier-block-" ++ tierClass(tier)}>
              <div className="tier-header">
                <span className={"tier-pill " ++ tierClass(tier)}>
                  {React.string(tierShort(tier) ++ " - " ++ tierTitle(tier))}
                </span>
                <span className="tier-hint">{React.string(hint)}</span>
              </div>
              <ul className="criteria-list">
                {criteria
                 |> List.map(c =>
                      <li key={c.text} className="criterion">
                        {React.string(c.text)}
                      </li>
                    )
                 |> Array.of_list
                 |> React.array}
              </ul>
            </div>
          )
       |> React.array}

      {s.red_flags != []
        ? <div className="guide-section">
            <h3 className="guide-section-title">
              <span className="section-icon">{"!" |> React.string}</span>
              {React.string("Red Flags")}
            </h3>
            <p className="guide-section-hint">
              {"Disqualifying signals regardless of other positives." |> React.string}
            </p>
            <ul className="red-flags-list">
              {s.red_flags
               |> List.map(flag =>
                    <li key={flag} className="red-flag">{React.string(flag)}</li>
                  )
               |> Array.of_list
               |> React.array}
            </ul>
          </div>
        : React.null}

      {s.sourcing_strings != []
        ? <div className="guide-section">
            <h3 className="guide-section-title">
              <span className="section-icon">{"#" |> React.string}</span>
              {React.string("Sourcing Strings")}
            </h3>
            {s.sourcing_strings
             |> List.map((ss : sourcing_string) =>
                  <div key={ss.label} className="source-card">
                    <div className="source-card-head">
                      <span className="platform-pill">
                        {React.string(platformLabel(ss.platform))}
                      </span>
                      <span className="source-card-label">{React.string(ss.label)}</span>
                      <button
                        className={"copy-btn " ++ (copied == Some(ss.label) ? "copied" : "")}
                        onClick={_ => copyQuery(ss.label, ss.query)}>
                        {React.string(copied == Some(ss.label) ? "Copied" : "Copy")}
                      </button>
                    </div>
                    <pre className="source-pre">{React.string(ss.query)}</pre>
                  </div>
                )
             |> Array.of_list
             |> React.array}
          </div>
        : React.null}

      {s.comp_ranges != []
        ? <div className="guide-section">
            <h3 className="guide-section-title">
              <span className="section-icon">{"$" |> React.string}</span>
              {React.string("Compensation")}
            </h3>
            <p className="guide-section-hint">{"US market, Series B-D. Adjust for location and stage." |> React.string}</p>
            <table className="comp-table">
              <thead>
                <tr>
                  <th>{React.string("Level")}</th>
                  <th>{React.string("Base Range")}</th>
                </tr>
              </thead>
              <tbody>
                {s.comp_ranges
                 |> List.map(cr =>
                      <tr key={seniorityLabel(cr.level)}>
                        <td>{React.string(seniorityLabel(cr.level))}</td>
                        <td>{React.string(
                          "$" ++ string_of_int(cr.base_min / 1000) ++ "k - $" ++
                          string_of_int(cr.base_max / 1000) ++ "k"
                        )}</td>
                      </tr>
                    )
                 |> Array.of_list
                 |> React.array}
              </tbody>
            </table>
          </div>
        : React.null}
    </div>
  };

  /* ── Score tab ── */
  let renderScore = (s: skill_record) =>
    <div className="panel">
      <p className="panel-hint">
        {"Paste any text about the candidate: resume, LinkedIn bio, interview notes. We extract keywords and check each criterion (met when 2+ keywords match)." |> React.string}
      </p>
      <form
        className="panel-form"
        onSubmit={e => handleScore(e, s)}>
        <div className="form-row-inline">
          <label className="field-label">
            {React.string("Candidate name")}
            <input
              type_="text"
              value=candidateName
              onChange={e => setCandidateName(_ => React.Event.Form.target(e)##value)}
              placeholder="Optional - e.g. Jane Smith"
              className="field-input"
            />
          </label>
          <label className="field-label">
            {React.string("Seniority")}
            <select
              value={seniorityLabel(scoreSeniority)}
              onChange={e => setScoreSeniority(_ => parseSeniority(React.Event.Form.target(e)##value))}
              className="field-input">
              {seniorityOptions
               |> List.map(s =>
                    <option key={seniorityLabel(s)} value={seniorityLabel(s)}>
                      {React.string(seniorityLabel(s))}
                    </option>
                  )
               |> Array.of_list
               |> React.array}
            </select>
          </label>
        </div>
        <label className="field-label">
          {React.string("Candidate notes")}
          <textarea
            value=notes
            onChange={e => setNotes(_ => React.Event.Form.target(e)##value)}
            placeholder="Paste the candidate's resume, LinkedIn summary, cover letter, or your notes from the interview here. The more detail, the better the score."
            rows=7
            className="field-input field-textarea"
          />
        </label>
        <button
          type_="submit"
          className="btn-primary"
          disabled={scoreLoading || notes == ""}>
          {React.string(scoreLoading ? "Scoring..." : "Score Candidate")}
        </button>
      </form>

      {switch (scoreResult) {
       | None => React.null
       | Some(r) =>
         <div className="score-result">
           <div className="score-result-top">
             <div className="score-big">
               <span className="score-pct">
                 {React.string(Js.Float.toPrecision(~digits=1, r.overall_score *. 100.0) ++ "%")}
               </span>
               <span className="score-pct-label">{React.string("overall score")}</span>
             </div>
             <div className="score-rec-wrap">
               <span className={"rec-badge " ++ recClass(r.recommendation)}>
                 {React.string(recLabel(r.recommendation))}
               </span>
               <span className="score-name">
                 {React.string(
                   r.candidate_id == "Candidate" ? "" : r.candidate_id
                 )}
               </span>
             </div>
             <button
               className="btn-ghost"
               onClick={_ => exportMarkdown(r)}>
               {React.string("Copy markdown")}
             </button>
             {switch (savedId) {
              | None => React.null
              | Some(_) =>
                <span className="saved-badge">{"Saved to pool" |> React.string}</span>
              }}
           </div>

           <div className="tier-scores">
             {r.tier_scores
              |> List.map((ts: tier_score) =>
                   <div key={tierShort(ts.tier)} className="tier-score-row">
                     <span className={"tier-score-label tier-score-" ++ tierClass(ts.tier)}>
                       {React.string(tierShort(ts.tier))}
                     </span>
                     <div className="tier-bar-track">
                       <div
                         className={"tier-bar-fill tier-bar-" ++ tierClass(ts.tier)}
                         style={ReactDOM.Style.make(
                           ~width=Js.Float.toPrecision(~digits=1, ts.score *. 100.0) ++ "%",
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

           {r.red_flags_hit != []
             ? <div className="red-flags-hit">
                 <strong>{React.string("Red flags: ")}</strong>
                 {React.string(String.concat(", ", r.red_flags_hit))}
               </div>
             : React.null}

           <div className="checklist">
             <div className="checklist-title">{React.string("Criteria checklist")}</div>
             {r.criterion_results
              |> List.map(cr =>
                   <div
                     key={cr.criterion.text}
                     className={"check-row " ++ (cr.met ? "met" : "unmet")}>
                     <span className="check-icon">
                       {React.string(cr.met ? "+" : "-")}
                     </span>
                     <span className={"check-tier " ++ tierClass(cr.criterion.tier)}>
                       {React.string(tierShort(cr.criterion.tier))}
                     </span>
                     <span className="check-text">
                       {React.string(cr.criterion.text)}
                     </span>
                   </div>
                 )
              |> Array.of_list
              |> React.array}
           </div>
         </div>
       }}
    </div>;

  /* ── Source tab ── */
  let renderSource = (s: skill_record) =>
    <div className="panel">
      <p className="panel-hint">
        {"Select a seniority level and platform to generate Boolean search strings. Copy any string and paste it directly into Google, LinkedIn, or GitHub." |> React.string}
      </p>
      <form
        className="panel-form"
        onSubmit={e => handleSource(e, s)}>
        <div className="form-row-inline">
          <label className="field-label">
            {React.string("Seniority")}
            <select
              value={seniorityLabel(sourceSeniority)}
              onChange={e => setSourceSeniority(_ => parseSeniority(React.Event.Form.target(e)##value))}
              className="field-input">
              {seniorityOptions
               |> List.map(sv =>
                    <option key={seniorityLabel(sv)} value={seniorityLabel(sv)}>
                      {React.string(seniorityLabel(sv))}
                    </option>
                  )
               |> Array.of_list
               |> React.array}
            </select>
          </label>
          <label className="field-label">
            {React.string("Platform")}
            <select
              value={switch (platform) {
                | None => "All platforms"
                | Some(LinkedIn_XRay) => "LinkedIn X-Ray"
                | Some(GitHub_XRay) => "GitHub X-Ray"
                | Some(GitHub_Search) => "GitHub Search"
                | Some(ArXiv) => "ArXiv"
                | Some(Custom(x)) => x
              }}
              onChange={e => {
                let v = React.Event.Form.target(e)##value;
                setPlatform(_ => switch (v) {
                  | "LinkedIn X-Ray" => Some(LinkedIn_XRay)
                  | "GitHub X-Ray"   => Some(GitHub_XRay)
                  | "GitHub Search"  => Some(GitHub_Search)
                  | "ArXiv"          => Some(ArXiv)
                  | _                => None
                });
              }}
              className="field-input">
              {["All platforms", "LinkedIn X-Ray", "GitHub X-Ray", "GitHub Search", "ArXiv"]
               |> List.map(p =>
                    <option key=p value=p>{React.string(p)}</option>
                  )
               |> Array.of_list
               |> React.array}
            </select>
          </label>
        </div>
        <button type_="submit" className="btn-primary" disabled=sourceLoading>
          {React.string(sourceLoading ? "Loading..." : "Get Strings")}
        </button>
      </form>

      {switch (sourceResult) {
       | None => React.null
       | Some(r) =>
         r.strings == []
           ? <p className="panel-empty">
               {"No strings found for this combination." |> React.string}
             </p>
           : <div className="source-results">
               {r.strings
                |> List.map((ss : sourcing_string) =>
                     <div key={ss.label} className="source-card">
                       <div className="source-card-head">
                         <span className="platform-pill">
                           {React.string(platformLabel(ss.platform))}
                         </span>
                         <span className="source-card-label">
                           {React.string(ss.label)}
                         </span>
                         <button
                           className={"copy-btn " ++ (copied == Some(ss.label) ? "copied" : "")}
                           onClick={_ => copyQuery(ss.label, ss.query)}>
                           {React.string(copied == Some(ss.label) ? "Copied" : "Copy")}
                         </button>
                       </div>
                       <pre className="source-pre">{React.string(ss.query)}</pre>
                     </div>
                   )
                |> Array.of_list
                |> React.array}
             </div>
       }}
    </div>;

  /* ── Pool tab ── */
  let renderPool = () =>
    <div className="panel">
      {poolLoading
        ? <div className="loading-small">{"Loading matched candidates..." |> React.string}</div>
        : matchedCandidates == []
          ? <div className="panel-empty">
              <p>{"No candidates have been scored against this role yet." |> React.string}</p>
              <button className="btn-primary" onClick={_ => setTab(_ => Score)}>
                {"Score a candidate" |> React.string}
              </button>
            </div>
          : <>
              <div className="matched-header">
                <span className="matched-count">
                  {React.string(
                    string_of_int(List.length(matchedCandidates)) ++
                    (List.length(matchedCandidates) == 1 ? " candidate matched" : " candidates matched")
                  )}
                </span>
              </div>
              <table className="candidates-table">
                <thead>
                  <tr>
                    <th>{React.string("Candidate")}</th>
                    <th>{React.string("Score")}</th>
                    <th>{React.string("Result")}</th>
                    <th>{React.string("Stage")}</th>
                    <th>{React.string("")}</th>
                  </tr>
                </thead>
                <tbody>
                  {matchedCandidates
                   |> List.map((c: candidate_summary) =>
                        <tr key={c.id}>
                          <td className="pool-name">{React.string(c.name == "" ? "Unnamed" : c.name)}</td>
                          <td className="pool-score">
                            <div className="score-bar-wrap">
                              <div className="score-bar-track">
                                <div
                                  className="score-bar-fill"
                                  style={ReactDOM.Style.make(
                                    ~width=Js.Float.toPrecision(~digits=1, c.top_score *. 100.0) ++ "%",
                                    ()
                                  )}
                                />
                              </div>
                              <span className="score-bar-pct">
                                {React.string(Js.Float.toPrecision(~digits=1, c.top_score *. 100.0) ++ "%")}
                              </span>
                            </div>
                          </td>
                          <td>
                            <span className={"rec-badge " ++ recClass(c.top_recommendation)}>
                              {React.string(recLabel(c.top_recommendation))}
                            </span>
                          </td>
                          <td>
                            <span className={"stage-pill " ++ stageClass(c.ats_stage)}>
                              {React.string(stageLabel(c.ats_stage))}
                            </span>
                          </td>
                          <td>
                            <button
                              className="btn-ghost btn-sm"
                              onClick={_ => onSelectCandidate(c.id)}>
                              {React.string("View")}
                            </button>
                          </td>
                        </tr>
                      )
                   |> Array.of_list
                   |> React.array}
                </tbody>
              </table>
            </>
      }
    </div>;

  /* ── Render ── */
  if (loading) {
    <div className="loading-state">{"Loading..." |> React.string}</div>
  } else {
    switch (skill) {
    | None =>
      <div className="error">{"Role not found." |> React.string}</div>
    | Some(s) =>
      <div className="role-hub">
        <div className="role-hub-head">
          <button className="back-link" onClick={_ => onBack()}>
            {React.string({js|← Back|js})}
          </button>
          <div className="role-hub-name">{React.string(s.discipline.name)}</div>
          <div className="role-hub-desc">{React.string(s.discipline.description)}</div>
        </div>

        <div className="role-tabs">
          <button
            className={"role-tab " ++ (tab == Guide ? "active" : "")}
            onClick={_ => setTab(_ => Guide)}>
            {React.string("Hiring Guide")}
          </button>
          {isAnonymous ? React.null :
            <button
              className={"role-tab " ++ (tab == Score ? "active" : "")}
              onClick={_ => setTab(_ => Score)}>
              {React.string("Score a Candidate")}
            </button>}
          {isAnonymous ? React.null :
            <button
              className={"role-tab " ++ (tab == Source ? "active" : "")}
              onClick={_ => setTab(_ => Source)}>
              {React.string("Find Candidates")}
            </button>}
          {isAnonymous ? React.null :
            <button
              className={"role-tab " ++ (tab == Pool ? "active" : "")}
              onClick={_ => setTab(_ => Pool)}>
              {React.string("Matched Candidates")}
            </button>}
        </div>

        <div className="role-tab-body">
          {switch (tab) {
           | Guide => renderGuide(s)
           | Score => renderScore(s)
           | Source => renderSource(s)
           | Pool => renderPool()
           }}
        </div>
      </div>
    };
  };
};
