open Ahrefs_types.Types;

type poolView =
  | Cards
  | RoleDetail(string, string); /* (role_id, role_name) */

[@react.component]
let make = (~onSelectCandidate: string => unit, ~onGoScore: unit => unit) => {
  let (pool, setPool)       = React.useState(() => []);
  let (skills, setSkills)   = React.useState(() => []);
  let (loading, setLoading) = React.useState(() => true);
  let (category, setCategory) = React.useState(() => "");
  let (view, setView)       = React.useState(() => Cards);
  let (q, setQ)             = React.useState(() => "");
  let (stageFilter, setStageFilter) = React.useState(() => None);

  React.useEffect0(() => {
    Ahrefs_frontend_api.Api.getPool(~role="", ~q="", ())
    |> Js.Promise.then_(candidates => {
      setPool(_ => candidates);
      setLoading(_ => false);
      Js.Promise.resolve();
    })
    |> ignore;
    Ahrefs_frontend_api.Api.getSkills()
    |> Js.Promise.then_(s => {
      setSkills(_ => List.filter(
        (sk: skill_summary) => Ahrefs_frontend_api.Api.isRolePlaybook(sk.id), s
      ));
      Js.Promise.resolve();
    })
    |> ignore;
    None;
  });

  let catLabel = cat => switch (cat) {
    | Tech => "Tech" | Marketing => "Marketing" | Sales => "Sales"
  };

  let catColor = cat => switch (cat) {
    | Tech => "cat-tech" | Marketing => "cat-marketing" | Sales => "cat-sales"
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

  let recLabel = r => switch (r) {
    | StrongProgress => "Strong" | Progress => "Progress"
    | Maybe => "Maybe" | Reject => "Reject"
  };

  let recClass = r => switch (r) {
    | StrongProgress => "rec-green" | Progress => "rec-blue"
    | Maybe => "rec-yellow" | Reject => "rec-red"
  };

  let pct = f => string_of_int(int_of_float(f *. 100.0)) ++ "%";

  let containsStr = (haystack, needle) => {
    let hs = String.lowercase_ascii(haystack);
    let nd = String.lowercase_ascii(needle);
    Js.String.includes(~search=nd, hs);
  };

  /* Build role groups: (skill_summary, list(candidate_summary))
     A candidate appears in every playbook bucket they matched */
  let roleGroups =
    skills
    |> List.filter_map((sk: skill_summary) => {
      let candidates = List.filter(
        (c: candidate_summary) => List.mem(sk.id, c.scored_role_ids),
        pool
      );
      List.length(candidates) > 0 ? Some((sk, candidates)) : None
    });

  /* Apply category filter */
  let filteredGroups =
    if (category == "") roleGroups
    else List.filter(
      ((sk: skill_summary, _)) => catLabel(sk.category) == category,
      roleGroups
    );

  let totalCandidates = List.length(pool);

  switch (view) {
  | Cards =>
    <div className="pool-view">
      <div className="page-header">
        <div className="page-header-left">
          <h1 className="page-title">{"Talent Pool" |> React.string}</h1>
          <p className="page-subtitle">
            {(string_of_int(totalCandidates) ++ " candidate" ++
              (totalCandidates == 1 ? "" : "s") ++
              (List.length(roleGroups) > 0
                ? " across " ++ string_of_int(List.length(roleGroups)) ++ " role" ++
                  (List.length(roleGroups) == 1 ? "" : "s")
                : "")
            ) |> React.string}
          </p>
        </div>
        <button className="btn-primary" onClick={_ => onGoScore()}>
          {React.string("+ Score Candidate")}
        </button>
      </div>

      {List.length(roleGroups) > 0
        ? <div className="picker-controls">
            <div className="cat-filters">
              <button
                className={category == "" ? "cat-btn active" : "cat-btn"}
                onClick={_ => setCategory(_ => "")}>
                {React.string("All")}
              </button>
              {[Tech, Marketing, Sales]
               |> List.filter(cat =>
                    List.exists(
                      ((sk: skill_summary, _)) => sk.category == cat,
                      roleGroups
                    )
                  )
               |> List.map(cat =>
                    <button
                      key={catLabel(cat)}
                      className={
                        "cat-btn " ++ catColor(cat) ++
                        (category == catLabel(cat) ? " active" : "")
                      }
                      onClick={_ => setCategory(_ => catLabel(cat))}>
                      {React.string(catLabel(cat))}
                    </button>
                  )
               |> Array.of_list
               |> React.array}
            </div>
          </div>
        : React.null}

      {loading
        ? <div className="loading-state">{"Loading pool..." |> React.string}</div>
        : List.length(roleGroups) == 0
          ? <div className="empty-state">
              <p className="empty-msg">
                {"No candidates scored yet." |> React.string}
              </p>
              <button className="btn-primary" onClick={_ => onGoScore()}>
                {React.string("Score your first candidate")}
              </button>
            </div>
          : filteredGroups == []
            ? <div className="empty-state">
                <p className="empty-msg">
                  {"No candidates in that category yet." |> React.string}
                </p>
              </div>
            : <div className="role-grid">
                {filteredGroups
                 |> List.map(((sk: skill_summary, candidates)) => {
                      let best = List.fold_left(
                        (acc, (c: candidate_summary)) =>
                          c.top_score > acc ? c.top_score : acc,
                        0.0, candidates
                      );
                      let count = List.length(candidates);
                      <button
                        key={sk.id}
                        className={"role-card pool-role-card " ++ catColor(sk.category)}
                        onClick={_ => setView(_ => RoleDetail(sk.id, sk.name))}>
                        <div className="role-card-cat">
                          {React.string(catLabel(sk.category))}
                        </div>
                        <div className="role-card-name">{React.string(sk.name)}</div>
                        <div className="pool-card-stats">
                          <span className="pool-card-count">
                            {React.string(
                              string_of_int(count) ++ " candidate" ++
                              (count == 1 ? "" : "s")
                            )}
                          </span>
                          <span className="pool-card-best">
                            {React.string("Best " ++ pct(best))}
                          </span>
                        </div>
                        <div className="role-card-meta">
                          <span className="role-card-arrow">{React.string({js|→|js})}</span>
                        </div>
                      </button>
                    })
                 |> Array.of_list
                 |> React.array}
              </div>}
    </div>

  | RoleDetail(roleId, roleName) =>
    let roleCandidates =
      pool
      |> List.filter((c: candidate_summary) => List.mem(roleId, c.scored_role_ids))
      |> (candidates => {
           let filtered = q != ""
             ? List.filter(
                 (c: candidate_summary) => containsStr(c.name, q),
                 candidates
               )
             : candidates;
           switch (stageFilter) {
           | None => filtered
           | Some(s) => List.filter((c: candidate_summary) => c.ats_stage == s, filtered)
           }
         });

    <div className="pool-view">
      <div className="page-header">
        <div className="page-header-left">
          <button className="back-link" onClick={_ => { setView(_ => Cards); setQ(_ => ""); setStageFilter(_ => None); }}>
            {React.string({js|← Talent Pool|js})}
          </button>
          <h1 className="page-title">{React.string(roleName)}</h1>
          <p className="page-subtitle">
            {(string_of_int(List.length(roleCandidates)) ++ " candidate" ++
              (List.length(roleCandidates) == 1 ? "" : "s")) |> React.string}
          </p>
        </div>
        <button className="btn-primary" onClick={_ => onGoScore()}>
          {React.string("+ Score Candidate")}
        </button>
      </div>

      <div className="pool-filters">
        <div className="search-wrap">
          <input
            type_="text"
            placeholder="Search by name..."
            value=q
            onChange={e => setQ(_ => React.Event.Form.target(e)##value)}
            className="search-input"
            autoFocus=true
          />
          {q != ""
            ? <button className="search-clear" onClick={_ => setQ(_ => "")}>
                {React.string("x")}
              </button>
            : React.null}
        </div>
        <select
          value={switch (stageFilter) { | None => "" | Some(s) => stageLabel(s) }}
          onChange={e => {
            let v = React.Event.Form.target(e)##value;
            setStageFilter(_ => switch (v) {
              | "Screening" => Some(Screening) | "Interview" => Some(Interview)
              | "Final Round" => Some(FinalRound) | "Offer" => Some(Offer)
              | "Hired" => Some(Hired) | "Rejected" => Some(Rejected)
              | "Withdrawn" => Some(Withdrawn)
              | _ => None
            });
          }}
          className="filter-select">
          <option value="">{"All stages" |> React.string}</option>
          {[Screening, Interview, FinalRound, Offer, Hired, Rejected, Withdrawn]
           |> List.map(s =>
                <option key={stageLabel(s)} value={stageLabel(s)}>
                  {React.string(stageLabel(s))}
                </option>
              )
           |> Array.of_list
           |> React.array}
        </select>
      </div>

      {roleCandidates == []
        ? <div className="empty-state">
            <p className="empty-msg">
              {(q != "" || stageFilter != None
                ? "No candidates match those filters."
                : "No candidates for this role yet.") |> React.string}
            </p>
          </div>
        : <div className="pool-table">
            <div className="pool-table-head">
              <span>{"Candidate" |> React.string}</span>
              <span>{"Score" |> React.string}</span>
              <span>{"Result" |> React.string}</span>
              <span>{"Stage" |> React.string}</span>
              <span>{"Scored" |> React.string}</span>
            </div>
            {roleCandidates
             |> List.map((c: candidate_summary) =>
                  <button
                    key={c.id}
                    className="pool-row"
                    onClick={_ => onSelectCandidate(c.id)}>
                    <span className="pool-row-name">
                      {List.length(c.scored_role_ids) > 1
                        ? <span className="pool-clip" title="Matched multiple playbooks" />
                        : React.null}
                      {React.string(c.name)}
                    </span>
                    <span className="pool-row-score">
                      <span className="score-bar-wrap">
                        <span
                          className="score-bar-fill"
                          style={ReactDOM.Style.make(~width=pct(c.top_score), ())}
                        />
                      </span>
                      <span className="score-bar-label">
                        {React.string(pct(c.top_score))}
                      </span>
                    </span>
                    <span className={"rec-badge-sm " ++ recClass(c.top_recommendation)}>
                      {React.string(recLabel(c.top_recommendation))}
                    </span>
                    <span className={"stage-pill " ++ stageClass(c.ats_stage)}>
                      {React.string(stageLabel(c.ats_stage))}
                    </span>
                    <span className="pool-row-date">
                      {React.string(String.sub(c.created_at, 0, min(10, String.length(c.created_at))))}
                    </span>
                  </button>
                )
             |> Array.of_list
             |> React.array}
          </div>}
    </div>
  };
};
