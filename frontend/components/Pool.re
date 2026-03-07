open Ahrefs_types.Types;

[@react.component]
let make = (~onSelectCandidate: string => unit, ~onGoScore: unit => unit) => {
  let (pool, setPool) = React.useState(() => []);
  let (loading, setLoading) = React.useState(() => true);
  let (q, setQ) = React.useState(() => "");
  let (roleFilter, setRoleFilter) = React.useState(() => "");
  let (stageFilter, setStageFilter) = React.useState(() => None);

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

  let reload = () => {
    setLoading(_ => true);
    Ahrefs_frontend_api.Api.getPool(~role=roleFilter, ~stage=?stageFilter, ~q, ())
    |> Js.Promise.then_(candidates => {
      setPool(_ => candidates);
      setLoading(_ => false);
      Js.Promise.resolve();
    })
    |> ignore;
  };

  React.useEffect3(() => {
    reload();
    None;
  }, (q, roleFilter, stageFilter));

  let pct = f => string_of_int(int_of_float(f *. 100.0)) ++ "%";

  let roles =
    pool
    |> List.filter_map((c: candidate_summary) =>
         c.top_role_id != "" ? Some((c.top_role_id, c.top_role_name)) : None
       )
    |> List.sort_uniq((a, b) => String.compare(fst(a), fst(b)));

  <div className="pool-view">
    <div className="page-header">
      <div className="page-header-left">
        <h1 className="page-title">{"Talent Pool" |> React.string}</h1>
        <p className="page-subtitle">
          {(string_of_int(List.length(pool)) ++ " candidates") |> React.string}
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
        value=roleFilter
        onChange={e => setRoleFilter(_ => React.Event.Form.target(e)##value)}
        className="filter-select">
        <option value="">{"All roles" |> React.string}</option>
        {roles
         |> List.map(((id, name)) =>
              <option key=id value=id>{React.string(name)}</option>
            )
         |> Array.of_list
         |> React.array}
      </select>

      <select
        value={switch (stageFilter) {
          | None => "" | Some(s) => stageLabel(s)
        }}
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

    {loading
      ? <div className="loading-state">{"Loading pool..." |> React.string}</div>
      : pool == []
        ? <div className="empty-state">
            <p className="empty-msg">
              {(q != "" || roleFilter != "" || stageFilter != None
                ? "No candidates match those filters."
                : "No candidates scored yet.") |> React.string}
            </p>
            <button className="btn-primary" onClick={_ => onGoScore()}>
              {React.string("Score your first candidate")}
            </button>
          </div>
        : <div className="pool-table">
            <div className="pool-table-head">
              <span>{"Candidate" |> React.string}</span>
              <span>{"Role" |> React.string}</span>
              <span>{"Score" |> React.string}</span>
              <span>{"Result" |> React.string}</span>
              <span>{"Stage" |> React.string}</span>
              <span>{"Scored" |> React.string}</span>
            </div>
            {pool
             |> List.map((c: candidate_summary) =>
                  <button
                    key={c.id}
                    className="pool-row"
                    onClick={_ => onSelectCandidate(c.id)}>
                    <span className="pool-row-name">{React.string(c.name)}</span>
                    <span className="pool-row-role">
                      {React.string(
                        c.top_role_name != ""
                          ? c.top_role_name
                          : c.top_role_id
                       )}
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
  </div>;
};
