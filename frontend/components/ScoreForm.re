open Roladeck_types.Types;

let handleFileRead : (React.Event.Form.t, (string => unit)) => unit = [%raw {|
  function(e, cb) {
    var files = e.target.files;
    if (files && files.length > 0) {
      var reader = new FileReader();
      reader.onload = function(evt) { cb(evt.target.result || ""); };
      reader.onerror = function() { cb(""); };
      reader.readAsText(files[0]);
    }
  }
|}];

[@react.component]
let make = (~initialRoleId: option(string), ~onSaved: string => unit) => {
  let (skills, setSkills) = React.useState(() => []);
  let (roleId, setRoleId) = React.useState(() =>
    switch (initialRoleId) { | Some(id) => id | None => "" }
  );
  let (seniority, setSeniority) = React.useState(() => Senior);
  let (candidateName, setCandidateName) = React.useState(() => "");
  let (notes, setNotes) = React.useState(() => "");
  let (stage, setStage) = React.useState(() => Screening);
  let (scoreCat, setScoreCat) = React.useState(() => None);
  let (loading, setLoading) = React.useState(() => false);
  let (result, setResult) = React.useState(() => None);
  let (urlInput, setUrlInput) = React.useState(() => "");
  let (fetchingUrl, setFetchingUrl) = React.useState(() => false);
  let (fetchMsg, setFetchMsg) = React.useState(() => None);
  let (detecting, setDetecting) = React.useState(() => false);
  let (suggestions, setSuggestions) = React.useState(() => []);

  React.useEffect0(() => {
    Roladeck_frontend_api.Api.getSkills()
    |> Js.Promise.then_(all => {
      let playbooks = List.filter(
        (s: skill_summary) => Roladeck_frontend_api.Api.isRolePlaybook(s.id),
        all
      );
      setSkills(_ => playbooks);
      Js.Promise.resolve();
    })
    |> ignore;
    None;
  });

  let seniorityOptions = [Junior, Mid, Senior, Staff, Principal, Manager, Director, VP];

  let filteredSkills = switch (scoreCat) {
    | None => skills
    | Some(cat) => List.filter((s: skill_summary) => s.category == cat, skills)
  };

  let seniorityLabel = s => switch (s) {
    | Junior => "Junior" | Mid => "Mid" | Senior => "Senior"
    | Staff => "Staff" | Principal => "Principal"
    | Coordinator => "Coordinator" | Manager => "Manager"
    | SeniorManager => "Senior Manager" | Director => "Director"
    | VP => "VP" | CMO => "CMO" | CRO => "CRO"
  };

  let stageLabel = s => switch (s) {
    | Screening => "Screening" | Interview => "Interview"
    | FinalRound => "Final Round" | Offer => "Offer"
    | Hired => "Hired" | Rejected => "Rejected" | Withdrawn => "Withdrawn"
  };

  let parseSeniority = s => switch (s) {
    | "Junior" => Junior | "Mid" => Mid | "Staff" => Staff
    | "Principal" => Principal | "Coordinator" => Coordinator
    | "Manager" => Manager | "Senior Manager" => SeniorManager
    | "Director" => Director | "VP" => VP | "CMO" => CMO | "CRO" => CRO
    | _ => Senior
  };

  let parseStage = s => switch (s) {
    | "Interview" => Interview | "Final Round" => FinalRound
    | "Offer" => Offer | "Hired" => Hired
    | "Rejected" => Rejected | "Withdrawn" => Withdrawn
    | _ => Screening
  };

  let tierShort = t => switch (t) {
    | T1_MustHave => "T1" | T2_Differentiator => "T2" | T3_RareUpside => "T3"
  };

  let tierClass = t => switch (t) {
    | T1_MustHave => "t1" | T2_Differentiator => "t2" | T3_RareUpside => "t3"
  };

  let recLabel = r => switch (r) {
    | StrongProgress => "Strong Progress" | Progress => "Progress"
    | Maybe => "Maybe" | Reject => "Reject"
  };

  let recClass = r => switch (r) {
    | StrongProgress => "rec-green" | Progress => "rec-blue"
    | Maybe => "rec-yellow" | Reject => "rec-red"
  };

  let handleFetchUrl = _e => {
    let trimmed = String.trim(urlInput);
    if (String.length(trimmed) > 0) {
      setFetchingUrl(_ => true);
      setFetchMsg(_ => None);
      Roladeck_frontend_api.Api.fetchUrl(trimmed)
      |> Js.Promise.then_(text => {
        setNotes(_ => text);
        setFetchingUrl(_ => false);
        setFetchMsg(_ => Some("Content loaded. Review below and score when ready."));
        Js.Promise.resolve();
      })
      |> Js.Promise.catch(err => {
        let msg = switch (Js.Json.stringifyAny(err)) { | Some(s) => s | None => "Request failed" };
        setFetchingUrl(_ => false);
        setFetchMsg(_ => Some("Could not fetch: " ++ msg));
        Js.Promise.resolve();
      })
      |> ignore;
    };
  };

  let handleDetect = _e => {
    if (String.length(String.trim(notes)) >= 30) {
      setDetecting(_ => true);
      setSuggestions(_ => []);
      Roladeck_frontend_api.Api.classifyCandidate(~candidateNotes=notes)
      |> Js.Promise.then_(matches => {
        setSuggestions(_ => matches);
        setDetecting(_ => false);
        Js.Promise.resolve();
      })
      |> Js.Promise.catch(_ => {
        setDetecting(_ => false);
        Js.Promise.resolve();
      })
      |> ignore;
    };
  };

  let handleSubmit = e => {
    React.Event.Form.preventDefault(e);
    if (roleId != "" && notes != "") {
      setLoading(_ => true);
      setResult(_ => None);
      Roladeck_frontend_api.Api.saveToPool(
        ~name=candidateName,
        ~disciplineId=roleId,
        ~seniority,
        ~notes,
        ~stage
      )
      |> Js.Promise.then_(r => {
        setResult(_ => r);
        setLoading(_ => false);
        Js.Promise.resolve();
      })
      |> ignore;
    };
  };

  <div className="score-form-view">
    <div className="page-header">
      <div className="page-header-left">
        <h1 className="page-title">{"Score a Candidate" |> React.string}</h1>
        <p className="page-subtitle">
          {"Paste a resume, LinkedIn profile, or interview notes. Pick a role and get a structured breakdown in seconds." |> React.string}
        </p>
      </div>
    </div>

    <div className="score-form-layout">
      <div className="score-form-col">
        <form className="panel-form" onSubmit=handleSubmit>
          <div className="score-cat-row">
            <span className="score-cat-label">{React.string("Category")}</span>
            <div className="score-cat-filters">
              {[
                (None,             "All"),
                (Some(Tech),       "Tech"),
                (Some(Marketing),  "Marketing"),
                (Some(Sales),      "Sales"),
              ]
              |> List.map(((cat, label)) =>
                   <button
                     key=label
                     type_="button"
                     className={
                       "score-cat-btn " ++
                       (switch (cat) { | None => "" | Some(Tech) => "cat-tech" | Some(Marketing) => "cat-marketing" | Some(Sales) => "cat-sales" }) ++
                       (scoreCat == cat ? " active" : "")
                     }
                     onClick={_ => { setScoreCat(_ => cat); setRoleId(_ => ""); }}>
                     {React.string(label)}
                   </button>
                 )
              |> Array.of_list
              |> React.array}
            </div>
          </div>

          <div className="form-row-inline">
            <label className="field-label">
              {React.string("Role")}
              <select
                value=roleId
                onChange={e => setRoleId(_ => React.Event.Form.target(e)##value)}
                className="field-input"
                required=true>
                <option value="">{"Select a role..." |> React.string}</option>
                {filteredSkills
                 |> List.map((s: skill_summary) =>
                      <option key={s.id} value={s.id}>{React.string(s.name)}</option>
                    )
                 |> Array.of_list
                 |> React.array}
              </select>
            </label>
            <label className="field-label">
              {React.string("Seniority")}
              <select
                value={seniorityLabel(seniority)}
                onChange={e => setSeniority(_ => parseSeniority(React.Event.Form.target(e)##value))}
                className="field-input">
                {seniorityOptions
                 |> List.map((s: seniority_level) =>
                      <option key={seniorityLabel(s)} value={seniorityLabel(s)}>
                        {React.string(seniorityLabel(s))}
                      </option>
                    )
                 |> Array.of_list
                 |> React.array}
              </select>
            </label>
          </div>

          <div className="form-row-inline">
            <label className="field-label">
              {React.string("Candidate name")}
              <input
                type_="text"
                value=candidateName
                onChange={e => setCandidateName(_ => React.Event.Form.target(e)##value)}
                placeholder="Optional"
                className="field-input"
              />
            </label>
            <label className="field-label">
              {React.string("Pipeline stage")}
              <select
                value={stageLabel(stage)}
                onChange={e => setStage(_ => parseStage(React.Event.Form.target(e)##value))}
                className="field-input">
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
          </div>

          <div className="input-source-section">
            <div className="form-section-label">{"Add candidate info" |> React.string}</div>
            <label className="upload-zone">
              <input
                type_="file"
                className="upload-input"
                onChange={e => handleFileRead(e, text => setNotes(_ => text))}
              />
              <span className="upload-zone-label">{"Upload a file" |> React.string}</span>
              <span className="upload-zone-hint">{"Any text file, HTML export, or saved profile" |> React.string}</span>
            </label>
            <div className="url-fetch-row">
              <input
                type_="text"
                className="field-input"
                placeholder="Or paste a URL  -  GitHub, portfolio, blog post..."
                value=urlInput
                onChange={e => setUrlInput(_ => React.Event.Form.target(e)##value)}
                onKeyDown={e => {
                  if (React.Event.Keyboard.key(e) == "Enter") {
                    React.Event.Keyboard.preventDefault(e);
                    handleFetchUrl(e);
                  }
                }}
              />
              <button
                type_="button"
                className="btn-ghost"
                disabled=fetchingUrl
                onClick=handleFetchUrl>
                {React.string(fetchingUrl ? "Fetching..." : "Fetch")}
              </button>
            </div>
            {switch (fetchMsg) {
             | None => React.null
             | Some(msg) => <p className="fetch-msg">{React.string(msg)}</p>
             }}
          </div>

          <label className="field-label">
            {React.string("Candidate notes")}
            <textarea
              value=notes
              onChange={e => setNotes(_ => React.Event.Form.target(e)##value)}
              placeholder="Or type / paste directly here  -  resume, interview notes, cover letter..."
              rows=8
              className="field-input field-textarea"
            />
          </label>

          <div className="detect-row">
            <button
              type_="button"
              className="btn-ghost detect-btn"
              disabled={detecting || String.length(String.trim(notes)) < 30}
              onClick=handleDetect>
              {React.string(detecting ? "Detecting..." : "Detect playbook")}
            </button>
            {suggestions != []
              ? <div className="suggest-chips">
                  {suggestions
                   |> List.map((m: Roladeck_frontend_api.Api.playbookMatch) =>
                        <button
                          key={m.playbook_id}
                          type_="button"
                          className={"suggest-chip" ++ (roleId == m.playbook_id ? " active" : "")}
                          onClick={_ => setRoleId(_ => m.playbook_id)}>
                          <span className="suggest-chip-name">{React.string(m.playbook_name)}</span>
                          <span className="suggest-chip-pct">
                            {React.string(string_of_int(int_of_float(m.confidence *. 100.0)) ++ "%")}
                          </span>
                        </button>
                      )
                   |> Array.of_list
                   |> React.array}
                </div>
              : React.null}
          </div>

          <button
            type_="submit"
            className="btn-primary"
            disabled={loading || notes == "" || roleId == ""}>
            {React.string(loading ? "Scoring..." : "Score Candidate")}
          </button>
        </form>
      </div>

      <div className="score-result-col">
        {switch (result) {
         | None =>
           <div className="score-result-empty">
             <p>{"Fill in the form and paste candidate text to see their score here." |> React.string}</p>
           </div>
         | Some(record) =>
           switch (record.scores) {
           | [] => React.null
           | [cs, ..._] =>
             <div className="score-result">
               <div className="score-result-top">
                 <div className="score-big">
                   <span className="score-pct">
                     {React.string(string_of_int(int_of_float(cs.overall_score *. 100.0)) ++ "%")}
                   </span>
                   <span className="score-pct-label">{React.string("overall score")}</span>
                 </div>
                 <div className="score-rec-wrap">
                   <span className={"rec-badge " ++ recClass(cs.recommendation)}>
                     {React.string(recLabel(cs.recommendation))}
                   </span>
                   {record.name != "Candidate" && record.name != ""
                     ? <span className="score-name">{React.string(record.name)}</span>
                     : React.null}
                 </div>
                 <button
                   className="btn-ghost"
                   onClick={_ => onSaved(record.id)}>
                   {React.string("View in pool")}
                 </button>
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
                 <div className="checklist-title">{React.string("Criteria")}</div>
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
             </div>
           }
         }}
      </div>
    </div>
  </div>;
};
