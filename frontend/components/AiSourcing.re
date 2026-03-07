open Ahrefs_types.Types;

type loadState =
  | Idle
  | Loading
  | Done(ai_sourcing_result)
  | Failed(string);

let seniorityOptions = [
  (Junior, "Junior"),
  (Mid, "Mid"),
  (Senior, "Senior"),
  (Staff, "Staff"),
  (Principal, "Principal"),
  (Manager, "Manager"),
  (Director, "Director"),
  (VP, "VP"),
];

let seniorityOfString = s =>
  switch (s) {
  | "Junior" => Junior | "Mid" => Mid | "Senior" => Senior
  | "Staff" => Staff | "Principal" => Principal
  | "Manager" => Manager | "Director" => Director | "VP" => VP
  | _ => Senior
  };

let seniorityToString = s =>
  switch (s) {
  | Junior => "Junior" | Mid => "Mid" | Senior => "Senior"
  | Staff => "Staff" | Principal => "Principal"
  | Coordinator => "Coordinator" | Manager => "Manager"
  | SeniorManager => "SeniorManager" | Director => "Director"
  | VP => "VP" | CMO => "CMO" | CRO => "CRO"
  };

[@react.component]
let make = () => {
  let (skills : list(skill_summary), setSkills) = React.useState(() => []);
  let (category, setCategory)     = React.useState(() => "");
  let (roleId, setRoleId)         = React.useState(() => "");
  let (seniority, setSeniority)   = React.useState(() => Senior);
  let (context, setContext)       = React.useState(() => "");
  let (state, setState)           = React.useState(() => Idle);
  let (copied, setCopied)         = React.useState(() => "");

  React.useEffect0(() => {
    Ahrefs_frontend_api.Api.getSkills()
    |> Js.Promise.then_((all : list(skill_summary)) => {
      let roles = List.filter(
        (s : skill_summary) => Ahrefs_frontend_api.Api.isRolePlaybook(s.id),
        all
      );
      setSkills(_ => roles);
      switch (List.nth_opt(roles, 0)) {
      | Some(first) => setRoleId(_ => first.id)
      | None => ()
      };
      Js.Promise.resolve();
    })
    |> ignore;
    None;
  });

  let categoryLabel = (s : skill_summary) => switch (s.category) {
    | Tech => "Tech" | Marketing => "Marketing" | Sales => "Sales"
  };

  let filteredSkills =
    if (String.length(category) == 0) skills
    else List.filter((s : skill_summary) => categoryLabel(s) == category, skills);

  /* When category changes, auto-select first role in filtered list */
  React.useEffect1(() => {
    switch (List.nth_opt(filteredSkills, 0)) {
    | Some(first) => setRoleId(_ => first.id)
    | None => setRoleId(_ => "")
    };
    None;
  }, [|category|]);

  let handleSubmit = _ => {
    if (String.length(roleId) == 0) { () } else {
      setState(_ => Loading);
      Ahrefs_frontend_api.Api.postAiSource(~roleId, ~seniority, ~context)
      |> Js.Promise.then_(result => {
        setState(_ => Done(result));
        Js.Promise.resolve();
      })
      |> Js.Promise.catch(err => {
        let msg = switch (Js.Json.stringifyAny(err)) {
          | Some(s) => s | None => "Unknown error"
        };
        setState(_ => Failed(msg));
        Js.Promise.resolve();
      })
      |> ignore;
    }
  };

  let copyToClipboard = (text, key) => {
    Ahrefs_frontend_api.Api.Clipboard.writeText(text)
    |> Js.Promise.then_(_ => {
      setCopied(_ => key);
      let _ = Js.Global.setTimeout(~f=() => setCopied(_ => ""), 2000);
      Js.Promise.resolve();
    })
    |> ignore;
  };

  <div className="ai-sourcing-view">
    <div className="page-header">
      <div>
        <h1 className="page-title">{"AI Sourcing" |> React.string}</h1>
        <p className="page-subtitle">
          {"AI searches GitHub, conferences, blogs, and the web to find real candidates for your role." |> React.string}
        </p>
      </div>
    </div>

    <div className="ai-sourcing-form">
      <div className="form-row">
        <label className="form-label">{"Category" |> React.string}</label>
        <select
          className="filter-select"
          value=category
          onChange={e => setCategory(_ => React.Event.Form.target(e)##value)}>
          <option value="">{"All categories" |> React.string}</option>
          <option value="Tech">{"Tech" |> React.string}</option>
          <option value="Marketing">{"Marketing" |> React.string}</option>
          <option value="Sales">{"Sales" |> React.string}</option>
        </select>
      </div>

      <div className="form-row">
        <label className="form-label">{"Role Playbook" |> React.string}</label>
        <select
          className="filter-select"
          value=roleId
          onChange={e => setRoleId(_ => React.Event.Form.target(e)##value)}>
          {filteredSkills
           |> List.map((s : skill_summary) =>
             <option key={s.id} value={s.id}>
               {React.string(s.name)}
             </option>
           )
           |> Array.of_list
           |> React.array}
        </select>
      </div>

      <div className="form-row">
        <label className="form-label">{"Seniority" |> React.string}</label>
        <select
          className="filter-select"
          value={seniorityToString(seniority)}
          onChange={e => setSeniority(_ => seniorityOfString(React.Event.Form.target(e)##value))}>
          {seniorityOptions
           |> List.map(((_lvl, label)) =>
             <option key=label value=label>
               {React.string(label)}
             </option>
           )
           |> Array.of_list
           |> React.array}
        </select>
      </div>

      <div className="form-row">
        <label className="form-label">
          {"Additional Context " |> React.string}
          <span className="form-label-hint">{"(optional)" |> React.string}</span>
        </label>
        <textarea
          className="field-input ai-context-input"
          rows=6
          placeholder="e.g. We need someone with distributed systems experience, ideally from fintech or infra companies."
          value=context
          onChange={e => setContext(_ => React.Event.Form.target(e)##value)}
        />
      </div>

      <button
        className={"btn-primary " ++ (state == Loading ? "btn-loading" : "")}
        disabled={state == Loading || String.length(roleId) == 0}
        onClick=handleSubmit>
        {(state == Loading
          ? "Searching the web..."
          : "Find Candidates"
        ) |> React.string}
      </button>
      {state == Loading
        ? <p className="loading-hint">
            {"This typically takes 30-90 seconds." |> React.string}
          </p>
        : React.null}
    </div>

    {switch (state) {
     | Failed(msg) =>
       <div className="error-panel">
         <strong>{"Error: " |> React.string}</strong>
         {React.string(msg)}
       </div>

     | Done(result) =>
       <div className="ai-sourcing-results">

         {List.length(result.candidates) > 0
           ? <div className="result-section">
               <h2 className="result-section-title">
                 {"Candidate Profiles" |> React.string}
               </h2>
               <div className="candidates-list">
                 {result.candidates
                  |> List.mapi((i, (c : sourcing_candidate)) =>
                    <div key={string_of_int(i)} className="candidate-card">
                      <a
                        className="candidate-link"
                        href={c.profile_url}
                        target="_blank"
                        rel="noopener noreferrer">
                        {React.string(c.name)}
                      </a>
                      <p className="candidate-rationale">
                        {React.string(c.rationale)}
                      </p>
                    </div>
                  )
                  |> Array.of_list
                  |> React.array}
               </div>
             </div>
           : React.null}

         {List.length(result.boolean_strings) > 0
           ? <div className="result-section">
               <h2 className="result-section-title">
                 {"Boolean Search Strings" |> React.string}
               </h2>
               {result.boolean_strings
                |> List.mapi((i, (b : ai_boolean_string)) =>
                  <div key={string_of_int(i)} className="boolean-card">
                    <div className="boolean-header">
                      <span className="platform-badge">
                        {React.string(b.platform)}
                      </span>
                      <span className="boolean-label">
                        {React.string(b.label)}
                      </span>
                      <button
                        className="copy-btn"
                        onClick={_ => copyToClipboard(b.query, "bool-" ++ string_of_int(i))}>
                        {(copied == "bool-" ++ string_of_int(i) ? "Copied!" : "Copy") |> React.string}
                      </button>
                    </div>
                    <code className="boolean-query">
                      {React.string(b.query)}
                    </code>
                  </div>
                )
                |> Array.of_list
                |> React.array}
             </div>
           : React.null}

         {List.length(result.target_companies) > 0
           ? <div className="result-section">
               <h2 className="result-section-title">
                 {"Target Companies" |> React.string}
               </h2>
               <div className="company-chips">
                 {result.target_companies
                  |> List.mapi((i, company) =>
                    <span key={string_of_int(i)} className="company-chip">
                      {React.string(company)}
                    </span>
                  )
                  |> Array.of_list
                  |> React.array}
               </div>
             </div>
           : React.null}

         {String.length(result.outreach_template) > 0
           ? <div className="result-section">
               <h2 className="result-section-title">
                 {"Outreach Template" |> React.string}
               </h2>
               <div className="outreach-card">
                 <div className="outreach-header">
                   <button
                     className="copy-btn"
                     onClick={_ => copyToClipboard(result.outreach_template, "outreach")}>
                     {(copied == "outreach" ? "Copied!" : "Copy") |> React.string}
                   </button>
                 </div>
                 <pre className="outreach-body">
                   {React.string(result.outreach_template)}
                 </pre>
               </div>
             </div>
           : React.null}

       </div>

     | _ => React.null
     }}
  </div>;
};
