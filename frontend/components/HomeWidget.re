open Roladeck_types.Types;

type loadState =
  | Idle
  | Loading
  | Done(ai_sourcing_result)
  | Failed(string);

module CompactSourcing = {
  [@react.component]
  let make = () => {
    let (skills, setSkills)       = React.useState(() => []);
    let (category, setCategory)   = React.useState(() => "");
    let (roleId, setRoleId)       = React.useState(() => "");
    let (seniority, setSeniority) = React.useState(() => Senior);
    let (context, setContext)     = React.useState(() => "");
    let (state, setState)         = React.useState(() => Idle);
    let (copied, setCopied)       = React.useState(() => "");

    React.useEffect0(() => {
      Roladeck_frontend_api.Api.getSkills()
      |> Js.Promise.then_((all : list(skill_summary)) => {
        let roles = List.filter(
          (s : skill_summary) => Roladeck_frontend_api.Api.isRolePlaybook(s.id),
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

    let seniorityToString = s => switch (s) {
      | Junior => "Junior" | Mid => "Mid" | Senior => "Senior"
      | Staff => "Staff" | Principal => "Principal"
      | Coordinator => "Coordinator" | Manager => "Manager"
      | SeniorManager => "SeniorManager" | Director => "Director"
      | VP => "VP" | CMO => "CMO" | CRO => "CRO"
    };

    let seniorityOfString = s => switch (s) {
      | "Junior" => Junior | "Mid" => Mid | "Staff" => Staff
      | "Principal" => Principal | "Manager" => Manager
      | "Director" => Director | "VP" => VP
      | _ => Senior
    };

    let categoryOf = (s : skill_summary) => switch (s.category) {
      | Tech => "Tech" | Marketing => "Marketing" | Sales => "Sales"
    };

    let filteredSkills =
      if (String.length(category) == 0) skills
      else List.filter((s : skill_summary) => categoryOf(s) == category, skills);

    /* auto-select first role in filtered list when category changes */
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
        Roladeck_frontend_api.Api.postAiSource(~roleId, ~seniority, ~context)
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

    let copyText = (text, key) => {
      Roladeck_frontend_api.Api.Clipboard.writeText(text)
      |> Js.Promise.then_(_ => {
        setCopied(_ => key);
        let _ = Js.Global.setTimeout(~f=() => setCopied(_ => ""), 2000);
        Js.Promise.resolve();
      })
      |> ignore;
    };

    let isLoading = state == Loading;

    <div className="home-widget-panel">
      <div className="home-widget-header">
        <h3 className="home-widget-title">{"AI Sourcing" |> React.string}</h3>
        <p className="home-widget-desc">
          {"Find candidates on GitHub, conference sites, and LinkedIn." |> React.string}
        </p>
      </div>

      <div className="home-widget-form">
        <div className="widget-form-row">
          <select
            className="filter-select widget-select"
            value=category
            onChange={e => setCategory(_ => React.Event.Form.target(e)##value)}>
            <option value="">{"All categories" |> React.string}</option>
            <option value="Tech">{"Tech" |> React.string}</option>
            <option value="Marketing">{"Marketing" |> React.string}</option>
            <option value="Sales">{"Sales" |> React.string}</option>
          </select>
          <select
            className="filter-select widget-select"
            value=roleId
            onChange={e => setRoleId(_ => React.Event.Form.target(e)##value)}>
            {filteredSkills
             |> List.map((s : skill_summary) =>
               <option key={s.id} value={s.id}>{React.string(s.name)}</option>
             )
             |> Array.of_list
             |> React.array}
          </select>
          <select
            className="filter-select widget-select"
            value={seniorityToString(seniority)}
            onChange={e => setSeniority(_ => seniorityOfString(React.Event.Form.target(e)##value))}>
            {[Junior, Mid, Senior, Staff, Principal, Manager, Director, VP]
             |> List.map(s =>
               <option key={seniorityToString(s)} value={seniorityToString(s)}>
                 {React.string(seniorityToString(s))}
               </option>
             )
             |> Array.of_list
             |> React.array}
          </select>
        </div>

        <textarea
          className="ai-context-input widget-context"
          rows=2
          placeholder="Optional context (focus area, location, etc.)"
          value=context
          onChange={e => setContext(_ => React.Event.Form.target(e)##value)}
        />

        <button
          className={"btn-primary " ++ (isLoading ? "btn-loading" : "")}
          disabled={isLoading || String.length(roleId) == 0}
          onClick=handleSubmit>
          {(isLoading ? "Searching..." : "Find Candidates") |> React.string}
        </button>
        {isLoading
          ? <p className="loading-hint">{"Takes 30-90 seconds." |> React.string}</p>
          : React.null}
      </div>

      {switch (state) {
       | Failed(msg) =>
         <div className="error-panel widget-error">{React.string(msg)}</div>

       | Done(result) =>
         <div className="widget-results">
           {List.length(result.candidates) > 0
             ? <div className="result-section">
                 <div className="result-section-title-sm">{"Candidates" |> React.string}</div>
                 {result.candidates
                  |> List.mapi((i, (c : sourcing_candidate)) =>
                    <div key={string_of_int(i)} className="candidate-card">
                      <a className="candidate-link" href={c.profile_url}
                         target="_blank" rel="noopener noreferrer">
                        {React.string(c.name)}
                      </a>
                      <p className="candidate-rationale">{React.string(c.rationale)}</p>
                    </div>
                  )
                  |> Array.of_list
                  |> React.array}
               </div>
             : React.null}

           {List.length(result.boolean_strings) > 0
             ? <div className="result-section">
                 <div className="result-section-title-sm">{"Boolean Strings" |> React.string}</div>
                 {result.boolean_strings
                  |> List.mapi((i, (b : ai_boolean_string)) =>
                    <div key={string_of_int(i)} className="boolean-card">
                      <div className="boolean-header">
                        <span className="platform-badge">{React.string(b.platform)}</span>
                        <span className="boolean-label">{React.string(b.label)}</span>
                        <button className="copy-btn"
                          onClick={_ => copyText(b.query, "b" ++ string_of_int(i))}>
                          {(copied == "b" ++ string_of_int(i) ? "Copied!" : "Copy") |> React.string}
                        </button>
                      </div>
                      <code className="boolean-query">{React.string(b.query)}</code>
                    </div>
                  )
                  |> Array.of_list
                  |> React.array}
               </div>
             : React.null}

           {String.length(result.outreach_template) > 0
             ? <div className="result-section">
                 <div className="boolean-header">
                   <div className="result-section-title-sm">{"Outreach" |> React.string}</div>
                   <button className="copy-btn"
                     onClick={_ => copyText(result.outreach_template, "outreach")}>
                     {(copied == "outreach" ? "Copied!" : "Copy") |> React.string}
                   </button>
                 </div>
                 <pre className="outreach-body">{React.string(result.outreach_template)}</pre>
               </div>
             : React.null}
         </div>

       | _ => React.null
       }}
    </div>;
  };
};

[@react.component]
let make = (~onGoScore: unit => unit) => {
  let pref =
    Roladeck_frontend_api.Api.LocalStorage.get("home_widget_pref")
    |> Option.value(~default="sourcing");

  switch (pref) {
  | "none" => React.null
  | "scoring" =>
    <div className="home-widget-panel">
      <div className="home-widget-header">
        <h3 className="home-widget-title">{"Quick Score" |> React.string}</h3>
        <p className="home-widget-desc">
          {"Paste a resume or profile to score against any role playbook." |> React.string}
        </p>
      </div>
      <button className="btn-primary" onClick={_ => onGoScore()}>
        {"Open Score Form" |> React.string}
      </button>
    </div>
  | _ =>
    <CompactSourcing />
  };
};
