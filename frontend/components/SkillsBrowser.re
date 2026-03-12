open Roladeck_types.Types;

[@react.component]
let make = (
  ~onSelectSkill: string => unit,
  ~onCreatePlaybook: option(string) => unit=(_ => ()),
  ~playbooks_only: bool,
  ~isAnonymous: bool=true,
) => {
  let (skills, setSkills)           = React.useState(() => []);
  let (customSkills, setCustomSkills) = React.useState(() => []);
  let (query, setQuery)             = React.useState(() => "");
  let (category, setCategory)       = React.useState(() => None);
  let (loading, setLoading)         = React.useState(() => true);

  React.useEffect2(() => {
    setLoading(_ => true);
    Roladeck_frontend_api.Api.searchSkills(~q=query, ~category?, ())
    |> Js.Promise.then_(results => {
      let filtered = playbooks_only
        ? List.filter((s: skill_summary) => Roladeck_frontend_api.Api.isRolePlaybook(s.id), results)
        : List.filter((s: skill_summary) => !Roladeck_frontend_api.Api.isRolePlaybook(s.id), results);
      setSkills(_ => filtered);
      setLoading(_ => false);
      Js.Promise.resolve();
    })
    |> ignore;
    None;
  }, (query, category));

  /* Load custom skills for playbooks view when logged in */
  React.useEffect0(() => {
    if (playbooks_only && !isAnonymous) {
      Roladeck_frontend_api.Api.getCustomSkills()
      |> Js.Promise.then_(cs => {
        setCustomSkills(_ => cs);
        Js.Promise.resolve();
      })
      |> ignore;
    };
    None;
  });

  let catLabel = cat => switch (cat) {
    | Tech => "Tech" | Marketing => "Marketing" | Sales => "Sales"
  };

  let catColor = cat => switch (cat) {
    | Tech => "cat-tech" | Marketing => "cat-marketing" | Sales => "cat-sales"
  };

  let count = List.length(skills);

  <div className="picker">
    <div className="page-header">
      <div className="page-header-left">
        <h1 className="page-title">
          {React.string(playbooks_only ? "Role Playbooks" : "Recruiter Resources")}
        </h1>
        <p className="page-subtitle">
          {React.string(
            playbooks_only
              ? "Scoring criteria and interview guides for every discipline."
              : "Frameworks, templates and reference guides for recruiters."
          )}
        </p>
      </div>
      {playbooks_only && !isAnonymous
        ? <button className="btn-primary" onClick={_ => onCreatePlaybook(None)}>
            {React.string("+ Create Playbook")}
          </button>
        : React.null}
    </div>

    {playbooks_only && !isAnonymous && customSkills != []
      ? <div className="custom-playbooks-section">
          <h2 className="section-heading">{"Your Playbooks" |> React.string}</h2>
          <div className="role-grid">
            {customSkills
             |> List.map((sk: skill_summary) =>
                  <div key={sk.id} className={"custom-playbook-row " ++ catColor(sk.category)}>
                    <button
                      className={"role-card role-card-custom " ++ catColor(sk.category)}
                      onClick={_ => onSelectSkill(sk.id)}>
                      <div className="role-card-cat">
                        {React.string(catLabel(sk.category))}
                        <span className="custom-badge">{"Custom" |> React.string}</span>
                      </div>
                      <div className="role-card-name">{React.string(sk.name)}</div>
                      <div className="role-card-desc">
                        {React.string(
                          String.length(sk.description) > 110
                            ? String.sub(sk.description, 0, 110) ++ "..."
                            : sk.description
                        )}
                      </div>
                      <div className="role-card-meta">
                        <span>{React.string(string_of_int(sk.criteria_count) ++ " criteria")}</span>
                        <span className="role-card-arrow">{React.string({js|→|js})}</span>
                      </div>
                    </button>
                    {!isAnonymous
                      ? <button
                          className="btn-ghost custom-edit-btn"
                          onClick={_ => onCreatePlaybook(Some(sk.id))}>
                          {"Edit" |> React.string}
                        </button>
                      : React.null}
                  </div>
                )
             |> Array.of_list
             |> React.array}
          </div>
        </div>
      : React.null}

    <div className="picker-controls">
      <div className="search-wrap">
        <input
          type_="text"
          placeholder={
            playbooks_only
              ? "Search " ++ string_of_int(count) ++ " role playbooks..."
              : "Search recruiter resources..."
          }
          value=query
          onChange={e => setQuery(_ => React.Event.Form.target(e)##value)}
          className="search-input"
          autoFocus=true
        />
        {query != ""
          ? <button className="search-clear" onClick={_ => setQuery(_ => "")}>
              {React.string("x")}
            </button>
          : React.null}
      </div>
      {playbooks_only
        ? <div className="cat-filters">
            <button
              className={category == None ? "cat-btn active" : "cat-btn"}
              onClick={_ => setCategory(_ => None)}>
              {React.string("All")}
            </button>
            {[Tech, Marketing, Sales]
             |> List.map(cat =>
                  <button
                    key={catLabel(cat)}
                    className={
                      "cat-btn " ++ catColor(cat) ++
                      (category == Some(cat) ? " active" : "")
                    }
                    onClick={_ => setCategory(_ => Some(cat))}>
                    {React.string(catLabel(cat))}
                  </button>
                )
             |> Array.of_list
             |> React.array}
          </div>
        : React.null}
    </div>

    {loading
      ? <div className="loading-state">{"Loading..." |> React.string}</div>
      : skills == []
        ? <div className="empty-state">
            <p className="empty-msg">
              {React.string(
                query != ""
                  ? "No results for \"" ++ query ++ "\""
                  : "None found."
              )}
            </p>
            {query != ""
              ? <button className="empty-reset" onClick={_ => { setQuery(_ => ""); setCategory(_ => None); }}>
                  {React.string("Clear search")}
                </button>
              : React.null}
          </div>
        : <div className="role-grid">
            {skills
             |> List.map((skill: skill_summary) =>
                  <button
                    key={skill.id}
                    className={"role-card " ++ (playbooks_only ? catColor(skill.category) : "")}
                    onClick={_ => onSelectSkill(skill.id)}>
                    {playbooks_only
                      ? <div className="role-card-cat">
                          {React.string(catLabel(skill.category))}
                        </div>
                      : React.null}
                    <div className="role-card-name">{React.string(skill.name)}</div>
                    <div className="role-card-desc">
                      {React.string(
                        String.length(skill.description) > 110
                          ? String.sub(skill.description, 0, 110) ++ "..."
                          : skill.description
                      )}
                    </div>
                    <div className="role-card-meta">
                      {playbooks_only
                        ? <span>
                            {React.string(string_of_int(skill.criteria_count) ++ " criteria")}
                          </span>
                        : React.null}
                      <span className="role-card-arrow">{React.string({js|→|js})}</span>
                    </div>
                  </button>
                )
             |> Array.of_list
             |> React.array}
          </div>}
  </div>;
};
