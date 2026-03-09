open Ahrefs_types.Types;

[@react.component]
let make = (~onSelectSkill: string => unit, ~playbooks_only: bool) => {
  let (skills, setSkills) = React.useState(() => []);
  let (query, setQuery) = React.useState(() => "");
  let (category, setCategory) = React.useState(() => None);
  let (loading, setLoading) = React.useState(() => true);

  React.useEffect2(() => {
    setLoading(_ => true);
    Ahrefs_frontend_api.Api.searchSkills(~q=query, ~category?, ())
    |> Js.Promise.then_(results => {
      let filtered = playbooks_only
        ? List.filter((s: skill_summary) => Ahrefs_frontend_api.Api.isRolePlaybook(s.id), results)
        : List.filter((s: skill_summary) => !Ahrefs_frontend_api.Api.isRolePlaybook(s.id), results);
      setSkills(_ => filtered);
      setLoading(_ => false);
      Js.Promise.resolve();
    })
    |> ignore;
    None;
  }, (query, category));

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
    </div>
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
                    className={"role-card " ++ catColor(skill.category)}
                    onClick={_ => onSelectSkill(skill.id)}>
                    <div className="role-card-cat">
                      {React.string(catLabel(skill.category))}
                    </div>
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
