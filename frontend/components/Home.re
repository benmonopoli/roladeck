open Ahrefs_types.Types;

[@react.component]
let make = (
  ~onGoPool: unit => unit,
  ~onGoScore: unit => unit,
  ~onGoRoles: unit => unit,
  ~onGoResources: unit => unit,
  ~onGoAiSourcing: unit => unit,
  ~onGoSettings: unit => unit,
  ~onGoSignup: unit => unit,
  ~companyConfigured: bool,
  ~isAnonymous: bool,
) => {
  let (stats, setStats) = React.useState(() => None);
  let (integrations, setIntegrations) = React.useState(() => None);

  React.useEffect0(() => {
    Ahrefs_frontend_api.Api.getPoolStats()
    |> Js.Promise.then_(s => {
      setStats(_ => Some(s));
      Js.Promise.resolve();
    })
    |> ignore;
    Ahrefs_frontend_api.Api.getIntegrationSettings()
    |> Js.Promise.then_(cfg => {
      setIntegrations(_ => Some(cfg));
      Js.Promise.resolve();
    })
    |> ignore;
    None;
  });

  <div className="home">
    <div className="home-hero">
      <h1 className="home-hero-title">
        <span className="hero-line-1">{React.string("Recruiting,")}</span>
        <br />
        <span className="hero-line-2">{React.string("on deck.")}</span>
      </h1>
      <p className="home-hero-sub">
        {"Score candidates. Source talent. Build your pipeline." |> React.string}
      </p>

      {switch (stats) {
       | Some(s) when s.total_candidates > 0 =>
         <div className="home-stats-pill">
           <span className="home-stat">
             <strong>{React.string(string_of_int(s.total_candidates))}</strong>
             {" candidates" |> React.string}
           </span>
           <span className="home-stat-sep">{"/" |> React.string}</span>
           <span className="home-stat">
             <strong>{React.string(string_of_int(s.roles_covered))}</strong>
             {" roles" |> React.string}
           </span>
           {s.hired_count > 0
             ? <>
                 <span className="home-stat-sep">{"/" |> React.string}</span>
                 <span className="home-stat">
                   <strong>{React.string(string_of_int(s.hired_count))}</strong>
                   {" hired" |> React.string}
                 </span>
               </>
             : React.null}
         </div>
       | _ => React.null
       }}

      <div className="hero-foot">
        {companyConfigured
          ? React.null
          : <div className="callout-sticky">
              <p className="callout-sticky-text">
                {"Add your company details so RolaDeck can personalise AI sourcing and outreach." |> React.string}
              </p>
              <button className="callout-sticky-btn" onClick={_ => isAnonymous ? onGoSignup() : onGoSettings()}>
                {"Set up your company" |> React.string}
              </button>
            </div>}

        {switch (integrations) {
         | None => React.null
         | Some(cfg) =>
           let tags = ref([]);
           if (cfg.greenhouse_configured) {
             tags := tags^ @ [
               <span key="gh" className="was-here-tag was-here-greenhouse">
                 {"Greenhouse" |> React.string}<br />{"Was Here" |> React.string}
               </span>
             ];
           };
           if (cfg.ai_key_set) {
             let tag = switch (cfg.ai_provider) {
             | "openai" =>
               <span key="ai" className="was-here-tag was-here-openai">
                 {"Cha" |> React.string}
                 <span className="wh-cross">{"d" |> React.string}</span>
                 <span className="wh-insert">{"T" |> React.string}</span>
                 <br />{"Was Here" |> React.string}
               </span>
             | "perplexity" =>
               <span key="ai" className="was-here-tag was-here-perplexity">
                 {"Perplexity" |> React.string}<br />{"Was Here" |> React.string}
               </span>
             | _ =>
               <span key="ai" className="was-here-tag was-here-claude">
                 {"Claude" |> React.string}<br />{"Was Here" |> React.string}
               </span>
             };
             tags := tags^ @ [tag];
           };
           tags^ == []
             ? React.null
             : <div className="was-here-row">
                 {tags^ |> Array.of_list |> React.array}
               </div>
         }}
      </div>
    </div>

    <div className="home-actions">
      <button className="home-action-card" onClick={_ => onGoScore()}>
        <div className="home-action-label">{"Score a Candidate" |> React.string}</div>
        <p className="home-action-desc">
          {"Paste a resume or LinkedIn profile. Get a structured T1/T2/T3 breakdown in seconds." |> React.string}
        </p>
        <span className="home-action-arrow">{"Start scoring" |> React.string}</span>
      </button>

      <button className="home-action-card" onClick={_ => onGoAiSourcing()}>
        <div className="home-action-label">{"Source with AI" |> React.string}</div>
        <p className="home-action-desc">
          {"AI searches GitHub, blogs, and conferences to surface real candidate profiles." |> React.string}
        </p>
        <span className="home-action-arrow">{"Find talent" |> React.string}</span>
      </button>

      <button className="home-action-card" onClick={_ => onGoPool()}>
        <div className="home-action-label">{"Talent Pool" |> React.string}</div>
        <p className="home-action-desc">
          {switch (stats) {
           | Some(s) when s.total_candidates > 0 =>
             React.string(
               string_of_int(s.total_candidates) ++ " candidates across " ++
               string_of_int(s.roles_covered) ++ " role" ++
               (s.roles_covered == 1 ? "" : "s") ++ "."
             )
           | _ => "Every scored candidate, searchable. Ready when you open a new role." |> React.string
           }}
        </p>
        <span className="home-action-arrow">{"View pool" |> React.string}</span>
      </button>
    </div>

    <div className="home-secondary">
      <button className="home-secondary-link" onClick={_ => onGoRoles()}>
        {"Role Playbooks" |> React.string}
      </button>
      <button className="home-secondary-link" onClick={_ => onGoResources()}>
        {"Recruiter Resources" |> React.string}
      </button>
    </div>
  </div>;
};
