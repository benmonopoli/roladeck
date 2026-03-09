type view =
  | Home
  | Pool
  | Score(option(string))
  | CandidateDetail(string)
  | RoleGuide(string)
  | Resources
  | ResourceDetail(string)
  | AiSourcing
  | Settings
  | Login
  | Signup;

type authState =
  | Checking
  | Anonymous
  | LoggedIn(Ahrefs_frontend_api.Api.userInfo);

[@react.component]
let make = () => {
  let (view, setView)                   = React.useState(() => Home);
  let (companyConfigured, setCompanyConfigured) = React.useState(() => false);
  let (authState, setAuthState)         = React.useState(() => Checking);

  React.useEffect0(() => {
    Ahrefs_frontend_api.Api.getMe()
    |> Js.Promise.then_(result => {
      switch (result) {
      | Some(user) =>
        setAuthState(_ => LoggedIn(user));
        Ahrefs_frontend_api.Api.getCompanyProfile()
        |> Js.Promise.then_(p => {
          setCompanyConfigured(_ => String.length(p.Ahrefs_frontend_api.Api.company_name) > 0);
          Js.Promise.resolve();
        })
        |> ignore;
      | None =>
        setAuthState(_ => Anonymous)
      };
      Js.Promise.resolve();
    })
    |> ignore;
    None;
  });

  let isActiveView = v => switch (v, view) {
    | (Resources, Resources) | (Resources, ResourceDetail(_)) => true
    | (RoleGuide(""), RoleGuide(_)) => true
    | _ => v == view
  };

  let navItem = (label, current, target) =>
    <button
      className={"nav-link " ++ (isActiveView(current) ? "active" : "")}
      onClick={_ => setView(_ => target)}>
      {React.string(label)}
    </button>;

  let isAnonymous = switch (authState) { | Anonymous => true | _ => false };

  let authGate = _pageName =>
    <div className="auth-gate">
      <div className="auth-gate-note">
        <p className="auth-gate-note-text">
          {React.string("You need an account to access this feature. ")}
          <button className="auth-gate-hl auth-gate-hl-pink" onClick={_ => setView(_ => Login)}>
            {React.string("Sign in")}
          </button>
          {React.string(" or ")}
          <button className="auth-gate-hl auth-gate-hl-blue" onClick={_ => setView(_ => Signup)}>
            {React.string("set up your company")}
          </button>
          {React.string(" to continue.")}
        </p>
      </div>
    </div>;

  let handleLogout = _ => {
    Ahrefs_frontend_api.Api.logout()
    |> Js.Promise.then_(_ => {
      setAuthState(_ => Anonymous);
      setView(_ => Home);
      Js.Promise.resolve();
    })
    |> ignore;
  };

  switch (authState) {
  | Checking =>
    <div className="auth-checking">
      {"Loading..." |> React.string}
    </div>

  | _ =>
    switch (view) {
    | Login =>
      <Components.Login
        onLogin={user => {
          setAuthState(_ => LoggedIn(user));
          Ahrefs_frontend_api.Api.getCompanyProfile()
          |> Js.Promise.then_(p => {
            setCompanyConfigured(_ => String.length(p.Ahrefs_frontend_api.Api.company_name) > 0);
            Js.Promise.resolve();
          })
          |> ignore;
          setView(_ => Home);
        }}
        onGoSignup={() => setView(_ => Signup)}
        onGoHome={() => setView(_ => Home)}
      />
    | Signup =>
      <Components.Signup
        onSignup={user => {
          setAuthState(_ => LoggedIn(user));
          setCompanyConfigured(_ => false);
          setView(_ => Settings);
        }}
        onGoLogin={() => setView(_ => Login)}
        onGoHome={() => setView(_ => Home)}
      />
    | _ =>
      <div className="app">
        <header className="app-header">
          <button className="header-brand" onClick={_ => setView(_ => Home)}>
            <span className="brand-mark">
              <svg viewBox="0 0 34 30" width="34" height="30" fill="none">
                <rect x="2" y="25" width="30" height="3" rx="1.5" fill="white" />
                <circle cx="22" cy="14" r="11" fill="white" />
                <line x1="22" y1="14" x2="2" y2="4"  stroke="white" strokeWidth="2" />
                <line x1="22" y1="14" x2="2" y2="9"  stroke="white" strokeWidth="2" />
                <line x1="22" y1="14" x2="2" y2="14" stroke="white" strokeWidth="2" />
                <circle cx="22" cy="14" r="2.5" fill="#C94124" />
              </svg>
            </span>
            <span className="brand-name">{"RolaDeck" |> React.string}</span>
          </button>
          <nav className="app-nav">
            {navItem("Home", Home, Home)}
            {navItem("Talent Pool", Pool, Pool)}
            {navItem("Score", Score(None), Score(None))}
            {navItem("Source", AiSourcing, AiSourcing)}
            {navItem("Playbooks", RoleGuide(""), RoleGuide(""))}
            {navItem("Resources", Resources, Resources)}
          </nav>
          <div className="header-end">
            {isAnonymous
              ? <button
                  className="nav-link"
                  onClick={_ => setView(_ => Login)}>
                  {"Sign in" |> React.string}
                </button>
              : <>
                  {navItem("Settings", Settings, Settings)}
                  <button className="nav-logout" onClick=handleLogout>
                    {"Log out" |> React.string}
                  </button>
                </>}
          </div>
        </header>

        <main className="app-main">
          {switch (view) {
           | Home =>
             <Components.Home
               onGoPool={() => setView(_ => Pool)}
               onGoScore={() => setView(_ => Score(None))}
               onGoRoles={() => setView(_ => RoleGuide(""))}
               onGoResources={() => setView(_ => Resources)}
               onGoAiSourcing={() => setView(_ => AiSourcing)}
               onGoSettings={() => setView(_ => Settings)}
               onGoSignup={() => setView(_ => Signup)}
               companyConfigured
               isAnonymous
             />
           | Pool =>
             isAnonymous ? authGate("Talent Pool") :
             <Components.Pool
               onSelectCandidate={id => setView(_ => CandidateDetail(id))}
               onGoScore={() => setView(_ => Score(None))}
             />
           | Score(initialRole) =>
             isAnonymous ? authGate("Score a Candidate") :
             <Components.ScoreForm
               initialRoleId=initialRole
               onSaved={id => setView(_ => CandidateDetail(id))}
             />
           | CandidateDetail(id) =>
             isAnonymous ? authGate("Candidate Detail") :
             <Components.CandidateDetail
               candidateId=id
               onSelectRole={roleId => setView(_ => RoleGuide(roleId))}
             />
           | RoleGuide("") =>
             <Components.SkillsBrowser
               onSelectSkill={id => setView(_ => RoleGuide(id))}
               playbooks_only=true
             />
           | RoleGuide(id) =>
             <Components.SkillDetail
               skillId=id
               onBack={() => setView(_ => RoleGuide(""))}
               onScore={id_ => setView(_ => Score(Some(id_)))}
               onSelectCandidate={cid => setView(_ => CandidateDetail(cid))}
             />
           | Resources =>
             <Components.SkillsBrowser
               onSelectSkill={id => setView(_ => ResourceDetail(id))}
               playbooks_only=false
             />
           | ResourceDetail(id) =>
             <Components.SkillDetail
               skillId=id
               onBack={() => setView(_ => Resources)}
               onScore={id_ => setView(_ => Score(Some(id_)))}
               onSelectCandidate={cid => setView(_ => CandidateDetail(cid))}
             />
           | AiSourcing =>
             isAnonymous ? authGate("AI Sourcing") :
             <Components.AiSourcing />
           | Settings =>
             isAnonymous ? authGate("Settings") :
             <Components.Settings />
           | Login => React.null
           | Signup => React.null
           }}
        </main>
      </div>
    }
  };
};
