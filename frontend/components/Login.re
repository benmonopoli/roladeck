[@react.component]
let make = (~onLogin: Ahrefs_frontend_api.Api.userInfo => unit, ~onGoSignup: unit => unit) => {
  let (email, setEmail)       = React.useState(() => "");
  let (password, setPassword) = React.useState(() => "");
  let (loading, setLoading)   = React.useState(() => false);
  let (error, setError)       = React.useState(() => "");

  let handleSubmit = _ => {
    if (String.length(email) == 0 || String.length(password) == 0) {
      setError(_ => "Please enter your email and password.");
    } else {
      setLoading(_ => true);
      setError(_ => "");
      Ahrefs_frontend_api.Api.login(~email, ~password)
      |> Js.Promise.then_(user => {
        setLoading(_ => false);
        onLogin(user);
        Js.Promise.resolve();
      })
      |> Js.Promise.catch(_ => {
        setLoading(_ => false);
        setError(_ => "Invalid email or password.");
        Js.Promise.resolve();
      })
      |> ignore;
    }
  };

  <div className="auth-wrap">
    <div className="auth-card">
      <div className="brand-row">
        <span className="brand-mark">
          <svg viewBox="0 0 34 30" width="28" height="25" fill="none">
            <rect x="2" y="25" width="30" height="3" rx="1.5" fill="white" />
            <circle cx="22" cy="14" r="11" fill="white" />
            <line x1="22" y1="14" x2="2" y2="4"  stroke="white" strokeWidth="2" />
            <line x1="22" y1="14" x2="2" y2="9"  stroke="white" strokeWidth="2" />
            <line x1="22" y1="14" x2="2" y2="14" stroke="white" strokeWidth="2" />
            <circle cx="22" cy="14" r="2.5" fill="#C94124" />
          </svg>
        </span>
        <span className="auth-brand-name">{"RolaDeck" |> React.string}</span>
      </div>
      <h1 className="auth-title">{"Sign in" |> React.string}</h1>
      <p className="auth-subtitle">{"Welcome back." |> React.string}</p>

      {String.length(error) > 0
        ? <div className="auth-error">{React.string(error)}</div>
        : React.null}

      <div className="auth-field">
        <label className="auth-label">{"Email" |> React.string}</label>
        <input
          className="auth-input"
          type_="email"
          placeholder="you@company.com"
          value=email
          onChange={e => setEmail(_ => React.Event.Form.target(e)##value)}
          autoFocus=true
        />
      </div>

      <div className="auth-field">
        <label className="auth-label">{"Password" |> React.string}</label>
        <input
          className="auth-input"
          type_="password"
          placeholder={js|\u2022\u2022\u2022\u2022\u2022\u2022\u2022\u2022|js}
          value=password
          onChange={e => setPassword(_ => React.Event.Form.target(e)##value)}
          onKeyDown={e => if (React.Event.Keyboard.key(e) == "Enter") handleSubmit(())}
        />
      </div>

      <button
        className={"btn-primary " ++ (loading ? "btn-loading" : "")}
        disabled=loading
        onClick=handleSubmit>
        {(loading ? "Signing in..." : "Sign in") |> React.string}
      </button>

      <p className="auth-switch">
        {"Don't have an account? " |> React.string}
        <span className="auth-link" onClick={_ => onGoSignup()}>
          {"Set up your company" |> React.string}
        </span>
      </p>
    </div>
  </div>;
};
