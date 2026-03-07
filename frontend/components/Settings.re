open Ahrefs_types.Types;

[@react.component]
let make = () => {
  /* Company profile */
  let (companyName, setCompanyName)   = React.useState(() => "");
  let (companyUrls, setCompanyUrls)   = React.useState(() => "");
  let (companyBrief, setCompanyBrief) = React.useState(() => "");
  let (briefAt, setBriefAt)           = React.useState(() => None);
  let (briefExpanded, setBriefExpanded) = React.useState(() => false);
  let (researchLoading, setResearchLoading) = React.useState(() => false);
  let (researchMsg, setResearchMsg)   = React.useState(() => None);

  /* AI settings */
  let (aiProvider, setAiProvider) = React.useState(() => "anthropic");
  let (aiApiKey, setAiApiKey)     = React.useState(() => "");
  let (aiKeyHint, setAiKeyHint)   = React.useState(() => "");
  let (aiKeySet, setAiKeySet)     = React.useState(() => false);
  let (aiSaving, setAiSaving)     = React.useState(() => false);
  let (aiSaveMsg, setAiSaveMsg)   = React.useState(() => None);

  /* Greenhouse settings */
  let (subdomain, setSubdomain)   = React.useState(() => "");
  let (apiKey, setApiKey)         = React.useState(() => "");
  let (apiKeyHint, setApiKeyHint) = React.useState(() => "");
  let (ghConfigured, setGhConfigured) = React.useState(() => false);
  let (_anthropicSet, setAnthropicSet) = React.useState(() => false);
  let (saving, setSaving)         = React.useState(() => false);
  let (saveMsg, setSaveMsg)       = React.useState(() => None);
  let (testing, setTesting)       = React.useState(() => false);
  let (testResult, setTestResult) = React.useState(() => None);

  /* Greenhouse sync status */
  let (syncStatus, setSyncStatus) = React.useState(() => None);
  let (syncing, setSyncing)       = React.useState(() => false);

  React.useEffect0(() => {
    Ahrefs_frontend_api.Api.getCompanyProfile()
    |> Js.Promise.then_(p => {
      setCompanyName(_ => p.Ahrefs_frontend_api.Api.company_name);
      setCompanyUrls(_ => String.concat("\n", p.Ahrefs_frontend_api.Api.company_urls));
      setCompanyBrief(_ => p.Ahrefs_frontend_api.Api.company_brief);
      setBriefAt(_ => p.Ahrefs_frontend_api.Api.brief_generated_at);
      Js.Promise.resolve();
    })
    |> ignore;
    Ahrefs_frontend_api.Api.getSettingsStatus()
    |> Js.Promise.then_(s => {
      setAnthropicSet(_ => s.Ahrefs_types.Types.anthropic_key_set);
      setGhConfigured(_ => s.Ahrefs_types.Types.greenhouse_configured);
      Js.Promise.resolve();
    })
    |> ignore;
    Ahrefs_frontend_api.Api.getIntegrationSettings()
    |> Js.Promise.then_(s => {
      setSubdomain(_ => s.greenhouse_subdomain);
      setApiKeyHint(_ => s.greenhouse_api_key_hint);
      setGhConfigured(_ => s.greenhouse_configured);
      setAiProvider(_ => s.ai_provider);
      setAiKeyHint(_ => s.ai_api_key_hint);
      setAiKeySet(_ => s.ai_key_set);
      Js.Promise.resolve();
    })
    |> ignore;
    Ahrefs_frontend_api.Api.getGreenhouseSyncStatus()
    |> Js.Promise.then_(s => {
      setSyncStatus(_ => Some(s));
      Js.Promise.resolve();
    })
    |> ignore;
    None;
  });

  let handleSaveCompany = _ => {
    let urls = String.split_on_char('\n', companyUrls)
      |> List.map(String.trim)
      |> List.filter(s => String.length(s) > 0);
    setResearchLoading(_ => true);
    setResearchMsg(_ => None);
    Ahrefs_frontend_api.Api.saveAndResearchCompany(~name=companyName, ~urls)
    |> Js.Promise.then_(_ => {
      setResearchLoading(_ => false);
      setResearchMsg(_ => Some("Research started. The brief will be ready in ~30 seconds."));
      Js.Promise.resolve();
    })
    |> Js.Promise.catch(err => {
      let msg = switch (Js.Json.stringifyAny(err)) { | Some(s) => s | None => "Unknown error" };
      setResearchLoading(_ => false);
      setResearchMsg(_ => Some("Error: " ++ msg));
      Js.Promise.resolve();
    })
    |> ignore;
  };

  let handleSaveIntegrations = _ => {
    setSaving(_ => true);
    setSaveMsg(_ => None);
    Ahrefs_frontend_api.Api.saveIntegrationSettings(
      ~subdomain, ~apiKey, ~aiProvider, ~aiApiKey=""
    )
    |> Js.Promise.then_(ok => {
      setSaving(_ => false);
      setSaveMsg(_ => Some(ok ? "Saved." : "Save failed."));
      if (ok) {
        setGhConfigured(_ => String.length(subdomain) > 0 && String.length(apiKey) > 0);
        setApiKey(_ => "");
        setApiKeyHint(_ => String.length(apiKey) > 4 ? String.sub(apiKey, 0, 4) ++ "..." : "***");
      };
      Js.Promise.resolve();
    })
    |> ignore;
  };

  let handleSaveAi = _ => {
    setAiSaving(_ => true);
    setAiSaveMsg(_ => None);
    Ahrefs_frontend_api.Api.saveIntegrationSettings(
      ~subdomain, ~apiKey, ~aiProvider, ~aiApiKey
    )
    |> Js.Promise.then_(ok => {
      setAiSaving(_ => false);
      setAiSaveMsg(_ => Some(ok ? "Saved." : "Save failed."));
      if (ok) {
        setAiKeySet(_ => String.length(aiApiKey) > 0);
        setAiKeyHint(_ => String.length(aiApiKey) > 4 ? String.sub(aiApiKey, 0, 4) ++ "..." : "***");
        setAiApiKey(_ => "");
      };
      Js.Promise.resolve();
    })
    |> ignore;
  };

  let handleSyncNow = _ => {
    setSyncing(_ => true);
    Ahrefs_frontend_api.Api.triggerGreenhouseSync()
    |> Js.Promise.then_(_ => {
      /* Re-fetch status after a short delay to show updated count */
      let _ = Js.Global.setTimeout(~f=() => {
        Ahrefs_frontend_api.Api.getGreenhouseSyncStatus()
        |> Js.Promise.then_(s => {
          setSyncStatus(_ => Some(s));
          setSyncing(_ => false);
          Js.Promise.resolve();
        })
        |> ignore;
        ()
      }, 3000);
      Js.Promise.resolve();
    })
    |> Js.Promise.catch(_ => {
      setSyncing(_ => false);
      Js.Promise.resolve();
    })
    |> ignore;
  };

  let handleTestGreenhouse = _ => {
    setTesting(_ => true);
    setTestResult(_ => None);
    Ahrefs_frontend_api.Api.testGreenhouseConnection()
    |> Js.Promise.then_(((ok, err)) => {
      setTesting(_ => false);
      setTestResult(_ => Some((ok, err)));
      Js.Promise.resolve();
    })
    |> ignore;
  };

  <div className="settings-view">
    <div className="page-header">
      <div>
        <h1 className="page-title">{"Settings" |> React.string}</h1>
        <p className="page-subtitle">
          {"Configure your company profile, AI provider, and integrations." |> React.string}
        </p>
      </div>
    </div>

    <div className="settings-body">

      /* ── Your Company ── */
      <div className="settings-section">
        <h2 className="settings-section-title">{"Your Company" |> React.string}</h2>
        <p className="settings-section-desc">
          {"RolaDeck will research your company using these links and the web, then use what it learns to personalise AI sourcing and outreach." |> React.string}
        </p>

        <div className="form-row">
          <label className="form-label">{"Company name" |> React.string}</label>
          <input
            type_="text"
            className="field-input"
            placeholder="e.g. Ahrefs"
            value=companyName
            onChange={e => setCompanyName(_ => React.Event.Form.target(e)##value)}
          />
        </div>

        <div className="form-row">
          <label className="form-label">
            {"Company resources" |> React.string}
            <span className="form-label-hint">{" (one URL per line)" |> React.string}</span>
          </label>
          <textarea
            className="field-input company-urls-input"
            rows=4
            placeholder={"https://yourcompany.com\nhttps://yourcompany.com/about\nhttps://yourcompany.com/blog"}
            value=companyUrls
            onChange={e => setCompanyUrls(_ => React.Event.Form.target(e)##value)}
          />
        </div>

        <div className="integration-actions">
          <button
            className={"btn-primary " ++ (researchLoading ? "btn-loading" : "")}
            disabled=researchLoading
            onClick=handleSaveCompany>
            {React.string(researchLoading ? "Researching..." : "Save & Research Company")}
          </button>
          {switch (researchMsg) {
           | None => React.null
           | Some(msg) => <span className="save-msg">{React.string(msg)}</span>
           }}
        </div>

        {switch (briefAt) {
         | None =>
           String.length(companyBrief) == 0
             ? <p className="company-brief-status">{"No company brief yet." |> React.string}</p>
             : React.null
         | Some(at) =>
           <div className="company-brief-panel">
             <button
               className="company-brief-toggle"
               onClick={_ => setBriefExpanded(v => !v)}>
               {React.string((briefExpanded ? "v" : ">") ++ "Brief ready  -  last updated " ++ String.sub(at, 0, min(10, String.length(at))))}
             </button>
             {briefExpanded
               ? <pre className="company-brief-text">{React.string(companyBrief)}</pre>
               : React.null}
           </div>
         }}
      </div>

      /* ── AI Configuration ── */
      <div className="settings-section">
        <h2 className="settings-section-title">{"AI Configuration" |> React.string}</h2>
        <p className="settings-section-desc">
          {"Choose your AI provider and enter your API key. Used for AI Sourcing, company research, and analysis." |> React.string}
        </p>

        <div className="form-row">
          <label className="form-label">{"AI Provider" |> React.string}</label>
          <select
            className="field-input"
            value=aiProvider
            onChange={e => setAiProvider(_ => React.Event.Form.target(e)##value)}>
            <option value="anthropic">{"Anthropic (Claude Sonnet)  -  recommended" |> React.string}</option>
            <option value="openai">{"OpenAI (GPT-4o)" |> React.string}</option>
            <option value="perplexity">{"Perplexity (Sonar Pro  -  web search included)" |> React.string}</option>
          </select>
        </div>

        <div className="form-row">
          <label className="form-label">
            {"API Key" |> React.string}
          </label>
          <input
            type_="password"
            className="field-input"
            placeholder={aiKeySet
              ? (String.length(aiKeyHint) > 0 ? "Current: " ++ aiKeyHint ++ "  -  leave blank to keep" : "Leave blank to keep existing key")
              : switch (aiProvider) {
                | "openai" => "Paste your OpenAI API key (sk-...)"
                | "perplexity" => "Paste your Perplexity API key (pplx-...)"
                | _        => "Paste your Anthropic API key (sk-ant-...)"
                }}
            value=aiApiKey
            onChange={e => setAiApiKey(_ => React.Event.Form.target(e)##value)}
          />
        </div>

        {aiProvider == "anthropic"
          ? <p className="settings-hint">
              {"Anthropic enables live web search for AI Sourcing and company research. " |> React.string}
              <a href="https://console.anthropic.com/settings/keys" target="_blank" rel="noopener">
                {"Get a key ->" |> React.string}
              </a>
            </p>
          : aiProvider == "openai"
            ? <p className="settings-hint">
                {"OpenAI powers text generation. AI Sourcing generates strings without live web search. " |> React.string}
                <a href="https://platform.openai.com/api-keys" target="_blank" rel="noopener">
                  {"Get a key ->" |> React.string}
                </a>
              </p>
            : React.null}

        <div className="integration-actions">
          <div className="integration-status-inline">
            {aiKeySet
              ? <span className="status-badge status-ok">{"Connected" |> React.string}</span>
              : <span className="status-badge status-warn">{"No key set" |> React.string}</span>}
          </div>
          <button
            className="btn-primary"
            disabled=aiSaving
            onClick=handleSaveAi>
            {React.string(aiSaving ? "Saving..." : "Save")}
          </button>
          {switch (aiSaveMsg) {
           | None => React.null
           | Some(msg) => <span className="save-msg">{React.string(msg)}</span>
           }}
        </div>
      </div>

      /* ── Integrations ── */
      <div className="settings-section">
        <h2 className="settings-section-title">{"Integrations" |> React.string}</h2>

        /* Greenhouse */
        <div className="integration-block">
          <div className="integration-row">
            <div className="integration-info">
              <span className="integration-name">{"Greenhouse ATS" |> React.string}</span>
              <span className="integration-desc">
                {"Auto-score inbound candidates from your Greenhouse pipeline." |> React.string}
              </span>
            </div>
            <div className="integration-status">
              {ghConfigured
                ? <span className="status-badge status-ok">{"Configured" |> React.string}</span>
                : <span className="status-badge status-warn">{"Not configured" |> React.string}</span>}
            </div>
          </div>

          <div className="integration-fields">
            <div className="form-row">
              <label className="form-label">
                {"Subdomain" |> React.string}
                <span className="form-label-hint">{" (e.g. ahrefs)" |> React.string}</span>
              </label>
              <div className="greenhouse-domain-row">
                <input
                  type_="text"
                  className="field-input integration-input"
                  placeholder="yourcompany"
                  value=subdomain
                  onChange={e => setSubdomain(_ => React.Event.Form.target(e)##value)}
                />
                <span className="domain-suffix">{".greenhouse.io" |> React.string}</span>
              </div>
            </div>

            <div className="form-row">
              <label className="form-label">
                {"Harvest API Key" |> React.string}
              </label>
              <input
                type_="password"
                className="field-input integration-input"
                placeholder={String.length(apiKeyHint) > 0 ? "Current: " ++ apiKeyHint ++ " (leave blank to keep)" : "Paste your Harvest API key"}
                value=apiKey
                onChange={e => setApiKey(_ => React.Event.Form.target(e)##value)}
              />
            </div>

            <div className="integration-actions">
              <button
                className="btn-primary"
                disabled=saving
                onClick=handleSaveIntegrations>
                {React.string(saving ? "Saving..." : "Save")}
              </button>
              {ghConfigured
                ? <button
                    className="btn-ghost"
                    disabled=testing
                    onClick=handleTestGreenhouse>
                    {React.string(testing ? "Testing..." : "Test connection")}
                  </button>
                : React.null}
              {switch (saveMsg) {
               | None => React.null
               | Some(msg) => <span className="save-msg">{React.string(msg)}</span>
               }}
              {switch (testResult) {
               | None => React.null
               | Some((true, _)) =>
                 <span className="status-badge status-ok">{"Connection OK" |> React.string}</span>
               | Some((false, err)) =>
                 <span className="status-badge status-err">
                   {React.string("Failed: " ++ Option.value(~default="unknown", err))}
                 </span>
               }}
            </div>
          </div>

          {ghConfigured
            ? <div className="sync-status-panel">
                <div className="sync-status-row">
                  <div className="sync-status-info">
                    {switch (syncStatus) {
                     | None => <span className="sync-status-text">{"Loading sync status..." |> React.string}</span>
                     | Some(s) =>
                       <span className="sync-status-text">
                         {React.string(
                           switch (s.Ahrefs_frontend_api.Api.last_synced_at) {
                           | None => "Not yet synced"
                           | Some(t) =>
                             string_of_int(s.Ahrefs_frontend_api.Api.total_synced) ++
                             " candidates imported  -  last synced " ++
                             String.sub(t, 0, min(10, String.length(t)))
                           }
                         )}
                       </span>
                     }}
                    {switch (syncStatus) {
                     | Some(s) =>
                       switch (s.Ahrefs_frontend_api.Api.last_error) {
                       | Some(e) =>
                         <span className="status-badge status-err">{React.string("Error: " ++ e)}</span>
                       | None => React.null
                       }
                     | None => React.null
                     }}
                  </div>
                  <button
                    className="btn-ghost"
                    disabled=syncing
                    onClick=handleSyncNow>
                    {React.string(syncing ? "Syncing..." : "Sync now")}
                  </button>
                </div>
                <p className="sync-status-hint">
                  {"RolaDeck syncs automatically every 5 minutes." |> React.string}
                </p>
              </div>
            : React.null}
        </div>
      </div>
    </div>
  </div>;
};
