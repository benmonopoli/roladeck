open Roladeck_types.Types;

let readFileAsText : (React.Event.Form.t, (string => unit)) => unit = [%raw {|
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

/* Slugify a name for use in an ID */
let slugify = name => {
  let lower = String.lowercase_ascii(name);
  let slug = Js.String.replaceByRe(~regexp=[%re "/[^a-z0-9]+/g"], ~replacement="-", lower);
  let slug = Js.String.replaceByRe(~regexp=[%re "/^-+|-+$/g"], ~replacement="", slug);
  String.length(slug) == 0 ? "playbook" : slug;
};

type draftCriterion = {
  dText: string,
  dTier: skill_tier,
};

type importTab = TabUpload | TabUrl | TabForm;

[@react.component]
let make = (~editId: option(string), ~onBack: unit => unit, ~onSaved: string => unit) => {
  let (loading, setLoading)   = React.useState(() => editId != None);
  let (saving, setSaving)     = React.useState(() => false);
  let (name, setName)         = React.useState(() => "");
  let (category, setCategory) = React.useState(() => Tech);
  let (description, setDesc)  = React.useState(() => "");
  let (criteria, setCriteria) = React.useState(() => [{ dText: "", dTier: T1_MustHave }]);
  let (redFlags, setRedFlags) = React.useState(() => [""]);
  let (skillId, setSkillId)   = React.useState(() => "");

  /* Create-mode import state */
  let (importTab, setImportTab) = React.useState(() => TabUpload);
  let (importUrl, setImportUrl) = React.useState(() => "");
  let (importing, setImporting) = React.useState(() => false);
  let (importError, setImportError) = React.useState(() => "");

  /* Load existing skill for edit */
  React.useEffect1(() => {
    switch (editId) {
    | None => ()
    | Some(id) =>
      Roladeck_frontend_api.Api.getSkill(id)
      |> Js.Promise.then_(result => {
        switch (result) {
        | Some((sk: skill_record)) =>
          setSkillId(_ => sk.id);
          setName(_ => sk.discipline.name);
          setCategory(_ => sk.discipline.category);
          setDesc(_ => sk.discipline.description);
          setCriteria(_ =>
            if (sk.criteria == []) [{ dText: "", dTier: T1_MustHave }]
            else List.map((c: skill_criterion) => { dText: c.text, dTier: c.tier }, sk.criteria)
          );
          setRedFlags(_ =>
            if (sk.red_flags == []) [""]
            else sk.red_flags @ [""]
          );
        | None => ()
        };
        setLoading(_ => false);
        Js.Promise.resolve();
      })
      |> ignore
    };
    None;
  }, [|editId|]);

  let tierLabel = t => switch (t) {
    | T1_MustHave => "T1 — Must Have"
    | T2_Differentiator => "T2 — Differentiator"
    | T3_RareUpside => "T3 — Rare Upside"
  };

  let parseTier = s => switch (s) {
    | "T2_Differentiator" => T2_Differentiator
    | "T3_RareUpside" => T3_RareUpside
    | _ => T1_MustHave
  };

  let parseCategory = s => switch (s) {
    | "Marketing" => Marketing
    | "Sales" => Sales
    | _ => Tech
  };

  let handleSave = _ => {
    let trimmedName = String.trim(name);
    if (String.length(trimmedName) == 0) ()
    else {
      setSaving(_ => true);
      let id = switch (editId) {
        | Some(_) => skillId
        | None =>
          "custom-hiring-" ++ slugify(trimmedName) ++ "-" ++
          string_of_int(int_of_float(Js.Date.now()))
      };
      let validCriteria = List.filter_map(
        (dc: draftCriterion) =>
          String.length(String.trim(dc.dText)) > 0
            ? Some({ text: String.trim(dc.dText), tier: dc.dTier })
            : None,
        criteria
      );
      let validFlags = List.filter(
        f => String.length(String.trim(f)) > 0,
        redFlags
      );
      let skill: skill_record = {
        id,
        discipline: {
          id,
          name: trimmedName,
          category,
          description: String.trim(description),
        },
        criteria: validCriteria,
        sourcing_strings: [],
        title_synonyms: [],
        seniority_signals: [],
        interview_stages: [],
        red_flags: validFlags,
        comp_ranges: [],
      };
      let promise = switch (editId) {
        | None => Roladeck_frontend_api.Api.createCustomSkill(skill)
        | Some(_) => Roladeck_frontend_api.Api.updateCustomSkill(id, skill)
      };
      promise
      |> Js.Promise.then_((saved: skill_record) => {
        setSaving(_ => false);
        onSaved(saved.id);
        Js.Promise.resolve();
      })
      |> Js.Promise.catch(_ => {
        setSaving(_ => false);
        Js.Promise.resolve();
      })
      |> ignore;
    };
  };

  let doImport = body => {
    setImporting(_ => true);
    setImportError(_ => "");
    Roladeck_frontend_api.Api.postJson(
      Roladeck_frontend_api.Api.base ++ "/api/custom-skills/import-md",
      body
    )
    |> Js.Promise.then_((saved: Js.Json.t) => {
      setImporting(_ => false);
      let id = switch (Js.Json.decodeObject(saved)) {
        | Some(obj) =>
          switch (Js.Dict.get(obj, "id")) {
          | Some(v) => switch (Js.Json.decodeString(v)) { | Some(s) => s | None => "" }
          | None => ""
          }
        | None => ""
      };
      if (String.length(id) > 0) onSaved(id);
      Js.Promise.resolve();
    })
    |> Js.Promise.catch(_ => {
      setImporting(_ => false);
      setImportError(_ => "Import failed. Check the file format matches the template.");
      Js.Promise.resolve();
    })
    |> ignore;
  };

  let handleFile = e => {
    let ts = string_of_int(int_of_float(Js.Date.now()));
    let importId = "custom-hiring-import-" ++ ts;
    readFileAsText(e, text => {
      let body = Js.Json.object_(Js.Dict.fromArray([|
        ("content", Js.Json.string(text)),
        ("id", Js.Json.string(importId)),
      |]));
      doImport(body);
    });
  };

  let renderForm = () =>
    <>
      <div className="editor-field">
        <label>{"Name" |> React.string}</label>
        <input
          type_="text"
          placeholder="e.g. Senior Backend Engineer"
          value=name
          onChange={e => setName(_ => React.Event.Form.target(e)##value)}
        />
      </div>

      <div className="editor-field">
        <label>{"Category" |> React.string}</label>
        <select
          value={switch (category) { | Tech => "Tech" | Marketing => "Marketing" | Sales => "Sales" }}
          onChange={e => setCategory(_ => parseCategory(React.Event.Form.target(e)##value))}>
          <option value="Tech">{"Tech" |> React.string}</option>
          <option value="Marketing">{"Marketing" |> React.string}</option>
          <option value="Sales">{"Sales" |> React.string}</option>
        </select>
      </div>

      <div className="editor-field">
        <label>{"Description" |> React.string}</label>
        <textarea
          placeholder="What does this role do? One or two sentences."
          value=description
          onChange={e => setDesc(_ => React.Event.Form.target(e)##value)}
        />
      </div>

      <div className="editor-field">
        <label>{"Criteria" |> React.string}</label>
        <div className="criteria-list">
          {criteria
           |> List.mapi((i, dc: draftCriterion) =>
                <div key={string_of_int(i)} className="criteria-row">
                  <input
                    type_="text"
                    placeholder="Criterion text..."
                    value={dc.dText}
                    onChange={e => {
                      let v = React.Event.Form.target(e)##value;
                      setCriteria(prev =>
                        List.mapi((j, c) => j == i ? { ...c, dText: v } : c, prev)
                      );
                    }}
                  />
                  <select
                    value={switch (dc.dTier) {
                      | T1_MustHave => "T1_MustHave"
                      | T2_Differentiator => "T2_Differentiator"
                      | T3_RareUpside => "T3_RareUpside"
                    }}
                    onChange={e => {
                      let v = React.Event.Form.target(e)##value;
                      setCriteria(prev =>
                        List.mapi((j, c) => j == i ? { ...c, dTier: parseTier(v) } : c, prev)
                      );
                    }}>
                    <option value="T1_MustHave">{React.string(tierLabel(T1_MustHave))}</option>
                    <option value="T2_Differentiator">{React.string(tierLabel(T2_Differentiator))}</option>
                    <option value="T3_RareUpside">{React.string(tierLabel(T3_RareUpside))}</option>
                  </select>
                  <button
                    className="btn-remove"
                    onClick={_ =>
                      setCriteria(prev =>
                        List.filteri((j, _) => j != i, prev)
                      )
                    }>
                    {"×" |> React.string}
                  </button>
                </div>
              )
           |> Array.of_list
           |> React.array}
        </div>
        <button
          className="btn-ghost btn-add-row"
          onClick={_ => setCriteria(prev => prev @ [{ dText: "", dTier: T1_MustHave }])}>
          {"+ Add criterion" |> React.string}
        </button>
      </div>

      <div className="editor-field">
        <label>{"Red flags (optional)" |> React.string}</label>
        <div className="criteria-list">
          {redFlags
           |> List.mapi((i, flag) =>
                <div key={string_of_int(i)} className="criteria-row">
                  <input
                    type_="text"
                    placeholder="Red flag signal..."
                    value=flag
                    onChange={e => {
                      let v = React.Event.Form.target(e)##value;
                      setRedFlags(prev =>
                        List.mapi((j, f) => j == i ? v : f, prev)
                      );
                    }}
                  />
                  <button
                    className="btn-remove"
                    onClick={_ =>
                      setRedFlags(prev =>
                        List.filteri((j, _) => j != i, prev)
                      )
                    }>
                    {"×" |> React.string}
                  </button>
                </div>
              )
           |> Array.of_list
           |> React.array}
        </div>
        <button
          className="btn-ghost btn-add-row"
          onClick={_ => setRedFlags(prev => prev @ [""])}>
          {"+ Add red flag" |> React.string}
        </button>
      </div>

      <div className="editor-actions">
        <button
          className="btn-primary"
          disabled=saving
          onClick=handleSave>
          {React.string(saving ? "Saving..." : (editId == None ? "Create Playbook" : "Save Changes"))}
        </button>
        <button className="btn-ghost" onClick={_ => onBack()}>
          {"Cancel" |> React.string}
        </button>
      </div>
    </>;

  if (loading) {
    <div className="loading-state">{"Loading..." |> React.string}</div>
  } else {
    <div className="playbook-editor">
      <div className="page-header">
        <div className="page-header-left">
          <button className="back-link" onClick={_ => onBack()}>
            {React.string({js|← Playbooks|js})}
          </button>
          <h1 className="page-title">
            {React.string(editId == None ? "New Playbook" : "Edit Playbook")}
          </h1>
        </div>
      </div>

      {switch (editId) {
       | Some(_) => renderForm()
       | None =>
         <>
           <div className="import-tabs">
             <button
               className={"import-tab" ++ (importTab == TabUpload ? " active" : "")}
               onClick={_ => setImportTab(_ => TabUpload)}>
               {"Upload .md" |> React.string}
             </button>
             <button
               className={"import-tab" ++ (importTab == TabUrl ? " active" : "")}
               onClick={_ => setImportTab(_ => TabUrl)}>
               {"Import URL" |> React.string}
             </button>
             <button
               className={"import-tab" ++ (importTab == TabForm ? " active" : "")}
               onClick={_ => setImportTab(_ => TabForm)}>
               {"Build manually" |> React.string}
             </button>
           </div>

           {switch (importTab) {
            | TabUpload =>
              <div className="import-tab-panel">
                <p className="import-desc">
                  {"Upload a markdown file following the playbook template." |> React.string}
                </p>
                <div style={ReactDOM.Style.make(~marginBottom="12px", ())}>
                  <a
                    className="template-link"
                    href="/playbook-template.md"
                    download="playbook-template.md">
                    {"Download template" |> React.string}
                  </a>
                </div>
                <input
                  className="import-file-input"
                  type_="file"
                  accept=".md"
                  onChange=handleFile
                />
                {importing
                  ? <p className="import-desc">{"Importing..." |> React.string}</p>
                  : React.null}
                {String.length(importError) > 0
                  ? <p className="import-error">{importError |> React.string}</p>
                  : React.null}
              </div>
            | TabUrl =>
              <div className="import-tab-panel">
                <p className="import-desc">
                  {"Paste a raw GitHub URL or direct link to a SKILL.md file." |> React.string}
                </p>
                <div className="import-url-row">
                  <input
                    type_="text"
                    placeholder="https://raw.githubusercontent.com/..."
                    value=importUrl
                    onChange={e => setImportUrl(_ => React.Event.Form.target(e)##value)}
                  />
                  <button
                    className="btn-primary"
                    disabled=importing
                    onClick={_ => {
                      let ts = string_of_int(int_of_float(Js.Date.now()));
                      let body = Js.Json.object_(Js.Dict.fromArray([|
                        ("url", Js.Json.string(importUrl)),
                        ("id", Js.Json.string("custom-hiring-import-" ++ ts)),
                      |]));
                      doImport(body);
                    }}>
                    {React.string(importing ? "Importing..." : "Import")}
                  </button>
                </div>
                {String.length(importError) > 0
                  ? <p className="import-error">{importError |> React.string}</p>
                  : React.null}
              </div>
            | TabForm =>
              renderForm()
            }}
         </>
       }}
    </div>
  };
};
