# RolaDeck

**Recruiting intelligence that gets sharper over time.**

RolaDeck connects to your ATS and automatically scores every inbound application against structured role playbooks. As candidates move through your pipeline, the scoring model learns — recalibrating what actually predicts success at your company, not just on paper.

Built as a multi-tenant cloud product. Each company gets an isolated workspace. Multiple recruiters share one view.

---

![Home](screenshots/01-home.png)

---

## How it works

### 1. Set up your workspace
Create your company account and connect your tools. RolaDeck uses your company context — industry, culture, what good looks like — to personalise AI sourcing and outreach to your specific needs.

![Setup](screenshots/07-signup.png)

### 2. Connect your ATS
Link Greenhouse and RolaDeck starts pulling in applications automatically. Every candidate is scored against the relevant role playbook the moment they arrive — no manual input required. You can also score candidates manually if you want to evaluate someone outside the ATS flow.

### 3. Structured scoring, not gut feel
Each role playbook defines three tiers of criteria: **T1** (must-haves), **T2** (differentiators), and **T3** (rare upside signals). Candidates are scored against all three, at the seniority level you're hiring for.

![Score a Candidate](screenshots/03-score-filled.png)

### 4. The model learns from your pipeline
As candidates progress — or don't — RolaDeck tracks which signals actually correlate with success in your process. Scores recalibrate over time, so what you're measuring stays honest and company-specific rather than generic.

### 5. Source the same way you score
AI searches GitHub, conference speaker lists, blogs, and the open web to find candidates who match your playbook criteria. You get real profiles, boolean search strings for LinkedIn and GitHub, target company lists, and a ready-to-send outreach template — all calibrated to your company.

![AI Sourcing](screenshots/04-ai-sourcing.png)

### 6. One pool, every role
Every scored candidate lives in a shared talent pool. When a new role opens, you already have a ranked shortlist. Search, filter by role or stage, and move candidates forward without starting from scratch.

![Talent Pool](screenshots/05-pool.png)

---

## Role Playbooks

57 playbooks across Tech, Marketing, and Sales — each with scoring criteria, seniority signals, sourcing strings, interview stages, and red flags.

![Role Playbooks](screenshots/06-playbooks.png)

---

## Configure your integrations

Connect your AI provider and ATS from Settings. RolaDeck supports Anthropic Claude, OpenAI, and Perplexity for AI features, and Greenhouse for ATS sync.

![Settings](screenshots/08-settings.png)

---

## Stack

- **Backend**: OCaml + [Dream](https://aantron.github.io/dream/) HTTP server
- **Frontend**: [ReasonML](https://reasonml.github.io/) + [Melange](https://melange.re/) → React
- **Build**: [Dune](https://dune.build/)
- **Auth**: PBKDF2-SHA256 sessions, domain-based multi-tenant isolation
- **AI**: Pluggable — Anthropic Claude, OpenAI, or Perplexity
- **ATS**: Greenhouse sync with automatic candidate ingestion

---

## Self-hosting

### Prerequisites

- OCaml + opam
- Node.js
- Dune

### Install

```bash
opam switch create . --deps-only
npm install
```

### Build & Run

```bash
dune build
dune exec bin/main.exe   # backend on :4000
npm run dev              # frontend on :3000
```

### Configuration

Connect your AI provider (Anthropic, OpenAI, or Perplexity) and Greenhouse via **Settings**. Each company's data is isolated under its own tenant directory.
