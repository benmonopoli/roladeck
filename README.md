# RolaDeck

**Structured recruiting, without the overhead.**

Score candidates against role playbooks, source with AI, and build a searchable talent pool — all in one tool built for technical recruiting teams.

---

![Home](screenshots/01-home.png)

---

## Features

### Score a Candidate
Paste a resume, LinkedIn profile, or interview notes. Get a T1/T2/T3 structured breakdown in seconds, calibrated to the role and seniority level.

![Score a Candidate](screenshots/03-score-filled.png)

---

### AI Sourcing
AI searches GitHub, conferences, blogs, and the open web to surface real candidate profiles, boolean search strings, target companies, and an outreach template.

![AI Sourcing](screenshots/04-ai-sourcing.png)

---

### Role Playbooks
57 scoring playbooks across Tech, Marketing, and Sales. Each playbook defines must-have criteria (T1), differentiators (T2), and rare upside signals (T3), with sourcing strings, seniority signals, and interview stages included.

![Role Playbooks](screenshots/06-playbooks.png)

---

### Talent Pool
Every scored candidate is saved automatically. Filter by role, stage, and score. Move candidates through pipeline stages. Integrates with Greenhouse.

![Talent Pool](screenshots/05-pool.png)

---

## Stack

- **Backend**: OCaml + [Dream](https://aantron.github.io/dream/) HTTP server
- **Frontend**: [ReasonML](https://reasonml.github.io/) + [Melange](https://melange.re/) → React
- **Build**: [Dune](https://dune.build/)
- **Auth**: PBKDF2-SHA256 sessions, multi-tenant by email domain
- **AI**: Pluggable — Anthropic Claude, OpenAI, or Perplexity
- **ATS**: Greenhouse integration with candidate sync

## Getting Started

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
# Build backend + frontend
dune build

# Start backend (port 4000)
dune exec bin/main.exe

# Start frontend dev server (port 3000)
npm run dev
```

Open http://localhost:3000.

### Configuration

Go to **Settings** to connect your AI provider (Anthropic, OpenAI, or Perplexity) and optionally Greenhouse. Data is stored locally under `data/`.
