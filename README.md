# PS200C: Causal Inference for the Social Sciences

UCLA Political Science — Spring 2026 — Chad Hazlett

**Course website: [chadhazlett.github.io/ps200c-2026](https://chadhazlett.github.io/ps200c-2026/)**

If you're a student looking for the syllabus, slides, or project guidelines, head to the website above.

---

## For contributors (instructor & TA)

### Folder structure

```
docs/            Course website (GitHub Pages source)
slides/          Beamer slide decks (one folder per topic)
  shared/        Shared preamble and macros
section/         TA section materials
syllabus/        Syllabus LaTeX source (archival)
archive/         2025 compiled PDFs for reference
pdf/             Compiled PDFs (served by the website)
```

### Building slides

```bash
make slides      # compile all slide decks
make pdf         # copy compiled PDFs into pdf/
make clean       # remove LaTeX aux files
```
