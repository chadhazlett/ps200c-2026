# PS200C: Causal Inference for the Social Sciences

UCLA Political Science — Spring 2026 — Chad Hazlett

## Folder structure

```
slides/          Beamer slide decks (one folder per topic)
  shared/        Shared preamble and macros
psets/           Problem sets
syllabus/        Syllabus source files
section/         TA section materials
docs/            GitHub Pages website source
pdf/             Compiled PDFs (served by the website)
```

## Building

```bash
make slides      # compile all slide decks
make psets       # compile all problem sets
make syllabus    # compile the syllabus
make pdf         # copy compiled PDFs into pdf/
make all         # everything
make clean       # remove LaTeX aux files
```
