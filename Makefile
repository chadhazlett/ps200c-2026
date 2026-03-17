LATEXMK = latexmk -pdf -interaction=nonstopmode -halt-on-error

# Slide directories
SLIDE_DIRS = 01-intro 02-pom 03-experiments 04-soo 05-did 06-iv 07-sensitivity 08-rdd 09-dags

.PHONY: all slides psets syllabus pdf clean $(addprefix slides-,01 02 03 04 05 06 07 08 09)

all: slides psets syllabus pdf

# Individual slide targets
slides-01:
	cd slides/01-intro && $(LATEXMK) *.tex

slides-02:
	cd slides/02-pom && $(LATEXMK) *.tex

slides-03:
	cd slides/03-experiments && $(LATEXMK) *.tex

slides-04:
	cd slides/04-soo && $(LATEXMK) *.tex

slides-05:
	cd slides/05-did && $(LATEXMK) *.tex

slides-06:
	cd slides/06-iv && $(LATEXMK) *.tex

slides-07:
	cd slides/07-sensitivity && $(LATEXMK) *.tex

slides-08:
	cd slides/08-rdd && $(LATEXMK) *.tex

slides-09:
	cd slides/09-dags && $(LATEXMK) *.tex

slides: slides-01 slides-02 slides-03 slides-04 slides-05 slides-06 slides-07 slides-08 slides-09

psets:
	for d in psets/pset*/; do \
		(cd "$$d" && $(LATEXMK) *.tex 2>/dev/null) || true; \
	done

syllabus:
	cd syllabus && $(LATEXMK) syllabus_2026.tex

pdf:
	@echo "Copying compiled PDFs to pdf/ ..."
	-cp slides/01-intro/*.pdf pdf/slides/ 2>/dev/null
	-cp slides/02-pom/*.pdf pdf/slides/ 2>/dev/null
	-cp slides/03-experiments/*.pdf pdf/slides/ 2>/dev/null
	-cp slides/04-soo/*.pdf pdf/slides/ 2>/dev/null
	-cp slides/05-did/*.pdf pdf/slides/ 2>/dev/null
	-cp slides/06-iv/*.pdf pdf/slides/ 2>/dev/null
	-cp slides/07-sensitivity/*.pdf pdf/slides/ 2>/dev/null
	-cp slides/08-rdd/*.pdf pdf/slides/ 2>/dev/null
	-cp slides/09-dags/*.pdf pdf/slides/ 2>/dev/null
	-cp psets/pset*/*.pdf pdf/psets/ 2>/dev/null
	-cp syllabus/*.pdf pdf/syllabus/ 2>/dev/null

clean:
	find slides psets syllabus -type f \( \
		-name '*.aux' -o -name '*.log' -o -name '*.nav' \
		-o -name '*.out' -o -name '*.snm' -o -name '*.toc' \
		-o -name '*.synctex.gz' -o -name '*.fls' \
		-o -name '*.fdb_latexmk' -o -name '*.vrb' \
		-o -name '*.bbl' -o -name '*.blg' \
	\) -delete
