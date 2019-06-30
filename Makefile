####################
# Makefile

RMD_DIR = slides_in
HTML_DIR = slides_out
HANDOUT_DIR = handouts
SCRIPT_DIR = scripts
DATA_DIR = data

CSS = $(wildcard css/*.css)
RMD_SOURCE = $(wildcard $(RMD_DIR)/*.Rmd)
HANDOUT_SOURCE = $(wildcard $(HANDOUT_DIR)/*.Rmd)
DATA_SOURCE = $(wildcard $(DATA_DIR)/*.R)

HTML_OUT = $(patsubst $(RMD_DIR)/%.Rmd, $(HTML_DIR)/%.html, $(RMD_SOURCE))
HANDOUT_OUT = $(HANDOUT_SOURCE:.Rmd=.docx)
SCRIPT_OUT = $(patsubst $(RMD_DIR)/%.Rmd,$(SCRIPT_DIR)/%.R,$(RMD_SOURCE))

KNIT_SLIDE = Rscript -e "rmarkdown::render('$<', output_dir = '$(HTML_DIR)')"
KNIT_HANDOUT = Rscript -e "rmarkdown::render('$<', output_format = 'word_document')"

DATA_OUT = $(DATA_SOURCE:.R=.rda)

all:slides handouts scripts

slides handouts scripts:common.R $(DATA_OUT)

common.R:

slides:$(HTML_OUT) $(CSS)

handouts:$(HANDOUT_OUT)

scripts: $(SCRIPT_OUT)

$(HTML_DIR)/%.html:$(RMD_DIR)/%.Rmd
	$(KNIT_SLIDE)

$(HANDOUT_DIR)/%.docx:$(HANDOUT_DIR)/%.Rmd
	$(KNIT_HANDOUT)

$(SCRIPT_DIR)/%.R:$(RMD_DIR)/%.Rmd
	Rscript -e "knitr::purl('$<', output = '$@', documentation = 0L)"

$(DATA_DIR)/%.rda:$(DATA_DIR)/%.R
	Rscript -e "source('$<')"

clean:
	rm -f -v $(HTML_OUT)
	rm -f -v $(HANDOUT_OUT)
	rm -f -v $(SCRIPT_OUT)