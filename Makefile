
.PHONY: all
all: data/icebirths.rda \
     data/nzdeaths.rda \
     README.md \
     documentation \
     vignettes/demprep.html


# icebirths (births in Iceland by year and by age of father)

data/icebirths.rda: data-raw/icebirths/icebirths.R \
  data-raw/icebirths/MAN05102.csv
	Rscript $<

# nzdeaths (deaths in New Zealand by age, sex, and year)

data/nzdeaths.rda: data-raw/nzdeaths/nzdeaths.R \
  data-raw/nzdeaths/VSD349202_20210630_082144_75.csv
	Rscript $<


## Update README

README.md : README.rmd
	Rscript -e 'knitr::knit("README.Rmd")'


## Documentation

.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"


## Vignette

vignettes/workflow.pdf: vignettes/workflow.tex
	pdflatex --interaction=batchmode $<
	mv workflow.pdf vignettes/workflow.pdf
	rm workflow.aux workflow.log

vignettes/workflow.png: vignettes/workflow.pdf
	convert -strip -density 400  -background white -alpha off $< $@

vignettes/demprep.html: vignettes/demprep.Rmd \
        vignettes/workflow.png
	R -e 'rmarkdown::render("$<")'
