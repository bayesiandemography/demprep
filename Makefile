
.PHONY: all
all: data/icebirths.rda \
     README.md \
     documentation


# icebirths (births in iceland by year and by age of father)

data/icebirths.rda: data-raw/icebirths/icebirths.R \
  data-raw/icebirths/MAN05102.csv
	Rscript $<


## Update README

README.md : README.rmd
	Rscript -e 'knitr::knit("README.Rmd")'


## Documentation

.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"

