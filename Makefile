
.PHONY: all
all: data/icebirths.rda \
     data/nzdeaths.rda \
     README.md \
     documentation


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

