Problems with the Big Five assessment in the World Values Survey
---

### Description and Data Sources
Reproducibility material for 'Problems with the Big Five assessment in the World Values Survey', _Personality and Individual Differences_.

The `analysis` folder contains all files required to reproduce the figures and information provided in the manuscript. The `00-preprocessing.R` cannot be used without the file `WV6_Stata_v_2016_01_01.dta` which is publicly available at [worldvaluessurvey.org](http://www.worldvaluessurvey.org/).

### Author/contact

 - Steven Ludeke, Department of Psychology, University of Southern Denmark, stevenludeke@gmail.com
 - Erik Gahner Larsen, Department of Political Science, University of Southern Denmark, egl@sam.sdu.dk

### Repository content

##### `/analysis/`

- `01-preprocessing.R` = R script used to create `WV6.csv`
- `02-analysis.R` = R script to conduct all recodings and figures
- `WV6.csv` = Relevant variables from the World Values Survey Wave 6
- `sessionInfo.txt` = Output from `sessionInfo()`` in `R`

##### `/appendix/`

- `appendix.Rmd` = R markdown file used to create `appendix.pdf`
- `appendix.pdf` = Appendix

##### `/figs/`

- `figure1.png` = Figure 1
- `figure2.png` = Figure 2

### Data: `WV6.csv`

Variables in file:

- `V2` = Unique country identifier
- `V160J` = Openness, has an active imagination
- `V160E` = Openness, has few artistic interests
- `V160H` = Conscientiousness, does a thorough job
- `V160C` = Conscientiousness, tends to be lazy
- `V160F` = Extraversion, is outgoing, sociable
- `V160A` = Extraversion, is reserved
- `V160B` = Agreeableness, is generally trusting
- `V160G` = Agreeableness, tends to find fault with others
- `V160D` = Emotional stability, is relaxed, handles stress well
- `V160I` = Emotional stability, gets nervous easily
- `V240` = Gender
- `V242` = Age

### Session info

The analyses were made with [RStudio](http://www.rstudio.com/) (Version 1.0.136) with the following R session:

```
## R version 3.3.1 (2016-06-21)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.12.3 (Sierra)

## locale:
## [1] da_DK.UTF-8/da_DK.UTF-8/da_DK.UTF-8/C/da_DK.UTF-8/da_DK.UTF-8

## attached base packages:
## [1] grid      stats     graphics  grDevices utils     datasets  methods   base     

## other attached packages:
## [1] rio_0.4.12      stargazer_5.2   tidyr_0.6.0     gridExtra_2.2.1 reshape2_1.4.1  ggplot2_2.1.0  

## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.7      cellranger_1.1.0 plyr_1.8.4       tools_3.3.1      digest_0.6.10    jsonlite_1.0    
##  [7] tibble_1.2       gtable_0.2.0     openxlsx_3.0.0   csvy_0.1.3       DBI_0.5          curl_1.2        
## [13] yaml_2.1.13      haven_0.2.1      dplyr_0.5.0      stringr_1.1.0    xml2_1.0.0       readODS_1.6.2   
## [19] triebeard_0.3.0  data.table_1.9.6 R6_2.1.3         readxl_0.1.1     foreign_0.8-66   readr_1.0.0     
## [25] magrittr_1.5     scales_0.4.0     urltools_1.5.1   assertthat_0.1   colorspace_1.2-6 labeling_0.3    
## [31] stringi_1.1.1    lazyeval_0.2.0   munsell_0.4.3    chron_2.3-47    
```
