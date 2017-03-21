# Load packages
library("rio")

# Load data (Stata)
wvs <- import("WV6_Stata_v_2016_01_01.dta")

# Export data frame with relevant variables (.csv)
export(wvs[c("V2","V160A", "V160B", "V160C", "V160D", "V160E", 
              "V160F", "V160G", "V160H", "V160I", "V160J", "V240", "V242")], "WV6.csv")
