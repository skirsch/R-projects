# OR analysis

# getwd() will get the working directory; defaults to repo root
# setwd("path/to/new") will change it

# read_excel will put in values for formulas it understands
# it will not interpret SUMIF, LET, etc cells
# the row numbering will be off by 1, so row 2 in excel = 1 in R

# library(readxl)
library(dplyr) # need for pipe operation to work
require(stats)

# tbl=read_excel("Nursing Home.xlsx", sheet="IFR analysis",
#                      range=cell_cols("U:V"))

directory="CMS Nursing Home/datasets/"
prefix="faclevel_202"
suffix=".csv"
cases="Residents.Weekly.Confirmed.COVID.19"
deaths="Residents.Weekly.COVID.19.Deaths"
week="Week.Ending"
tbl <- read.csv(paste0(directory, prefix, "0", suffix))
tbl = tbl[,c(week,cases,deaths)]

# now sort by col1 (this works only within a year since year is last)

tbl=tbl[ order(tbl[,1]),]

# if the database is reasonably valid then we should be able to change the
# demarcation point to any point and get OR of 1

columns=c(cases,deaths)

calc <- function (row_break){
  row_break=row_break*1000
  first_part= 1:row_break
  second_part= row_break+1:nrow(tbl)
  sums_before = colSums(tbl[first_part,columns], na.rm=TRUE)  # sum up to demark row
  sums_after = colSums(tbl[second_part,columns], na.rm = TRUE)# sum below demark row


  dead1=sums_before[[deaths]]
  alive1=sums_before[[cases]]-dead1

  dead2=sums_after[[deaths]]
  alive2=sums_after[[cases]]-dead2

  odds_ratio = (dead2/alive2)/(dead1/alive1)
  # return a string
  sprintf("Odds ratio=%.3f at break of %g", odds_ratio, row_break/1000)
}
for (i in seq(50,400,50)) print(calc(i))

