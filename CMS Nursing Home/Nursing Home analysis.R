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
tbl <- read.csv(paste0(directory, prefix, "0", suffix))
tbl0 = tbl[,c("Week.Ending","Residents.Weekly.Confirmed.COVID.19","Residents.Weekly.COVID.19.Deaths")]

# now sort by col1 (this works only within a year since year is last)

tbl0=tbl0[ order(tbl0[,1]),]

# if the database is reasonably valid then we should be able to change the
# demarcation point to any point and get OR of 1

demarcation_row = 200000   # where to make the split. second part is after this row
first_part= 1:demarcation_row
second_part= demarcation_row+1:nrow(tbl)
sums_before = colSums(tbl[first_part,], na.rm=TRUE)  # sum up to demark row
sums_after = colSums(tbl[second_part,], na.rm = TRUE)# sum below demark row

dead1=sums_before[[2]]
alive1=sums_before[[1]]-dead1

dead2=sums_after[[2]]
alive2=sums_after[[1]]-dead2

odds_ratio = (dead2/alive2)/(dead1/alive1)
sprintf("Odds ratio=%.3f", odds_ratio)

