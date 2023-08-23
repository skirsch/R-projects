# OR analysis

# getwd() will get the working directory; defaults to repo root
# setwd("path/to/new") will change it

# read_excel will put in values for formulas it understands
# it will not interpret SUMIF, LET, etc cells
# the row numbering will be off by 1, so row 2 in excel = 1 in R

library(readxl)
library(dplyr) # need for pipe operation to work

# tbl=read_excel("Nursing Home.xlsx", sheet="IFR analysis",
#                      range=cell_cols("U:V"))

readfile <- read.csv("testdata.txt")


demarcation_row = 21   # where to make the split. second part is after this row
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

