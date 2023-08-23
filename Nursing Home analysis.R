# OR analysis

# getwd() will get the working directory; defaults to repo root
# setwd("path/to/new") will change it

# read_excel will put in values for formulas it understands
# it will not interpret SUMIF, LET, etc cells
# the row numbering will be off by 1, so row 2 in excel = 1 in R

library(readxl)
library(dplyr) # need for pipe operation

source_tbl=read_excel("Nursing Home Small.xlsx")

# get two columns: num fatal and num deaths
tbl=source_tbl[c("Num Infec", "Num CD")]

demarcation_row = 3   # where to make the split. second part is after this row
first_part= 1:demarcation_row
second_part= demarcation_row+1:nrow(tbl)
sums_before = colSums(tbl[first_part,], na.rm=TRUE)
sums_after = colSums(tbl[second_part,], na.rm = TRUE)

