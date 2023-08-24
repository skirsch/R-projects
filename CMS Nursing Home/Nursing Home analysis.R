# OR analysis

# getwd() will get the working directory; defaults to repo root
# setwd("path/to/new") will change it

# read_excel will put in values for formulas it understands
# it will not interpret SUMIF, LET, etc cells
# the row numbering will be off by 1, so row 2 in excel = 1 in R

# library(readxl)
library(dplyr) # need for pipe operation to work
require(stats)
library(lubridate)


# tbl=read_excel("Nursing Home.xlsx", sheet="IFR analysis",
#                      range=cell_cols("U:V"))

mydir="CMS Nursing Home/datasets/"
# avoid using setwd because it's always relative to where you are instead of the project
prefix="faclevel_202"
suffix=".csv"
cases="Residents.Weekly.Confirmed.COVID.19"
deaths="Residents.Weekly.COVID.19.Deaths"
week="Week.Ending"
startyear=0   # 2020
endyear=0     # 2023 is last file read in

# data is available starts on week 21 (week ending 5/24) week 52 (week ending 12/27)
# set limits we will refuse to compute if any row is in this range
min_week=29    # default starting week
max_week = 40    # last start week to consider so 104 would by end of 2021 156 is end of 2022

weeks_since = function(date_string){
  # weeks since 1/1/20 start numbering at 1
  date1=mdy(date_string)
  weeks_since_start=as.numeric(difftime(date1, ymd("2020-01-01"), units = "weeks"))+1
  return(floor(weeks_since_start)) # round down to integer
}

# start with 0,0 data,frame and append all the files
tbl=data.frame()
for (i in seq(startyear,endyear,1)) {
  tbl1 <- read.csv(paste0(mydir, prefix, i, suffix))
  # tbl1=read.csv(paste0(directory,"test.csv"))
  tbl1 = tbl1[,c(week,cases,deaths)]
  tbl1=tbl1[ order(tbl1[,1]),]  # sort everything by date in column 1
  tbl=rbind(tbl,tbl1) #  append the new table entries
}

# we now have a complete table in date order called tbl

# now create a new column of weeks since Jan 2020 called Week.Num at the right
tbl$Week.Num <- weeks_since(tbl$Week.Ending)

# if the database is reasonably valid then we should be able to change the
# demarcation point to any point and get OR of <1 because people are becoming
# more immune over time and as treatments got better

# we can look at a period of time, e.g., two month before vs. after window
# and then plot out the OR as we move the demarcation point in time,
# basically looking for a "discontinuity" where an intervention made things
# better or worse.


# calc odds ratio with the given week included in the BEFORE odds
# calculates the OR in the before window (including that week) vs. the after window
# so expect strongest odds at week 52
# week calc(52) computes OR through week 52 inclusive as the before so should be strongest signal

calc <- function (week_num, window_size=4){
  # now narrow to the columns of interest for computing the sums
  columns=c(cases,deaths)

  # calculate first the week numbers for the break point determinations
  start1=week_num-window_size+1  # start here on the matched row
  end1=week_num+1 # end one row before the row first matching this week num
  end2=week_num+window_size+1        # end one row before this

  row_start1 <- which(tbl$Week.Num == start1)[1]  # starting row
  row_end1 <- which(tbl$Week.Num == end1)[1]-1  # end of first section

  row_start2=row_end1+1       # start of AFTER region
  row_end2   <- which(tbl$Week.Num == end2)[1]-1  # end of AFTER region

  # now do sanity check, else return 1
  if (is.na(row_start1) | is.na(row_end2))
    return(1) # OR is 1 by default

  first_part= row_start1:row_end1
  second_part= row_start2:row_end2

  sums_before = colSums(tbl[first_part,columns], na.rm=TRUE)
  sums_after = colSums(tbl[second_part,columns], na.rm=TRUE)

  min_alive = 1  # for a facility, assume alive must be a least 1 so OR doesn't blow up

  dead1=sums_before[[deaths]]
  alive1=max(sums_before[[cases]]-dead1, min_alive)

  dead2=sums_after[[deaths]]
  alive2=max(sums_after[[cases]]-dead2, min_alive)

  odds_ratio = (dead2/alive2)/(dead1/alive1)
  # return a string
  # sprintf("Odds ratio=%.3f at break of %g", odds_ratio, row_break/1000)
  return (odds_ratio)
}

# indices <- expand.grid(row = 28:125, col = seq(2,16,2))

# end by fill in the table entries using f(x,y)

calc_tbl <- function (
    week_range=seq(35,40),
    window_range=seq(1,2,1)){

  df <- data.frame(
    weeks = week_range          # weeks 20 to 52 is first column
  )


  for (j in window_range){
    ans=calc(df[,1],j)
    df=cbind(df,ans)
  }

# finishing touches: remove helper column
#  df=df[,-1]    # remove the first column
  colnames(df)=c("Week", window_range)
#  rownames(df)=week_range

  write.csv(df, file = paste0(mydir, "results.csv"), row.names = FALSE) # save result

  df    # return the answer too
}
calc_tbl()
