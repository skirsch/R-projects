# OR analysis
# Row limits 500 works but numbers are the same per week and OR is too big
# Row limits 300 gives errors

# problem it calls calc but never calculates ....

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
provider="Federal.Provider.Number"
week="Week.Ending"
startyear=0   # 2020
endyear=1     # 2023 is last file read in

# data is available starts on week 21 (week ending 5/24) week 52 (week ending 12/27)
# set limits we will refuse to compute if any row is in this range
min_week=33    # default starting week
max_week = 91    # last start week to consider so 104 would by end of 2021 156 is end of 2022

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
  # row_limits=1:500   # for testing
  tbl1 = tbl1[,c(week,provider, cases, deaths)]
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

columns=c(cases,deaths)
calc <- function (week_num, window_size=4){
  # now narrow to the columns of interest for computing the sums
  # it seems to call the function in parallel if calling it on a column
  print(paste("entered into  calc",week_num, window_size ))
  print(paste("week_num", week_num))

  # calculate first the week numbers for the break point determinations
  start1=week_num-window_size+1  # start here on the matched row
  end1=week_num+1 # end one row before the row first matching this week num
  end2=week_num+window_size+1        # end one row before this

  # this can throw warning about multiple of shorter length obj if it can't find the start week because [1] won't
  # work
  print("starting to find rows")
  row_start1 <- which(tbl$Week.Num == start1)[1]  # starting row
  row_end1 <- which(tbl$Week.Num == end1)[1]-1  # end of first section

  row_start2=row_end1+1       # start of AFTER region
  row_end2   <- which(tbl$Week.Num == end2)[1]-1  # end of AFTER region

  print("now doing sanity check")
  # now do sanity check, else return 1
  if (is.na(row_start1) | is.na(row_end2)){
    print(paste("whoops. NA for row or column not found", row_start1, row_end2) )
    return(NA) # can't compute the value so leave blank
      }

  print("now setting up parameters for sums")

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
  print(sprintf("row1start %g row1end %g row2start %g row2end %g dead1 %g alive1 %g dead2 %g alive2 %g  odds ratio %g",
                row_start1, row_end1, row_start2, row_end2, dead1, alive1, dead2, alive2, odds_ratio))


  # return a string
  # sprintf("Odds ratio=%.3f at break of %g", odds_ratio, row_break/1000)
  return (odds_ratio)
}

# indices <- expand.grid(row = 28:125, col = seq(2,16,2))

# end by fill in the table entries using f(x,y)

calc_tbl <- function (
                week_range=seq(min_week,max_week),
                window_range=seq(2,12,2)){


  m=matrix(nrow=length(week_range), ncol=length(window_range))
  for (i in seq(length(week_range))) {
    for (j in seq(length(window_range))) {
      print(paste("iteration with", i,j, week_range[i], window_range[j]))
      m[i,j]=calc(week_range[i],window_range[j])
    }
  }

  # Create combinations of row and column indices
#  indices <- expand.grid(row = week_range, window_range)

# Apply the function to each combination and store results in a matrix
#  result_matrix <- matrix(apply(indices, 1, function(row) {
#    calc(row[1], row[2])
#  }), nrow = length(week_range), byrow = TRUE)


  # Convert the matrix to a dataframe with appropriate column names
  df <- as.data.frame(m)
  # colnames(result_df) <- window_range

  # finishing touches: add the week column, then set colnames
  #  df=df[,-1]    # remove the first column
  df=cbind(week_range, df)

  colnames(df)=c("Week", window_range)
#  rownames(df)=week_range

  write.csv(df, file = paste0(mydir, "results.csv"), row.names = FALSE) # save result

  df    # return the answer too
}
calc_tbl()
