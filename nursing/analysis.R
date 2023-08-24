# analyze CMS Nursing Home data

# todo:
# modify the date format after read in the file (or do a sort by date)
# add column computing a slope ratio
# plot the IFR and odds ratio

library(xlsx)   # allow write to multiple sheets using write.xlsx()
library(dplyr) # need for pipe operation to work
require(stats)
library(lubridate)

mydir="nursing/data/"
prefix="faclevel_202"
suffix=".csv"
provider_state="Provider.State"
cases1="Residents.Weekly.Confirmed.COVID.19"
deaths1="Residents.Weekly.COVID.19.Deaths"
provider="Federal.Provider.Number"
week1="Week.Ending"

# cases="cases"
# deaths="deaths"
# week="week"

# settable parameters
startyear=0   # 2020
endyear=3     # 2023 is last file read in

main <- function(){
  # read in CMS file with week added. week week num, provider, state, counts
  df_cms=read_in_CMS_files()
  # add filter on state here
  df_summary=summarize_cms(df_cms)  # 1 row per week using summarize
  df_odds = cms_odds(df_summary)    # df of cases, dead, alive, IFR, odds
  }

read_in_CMS_files <- function(){
  tbl=data.frame()
  for (i in seq(startyear,endyear,1)) {
    tbl1 <- read.csv(paste0(mydir, prefix, i, suffix))
    tbl1 = tbl1[,c(week1,provider, provider_state, cases1, deaths1)]
    tbl1=tbl1[ order(tbl1[,1]),]  # sort everything by date in column 1
    tbl=rbind(tbl,tbl1) #  append the new table entries
  }
  # set new column names for use in summarize
  colnames(tbl)=c("week", "provider", "state", "cases", "deaths")
  tbl    # return the df
}

# add up cases and deaths
summarize_cms <- function (df) (
  df %>%
  group_by(week) %>%
  summarise(cases = sum(cases,na.rm=TRUE), deaths = sum(deaths, na.rm=TRUE))
)

# add new computed columns (so long as computed from values in same row it's easy)
cms_odds <- function (df){
  # input has week, cases, deaths column
  # i'm going to add some new computed columns: ifr and death odds
  df %>% mutate(ifr = deaths/cases) %>%  mutate(odds = deaths/(cases-deaths))
}

# run
df=main()
