# analyze CMS Nursing Home data

# todo:
# derivative of IFR and deriv of odds
# plot second derivative
# plot the IFR and odds ratio and

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
  # add filter on state here e.g., calif
  df_cms %>% combine_weeks() %>% calc_stats()
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
  tbl %>% mutate_at(vars(week), mdy)  # set date type for the date
}

# combine cases and deaths with the same week into one row
# one row per week (instead of 15,000 rows)
# need to filter out bad actors BEFORE combining rows
combine_weeks <- function (df) {
  df %>% filter_out_bad_actors() %>% group_by(week) %>%
  summarise(cases = sum(cases,na.rm=TRUE), deaths = sum(deaths, na.rm=TRUE))
}

# remove facilities with bogus counts
providers_to_remove <- c(102, 104)
filter_out_bad_actors <- function(df){
  df %>% filter(!provider %in% providers_to_remove)
}

# add new computed columns (so long as computed from values in same row it's easy)
calc_stats <- function (df){
  # input has week, cases, deaths columns
  # add 3 new computed columns: ifr, dead:alive odds, and derivative
  df %>% mutate(ifr = deaths/cases) %>%
         mutate(odds = deaths/(cases-deaths)) %>%
         mutate(ifr8 =   ifr - lag(ifr, n=8, default=0)) %>%
         mutate(odds8 = odds - lag(odds, n=8, default=0))
}

# run
df=main()
