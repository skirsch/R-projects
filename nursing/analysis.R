# analyze CMS Nursing Home data

# todo:
# look at non-COVID ACM
# plot of the IFR and odds ratio and
# write out answer as Excel
# locate bogus provider by grouping on the provider instead of the date

library(openxlsx2)   # write out xlsx; doesn't need java
library(dplyr) # need for pipe operation to work
require(stats)
library(lubridate)
library(ggplot2)

mydir="nursing/data/"
prefix="faclevel_202"
suffix=".csv"
provider_state="Provider.State"
cases1="Residents.Weekly.Confirmed.COVID.19"
deaths1="Residents.Weekly.COVID.19.Deaths"
acm1="Residents.Weekly.All.Deaths"
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
  df=read_in_CMS_files()
  # add filter on state here if wanted e.g., calif
  df %>% combine_weeks() %>% calc_stats() %>% plot_results()
}

read_in_CMS_files <- function(){
  tbl=data.frame()
  for (i in seq(startyear,endyear,1)) {
    tbl1 <- read.csv(paste0(mydir, prefix, i, suffix))
    tbl1 = tbl1[,c(week1,provider, provider_state, cases1, deaths1, acm1)]
    # sort everything by date in column 1 which makes debugging a little easier
    tbl1=tbl1[ order(tbl1[,1]),]
    tbl=rbind(tbl,tbl1) #  append the new table entries
  }
  # set new column names for use in summarize inside of combine_weeks
  colnames(tbl)=c("week", "provider", "state", "cases", "deaths", "acm")
  tbl %>% mutate_at(vars(week), mdy)  # set date type for the date
}

# combine cases and deaths with the same week into one row for each week
# one row per week (instead of 15,000 rows)
# need to filter out bad actors BEFORE combining rows
combine_weeks <- function (df) {
  df %>% filter_out_bad_actors() %>% group_by(week) %>%
  summarise(cases = sum(cases,na.rm=TRUE),
            deaths = sum(deaths, na.rm=TRUE),
            acm = sum(acm, na.rm=TRUE)
            )
}

# remove facilities with bogus counts (if we can find any)
providers_to_remove <- c(102, 104)
filter_out_bad_actors <- function(df){
  df # nothing to filter so far. use below line if find a bogus provider
  # df %>% filter(!provider %in% providers_to_remove)
}

# add new computed columns (so long as computed from values in same row it's easy)
calc_stats <- function (df){
  # input has week, cases, deaths columns
  # add 4 new computed columns: ncacm, ifr, dead:alive odds, and derivatives
  df %>% mutate(ncacm = acm-deaths) %>%
         mutate(ifr = deaths/cases) %>%
         mutate(odds = deaths/(cases-deaths)) %>%
         mutate(ifr8 =   ifr - lag(ifr, n=8, default=0)) %>%
         mutate(odds8 = odds - lag(odds, n=8, default=0))
}

# generic plot function, column names are at the end
plot_result <- function(df, mytitle, xlabel, ylabel, x_name, y_name, y2_name){
  line_plot <- ggplot(df, aes(x = x_name)) +
  geom_line(aes(y = y_name, color = y_name)) +
  geom_line(aes(y = y2_name, color = y2_name)) +
  labs(title = mytitle,
       x = xlabel,
       y = ylabel) +
  scale_color_manual(values = c(y_name = "blue", y2_name = "red"))

  print(line_plot)
}

plot_results <- function(df){
  # call plot_result several times for the plots desired
  df   # return df
}

# run
df=main()
# this didn't work
plot_result(df, "title", "x label", "y label", 'week', 'cases', 'deaths')

# this worked!!!

abc="week"
df %>% # data layer
  ggplot(aes(x = week, y = cases)) + # axes layer
  geom_line() + # geom layer
  labs(  # annotations layer
  title = "my cases per week") %>% print()

