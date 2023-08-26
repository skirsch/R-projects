# analyze CMS Nursing Home data

# todo:
# summarize data on a per facility basis so can weed out those with 0 IFR and high IFR
# write out the two tables as Excel sheets
# plot of the IFR and odds ratio and

# locate bogus provider by grouping on the provider instead of the date

library(openxlsx2)   # write out xlsx; doesn't need java
library(dplyr) # need for pipe operation to work
require(stats)
library(lubridate)
library(ggplot2)
library(rlang)

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
  df=read_in_CMS_files() %>% filter_out_bad_actors()
  # add filter on state here if wanted e.g., calif
  df %>% combine_weeks() %>% calc_stats()  # %>% plot_results()
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
combine_by <- function (df, col_name=week) {
  df %>% group_by(col_name) %>%
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
         mutate(odds_ratio=odds/lag(odds, n=8, default=0)) %>%
         mutate(ifr8 =   ifr - lag(ifr, n=8, default=0)) %>%
         mutate(odds8 = odds - lag(odds, n=8, default=0)) # change in odds
}

# plot multiple lines on a graph
# Usage:
# plot_multi_line(df, x_col = "Time", y_cols = c("Y1", "Y2"))

plot_multi_line <- function(df, x_col, y_cols, mytitle="My graph", ytitle="Number") {
  myplot=ggplot(df, aes(x = .data[[x_col]]))+
    scale_color_manual(values = c("blue", "red"))+      # ,"green","orange","magenta"))
    # now we can add a line plot for each y_col sent in
  for (i in seq(1,length(y_cols))){
    myplot=myplot+geom_line(aes(y = .data[[y_cols[i]]], color = y_cols[i]), linewidth = 1)
  }
  myplot=myplot+
        labs(title = mytitle,
         x = x_col,
         y = ytitle,
         color = "Legend") +
    # scale_x_continuous(breaks = seq(min(.data[[x_col]]), max(.data[[x_col]]), by = 1)) +
    scale_x_continuous(n.breaks=10, minor_breaks=seq(1,30)) +

  theme_minimal()
  print(myplot)
  df
}


plot_results <- function(df){
  # call plot_result several times for the plots desired
  df  %>% # ignore first row since very odd
    plot_multi_line('week', c('cases', 'deaths'), "Cases and deaths", "Count" )
  df   # return df
}


# run
df=main()

