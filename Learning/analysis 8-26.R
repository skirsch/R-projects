# analyze CMS Nursing Home data

# todo:
# compute OR, RRR, ARR relative to keyframe
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
file_prefix=paste0(mydir,"faclevel_202")
file_suffix=".csv"
output_file=paste0(mydir,"nursing.xlsx")

# original field names
provider_state1="Provider.State"
cases1="Residents.Weekly.Confirmed.COVID.19"
deaths1="Residents.Weekly.COVID.19.Deaths"
acm1="Residents.Weekly.All.Deaths"
provider_num1="Federal.Provider.Number"
week1="Week.Ending"
key_row_num = 29   # vax rollout is Dec 11 so this is week before that (12/6/2020)

# short names we use here
cases="cases"
deaths="deaths"
week="week"
provider_num="provider"
provider_state="state"
acm="acm"

# columns to summarize on
columns_of_interest=c(week, provider_num, provider_state)


# settable parameters
startyear=0   # 2020
endyear=0     # 2023 is last file read in

main <- function(){
  # read in CMS file with week added. week week num, provider, state, counts
  df=read_in_CMS_files() %>%
    limit_records()
  key_row_df=load_key_row(df)
  df %>%
    analyze_records(key_row_df) %>% # this returns a LIST of dataframes for saving
    save_to_disk()   # returns the saved list
  # return the full set of dataframes returned by analyze records including the
  # master
}

# get the comparison row from the df and return it. Others will need it
load_key_row <- function(df){
  df[key_row_num,]   # without the comma, returns col 1. Get a dataframe of 1 row
}

# use this to limit records we are processing
# including head, remove bad actors and selecting a concatentation of states
limit_records <- function(df){
  df %>%
   head(45000) #  %>%  limit number of records for debug
  # filter_out_bad_actors()  %>%
  # add filter on state here if wanted to limit everything below, e.g., calif
  #    filter_select(state, c('CA')
}


analyze_records <- function(df,key_row_df){
  # want to analyze by state, provider_num, week
  # df_list=list(master=df)    # initialize the list df is the "master" df with all values
  df_list=list()      # no need to write out the master df because we have everything we need
  for (col_name in columns_of_interest)
    df_list[col_name] = df %>% combine_by(col_name) %>% calc_stats(key_row_df)
}

read_in_CMS_files <- function(){
  tbl=data.frame()
  for (i in seq(startyear,endyear,1)) {
    tbl1 <- read.csv(paste0(file_prefix, i, file_suffix))
    tbl1 = tbl1[,c(week1,provider_num1, provider_state1, cases1, deaths1, acm1)]
    # sort everything by date in column 1 which makes debugging a little easier
    tbl1=tbl1[ order(tbl1[,1]),]
    tbl=rbind(tbl,tbl1) #  append the new table entries
  }
  # set new column names for use in summarize inside of combine_weeks
  colnames(tbl)=c(week, provider_num, provider_state, cases, deaths, acm)
  tbl %>% mutate_at(vars(week), mdy)  # set date type for the date
}

# combine cases and deaths with the same week into one row for each week
# one row per week (instead of 15,000 rows)
combine_by <- function (df, col_name=week) {
  # group_by wants a static column name rather than a variable
  field_symbol <- sym(col_name)

  # this will output 4 column df including the field you are grouping by
  df %>% group_by(!!field_symbol) %>%
  summarise(cases = sum(cases,na.rm=TRUE),
            deaths = sum(deaths, na.rm=TRUE),
            acm = sum(acm, na.rm=TRUE)
            )
}

# remove facilities with bogus counts (if we can find any)
provider_nums_to_remove= c(102, 104)  # concatenation of providers to remove
filter_out_bad_actors <- function(df){
  df # nothing to filter so far. use below line if find a bogus provider
  # filter out records at start based on provider number
    # df %>% filter(!provider_num %in% provider_nums_to_remove)
}

# only keep the records of a df where the col, value is a match, e.g.,
# provider_state, seq("CA", "OR")
filter_select <- function(df, col, val){
  df %>% filter(col %in% val)
}

# add new computed columns (so long as computed from values in same row it's easy)
calc_stats <- function (df, key_row_df){
  # input has week, cases, deaths columns
  # add 4 new computed columns: ncacm, ifr, dead:alive odds, and derivatives
  # key_row_df has the elements we need to compute the stats and is passed in
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


plot_results <- function(df_list){
  # call plot_result several times for the plots desired
  df_list  %>% # ignore first row since very odd
    plot_multi_line('week', c('cases', 'deaths'), "Cases and deaths", "Count" )
  df   # return df
}

save_to_disk <- function (dataframe_list){

  # Create a new Excel workbook
  wb <- createWorkbook()

  # Loop over the list and add each dataframe to a separate worksheet
  for (sheet_name in names(dataframe_list)) {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, dataframe_list[[sheet_name]])
  }
  # Save the workbook to the specified output file
  saveWorkbook(wb, output_file, overwrite = TRUE)
  dataframe_list  # return the dataframe_list for others to process
}


# run
dfl=main()

