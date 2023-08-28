# analyze CMS Nursing Home data

# todo:
# get "group by" working
# set key_row_num back to 29
# remove the limits on file size

# get keyframe working
# don't just limit to 2020'
# look for anomaly when group by provider and
# see if state data is consistent
# compute OR, RRR, ARR relative to the keyframe

# write this up, survey, pfizer study on
#
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
key_row_num = 1   # vax rollout is Dec 11 so this is week before that (12/6/2020) = row 29

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
  df=read_in_CMS_files() %>% limit_records()

  df_list=df %>% analyze_records()
  print(c("done analyzing records. dflist is", df_list))

  # all done. Take the list and save it
  df_list %>%  save_to_disk()   # returns the list of dataframe
  # return the full set of dataframes returned by analyze records including the
  # master
}

# get the comparison row from the df and return it. Others will need it
get_key_row <- function(df){
  print("loading key row")
  dfk=df[key_row_num,]    # without the comma, returns col 1. Get a dataframe of 1 row
  # now add computed columns IFR and Odds which makes other code easier
  cases_ref=dfk$cases  # grab cases
  deaths_ref=dfk$deaths # grab deaths
  print(c(cases_ref, deaths_ref))
  dfk %>% cbind(ifr=deaths_ref/cases_ref) %>%
  cbind(odds=deaths_ref/(cases_ref-deaths_ref))
}

# use this to limit records we are processing
# including head, remove bad actors and selecting a concatentation of states
limit_records <- function(df){
  df %>%
   head(520) #  %>%  limit number of records for debug
  # filter_out_bad_actors()  %>%
  # add filter on state here if wanted to limit everything below, e.g., calif
  #    filter_select(state, c('CA')
}


analyze_records <- function(df){
  # want to analyze by state, provider_num, week
  # df_list=list(master=df)    # initialize the list df is the "master" df with all values
  # make sure to do the get key row call after doing combine by week call and BEFORE calc stats
  # so make it a multi-line loop so can do this properly

  # this creates the key_row_df which is then no longer available outside this function

  key_row_df=NULL
  df_list=list()      # no need to write out the master df because we have everything we need
  for (col_name in columns_of_interest){
    # do one df at a time
    # always start with the original full dataframe when doing combine_by
    df1 = df %>% combine_by(col_name)
    if (is.null(key_row_df)){            # if first time, extract key row after the combine
          key_row_df=get_key_row(df1)   # get the core fields needed and compute other columns
          print(key_row_df)
    }
    df1=df1 %>% calc_stats(key_row_df)
    # now add this result to our list of dataframes
    print(c("analyze records: now adding df", col_name,df1))
    df_list[col_name]=df1     # df_list has list of dataframes named by the combine field
  }
  return(df_list)
}

read_in_CMS_files <- function(){
  tbl=data.frame()   # create empty container
  for (i in seq(startyear,endyear,1)) {
    tbl1 <- read.csv(paste0(file_prefix, i, file_suffix))
    tbl1 = tbl1[,c(week1,provider_num1, provider_state1, cases1, deaths1, acm1)]
    # sort everything by date in column 1 which makes debugging a little easier
    # tbl1=tbl1[ order(tbl1[,1]),]
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
  # so you'll only see weeks when group by weeks,
  # you'll only see providers (and 3 other columns) when group by providers
  # etc.
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
  # be sure to order these so if newest columns need older columns, they are there

  ifr_ref = key_row_df$ifr
  odds_ref = key_row_df$odds

  df %>% mutate(ncacm = acm-deaths) %>%
     mutate(ifr = deaths/cases) %>%
     mutate(odds = deaths/(cases-deaths)) %>%
     mutate(odds_ratio=odds/odds_ref) %>% # OR
     mutate(rr =   ifr/ifr_ref) %>% # RR
     mutate(arr = ifr_ref-ifr) # ARR... note the reference is first
}


# plot multiple lines on a graph
# Usage:
# plot_multi_line(df, x_col = "Time", y_cols = c("Y1", "Y2"))

plot_multi_line <- function(df, x_col, y_cols, mytitle="My graph", ytitle="Number") {

  df
}


plot_results <- function(df_list){
  # call plot_result several times for the plots desired
  df_list  %>% # ignore first row since very odd
    plot_multi_line('week', c('cases', 'deaths'), "Cases and deaths", "Count" )
  df   # return df
}

# https://cran.r-project.org/web/packages/openxlsx2/openxlsx2.pdf
save_to_disk <- function (dataframe_list){
  print("now saving dataframe_list to disk")
  print(dataframe_list)
  print("that was df list")
  # Create a new Excel workbook
  wb <- wb_workbook()

  # Loop over the list and add each dataframe to a separate worksheet
  # if the dataframes in the list don't have a name, nothing will be written
  # so pass in list(sheet1=df1, mysheet2=df2)
  # if the dataframes list is empty, you'll get a warning about no worksheets
  for (sheet_name in names(dataframe_list)) {
    wb$add_worksheet(sheet_name)
    wb$add_data(x=dataframe_list[[sheet_name]])
  }
  # Save the workbook to the specified output file
  wb$save(output_file)
  dataframe_list  # return the dataframe_list for others to process
}

# run
dfl=main()

