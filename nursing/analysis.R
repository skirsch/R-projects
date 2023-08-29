# analyze CMS Nursing Home data

# remove the _ in filename. Comment out the print.

# todo:
# rerun(state) will re-run it using the given state
# and save to a special filename



# Data structure

# root is a hashtable with keys = states_to_process
# root is a global so easy to access from all functions
# root has the following keys:
#    ALL
#    CA
#    TX
#    ...

# Each state has a hash table with keys:
#   input: df of all the rows and the key columns
#   week: derived from the input
#   state: derivied
#   provider: derived
#   name: name of the state

# most functions will pipe the state hash table between the function calls

library(openxlsx2)   # write out xlsx; doesn't need java
library(xlsx)  # this one works without stoi exception
library(dplyr) # need for pipe operation to work
require(stats)
library(lubridate)
library(ggplot2)
library(rlang)
library(r2r)  # hash tables

DEBUG=TRUE

mydir="nursing/data/"
file_prefix=paste0(mydir,"faclevel_202")
file_suffix=".csv"
output_filename_prefix=paste0(mydir,"nursing_")

# original field names
provider_state1="Provider.State"
cases1="Residents.Weekly.Confirmed.COVID.19"
deaths1="Residents.Weekly.COVID.19.Deaths"
acm1="Residents.Weekly.All.Deaths"
provider_num1="Federal.Provider.Number"
week1="Week.Ending"
beds1="Number.of.All.Beds"
key_row_num = 29   # vax rollout is Dec 11 so this is week before that (12/6/2020) = row 29 for our reference

all_columns_to_extract=c(week1,provider_num1, provider_state1, cases1, deaths1, acm1, beds1)

# field names in the df tables
cases="cases"
deaths="deaths"
week="week"
provider_num="provider"
provider_state="state"
acm="acm"

# define the key names in the dict
master='master'   # holds the master df with all the records
# week is key holding the df with the week records
# provider_num ...
# provider_state key containing the df for by state
filter_condition='filter'  # this key contains the state to generate results for. NULL means all states

# this stores the dict state after the first pass analysis. Always the same so
# reuse each iteration as starting point
reference='reference'

# columns to summarize on to create 3 summary sheets
# these are the sheet names to create at the end
columns_of_interest=c(week, provider_num, provider_state)


# settable parameters
startyear=0   # 2020
endyear=3    # 2023 is last file read in

# analyze is a dict containing 4 dataframes: master, state, provider, week
# if re-run, we can start with this to establish metrics for filtering etc. off the base data
analyzed='analyzed'

main <- function(dict=NULL, state_name=NULL){
  # we are coming in with either a null dict or it is preloaded
  # read in CMS file with week added. week week num, provider, state, counts
  if (is.null(dict)){
    df=read_in_CMS_files()
    dict=hashmap(list(master, df))  # start out with the key master=master db which is never modified
    dict= dict %>% analyze_records() # dict now has all the keys added to it

    # the lowest level dict has no reference key
    # the state dicts have a reference slot point
    # circular reference a bad idea
    dict[[reference]] = dict

  } else {
    dict=dict[[reference]]  # grab the existing pre-computed dictionary
  }
  dict[[filter_condition]]=state_name  # if not null, will only generate for that state, e.g., "CA"
  # at this point we are on our second pass through the records
  # state if not null, means only for that state
  dict %>% data_cleanup() %>% analyze_records()  %>%  save_to_disk()

  # if this is the first time, we run (no saved reference), save the final results of the
  # first analysis for inputs on subsequent runs
  if (is.null(dict[[reference]]))
      dict[[reference]] = dict

  # return the dict
  dict
}

# get the comparison row from the df and return it. Others will need it
get_key_row <- function(df){
  dfk=df[key_row_num,]    # without the comma, returns col 1. Get a dataframe of 1 row
  # now add computed columns IFR and Odds which makes other code easier
  cases_ref=dfk$cases  # grab cases
  deaths_ref=dfk$deaths # grab deaths
  # add the two computed columns
  dfk %>% cbind(ifr=deaths_ref/cases_ref) %>%
  cbind(odds=deaths_ref/(cases_ref-deaths_ref))
}

# use this to limit records we are processing
# including head, remove bad actors and selecting a concatentation of states
data_cleanup <- function(dict){
  # table1=analysis by provider; table2 is the master table
  # run the analysis to do the crosstabs
  # remove the offending entries
  # then re-run the data processing again
  # then output to sheet.
  # so no data cleanup on first run

  # criteria for provider removal
  # IFR >1
  # cases =0 or > 300
  # deaths range >150

  if (DEBUG) print("Entering data_cleanup")
  table1=dict[[provider_num]]
  table2=dict[[master]]

  state_name=dict[[filter_condition]]
  # Apply the filtering criteria to the provider table to get the provider IDs to remove
  # from the database
  filtered_table1 <- filter_criteria(table1, state_name)

  # Get a list of ProviderNumbers meeting the criteria
  selected_provider_numbers <- filtered_table1$provider

  # Delete records from table2 where ProviderNumber matches
  table2 <- table2[!(table2$provider %in% selected_provider_numbers), ]
  dict[[master]]=table2       # update the new master
  dict   # return the dictionary
  }

# Define the filtering criteria for when a record will be removed
# called by data_cleanup
# this receives a df created by analysis  NOT the dict
# the df passed in is always the PROVIDER dataframe with 11 columns
#
# the references here are the hard coded column names in this dataframe
# This function is used ONLY to determine the provider numbers
# which should be removed based on the overall stats for that provider.
# we need to add state as a column in the provider df here for this to work properly

# this function gets calle ONCE each time you call main
# if this is re-run, the database you are starting with has already had the bad QA
# records already tossed. So if you try to run the removal criteria again, you'll get 0 records
# for the second criteria. So solution is to use the initial criteria if state isn't specified,
# and use the other criteria when it is
filter_criteria <- function(df, state_name) {
  if (is.null(state_name))
    # filter will return the records that match the criteria
    # then we will take these records (which are "bad") and remove them
    # based on their provider ID
    df %>% filter(ifr > 1 | deaths > 150 | cases > 300 | cases ==0 |
             (cases>100 & deaths==0) )

  else
    # select the recrods which do NOT match the state name so they can be removed
    # so this will be a big list of all states we don't want
    # we basically just want to leave calif
    # note that state will be interpreted as a literal field name to match the column
    df %>% filter(state != state_name)
}

analyze_records <- function(dict){
  # takes the original dataframe and creates 3 output summary dataframes
  # specified in columns_of_interest: state, provider_num, week
  # want to analyze by state, provider_num, week
  # dict=list(master=df)    # initialize the list df is the "master" df with all values
  # make sure to do the get key row call after doing combine by week call and BEFORE calc stats
  # so make it a multi-line loop so can do this properly

  if (DEBUG) print("start of analyze records")

  # this creates the key_row_df which is then no longer available outside this function
  df=dict[[master]]  # get the df containing the FULL database
  key_row_df=NULL    # this will be set to values in the comparison row for OR calc

  for (col_name in columns_of_interest){
    # do one df at a time
    # always start with the original full dataframe when doing combine_by
    df1 = df %>% combine_by(col_name)
    if (is.null(key_row_df)){            # if first time, extract key row after the combine
          key_row_df=get_key_row(df1)   # get the core fields needed and compute other columns
    }
    dict[[col_name]]=df1 %>% calc_stats(key_row_df)
    # now add this result to our list of dataframes
  }
  return(dict)
}

read_in_CMS_files <- function(){
  tbl=data.frame()   # create empty container
  for (i in seq(startyear,endyear,1)) {
    tbl1 <- read.csv(paste0(file_prefix, i, file_suffix))
    # just interested in key columns in the original .csv file
    tbl1 = tbl1[, all_columns_to_extract]
    # sort everything by date in column 1 which makes debugging a little easier
    # tbl1=tbl1[ order(tbl1[,1]),]
    tbl=rbind(tbl,tbl1) #  append the new table entries to the bottom
  }
  # set new column names for use in summarize inside of combine_weeks
  colnames(tbl)=c(week, provider_num, provider_state, cases, deaths, acm, beds)
  tbl %>% mutate_at(vars(week), mdy)  # set date type for the date
}

# combine cases and deaths with the same week into one row for each week
# one row per week (instead of 15,000 rows)
# this is called by analyze records 3 times; once for each of the 3 derived
# dataframes of interest as listed here:
# columns_of_interest=c(week, provider_num, provider_state)
# so col_name is key on each... df will be the same input (master db)
combine_by <- function (df, col_name=week) {
  if (DEBUG) print(c("entering combine_by with col_name", col_name))
  # group_by wants a static column name rather than a variable
  field_symbol <- sym(col_name)

  # this will output 4 column df including the field you are grouping by
  # so you'll only see a weeks when you group by weeks,
  # you'll only see providers (and 3 other columns) when group by providers
  # etc.

  # so this is where we specify which columns appear in the output
  # before we start tacking on the analysis to the core columns

  # if we are summarizing by provider num, it makes sense to add state
  # to the output
  # used to be summarize, but they said use reframe
  # returns the dataframe

  # Note that group_by just does a calculation... you can't visably see
  # any modification to the data itself. So you have to use
  # reframe to see the results.

  if (col_name == provider_num )
    # for generating the provider tab, include the state of the provider in the output
    df=df %>% group_by(!!field_symbol) %>%
       reframe(cases = sum(cases,na.rm=TRUE),
            deaths = sum(deaths, na.rm=TRUE),
            acm = sum(acm, na.rm=TRUE),
            state=head(state,1)  # we can take any item since they are the same so take the first.
            )
    else
    df=df %>% group_by(!!field_symbol) %>%
       reframe(cases = sum(cases,na.rm=TRUE),
            deaths = sum(deaths, na.rm=TRUE),
            acm = sum(acm, na.rm=TRUE)
            )
    return(df)   # return the derived sheet (either weeks, provider, state)
}

# add new computed columns (so long as computed from values in same row it's easy)
# input has week, cases, deaths columns (columns of interest)
# add 4 new computed columns: ncacm, ifr, odds, OR, RR, and ARR
# key_row_df has the elements we need to compute the stats and is passed in
# be sure to order these so if newest columns need older columns, they are there

calc_stats <- function (df, key_row_df){

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


plot_results <- function(dict){
  # call plot_result several times for the plots desired
  dict  %>% # ignore first row since very odd
    plot_multi_line('week', c('cases', 'deaths'), "Cases and deaths", "Count" )
  df   # return df
}


# http://www.sthda.com/english/wiki/writing-data-from-r-to-excel-files-xls-xlsx
save_to_disk <- function (dict){
  if (DEBUG) print("entering save to disk")

  # will give error if sheet name already exists
  filename=paste0(output_filename_prefix,state,".xlsx")

  for (sheet_name in columns_of_interest){
    sheet_unique=paste0(sheet_name, dict[[filter_condition]])
    write.xlsx(dict[[sheet_name]], output_file, sheetName = sheet_unique,
    col.names = TRUE, row.names = FALSE, append = TRUE)
  }
}

# https://cran.r-project.org/web/packages/openxlsx2/openxlsx2.pdf
old_save_to_disk = function(dict){
  wb <- wb_workbook()

  # Loop over the list and add each dataframe to a separate worksheet
  # if the dataframes in the list don't have a name, nothing will be written
  # so pass in list(sheet1=df1, mysheet2=df2)
  # if the dataframes list is empty, you'll get a warning about no worksheets

  for (sheet_name in columns_of_interest) {
    # create empty sheet with given name
    wb$add_worksheet(sheet_name)
    # now add the dataframe to that sheet
    wb$add_data(x=dict[[sheet_name]])
  }
  # Save the workbook to the specified output file
  file_suffix=dict[[filter_condition]]  # get the state
  if (is.null(file_suffix))
    # it's the first run so save as specified name
    output_filename=output_file
  else
    # append _CA if calif
    output_filename=sub("\\.", paste0("_", file_suffix,"."),output_file)
  wb$save(output_filename)
  dict # return the dict for others to process
}


# run
dict=main()

# create keys for the dict for each state
# TX creates stoi error when write out workbook
for (s in c('CA', 'TX', 'FL', 'NY','PA')){
  if (DEBUG)
    print(paste0("Now computing for state:",s))
  dict[[s]]=main(dict, state=s)   # run for top 5 states


}

