# analyze CMS Nursing Home data

# to do
# compute for each provider the IFR for an 8 week window
# prior to the reference week vs. 8 weeks after the reference week
# to see if can find a provider where the IFR dropped by 3X or more
#
# compute first without reference ... just compute it the first time and then compute
# again so double compute
# replace poisson coefficients with gamma distribution
# state graphs need to use the state computed OR reference, not global ref.
#
# implement min_deaths
# calculate the OR reference using the new method

# Now have do QA on the tabs because
# when we did the lag(case,1) it means that can only compute extra stats
# on the week table, not on provider or state tables which we are clearly doing!
# so need to calculate those without a lag. Check that OR on those tabs is
# correctly computed from the reference OR!

# 8/31/23 everything works.

# 8/30/23: revised analysis with time lag worked perfectly.
# vaccine made things worse.
# OR nearly 2:1 on 2/28/01 *after* the case rates no longer falling
# so can't use rapidly falling case rates as "excuse" for high OR and IFR.
# We have 1. neutralized that with the time lag and 2. the peak happened when
# case rates had stabilized and 3. The OR stayed above 1 until 4/4/21!!!

### CODE STRUCTURE
# main
#  process_state for states of interest starting with ALL
#    read in original data files (or reload from saved cache)
#    extract_records (ALL=remove bad providers; per state=remove providers in other states)
#	       filter_criteria
#    analyze_records (week, provider, state): Loops through type to call these functions
#     	combine_by (collapse into single record a week, per provider, or for the state)
#	      calc_stats (add new computed columns)
#  summarize columns (create summaries in ALL over all states for just odds_ratio and ARR for scatter plot
#  save to disk

# field names inside the df tables
cases="cases"
deaths="deaths"
week="week"
provider_num="provider"
provider_state="state"
acm="acm"
beds="beds"
odds_ratio="odds_ratio"
arr="arr"


##########
# CONFIGURATION PARAMETERS
#
DEBUG=FALSE
SAVE_TO_DISK=TRUE
ALL_STATES=FALSE  # FALSE will do 5 largest states only
ALL_ONLY=TRUE    # set to TRUE to limit analysis to just ALL, no states

# Config which facilities will be included in the calculations
# this is based on stats over the ENTIRE period. The idea is that not all facilities accurately report
# so this should remove most, if not all, the facilities reporting suspect data
MIN_DEATHS=0      # a facility with less than this number of deaths will be ignored
MAX_DEATHS=50  # a facility with more than this number of deaths will be ignored
MAX_CASES=400 # filter out facilities with more than MAX_cases
MIN_CASES=0   # filter out facilities with fewer than this num of cases. Set to 0 for no filtering.
MAX_IFR=.5     # don't allow a provider whose IFR >.5 for the entire period. This is liberal to allow for few hundred more homes
              # but a more sane limit would be .25 or less. It doesn't change the outcome
MIN_NCACM=0   # Non-COVID ACM shouldn't be negative for a site
MAX_NCACM=260 # prevent single large sites from skewing the data (could be data error if more than this)
MAX_ACM=300   # Max reasonable ACM for a facility; virtually none are higher than this

# columns to summarize for each week for each state analyzed
# we can run stats on the arr since it should have mean of 0
# to see how the states compared with their reference
columns_to_summarize=c("odds_ratio", "arr")


# For limiting data read in
startyear=0   # 2020
endyear=3    # 2023 is last file read in

# min max values to apply to the summarized data
# this ONLY applies to values in the "odds_ratio" and "arr" tabs of the spreadsheet
# this was useful to apply these limits for excel graphing to save time
# this is only useful if you are analyzing each state
columns_to_summarize_limits=list(
  # excel does a horrible job with y-axis labels on
  # scatter plots so this limits to a factor of 4x if you need it
  odds_ratio=list(.05,20),  # limit by 20X in each direction
  arr=list(-1,1))  # arr should always be between these limits

# Reference week number (week before the vaccine rolled out)
# vaccines were first rolled out on Dec 18, 2020 to a small number of homes, more on Monday
# so the week ending 12/20/20 is the last week of virtually no vaccine
# 29 = 12/6/20 which is week ending before when vax first available for anyone (12/11/20)
# 32 = 12/27/20 which is right before the big push in US nursing homes
# 88 = 1/23/22 which is another peak point to prove it isn't rising cases

reference_row_num = 32

# For each provider, we'll calculate the IFR for the set number of weeks prior to the
# reference week and after the reference week and put that in the provider table
# so we can easily see if any providers reduced their IFRs post vaccine
# the before and after does NOT include the reference row.
IFR_CALC_WINDOW=12

# specify which column will by used for the "all analysis" function
# One column per state makes the most sense
# this will generate OR and ARR tables; each table is week x state
# this allows a scatterplot so you can see how the states are individually
# moving each week
ALL_ANALYSIS_COLUMNS=c(provider_state)

# weights of cases starting with the current case
# leave off the final value; it is computed so will sum to 1
# this is optimized from the global data full set

# change so can rerun

# cases are basically shifted 1 week
# specify just two values. third is filled in automatically
# correlation is .996 with .2, .6, .2
# so when we compute things like odds and IFR for a given week, we are using the ACTUAL deaths on that week
# and we are using weighted average of the number of case
# so .2*current week cases, .6*# cases 1 week ago, and .2*# of cases 2 weeks ago = effective number of cases for the week
case_weights=c(.2, .6)  # this is effectively a .2, .6, .2 weighting because it fills in the missing weight

# to do...
# add new global_summary() function like I asked chatgpt that extracts
# the "target row" of state db, plus some interesting columns as well, e.g., 4 weeks OR

# do a transform on the cases
# cases that lag by a preset amount (e.g., 3/7 of a weed)
# with a goal of minimizing the IFR peaks and valley
# this is easily done by doing a transform on the cases column
# where cases <- .2*cases + .8*lag(c) which shifts the cases to the future
# by a small amount
#
# check this in Excel and see if it makes an impact on the IFR curve before coding


# Data structure

# root is a hashtable with keys = states_to_process
# root is a global so easy to access from all functions
# root has the following keys:
#    ALL
#    CA
#    TX
#    ...

# Each state has a hash table with keys:
#   records: df of all the rows and the key columns
#   week: derived from the input
#   state: derivied
#   provider: derived
#   name: ALL or name of the state

# most functions will pipe the state hash table between the function calls, not the df
# that way there is no arguments ever needed between the functions



library(openxlsx2)   # write out xlsx; doesn't need java
# library(xlsx)  # requires Java and openxlsx2 works great.
library(dplyr) # need for pipe operation to work
require(stats)
library(lubridate)
library(ggplot2)
library(rlang)
library(r2r)  # hash tables


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


# define the key names used in each state (and ALL) dict
# each of these 4 keys will hold a dataframe
records='records'   # holds the master df with all the records. this is usually initialzed from root[[ALL]][[records]]
original_records='original_records'  # this has exactly what was read in from CMS
# week is key holding the df with the week records
# provider_num ...
# provider_state key containing the df for by state
# name is state name or ALL
name='name'

#
# things of interest
#
# fields to extract from the source file (do this just once)
columns_to_extract=c(week1,provider_num1, provider_state1, cases1, deaths1, acm1, beds1)

# columns to summarize on to create 3 summary sheets
# these are the sheet names to create at the end which appear as "week" "provider" "state"
columns_of_interest=c(week, provider_num, provider_state)

# key names in root are the states of interest
ALL = 'ALL'         # ALL states analysis; must do first since reuses this
# Note that ALL must be listed first in each list
state_list <- c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'FL', 'GA', 'HI', 'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY')
all_states <- c(ALL, state_list)
top5_states <- c(ALL, 'CA', 'TX', 'FL', 'NY','PA') # top 5 largest states

states_of_interest=top5_states  # default analysis is on top 5 states

if (ALL_STATES)
  states_of_interest=all_states

if (ALL_ONLY)
  states_of_interest=ALL




# this has the container for everything
root=hashmap()



main <- function(){
  for (s in states_of_interest){
    dict=root[[s]]
    if (is.null(dict))    # only create if not already there; otherwise re-use
        dict=root[[s]]=hashmap()
    dict[[name]]=s
    process_state(dict)
  }
  print("Creating summary tab")
  summarize_columns()

  print("Saving to disk")
  save_to_disk()
}

# called once for each dict in root
process_state <- function(dict){
  if (DEBUG) print(paste("processing",dict[[name]]))
  if (dict[[name]]==ALL){
    print("Reading in the data files...")
    dict[[records]]=read_in_CMS_files()  # get original records from file or cache
    print("Analyzing records...")
    dict %>% analyze_records() # dict now has all the keys added to it (like IFR, odds, ncacm, ...)
    # everyone will start at root[[ALL]][[records]] dict with the 3 tables in it.
  }
  else
    # everyone's records starts out being initialized from the root.
    # need the records table and provider tables for the extraction to work before analyze
    dict[[records]]=root[[ALL]][[records]]
    dict[[provider_num]]=root[[ALL]][[provider_num]]

  # extract the records of this state, create the 3 tables, and put them in the root[state]
  dict %>% extract_records() %>% analyze_records()

}

read_in_CMS_files <- function(){
  df=root[[ALL]][[original_records]]   #
  if (!is.null(df))
    # no need to read in if already there
    return(df)
  df=data.frame()   # create empty container
  for (i in seq(startyear,endyear,1)) {
    tbl1 <- read.csv(paste0(file_prefix, i, file_suffix))
    # just interested in the key columns in the original .csv file
    tbl1 = tbl1[, columns_to_extract]
    # sort everything by date in column 1 which makes debugging a little easier
    # tbl1=tbl1[ order(tbl1[,1]),]
    df=rbind(df,tbl1) #  append the new table entries to the bottom
  }
  # set new column names for use in summarize inside of combine_weeks
  colnames(df)=c(week, provider_num, provider_state, cases, deaths, acm, beds)
  # save away the table permanently as "original records" so no need to re-read
  # if run with different param
  root[[ALL]][[original_records]]= df %>% mutate_at(vars(week), mdy)  # set date type for the date
}

# use this to limit records we are processing on this run
# if dict[[name]]==ALL, we want to remove the BAD records provider records
# otherwise we will filter out all other states and make sure dict[[records]] is set to the result
#
# Purpose: replace the records dict with the extracted records dict (either just this state or cleaned original records if ALL)
extract_records <- function(dict){
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

  # the filter_criteria function is where the filterin magic happens
  # different for ALL vs other states! So check there

  if (DEBUG) print("Entering extract_records")
  table1=dict[[provider_num]] # the provider table so we do analysis on the providers to determine which ones to remove
  table2=dict[[records]]  # all the records

  state_name=dict[[name]]  # the name of the state (or "ALL")

  # Apply the filtering criteria to the provider table to get the provider IDs to remove
  # from the database
  # state_name will be a specific state name or ALL and the function will do the right thing
  filtered_table1 <- filter_criteria(table1, state_name)
  # For case ALL, filtered_table1 returned should be a very SHORT list of providers which are problematic (under 1,000)
  # For all other cases, fitered_table1 will be a LONG list of providers in every state except the desired state


  # Get a list of ProviderNumbers meeting the criteria
  selected_provider_numbers <- filtered_table1$provider

  # Delete records from table2 where ProviderNumber matches
  table2 <- table2[!(table2$provider %in% selected_provider_numbers), ]
  dict[[records]]=table2       # update the records to be the extracted records. Now we'll be ready to analyze
  dict   # return the dictionary. We can now use this to derive our three summary dataframes for weeks, provider, state
  }

# Define the filtering criteria for when a record will be removed
# called by extract_records
# this receives a df created by analysis  NOT the dict
# the df passed in is always the PROVIDER dataframe with 11 columns
#
# the references here are the hard coded column names in this dataframe
# This function is used ONLY to determine the provider numbers
# which should be removed based on the overall stats for that provider.
# we need to add state as a column in the provider df here for this to work properly

# this function gets called ONCE each time you call main
# if this is re-run, the database you are starting with has already had the bad QA
# records already tossed. So if you try to run the removal criteria again, you'll get 0 records
# for the second criteria. So solution is to use the initial criteria if state isn't specified,
# and use the other criteria when it is


filter_criteria <- function(df, state_name) {
  if (state_name == ALL){
    # filter will return the records that match the criteria
    # then we will take these records (which are "bad") and remove them
    # based on their provider ID
    # so we remove providers who supply nonsensical data by looking at the provider tab of ALL
    df %>% filter(ifr > MAX_IFR | deaths > MAX_DEATHS | cases > MAX_CASES | cases <MIN_CASES |
                    deaths < MIN_DEATHS | ncacm< MIN_NCACM | ncacm > MAX_NCACM | acm > MAX_ACM
                  )
          # more complex conditions can be added like:   (cases>100 & deaths<MIN_DEATHS)
  }
  else {
    # We are analyzing by an individual state.
    # So select the records which do NOT match the state name so they can be removed
    # so this will be a big list of all states we don't want, e.g., everything but CA for the CA pass
    # note that state will be interpreted as a literal field name to match the column
    df %>% filter(state != state_name)
  }
}

# analyze_records takes the original dataframe and creates 3 output summary dataframes
# specified in columns_of_interest: state, provider_num, week
# want to analyze by state, provider_num, week
analyze_records <- function(dict){

  if (DEBUG) print("start of analyze records")

  df=dict[[records]]  # get the df containing the FULL database

  for (col_name in columns_of_interest){
    # create one new df at a time with names: "week", "provider", "state"
    # always started with the original_records full dataframe when doing combine_by
    dict[[col_name]]= df %>% combine_by(col_name) %>% calc_stats(col_name)
    # add this result to our list of dataframes
  }
  return(dict)
}


# called by analyze_records
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

  # generate the 3 columns added to every df type
  if (col_name == provider_num )
    # for generating the provider tab, include the state of the provider as well as beds
    df=df %>% group_by(!!field_symbol) %>%
       reframe(cases = sum(cases,na.rm=TRUE),
            deaths = sum(deaths, na.rm=TRUE),
            acm = sum(acm, na.rm=TRUE),
            state=head(state,1),  # we can take any item since they are the same so take the first
            beds=head(beds,1)    # ditto
            )
    else
    df=df %>% group_by(!!field_symbol) %>%
       reframe(cases = sum(cases,na.rm=TRUE),
            deaths = sum(deaths, na.rm=TRUE),
            acm = sum(acm, na.rm=TRUE)
            )
  # next line not needed; analyze records will do this at the end
  # dict[[col_name]]=df        # save it away in the dict for that state
  return(df)   # return the derived sheet (either weeks, provider, state)
}


if (sum(case_weights)!=1)
  case_weights=c(case_weights, 1-sum(case_weights))  # add final value which could be negative!

mylag <- function(cases){
  cases_out=0
  for (i in seq(1,length(case_weights)))
    cases_out=cases_out+case_weights[i]*lag(cases, i-1)
  return(cases_out)
}

# calc_stats will calculate the columns to append to the current df and append them
# This is called by analyze_records after we've combined the records and created the aggregated outputs

# add new computed columns (so long as computed from values in same row it's easy)
# input has week, cases, deaths columns (columns of interest)
# add 4 new computed columns: ncacm, ifr, odds, OR, RR, and ARR
# key_row_df has the elements we need to compute the stats and is passed in
# be sure to order these so if newest columns need older columns, they are there


calc_stats <- function (df, col_name){
  # precalc the IFR and odds of the reference row for the week
  ref_cases=mylag(df$cases)[reference_row_num]
  ref_deaths=df$deaths[reference_row_num]
  ifr_ref= ref_deaths/ref_cases
  odds_ref =ref_deaths/(ref_cases-ref_deaths)

  # now we can use this for calculating AAR and odds_ratio

  if (col_name==week) {
    df=df %>% mutate(ncacm = acm-deaths) %>%
     mutate(ifr = deaths/mylag(cases)) %>%
     mutate(odds = deaths/(mylag(cases)-deaths)) %>%
     mutate(odds_ratio=odds/odds_ref) %>% # OR
     mutate(rr =   ifr/ifr_ref) %>% # RR
     mutate(arr = ifr_ref-ifr) # ARR... note the reference is first
  } else {  # since rows not weeks, lag no longer makes sense, nor does OR or ARR if not week analysis
    df=df %>% mutate(ncacm = acm-deaths) %>%
      mutate(ifr = deaths/cases) %>%
      mutate(odds = deaths/(cases-deaths))
    # If col_name is provider, add IFR calc around window both before and after reference
    if (col_name==provider_num) {
      # do two combine_by calls, one for the weeks before, the other for the weeks after
      # so what we did before, except limit the range of cells
      # use IFR_CALC_WINDOW
      # first cheat and get the record dataframe (not the original) (ordf)
      ordf=root[[ALL]][[records]]
      start_row=reference_row_num-IFR_CALC_WINDOW
      end_row=reference_row_num-1
      start2_row = reference_row_num+1
      end2_row=reference_row_num+IFR_CALC_WINDOW
      df_new=ordf %>% group_by(provider) %>%
       reframe(cases_before = sum(cases[start_row:end_row],na.rm=TRUE),
               deaths_before =  sum(deaths[start_row:end_row],na.rm=TRUE),
               cases_after = sum(cases[start2_row:end2_row],na.rm=TRUE),
               deaths_after =  sum(deaths[start2_row:end2_row],na.rm=TRUE))

      # next remove the first column (provider ID) so we are left with 4 columns
      df_new=df_new[,-1]
      df=cbind(df, df_new)
    }
  }
  # returns the df we created
  return(df)
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



# https://cran.r-project.org/web/packages/openxlsx2/openxlsx2.pdf
DO_NOT_OUTPUT=c(original_records, records, name)
save_to_disk = function(){
  if (!SAVE_TO_DISK) return()
  for (state in states_of_interest){
    filename=paste0(output_filename_prefix,state,".xlsx")
    dict=root[[state]]

    # output all keys in the dict except those in DO_NOT_OUTPUT
    # note that ALL state has more keys so will have more tabs
    # this is why we compute this on each iteration
    columns=keys(dict)
    columns=columns[!columns %in% DO_NOT_OUTPUT]

    wb <- wb_workbook()

    for (sheet_name in columns){
      wb$add_worksheet(sheet_name)
      # now add the dataframe to that sheet
      # avoid bug in openxlsx2
      if (is.null(dict[[sheet_name]]))
        next
      if (DEBUG) print(c("about to output", sheet_name))
      wb$add_data(x=dict[[sheet_name]])
    }
    wb$save(filename)
  }
}


summarize_columns_key="summary"   # hashmap key name for summary analysis stored in ALL
# the names of the columns to summarize. use !!sym(name) in the functions

# helper function for summarize columns
# this function extracts a given column from all dataframes
# and returns a dataframe of the extracted columns

extract_and_bind_columns <- function(dataframe_list, column_name) {
  extracted_columns <- lapply(dataframe_list, function(df) df[[column_name]])
  result_dataframe <- do.call(cbind, extracted_columns)
  colnames(result_dataframe) <- names(dataframe_list)
  return(result_dataframe)
}

# another helper function to create a named dataframe list (sorted)
create_dataframe_list <- function(hash_table){

  # Extract key-value pairs from the hash table
  # as.list(hash_table) doesn't work for hash tables
  key_value_pairs <- to_named_list(hash_table)

  # Sort key-value pairs by key
  sorted_pairs <- key_value_pairs[order(names(key_value_pairs))]

  # Create a named list from sorted pairs
  sorted_named_list <- setNames(sorted_pairs, names(sorted_pairs))
}

# another helper function to convert hashmap to
# a named list of the week dataframe in each hashmap key
to_named_list <- function(hashmap) {
  list <- list()

  for (key in keys(hashmap)) {
    list[[key]] <- hashmap[[key]][[week]]
  }

  names(list) <- keys(hashmap)

  list
}



# summarize_columns is called by main()
# it creates a dataframe with columns: week ALL AL AK ... <state names>
# and a row for each week
# the value is the odds_ratio, arr, or whatever else we are exracting
# basically one extracted statistic per dataframe created
#
# the key (sheet name) is the statistic being extracted
# the result is put in the root[[ALL]] key
# so there are two new keys added: odds_ratio, arr
# this allows scatterplot of states by date showing what is going on
summarize_columns=function(){
  # extract week column to get weeks
  # extract the columns to summarize to add keys to the ALL key in root

  # extract the week column from the ALL week dataframe once
  week_column = root[[ALL]][[week]][week]

  # get list of all the "week" dataframes in root as a named list of
  # dataframes in sorted order
  dataframe_list=create_dataframe_list(root)
  # we summarize the  odds_ratio and arr for all states
  # with a column for each state
  # rows are weeks, columns are states. there is one df for odds_ratio, another for ARR
  for (col_to_summarize in columns_to_summarize) {
    # extract out the column of interest of this iteration
    df=extract_and_bind_columns(dataframe_list, col_to_summarize)

    # concatenate the two dataframes, and then apply any limits
    result_df=cbind(week_column, df) %>% clean_up_dataframe(columns_to_summarize_limits[[col_to_summarize]])

    # save it in root under ALL
    root[[ALL]][[col_to_summarize]]=result_df
  }
}
# given a dataframe, limit the values to a range but don't apply to
# the named column
clean_up_dataframe <- function(df, limits_list, ignore_column=week){
  min_value=limits_list[[1]]
  max_value=limits_list[[2]]
  df %>%
    mutate(across(-all_of(ignore_column), ~ pmin(pmax(.x, min_value), max_value)))
}

# run
# call main() each time to run.
# type root=hashmap() to clear everything to force
# everything to run from scratch (if you change the QA criteria for providers)

main()

