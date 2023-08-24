# not for execution, but just code snippets

colnames(temp) <- c('year', 'month', str_replace_all(str_to_lower(s), ' ', '_'))

# Sample data frame
data <- data.frame(
  x = c(1, 2, 3, 4),
  y = c(5, 6, 7, 8)
)

# Using 'dplyr' to calculate the sum of columns 'x' and 'y'
# this will add a new column called sum_result (hence mutate) to the existing data structure computed
# from the existing columns. This is very powerful!!
# mutate will use first arg (piped df) as the context to eval the expression
data_with_sum <- data %>%
  mutate(sum_result = x + y)

# using with... it means evaluate "sum(x+y)" by taking x and y from the
# data frame column of that name (must be lowercase)
total_sum <- with(data, sum(x + y))
# this returns 36 but if we wanted the vector version with each element
vector_sum <- with(data, x + y)

poptotal <- poptotal %>%
        filter(year >= 2000) %>%
        #filter(year < 2015 | month <= 11) %>%
        with(ts(res_pop, start=c(2000,1), frequency = 12))
    #poptotal <- ts(select(filter(poptotal, year >= 2000), "res_pop"),
    #               start=c(2000,1),
    #               freqency=12)

    ## normalize gun sales by population
    totalSeasPop <- totalSeas / poptotal * 1000
    totalSeasScaled <- totalSeas / 280726

    ## create a new data frame that eventually stores all the
    ## data we need in the final piece
    out_data <- ts_to_dataframe(total, 'guns_total') %>%
        mutate(guns_total=round(guns_total, 3))

    ## expand the data.frame, adding more volumns
    out_data <- data.frame(out_data,
                           guns_total_seas=as.matrix(totalSeas),
                           guns_total_per_1000=round(as.matrix(totalSeasPop), digits=3),
                           guns_total_per_1000_scaled=round(as.matrix(totalSeasScaled), digits=3))
    if (debug) {
        print(head(out_data))
        print(tail(out_data))
    }

    ## create a temporary matrix for computing the
    ## handgun_share and longgun_share columns
    ## cbind works correctly here as it operates on timeseries object
    tmp <- cbind(final(seas(state_ts(alldata, 'Totals', 'handgun'))),
                 final(seas(state_ts(alldata, 'Totals', 'longgun'))),
                 final(seas(state_ts(alldata, 'Totals', 'other'))),
                 final(seas(state_ts(alldata, 'Totals', 'multiple_corrected'))))
    colnames(tmp) <- c('handgun', 'longgun', 'other', 'multiple')
    out_data <- data.frame(out_data, tmp)

    ## convert NAs to 0 in column other
    out_data$other[is.na(out_data$other)] <- 0

    ## compute the handgun/longgun share
    out_data <- within(out_data, {
        handgun_share=round(handgun / (handgun+longgun+other+multiple*0.5), 4)
        longgun_share=round(longgun / (handgun+longgun+other+multiple*0.5), 4)
        })

    ## plot percent of national for selected states
    show_states <- c('New Jersey', 'Maryland', 'Georgia',
                     'Louisiana', 'Mississippi', 'Missouri')

    for (s in show_states) {
        s.ts <- state_data(alldata, s, total, totalSeas)

        ## merge with out_data
        temp <- mutate(ts_to_dataframe(s.ts), value=round(value,3))
        colnames(temp) <- c('year', 'month', gsub(' ', '_', tolower(s)))
        out_data <- data.frame(out_data, temp[,3,drop=FALSE])
    }
    if (debug) {
        print(head(out_data))
        print(tail(out_data))
