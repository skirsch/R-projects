# not for execution, but just code snippets

getwd()
setwd(path)

library(readxl) # to allow read






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
    }


 https://dplyr.tidyverse.org/reference/summarise.html

    Other single table verbs: arrange(), filter(), mutate(), reframe(), rename(), select(), slice()

Examples
# A summary applied to ungrouped tbl returns a single row
mtcars %>%
  summarise(mean = mean(disp), n = n())
#>       mean  n
#> 1 230.7219 32

# Usually, you'll want to group first
mtcars %>%
  group_by(cyl) %>%
  summarise(mean = mean(disp), n = n())
#> # A tibble: 3 × 3
#>     cyl  mean     n
#>   <dbl> <dbl> <int>
#> 1     4  105.    11
#> 2     6  183.     7
#> 3     8  353.    14

# Each summary call removes one grouping level (since that group
# is now just a single row)
mtcars %>%
  group_by(cyl, vs) %>%
  summarise(cyl_n = n()) %>%
  group_vars()
#> `summarise()` has grouped output by 'cyl'. You can override using the
#> `.groups` argument.
#> [1] "cyl"

# BEWARE: reusing variables may lead to unexpected results
mtcars %>%
  group_by(cyl) %>%
  summarise(disp = mean(disp), sd = sd(disp))
#> # A tibble: 3 × 3
#>     cyl  disp    sd
#>   <dbl> <dbl> <dbl>
#> 1     4  105.    NA
#> 2     6  183.    NA
#> 3     8  353.    NA

# Refer to column names stored as strings with the `.data` pronoun:
var <- "mass"
summarise(starwars, avg = mean(.data[[var]], na.rm = TRUE))
#> # A tibble: 1 × 1
#>     avg
#>   <dbl>
#> 1  97.3
# Learn more in ?rlang::args_data_masking

# In dplyr 1.1.0, returning multiple rows per group was deprecated in favor
# of `reframe()`, which never messages and always returns an ungrouped
# result:
mtcars %>%
   group_by(cyl) %>%
   summarise(qs = quantile(disp, c(0.25, 0.75)), prob = c(0.25, 0.75))
#> Warning: Returning more (or less) than 1 row per `summarise()` group was
#> deprecated in dplyr 1.1.0.
#> ℹ Please use `reframe()` instead.
#> ℹ When switching from `summarise()` to `reframe()`, remember that
#>   `reframe()` always returns an ungrouped data frame and adjust
#>   accordingly.
#> `summarise()` has grouped output by 'cyl'. You can override using the
#> `.groups` argument.
#> # A tibble: 6 × 3
#> # Groups:   cyl [3]
#>     cyl    qs  prob
#>   <dbl> <dbl> <dbl>
#> 1     4  78.8  0.25
#> 2     4 121.   0.75
#> 3     6 160    0.25
#> 4     6 196.   0.75
#> 5     8 302.   0.25
#> 6     8 390    0.75
# ->
mtcars %>%
   group_by(cyl) %>%
   reframe(qs = quantile(disp, c(0.25, 0.75)), prob = c(0.25, 0.75))
#> # A tibble: 6 × 3
#>     cyl    qs  prob
#>   <dbl> <dbl> <dbl>
#> 1     4  78.8  0.25
#> 2     4 121.   0.75
#> 3     6 160    0.25
#> 4     6 196.   0.75
#> 5     8 302.   0.25
#> 6     8 390    0.75
