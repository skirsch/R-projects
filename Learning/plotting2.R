library(ggplot2)
library(dplyr)
library(lubridate)

generic_plot <- function(data, x_col, y_col1, y_col2 = NULL) {
  data <- data %>%
    mutate(Quarter = quarter(!!sym(x_col)),
           YearQuarter = paste("Q", Quarter, "-", year(!!sym(x_col))))

  gg <- ggplot(data, aes(x = !!sym(x_col))) +
    labs(title = "Generic Plot",
         x = "Year-Quarter",
         y = "Values")

  if (is.null(y_col2)) {
    gg <- gg +
      geom_line(aes_string(y = y_col1, color = y_col1)) +
      scale_x_date(date_labels = "%b-%y", date_breaks = "3 months")
  } else {
    gg <- gg +
      geom_line(aes_string(y = y_col1, color = y_col1)) +
      geom_line(aes_string(y = y_col2, color = y_col2)) +
      scale_x_date(date_labels = "%b-%y", date_breaks = "3 months") +
      scale_y_continuous(
        name = y_col1,
        sec.axis = sec_axis(trans = ~., name = y_col2, breaks = c(0, 50, 100))
      )
  }

  return(gg)
}

# Sample dataframe
df <- data.frame(
  Date = seq.Date(from = as.Date("2020-01-01"), by = "week", length.out = 208),
  Value1 = c(5, 10, 15, rep(20, 205)),
  Value2 = c(600, 95, 90, rep(333, 205))
)

# Call the generic plotting function with one column
plot1 <- generic_plot(df, x_col = "Date", y_col1 = "Value1")
print(plot1)

# Call the generic plotting function with two columns
plot2 <- generic_plot(df, x_col = "Date", y_col1 = "Value1", y_col2 = "Value2")
print(plot2)
