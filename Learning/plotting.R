library(ggplot2)
library(dplyr)
library(lubridate)

generic_plot <- function(data, x_col, y_col1, y_col2 = NULL) {
  data <- data %>%
    mutate(Quarter = quarter(!!sym(x_col)),
           Month = month(!!sym(x_col)))

  gg <- ggplot(data, aes(x = !!sym(x_col))) +
    labs(title = "Generic Plot",
         x = x_col,
         y = "Values")

  if (is.null(y_col2)) {
    gg <- gg +
      geom_line(aes(y = !!sym(y_col1)), color = "blue") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
  } else {
    gg <- gg +
      geom_line(aes(y = !!sym(y_col1)), color = "blue") +
      geom_line(aes(y = !!sym(y_col2)), color = "red") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      scale_y_continuous(
        sec.axis = sec_axis(trans = ~ ., name = y_col2),
        breaks = data$Quarter[data$Month == 12]
      )
  }

  return(gg)
}

# Sample dataframe
df <- data.frame(
  Date = seq.Date(from = as.Date("2020-01-01"), by = "week", length.out = 200),
  Value1 = seq(1,200,1),
  Value2 = seq(1001,1200,1)
)

# Call the generic plotting function with one column
plot1 <- generic_plot(df, x_col = "Date", y_col1 = "Value1")
print(plot1)

# Call the generic plotting function with two columns
plot2 <- generic_plot(df, x_col = "Date", y_col1 = "Value1", y_col2 = "Value2")
print(plot2)
