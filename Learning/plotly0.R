library(plotly)
# from chatgpt
plotly_two_line_plot <- function(data, x_col, y_col1, y_col2, same_y_axis = TRUE) {
  p <- plot_ly(data, x = ~get(x_col)) %>%
    layout(
      title = "Two-Line Line Plot",
      xaxis = list(title = x_col, minorgridcount = 4, minorgrid=TRUE)
    )

  if (same_y_axis) {
    p <- p %>%
      add_trace(y = ~get(y_col1), type = "scatter", mode = "lines", name = y_col1) %>%
      add_trace(y = ~get(y_col2), type = "scatter", mode = "lines", name = y_col2, yaxis = "y1")
  } else {
    p <- p %>%
      add_trace(y = ~get(y_col1), type = "scatter", mode = "lines", name = y_col1, yaxis = "y2") %>%
      add_trace(y = ~get(y_col2), type = "scatter", mode = "lines", name = y_col2, yaxis = "y3")

    p <- p %>%
      layout(
        yaxis2 = list(title = y_col1),
        yaxis3 = list(title = y_col2, overlaying = "y2", side = "right")
      )
  }

  return(p)
}

# Sample dataframe
df <- data.frame(
  Date = seq.Date(from = as.Date("2020-01-01"), by = "week", length.out = 200),
  Value1 = seq(1,200,1),
  Value2 = seq(200,1,-1)
)

# Call the function to create the plot with same y-axis
plot_same_y <- plotly_two_line_plot(df, x_col = "Date", y_col1 = "Value1", y_col2 = "Value2", same_y_axis = TRUE)
print(plot_same_y)

# Call the function to create the plot with separate y-axes
plot_separate_y <- plotly_two_line_plot(df, x_col = "Date", y_col1 = "Value1", y_col2 = "Value2", same_y_axis = FALSE)
print(plot_separate_y)

