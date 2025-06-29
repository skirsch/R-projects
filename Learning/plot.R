
library(ggplot2)
library(rlang)

# try doing as if ... and hard code each additino

plot_multi_line <- function(df, x_col, y_cols, mytitle="My graph", ytitle="Number") {
  myplot=ggplot(df, aes(x = .data[[x_col]]))
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
    scale_color_manual(values = c("blue", "red"))+      # ,"green","orange","magenta"))
    scale_x_continuous(n.breaks=10, minor_breaks=seq(1,30)) +
    theme_minimal()
  print(myplot)
  df
}


plot_multi_line2 <- function(df, x_col, y_cols) {
  ggplot(df, aes(x = .data[[x_col]])) +
    geom_line(aes(y = .data[[y_cols[1]]], color = y_cols[1]), linewidth = 1.5) +
    geom_line(aes(y = .data[[y_cols[2]]], color = y_cols[2]), linewidth = 1.5) +
    labs(title = "Multi-Line Graph",
         x = x_col,
         y = "Values",
         color = "Series") +  # Custom legend label
    scale_color_manual(values = c("blue", "red")) +
    theme_minimal()
}

plot_multi_line3 <- function(df, x_col, y_cols) {
  x=ggplot(df, aes(x = .data[[x_col]]))
    x=x+  geom_line(aes(y = .data[[y_cols[1]]], color = y_cols[1]), linewidth = 1.5)
    x=x+geom_line(aes(y = .data[[y_cols[2]]], color = y_cols[2]), linewidth = 1.5)
    x=x+
    labs(title = "Multi-Line Graph3",
         x = x_col,
         y = "Values",
         color = "Series") +  # Custom legend label
    # scale_color_manual(values = c("blue", "red")) +
    theme_minimal()
    x
}
plot_multi_line4 <- function(df, x_col, y_cols) {
    x=ggplot(df, aes(x = .data[[x_col]]))
    for (i in range(1,2))
      x=x+  geom_line(aes(y = .data[[y_cols[i]]], color = y_cols[i]), linewidth = i)
    x=x+
    labs(title = "Multi-Line Graph4",
         x = x_col,
         y = "Values",
         color = "Series") +  # Custom legend label
    # scale_color_manual(values = c("blue", "red")) +
    theme_minimal()
    x
}

# Sample dataframe
df <- data.frame(
  Time = 1:10,
  Y1 = c(5, 8, 12, 18, 22, 25, 30, 35, 38, 40),
  Y2 = c(15, 18, 22, 28, 32, 35, 40, 45, 48, 50)
)

# Call the custom plotting function
plot_multi_line(df, x_col = "Time", y_cols = c("Y1", "Y2"))
plot_multi_line2(df, x_col = "Time", y_cols = c("Y1", "Y2"))
plot_multi_line3(df, x_col = "Time", y_cols = c("Y1", "Y2"))
plot_multi_line4(df, x_col = "Time", y_cols = c("Y1", "Y2"))
