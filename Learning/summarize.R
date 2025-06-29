dataframes <- list(
  data.frame(A = 1:50, B = 51:100, C = 101:150),
  data.frame(A = 2:51, B = 52:101, C = 102:151)
  # ... and so on for other dataframes
)
# Custom function to summarize dataframes
summarize_dataframes <- function(dataframes, extraction_specs) {
  num_fields <- sum(sapply(extraction_specs, function(spec) length(spec[[1]])))
  summary_df <- data.frame(matrix(nrow = length(dataframes), ncol = num_fields))

  for (i in seq_along(dataframes)) {
    dataframe <- dataframes[[i]]
    summary_row <- numeric()
    col_counter <- 1

    for (spec in extraction_specs) {
      fields <- spec[[1]]
      rows <- spec[[2]]

      values <- dataframe[rows, fields]
      num_values <- length(unlist(values))
      summary_df[i, col_counter:(col_counter + num_values - 1)] <- unlist(values)

      col_counter <- col_counter + num_values
    }
  }

  colnames(summary_df) <- unlist(sapply(extraction_specs, "[[", 1))
  return(summary_df)
}

# Define extraction specifications
extraction_specs <- list(
  list(c("A", "B"), 32),
  list("C", seq(15, 30))
)

# Summarize dataframes using the function
summary_dataframe <- summarize_dataframes(dataframes, extraction_specs)

# Print the summary dataframe
print(summary_dataframe)
