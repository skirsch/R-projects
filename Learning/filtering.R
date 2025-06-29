# filtering


filter_out_junk <- function(table1, table2){
  # applies criteria to df1 to determine which providers to remove from df2

  # Apply the filtering criteria to the first table
  filtered_table1 <- filter_criteria(table1)

  # Get a list of ProviderNumbers meeting the criteria
  selected_provider_numbers <- filtered_table1$provider

  # Delete records from table2 where ProviderNumber matches
  table2 <- table2[!(table2$provider %in% selected_provider_numbers), ]

  }

# Define the filtering criteria
filter_criteria <- function(df) {
  df %>%
    filter(Field1 < 0 | Field1 > 10 | Field2 > 20 | Field2 < 10)
}

