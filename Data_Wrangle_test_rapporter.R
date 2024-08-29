# Load required libraries
library(dplyr)
library(lubridate)

# Load the data
Test_data_AP <- read.csv("C:/Users/nilsa/OneDrive/Hammarby/Test_Data_Hammarby/Test_data_AP.csv")

# Function to convert datetime format to seconds
convert_to_seconds <- function(time_str) {
  if (is.na(time_str) || time_str == "") {
    return(NA)
  }
  
  tryCatch({
    if (grepl("^1899-12-31", time_str)) {
      # Parse the datetime string
      dt <- parse_date_time(time_str, "ymd HMS")
      # Extract hours, minutes, and seconds
      hours <- hour(dt)
      minutes <- minute(dt)
      seconds <- second(dt)
      # Calculate total seconds
      total_seconds <- hours * 3600 + minutes * 60 + seconds
    } else {
      warning(paste("Unexpected time format:", time_str))
      return(NA)
    }
    
    return(total_seconds)
  }, error = function(e) {
    warning(paste("Unable to convert time:", time_str))
    return(NA)
  })
}

# Convert Cooper times to seconds and add test date
Test_data_AP <- Test_data_AP %>%
  mutate(
    Cooper_Seconds = sapply(Cooper, convert_to_seconds),
    Test_Date = as.Date("2024-08-17")  # Add the test date
  )

# Print summary of original Cooper column and new Cooper_Seconds column
cat("\nSummary of original Cooper column:\n")
print(summary(Test_data_AP$Cooper))

cat("\nSummary of new Cooper_Seconds column:\n")
print(summary(Test_data_AP$Cooper_Seconds))

# Print rows where Cooper_Seconds is NA
cat("\nRows where Cooper_Seconds is NA:\n")
print(Test_data_AP[is.na(Test_data_AP$Cooper_Seconds), c("Namn", "Cooper", "Cooper_Seconds", "Test_Date")])

# Save the updated data
write.csv(Test_data_AP, "C:/Users/nilsa/OneDrive/Hammarby/Test_Data_Hammarby/Test_data_AP_seconds.csv", row.names = FALSE)

cat("\nUpdated data has been saved in 'Test_data_AP_seconds.csv'")

# Print a few rows to verify conversion and new date column
cat("\nSample of converted data with test date:\n")
print(head(Test_data_AP[, c("Namn", "Cooper", "Cooper_Seconds", "Test_Date")]))

