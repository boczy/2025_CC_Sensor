library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# Define the file path
file_path <- "C:/Users/Chris/OneDrive - University of Georgia/Trials/2024/GA Cotton Commission 2024/Data/2024 sensor last data/CC WU Early/Hourly/Cotton Commission Watkinsville Early_Hourly.dat"

# Read the file, skipping the first row
raw_data <- read_csv(file_path, skip = 1)

# Extract the units row
units_row <- raw_data[1, ]

# Remove the units row from the data
cleaned_data <- raw_data[-1,]

# Combine column names and units
new_colnames <- paste0(names(cleaned_data), " (", unlist(units_row), ")")
names(cleaned_data) <- new_colnames

# Remove unwanted columns
cleaned_data <- cleaned_data %>%
  select(-`RECORD (RN)`, -`BattV_Avg (Volts)`, -`PTemp_C_Avg (Deg C)`)

# Remove duplicate/missing data, convert column names to lower-case, replace spaces with underscores
cleaned_data <- cleaned_data %>%
  na.omit() %>%  # Remove rows with missing values
  distinct() %>%  # Remove duplicate rows
  rename_all(tolower) %>%  # Convert column names to lowercase
  rename_all(~gsub(" ", "_", .))  # Replace spaces with underscores

# Convert the original timestamp column to date-time format and rename it
cleaned_data <- cleaned_data %>%
  mutate(timestamp = ymd_hms(`timestamp_(ts)`)) %>%
  select(-`timestamp_(ts)`, everything())

# Filter the dataset to remove observations after November 11, 2024
cleaned_data <- cleaned_data %>%
  filter(timestamp <= as.POSIXct("2024-11-11 23:59:59"))

# Tidy data: make sensor depth and cover-crop treatment into variables
tidy_data <- cleaned_data %>%
  pivot_longer(
    cols = -timestamp,
    names_to = c("measure", "treatment", "depth", "stat", "units"),
    names_sep = "_"
  ) %>%
  mutate(
    measure_units = case_when(
      measure == "t" & units == "(deg" ~ "t_(deg_c)",
      TRUE ~ paste0(measure, "_", units)
    ),
    value = as.numeric(value)  # Convert value column to numeric
  ) %>%
  select(-measure, -units) %>%
  filter(!is.na(measure_units))  # Remove rows where measure_units is NA

# Pivot the data so that measure_units becomes columns
wide_data <- tidy_data %>%
  pivot_wider(names_from = measure_units, values_from = value)

# Data table for 'avg'
avg_data <- wide_data %>%
  filter(stat == "avg")

# Data table for 'min'
min_data <- wide_data %>%
  filter(stat == "min")

# Data table for 'max'
max_data <- wide_data %>%
  filter(stat == "max")

#Write CSV files
# Define the folder path
folder_path <- "C:/Users/Chris/OneDrive - University of Georgia/Trials/2024/GA Cotton Commission 2024/Data/2024 sensor last data/CC WU Early/Hourly/"

# Define the output file paths and names for each object
file_path_tidy <- file.path(folder_path, "tidy_data_CC_WU_Early_Hourly_cleaned.csv")
file_path_avg <- file.path(folder_path, "AVG_CC_WU_Early_Hourly_cleaned.csv")
file_path_max <- file.path(folder_path, "Max_CC_WU_Early_Hourly_cleaned.csv")
file_path_min <- file.path(folder_path, "Min_CC_WU_Early_Hourly_cleaned.csv")

# Save the wide_data object
write_csv(wide_data, file_path_tidy)
cat("wide_data has been saved to", file_path_tidy, "\n")

# Save the avg_data object
write_csv(avg_data, file_path_avg)
cat("avg_data has been saved to", file_path_avg, "\n")

# Save the max_data object
write_csv(max_data, file_path_max)
cat("max_data has been saved to", file_path_max, "\n")

# Save the min_data object
write_csv(min_data, file_path_min)
cat("min_data has been saved to", file_path_min, "\n")
