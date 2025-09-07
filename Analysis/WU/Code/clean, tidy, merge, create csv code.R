library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)

# Define the file path
file_path <- "C:/Users/Chris/OneDrive - University of Georgia/Trials/2024/GA Cotton Commission 2024/Data/2024 sensor last data/CC WU Early/Daily/Cotton Commission Watkinsville Early_Daily.dat"

# Define the folder path
folder_path <- "C:/Users/Chris/OneDrive - University of Georgia/Trials/2024/GA Cotton Commission 2024/Data/2024 sensor last data/CC WU Late/Hourly/"

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
  select(-`RECORD (RN)`, -`BattV_Min (Volts)`, -`PTemp_C_Avg (Deg C)`)

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

# Add the plant_time column with the value "late" to the wide_data tibble
wide_data <- wide_data %>%
  mutate(plant_time = "early")

# Add the measurement_freq column with the value "daily" to the wide_data tibble
wide_data <- wide_data %>%
  mutate(measurement_freq = "daily")

#create a unique object with a descriptive name for the resulting data
early_daily_data <- wide_data

# Use the above code to create objects for all 4 raw data file from WU sensors, it would be good to set that up to happen programmatically  

# Combine the datasets
wu_sensor_data_2024 <- bind_rows(
  early_hourly_data,
  early_daily_data,
  late_hourly_data,
  late_daily_data
)

# Remove the timestamp_NA column
wu_sensor_data_2024 <- wu_sensor_data_2024 %>%
  select(-timestamp_NA)

# Display the structure of the resulting dataset
str(wu_sensor_data_2024)


#Save the resulting merged dataset
# Define the folder path
folder_path <- "C:/Users/Chris/OneDrive - University of Georgia/Trials/2024/GA Cotton Commission 2024/Data/2024 sensor last data/CC WU Late/Hourly/"

# Define the file path
file_path <- paste0(folder_path, "wu_sensor_data_2024.csv")

# Save the dataset to the specified file path
write_csv(wu_sensor_data_2024, file_path)

# Confirm the file has been saved
print(paste("Dataset saved to:", file_path))

