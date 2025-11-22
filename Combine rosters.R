# --- Set Working Directory ---
# Set the path to the directory where your files are located.
# R uses forward slashes '/' instead of backslashes '\' for paths.
setwd("C:/Users/james.reinhardt/Documents/IPEDS BASEBALL/NCAABaseballMap/app2")

# --- Load Required Libraries ---

# 'readr' is for reading and writing CSV files efficiently.
# 'dplyr' is for data manipulation and provides the 'bind_rows()' function.

# Install packages if you don't have them (uncomment the lines below)
# install.packages("readr")
# install.packages("dplyr")

library(readr)
library(dplyr)

# --- Specify File Names ---

# A vector containing the names of the files you want to read.
# This makes it easy to add or remove files later.
file_names <- c(
  "ncaa_all_rosters_2022.csv",
  "ncaa_all_rosters_2023.csv",
  "ncaa_all_rosters_2024.csv",
  "ncaa_all_rosters_2025.csv"
)

# --- Read and Combine Files ---

cat("Starting to read and combine files...\n")

# We can use 'lapply' to read all files into a list of data frames.
# 'read_csv' from the 'readr' package is generally faster than base R 'read.csv'.
list_of_data_frames <- lapply(file_names, read_csv)

# 'bind_rows()' from 'dplyr' stacks the data frames on top of each other.
# It's flexible and will handle cases where columns are not in the
# exact same order (it matches them by name).
# It will also add 'NA' for columns present in one file but not another.
combined_data <- bind_rows(list_of_data_frames)

cat("Files successfully combined.\n")
cat("Total rows in combined data:", nrow(combined_data), "\n")
cat("Total columns in combined data:", ncol(combined_data), "\n")

# --- Write the Combined Data to a New CSV ---

output_file_name <- "combined_ncaa_rosters.csv"

# 'write_csv' from 'readr' writes the new file.
# 'na = ""' writes empty strings for missing values instead of "NA".
write_csv(combined_data, output_file_name, na = "")

cat("Successfully wrote combined data to:", output_file_name, "\n")