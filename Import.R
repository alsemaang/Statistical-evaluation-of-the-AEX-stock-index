# Mathematical Statistics I: AEX project

# 1. Identify the available data files
list.files("data", recursive = TRUE)

# Store the path to the dataset
data_path <- "data/AEX_1995_2024.csv"

# Inspect the first six lines before importing
readLines(data_path, n = 6)

# Import the CSV file
aex <- read.csv(data_path, stringsAsFactors = FALSE)

# Convert the date column from text to Date
aex$Date <- as.Date(aex$Date, format = "%Y-%m-%d")

# Sort observations from oldest to newest
aex <- aex[order(aex$Date), ]

# Check column types and number of observations
str(aex)

# Check the date range
range(aex$Date, na.rm = TRUE)

# Count missing values in each column
colSums(is.na(aex))

# Count duplicated dates
sum(duplicated(aex$Date))
