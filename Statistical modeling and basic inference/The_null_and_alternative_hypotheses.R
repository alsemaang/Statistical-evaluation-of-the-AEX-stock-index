# Mathematical Statistics I: AEX project

# 1. Identify the available data files
list.files("data", recursive = TRUE)

# Store the path to the dataset
data_path <- "R project/AEX_1995_2024.csv"

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

# Exercise 2k
# Daily returns
r <- 100 * (aex$Close - aex$Open) / aex$Open

# daily returns in 2005 and 2020
r2005 <- r[format(aex$Date, "%Y") == "2005"]
r2020 <- r[format(aex$Date, "%Y") == "2020"]

# significance level 
alpha <- 0.05

#calculate t-test
t0 <- function(x, mu0 = 0) {
  (mean(x)-mu0) / (sd(x) / sqrt(length(x)))
}

#Test statistic for 2005 and 2020
t2005 <- t0(r2005, mu0 = 0)
t2020 <- t0(r2020, mu0 = 0)

print(t2005)
print(t2020)



#build-in t-test
test2005 <- t.test(r2005, mu = 0)
test2020 <- t.test(r2020, mu = 0)


results2005 <- data.frame(
  Year = c(2005),
  N = c(length(r2005)),
  Mean = c(mean(r2005)),
  SD = c(sd(r2005)),
  #Sample standard deviation
  T_statistic = c(t2005),
  P_value = c(test2005$p.value),
  CI_lower = c(test2005$conf.int[1]),
  CI_upper = c(test2005$conf.int[2]),
  #confidence interval: [CI_lower, CI_upper]
  Decision = c(
    ifelse(test2005$p.value < alpha,
           "Reject H0", "Do not reject H0")
  )
)


results2020 <- data.frame(
  Year = c(2020),
  N = c(length(r2020)),
  Mean = c(mean(r2020)),
  SD = c(sd(r2020)),
  T_statistic = c(t2020),
  P_value = c(test2020$p.value),
  CI_lower = c(test2020$conf.int[1]),
  CI_upper = c(test2020$conf.int[2]),
  Decision = c(
    ifelse(test2020$p.value < alpha,
           "Reject H0", "Do not reject H0")
  )
)

print(results2005)
print(results2020)
