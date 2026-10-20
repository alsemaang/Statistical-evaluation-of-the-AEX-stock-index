# Mathematical Statistics I: AEX project
# Exploratory data analysis

# Import and check the data using our existing script
source("Import.R")

# Question 1(a): opening and closing prices over time
plot(
  aex$Date,
  aex$Open,
  type = "l",
  col = "steelblue",
  lwd = 1,
  ylim = range(c(aex$Open, aex$Close)),
  xlab = "Year",
  ylab = "AEX index value",
  main = "AEX opening and closing prices, 1995–2024"
)

# Add closing prices to the same plot
lines(
  aex$Date,
  aex$Close,
  col = "darkorange",
  lwd = 1
)

# Identify the two lines
legend(
  "topleft",
  legend = c("Opening price", "Closing price"),
  col = c("steelblue", "darkorange"),
  lty = 1,
  lwd = 1,
  bty = "n"
)

# Create a folder for figures
dir.create("figures", showWarnings = FALSE)

# Open a PNG graphics device
png(
  "figures/aex_open_close.png",
  width = 2700,
  height = 1500,
  res = 300
)

# Plot opening prices
plot(
  aex$Date, aex$Open,
  type = "l",
  col = "steelblue",
  lwd = 1,
  ylim = range(c(aex$Open, aex$Close)),
  xlab = "Year",
  ylab = "AEX index value",
  main = "AEX opening and closing prices, 1995–2024"
)

# Add closing prices
lines(
  aex$Date, aex$Close,
  col = "darkorange",
  lwd = 1
)

# Add a legend
legend(
  "topleft",
  legend = c("Opening price", "Closing price"),
  col = c("steelblue", "darkorange"),
  lty = 1,
  lwd = 1,
  bty = "n"
)

# Finish writing the image
dev.off()


# Question 1(c): The daily returns

source("Import.R")
dir.create("figures", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)


# Calculate daily returns: (Close - Open) / Open
aex$Return <- (aex$Close - aex$Open) / aex$Open
r <- 100 * aex$Return  # Percentages for tables and figures


# Numerical summaries
return_summary <- data.frame(
  N = length(r),
  Mean = mean(r),
  Median = median(r),
  SD = sd(r),
  Minimum = min(r),
  Q1 = unname(quantile(r, 0.25)),
  Q3 = unname(quantile(r, 0.75)),
  Maximum = max(r),
  IQR = IQR(r)
)

# Returns are in %, SD and IQR are in percentage points
print(round(return_summary, 4))

write.csv(
  return_summary,
  "tables/aex_return_summary.csv",
  row.names = FALSE
)


# Plot daily returns over time
png("figures/aex_daily_returns.png",
    width = 2700, height = 1500, res = 300)

plot(
  aex$Date, r,
  type = "l",
  col = "steelblue",
  xlab = "Year",
  ylab = "Daily intraday return (%)",
  main = "AEX daily intraday returns, 1995–2024"
)
abline(h = 0, col = "grey40", lty = 2)

dev.off()

