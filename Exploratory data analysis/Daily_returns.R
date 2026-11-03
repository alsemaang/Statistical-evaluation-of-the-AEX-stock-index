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


# Check the figures quoted in the report
sum(aex$Volume == 0)

aex[c(1, nrow(aex)), c("Date", "Close")]

aex[aex$Close == max(aex$Close), c("Date", "Close")]

sum(aex$Volume == 0)
aex[aex$Close == max(aex$Close), c("Date", "Close")]


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