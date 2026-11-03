# Question 1(e): Sell in May
source("Import.R")

dir.create("figures", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)

# Identify years and months
year <- as.integer(format(aex$Date, "%Y"))
month <- as.integer(format(aex$Date, "%m"))

returns <- data.frame(
  Year = sort(unique(year)),
  September = NA_real_,
  November = NA_real_
)

# Calculate each year's holding-period returns (%)
for (i in seq_len(nrow(returns))) {
  y <- returns$Year[i]
  
  may_close <- tail(aex$Close[year == y & month == 5], 1)
  sep_open <- head(aex$Open[year == y & month == 9], 1)
  nov_open <- head(aex$Open[year == y & month == 11], 1)
  
  returns$September[i] <- 100 * (sep_open / may_close - 1)
  returns$November[i] <- 100 * (nov_open / may_close - 1)
}

# Print and save annual returns
print(round(returns, 2))
write.csv(returns, "tables/aex_1e_returns.csv", row.names = FALSE)

# Summarise both periods
summary_table <- data.frame(
  Period = c("May–September", "May–November"),
  Mean = c(mean(returns$September), mean(returns$November)),
  Median = c(median(returns$September), median(returns$November)),
  Negative_years = c(
    sum(returns$September < 0),
    sum(returns$November < 0)
  ),
  Total_years = nrow(returns)
)

print(summary_table, row.names = FALSE)
write.csv(summary_table, "tables/aex_1e_summary.csv", row.names = FALSE)

# Compare returns by year
png("figures/aex_sell_in_may.png",
    width = 3000, height = 1700, res = 300)

# Extra bottom space for year labels and legend
par(mar = c(9, 5, 4, 2))

barplot(
  rbind(returns$September, returns$November),
  beside = TRUE,
  names.arg = returns$Year,
  col = c("steelblue", "darkorange"),
  border = NA,
  las = 2,
  cex.names = 0.7,
  ylab = "Holding-period return (%)",
  main = "AEX returns from end of May",
  ylim = range(c(0, returns$September, returns$November))
)

abline(h = 0, col = "grey40")

# Legend below the plot, on the right
legend(
  "bottomright",
  inset = c(0, -0.40),
  xpd = NA,
  legend = c("To beginning of September", "To beginning of November"),
  fill = c("steelblue", "darkorange"),
  bty = "n",
  cex = 0.8
)

dev.off()

