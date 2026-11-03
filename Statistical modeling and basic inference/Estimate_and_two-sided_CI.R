# 2j: Mean daily return and two-sided 95% confidence interval
# Returns are expressed in percentages

for (year in c(2005, 2020)) {
  
  data_year <- aex[format(aex$Date, "%Y") == year, ]
  
  returns <- 100 * (data_year$Close - data_year$Open) / data_year$Open
  
  ci <- t.test(returns, conf.level = 0.95)$conf.int
  
  cat(
    "\nYear:", year,
    "\nMean daily return (%):", round(mean(returns), 4),
    "\n95% confidence interval (%):", round(ci[1], 4), "to", round(ci[2], 4),
    "\nContains zero:", ci[1] <= 0 && ci[2] >= 0,
    "\n"
  )
}
