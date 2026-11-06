# 2l: One-sided t-tests for a positive mean daily return

for (year in c(2005, 2020)) {
  
  data_year <- aex[format(aex$Date, "%Y") == year, ]
  
  returns <- 100 * (data_year$Close - data_year$Open) / data_year$Open
  
  test <- t.test(returns, mu = 0, alternative = "greater")
  
  cat(
    "\nYear:", year,
    "\nt-statistic:", round(unname(test$statistic), 4),
    "\nOne-sided p-value:", round(test$p.value, 4),
    "\nReject H0:", test$p.value < 0.05,
    "\n"
  )
}
