# Question 1(f)

year <- as.integer(format(aex$Date, "%Y"))
month <- as.integer(format(aex$Date, "%m"))

years <- 1995:2024
january_returns <- numeric(length(years))
rest_returns <- numeric(length(years))

for (i in 1:length(years)) {
    january <- aex[year == years[i] & month == 1, ]
  rest <- aex[year == years[i] & month >= 2, ]
  
  # January return
  january_returns[i] <- 100 *
    (tail(january$Close, 1) - january$Open[1])/january$Open[1]
  
  # February-December return
  rest_returns[i] <- 100 * (tail(rest$Close, 1) - rest$Open[1])/rest$Open[1]}

# Bar graph
returns_matrix <- rbind(january_returns, rest_returns)

colnames(returns_matrix) <- years

barplot(
  returns_matrix,
  beside = TRUE,
  col = c("steelblue", "darkorange"),
  names.arg = years,
  las = 2,
  cex.names = 0.7,
  ylim = c(-50, 40),
  ylab = "Return (%)",
  main = "January vs February-December returns"
)

abline(h = 0, col = "grey40")

legend(
  "topright",
  legend = c("January", "February-December"),
  fill = c("steelblue", "darkorange"),
  bty = "n",
  inset = c(0.02, -0.05),
  xpd = TRUE
)