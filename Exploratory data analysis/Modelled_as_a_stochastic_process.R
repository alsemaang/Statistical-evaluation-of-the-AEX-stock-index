# Question 1(b): ACF of closing values and daily log-returns

# Sort observations chronologically
aex <- aex[order(aex$Date), ]

# Extract closing prices
close <- aex$Close

# Calculate daily log-returns
res <- diff(log(close))

plot_acf_compare <- function()
{
  # Set up two plots next to each other
  par(mfrow = c(1, 2))
  
  # ACF of closing values
  acf(close, lag.max = 1000,
      col = "steelblue",
      main = "ACF of closing values")
  
  # Plot daily log-returns
  plot(aex$Date[-1], res,
       type = "l",
       col = "steelblue",
       lwd = 0.5,
       xlab = "Year",
       ylab = "Log-return",
       main = "Daily log-returns")
}

# Show in R plot window
plot_acf_compare()


# Reset plotting layout
par(mfrow = c(1, 1))


