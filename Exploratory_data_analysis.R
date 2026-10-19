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

