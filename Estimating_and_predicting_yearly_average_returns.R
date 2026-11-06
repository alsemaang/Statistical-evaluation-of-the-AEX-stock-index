# 2m: Estimating and predicting yearly average daily returns

dir.create("Figures", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)


# Step 1: Calculate yearly average daily returns
bonus <- data.frame(
  Year = as.integer(format(aex$Date, "%Y")),
  Return = 100 * (aex$Close - aex$Open) / aex$Open
)

bonus <- subset(bonus, Year >= 1995 & Year <= 2024)

stopifnot(!anyNA(bonus), all(is.finite(bonus$Return)))

annual <- aggregate(Return ~ Year, bonus, mean)
annual <- annual[order(annual$Year), ]

stopifnot(identical(annual$Year, 1995:2024))

annual$Time <- annual$Year - 1995
print(annual)


# Step 2: Fit both models
model_constant <- lm(Return ~ 1, annual)
model_linear <- lm(Return ~ Time, annual)

print(coef(model_constant))
print(coef(model_linear))


# Step 3: Save the yearly-return figure
png(
  "Figures/bonus_yearly_returns.png",
  width = 2400, height = 1500, res = 300
)

old_par <- par(no.readonly = TRUE)
par(mar = c(7, 6, 4, 2))

plot(
  annual$Year, annual$Return,
  type = "b", pch = 16, cex = 0.7,
  xlab = "Year",
  ylab = "Average daily return (%)",
  main = "Yearly average daily returns",
  ylim = c(-0.25, 0.10),
  yaxt = "n"
)

y_ticks <- seq(-0.25, 0.10, 0.05)
axis(2, at = y_ticks, labels = sprintf("%.2f", y_ticks), las = 1)

abline(h = 0, lty = 2, col = "grey70")
lines(annual$Year, fitted(model_constant), col = "blue", lwd = 2, lty = 2)
lines(annual$Year, fitted(model_linear), col = "red", lwd = 2)

legend_y <- grconvertY(
  grconvertY(par("usr")[3], "user", "inches") -
    5 * par("csi") * par("mex"),
  "inches", "user"
)

legend(
  x = par("usr")[2], y = legend_y,
  xjust = 1, yjust = 1, xpd = NA,
  legend = c("Observed", "Constant mean", "Linear trend"),
  col = c("black", "blue", "red"),
  lty = c(1, 2, 1), pch = c(16, NA, NA),
  lwd = c(1, 2, 2),
  horiz = TRUE, bty = "n", cex = 0.8
)

par(old_par)
dev.off()


# Step 4: Evaluate one-year-ahead predictions
evaluation <- data.frame(
  Year = 2010:2024,
  Actual = NA_real_,
  Constant = NA_real_,
  Linear = NA_real_
)

for (i in seq_len(nrow(evaluation))) {
  
  year <- evaluation$Year[i]
  train <- subset(annual, Year < year)
  next_year <- subset(annual, Year == year)
  
  evaluation$Actual[i] <- next_year$Return
  
  evaluation$Constant[i] <- predict(
    lm(Return ~ 1, train), next_year
  )
  
  evaluation$Linear[i] <- predict(
    lm(Return ~ Time, train), next_year
  )
}

print(evaluation)


# Step 5: Compare MSE and select a model
mse_constant <- mean((evaluation$Actual - evaluation$Constant)^2)
mse_linear <- mean((evaluation$Actual - evaluation$Linear)^2)

mse_results <- data.frame(
  Model = c("Constant mean", "Linear trend"),
  MSE = c(mse_constant, mse_linear)
)

print(mse_results)

if (mse_linear < mse_constant) {
  selected_model <- model_linear
  selected_name <- "Linear trend"
} else {
  selected_model <- model_constant
  selected_name <- "Constant mean"
}

cat("\nSelected model:", selected_name, "\n")


# Step 6: Predict the 2025 yearly average
forecast <- predict(
  selected_model,
  newdata = data.frame(Year = 2025, Time = 30),
  interval = "prediction",
  level = 0.95
)

forecast_results <- data.frame(
  Year = 2025,
  Model = selected_name,
  Predicted_mean_daily_return_pct = forecast[1, "fit"],
  PI_lower_pct = forecast[1, "lwr"],
  PI_upper_pct = forecast[1, "upr"]
)

print(forecast_results, row.names = FALSE)


#Step 7: Save residual diagnostics
png(
  "Figures/bonus_residual_diagnostics.png",
  width = 2400, height = 1500, res = 300
)

old_par <- par(no.readonly = TRUE)
par(mfrow = c(1, 2), mar = c(5, 5, 3, 1))

res <- residuals(selected_model)

plot(
  annual$Year, res,
  type = "b", pch = 16, cex = 0.7,
  xlab = "Year",
  ylab = "Residual (percentage points)",
  main = "Residuals over time",
  las = 1
)

abline(h = 0, lty = 2, col = "grey70")

qqnorm(
  res,
  main = "Normal QQ plot",
  pch = 16, cex = 0.7,
  las = 1
)

qqline(res, col = "red")

par(old_par)
dev.off()


# Step 8: Save the historical results
tables <- list(
  bonus_yearly_averages = annual,
  bonus_forecast_evaluation = evaluation,
  bonus_model_mse = mse_results,
  bonus_2025_prediction = forecast_results
)

for (name in names(tables)) {
  write.csv(
    tables[[name]],
    paste0("results/", name, ".csv"),
    row.names = FALSE
  )
}


# Step 9: Import and prepare the actual 2025 data
raw2025 <- read.csv(
  "data/AEX Historical Data 2025.csv",
  header = FALSE,
  fileEncoding = "UTF-8-BOM",
  stringsAsFactors = FALSE
)

aex2025 <- read.csv(
  text = paste(raw2025[[1]], collapse = "\n"),
  stringsAsFactors = FALSE
)

aex2025$Date <- as.Date(aex2025$Date, "%m/%d/%Y")
aex2025$Open <- as.numeric(gsub(",", "", aex2025$Open))
aex2025$Close <- as.numeric(gsub(",", "", aex2025$Price))

stopifnot(
  nrow(aex2025) > 0,
  !anyNA(aex2025[, c("Date", "Open", "Close")]),
  !anyDuplicated(aex2025$Date),
  all(format(aex2025$Date, "%Y") == "2025"),
  all(is.finite(aex2025$Open)),
  all(is.finite(aex2025$Close)),
  all(aex2025$Open > 0)
)

print(range(aex2025$Date))
print(nrow(aex2025))


# Step 10: Compare the actual 2025 average with the forecast
returns2025 <- 100 * (aex2025$Close - aex2025$Open) / aex2025$Open
actual2025 <- mean(returns2025)

comparison2025 <- data.frame(
  Year = 2025,
  N = length(returns2025),
  Actual_pct = actual2025,
  Predicted_pct = forecast[1, "fit"],
  Error_pp = actual2025 - forecast[1, "fit"],
  PI_lower_pct = forecast[1, "lwr"],
  PI_upper_pct = forecast[1, "upr"],
  Inside_PI = actual2025 >= forecast[1, "lwr"] &&
    actual2025 <= forecast[1, "upr"]
)

print(comparison2025, row.names = FALSE)

write.csv(
  comparison2025,
  "results/bonus_2025_validation.csv",
  row.names = FALSE
)

