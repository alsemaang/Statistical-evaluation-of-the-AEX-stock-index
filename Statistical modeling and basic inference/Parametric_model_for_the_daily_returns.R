# Question 2(i): parametric model for the daily returns in 2005 and 2020

# Import and check the data using our existing script
source("Import.R")
library(MASS)

# Daily returns in percent, same definition as in 1(c)
aex$Return <- 100 * (aex$Close - aex$Open) / aex$Open

fit_year <- function(year) {
  x <- aex$Return[format(aex$Date, "%Y") == as.character(year)]
  n <- length(x)
  
  # Model 1: normal distribution N(mu, sigma^2), maximum likelihood estimates
  mu_hat    <- mean(x)
  sigma_hat <- sqrt(mean((x - mu_hat)^2))   # MLE (divides by n)
  ll_norm   <- sum(dnorm(x, mu_hat, sigma_hat, log = TRUE))
  
  # Model 2: scaled t-distribution (heavier tails), maximum likelihood
  fit_t <- fitdistr(x, "t", start = list(m = mu_hat, s = sd(x), df = 5),
                    lower = c(-Inf, 1e-6, 1))
  
  data.frame(year = year, n = n,
             mu_hat = mu_hat, sigma_hat = sigma_hat,
             t_m = unname(fit_t$estimate["m"]),
             t_s = unname(fit_t$estimate["s"]),
             t_df = unname(fit_t$estimate["df"]),
             AIC_normal = 2 * 2 - 2 * ll_norm,
             AIC_t      = 2 * 3 - 2 * fit_t$loglik)
}

res <- rbind(fit_year(2005), fit_year(2020))
print(res, digits = 4)

# Save the table
dir.create("tables", showWarnings = FALSE)
write.csv(res, "tables/aex_2i_estimates.csv", row.names = FALSE)

# Figure: histograms with the fitted densities and a kernel density estimate
dir.create("figures", showWarnings = FALSE)
png("figures/aex_2i_fit.png", width = 3000, height = 1350, res = 300)
par(mfrow = c(1, 2))
for (i in 1:2) {
  yr <- res$year[i]
  x  <- aex$Return[format(aex$Date, "%Y") == as.character(yr)]
  hist(x, breaks = 30, freq = FALSE, main = paste("Daily returns", yr),
       xlab = "Daily return (%)", col = "grey90", border = "white")
  curve(dnorm(x, res$mu_hat[i], res$sigma_hat[i]), add = TRUE, lwd = 2, col = "red")
  curve(dt((x - res$t_m[i]) / res$t_s[i], res$t_df[i]) / res$t_s[i],
        add = TRUE, lwd = 2, col = "blue", lty = 2)
  lines(density(x), lwd = 2, col = "darkgreen", lty = 3)   # kernel density estimate
  legend("topright", c("Normal fit", "t fit", "Kernel density"),
         col = c("red", "blue", "darkgreen"), lwd = 2, lty = c(1, 2, 3), bty = "n",
         cex = 0.8)
}
dev.off()
