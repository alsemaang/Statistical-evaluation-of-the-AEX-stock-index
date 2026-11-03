# Question 2(g)

returns_2005 <- r[year == 2005]
returns_2020 <- r[year == 2020]

breaks <- seq(-7, 5, by = 0.5)

h2005 <- hist(returns_2005, breaks = breaks, plot = FALSE)
h2020 <- hist(returns_2020, breaks = breaks, plot = FALSE)

max_freq <- max(c(h2005$counts, h2020$counts))

par(mfrow = c(1, 2))

hist(returns_2005,
     main = "Daily returns in 2005",
     xlab = "Daily return (%)",
     ylab = "Frequency",
     col = "steelblue",
     breaks = breaks,
     xlim = c(-7, 5),
     ylim = c(0, max_freq))

hist(returns_2020,
     main = "Daily returns in 2020",
     xlab = "Daily return (%)",
     ylab = "Frequency",
     col = "darkorange",
     breaks = breaks,
     xlim = c(-7, 5),
     ylim = c(0, max_freq))

par(mfrow = c(1, 1))

# Question 2(h)

par(mfrow = c(1, 2))

# QQ plot for 2005
qqnorm(returns_2005,
       main = "Normal QQ plot - 2005",
       col = "steelblue",
       pch = 19)

qqline(returns_2005, col = "red", lwd = 2)

# QQ plot for 2020
qqnorm(returns_2020,
       main = "Normal QQ plot - 2020",
       col = "darkorange",
       pch = 19)

qqline(returns_2020, col = "red", lwd = 2)

par(mfrow = c(1, 1))
