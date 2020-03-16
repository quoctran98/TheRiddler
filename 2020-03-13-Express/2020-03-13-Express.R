library(ggplot2)

df <- data.frame(x = 0:1000)
df$y <- 2^df$x
df$close10 <- 10^(round(log10(df$y) - log10(5.5) + 0.5))
df$diff <- (df$y - df$close10) / df$close10
df$diff <- df$diff * 100

ggplot(df[1:200,]) +
  geom_line(aes(x = x, y = diff)) +
  geom_point(aes(x = x, y = diff,
                 col = ifelse(abs(diff) <= 2.4, 1, 0))) +
  #geom_vline(xintercept = c(39, 133, 226, 329, 421, 524, 617)) +
  xlab("Power of 2") +
  ylab("Percent Change from Nearest Power of 10") +
  theme(legend.position="none")

answers <- df[abs(df$diff) <= 2.4,"x"]