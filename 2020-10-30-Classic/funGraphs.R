library(ggplot2)
library(ggpubr)

df <- data.frame(player = 1:61, wins = 0)
for (N in 1:1000) {
  winner <- match(FALSE, (1:61 %in% hotPumpkin(N)))
  df[winner, "wins"] <- df[winner, "wins"] + 1
}

ggplot(df) +
  geom_bar(aes(x = player,  y = wins), stat = "identity") +
  theme_pubr()
