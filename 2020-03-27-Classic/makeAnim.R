library(ggplot2)

dieGame <- function(nSides = 6) {
  nRolls <- 0
  currentDie <- 1:nSides
  while (length(unique(currentDie)) != 1) {
    currentDie <- sample(currentDie, nSides, replace = TRUE)
    nRolls <- nRolls + 1
  }
  return(nRolls)
}

df <- data.frame(allGames = NA, nSides = NA)
for (nSides in 2:50) {
  allGames <- c()
  for (i in 1:5000) {
    allGames <- c(allGames, dieGame(nSides = nSides))
  }
  df <- rbind(df, data.frame(allGames, nSides))
}
df <- df[-1,]

for (nSides in 2:50) {
  dfTemp <- df[df$nSides == nSides,]
  ggplot(dfTemp) +
    xlim(c(0,300)) +
    ylim(c(0,500))
    geom_histogram(aes(x = allGames), binwidth = 1)
  ggsave(paste("anim/",nSides, ".png", sep = ""))
}
