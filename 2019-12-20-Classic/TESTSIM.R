library(ggplot2)
nruns <- 5000
pairs <- 10
# Calculating our probability distribution
df <- data.frame(0,0)
colnames(df) <- c("x","y")
func <- function (x, N) {
  indep <- ((x-1)/(2*N-(x-1)))
  return(indep * (1-(sum(df[1:(x-1),"y"]))))
}
for (x in 1:12) {
  df[x,"x"] <- x
  df[x,"y"] <- func(x,pairs)
}
# Simulating the sock pulling
allRuns <- c()
for (run in 1:nruns) {
  sockDrawer <- c(1:pairs,1:pairs)
  sockHand <- c()
  sockMatch <- FALSE
  i <- 0
  while (sockMatch != TRUE) {
    i <<- i + 1
    sock <- sample(sockDrawer,1)
    sockDrawer <<- sockDrawer[-match(sock,sockDrawer)]
    if (sock %in% sockHand) {
      sockMatch <<- TRUE
    } else {
      sockHand <<- c(sockHand,sock)
    }
  }
  allRuns <- c(allRuns,i)
}
# Normalizing the probability distribution and plotting the distributions
df$y <- df$y * nruns
dfSim <- data.frame(allRuns)
ggplot() +
  geom_histogram(data=dfSim, aes(x=allRuns), binwidth=1) +
  geom_line(data=df, aes(x=x,y=y))
geom_vline(xintercept=mean(allRuns))
