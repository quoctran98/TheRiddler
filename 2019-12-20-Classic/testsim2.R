library(ggplot2)

df <- data.frame(0,0,0)
colnames(df) <- c("pairs","mean","stdev")

for (pairs in 1:100) {
  allRuns <- c()
  for (run in 1:1000) {
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
  row <- c(pairs,mean(allRuns),sd(allRuns))
  print(row)
  df <- rbind(df,row)
}

sqrtEstimate <- lm(mean~sqrt(pairs),data=df)
coef(sqrtEstimate)

func <- function(x) {
  return((sqrt(pi * x)) + 0.2275461)
}

dfCurve <- data.frame(0,0)
colnames(dfCurve) <- c("x","y")
for (x in 1:100) {
  dfCurve[x,"x"] <- x
  dfCurve[x,"y"] <- func(x)
}

ggplot() +
  geom_point(data=df, aes(x=pairs,y=mean)) +
  geom_line(data=dfCurve, aes(x=x,y=y))

