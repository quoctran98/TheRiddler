library(ggplot2)

df <- data.frame(0,0,0,0)
colnames(df) <- c("x","y","z","a")

func <- function (x, N) {
  indep <- ((x-1)/(2*N-(x-1)))
  return(indep)
}

funcz <- function (x, N) {
  indep <- ((x-1)/(2*N-(x-1)))
  return(indep * (1-(sum(df[1:(x-1),"z"]))))
}

for (x in 1:12) {
  df[x,"x"] <- x
  df[x,"y"] <- func(x,10) # indep simple calc
  df[x,"z"] <- funcz(x,10) # fits our sim
  df[x,"a"] <- sum(df[1:x,"z"]) # cum. of sim fitting model
}

ggplot() +
  geom_line(data=df, aes(x=x,y=y)) +
  geom_line(data=df, aes(x=x,y=z)) +
  geom_line(data=df, aes(x=x,y=a))
