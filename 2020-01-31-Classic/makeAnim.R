library(ggplot2)

vol <- function(n, N = 12, a = 1) {
  magnaTileTopAngle <- 360/N
  magnaTileBaseAngle <- (180 - magnaTileTopAngle) / 2
  
  b <- a * (sin(magnaTileBaseAngle *(pi/180))/sin(magnaTileTopAngle *(pi/180)))
  d <- a / (2 * tan((180/n) *(pi/180)))
  areaBase <- 0.5 * a * d
  c <- a * (sin((((n-2)*90)/n) *(pi/180)) / sin((360/n) *(pi/180)))
  H <- sqrt((b^2) - (c^2))
  V <- (1/3) * areaBase * H
  return(V)
}

df <- data.frame(matrix(ncol = 3))
colnames(df) <- c("x", "y", "N")
df <- df[-1,]

dfOpt <- data.frame(matrix(ncol = 3))
colnames(dfOpt) <- c("x", "y", "N")
dfOpt <- dfOpt[-1,]

for (magnaN in 12:50) {
  x <- 3:(magnaN-1)
  y <- vol(x, N = magnaN)
  y <- y - min(y)
  N <- rep.int(magnaN, length(x))
  
  dfTemp <- data.frame("x" = x, "y" = y, "N" = N)
  df <- rbind(df, dfTemp)
  
  optY <- max(y)
  optX <- x[match(optY, y)]
  
  dfTemp <- data.frame("x" = optX, "y" = optY, "N" = N)
  dfOpt <- rbind(dfOpt, dfTemp)
}

for (i in unique(df$N)) {
  xmax <- max(df[df$N <= i,"x"])
  ymax <- max(df[df$N <= i,"y"])
  
  ggplot(data = NULL) +
    geom_line(data = df[df$N <= i,], aes(x=x, y=y, color=(N), group=N)) +
    geom_point(data = dfOpt[dfOpt$N <= i,], aes(x=x, y=y)) +
    xlim(c(2, xmax)) +
    ylim(c(0, ymax)) +
    geom_vline(data = dfOpt, aes(xintercept = dfOpt[dfOpt$N == i, "x"][1])) +
    geom_vline(data = df, aes(xintercept = max(df[df$N == i, "x"]))) +
    xlab("Number of Magna-Tiles") +
    ylab("Normalized Pyramid Volume") + 
    labs(title = paste("Optimal Pyramid with Variable Sized Magna-Tiles (N = ", i, ")", sep = ""),
         subtitle = "where N is the number of Magna-Tiles needed to form a flat regular polygon") +
    theme(axis.text.y=element_blank(),
          legend.position="none")
  ggsave(paste("./anim/", i, ".png", sep = ""))
}


