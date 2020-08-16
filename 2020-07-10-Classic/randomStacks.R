ringStacksRandom <- sample(ringStacks, replace = TRUE)
plotList <- vector('list', length(ringStacksRandom))
plotList <- lapply(ringStacksRandom, drawRingStack)

do.call(ggarrange, c(plotList[1:50], list(nrow = 5, ncol = 10, labels = 1:50)))
