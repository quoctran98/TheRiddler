actors <- c(T,T,T,F,F)

two_right <- 0

for (i in 1:1000000) {
  if (sum(sample(actors, 3, replace = F)) >= 2) {
    two_right <- two_right + 1
  }
}

print(two_right/1000000)