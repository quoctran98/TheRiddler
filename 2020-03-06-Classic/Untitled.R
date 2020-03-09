library(reshape2)
df <- data.frame(turns = 1:4, ask3 = 0, ask2 = 0, guess = 0, win = 0, lose = 0)
df <- melt(df, id.vars = c("turns", "win", "lose"))
colnames(df) <- c("turns", "win", "lose", "path", "ratio")

for (i in 1:10000) {
  characters <- c(TRUE, FALSE, FALSE, FALSE)
  inGame <- TRUE
  choice <- 0
  turns <- 0
  path <- NA
  
  while (inGame) {
    turns <- turns + 1
    action <- sample(c("ask", "guess"), 1)
    if (length(characters) == 4) {
      if (action == "ask") {
        if (sample(c(TRUE, FALSE), 1)) {
          # ask 3:1
          path <- "ask3"
          if (sample(c(TRUE, FALSE), 1, prob = c(0.25, 0.75))) {
            characters <- c(TRUE)
          } else {
            characters <- c(TRUE, FALSE, FALSE)
          }
        } else {
          # ask 2:2
          path <- "ask2"
          characters <- c(TRUE,FALSE)
        }
      } else {
        path <- "guess"
        choice <- sample(1:4, 1)
        inGame <- FALSE
      }
    } else if (length(characters) == 3) {
      if (action == "ask") {
        # ask 2:1
        if (sample(c(TRUE, FALSE), 1, prob = c(0.33, 0.67))) {
          characters <- c(TRUE)
        } else {
          characters <- c(TRUE, FALSE)
        }
      } else {
        choice <- sample(1:3, 1)
        inGame <- FALSE
      }
    } else if (length(characters) == 2) {
      if (action == "ask") {
        # ask 1:1
        characters <- c(TRUE)
      } else {
        choice <- sample(1:2, 1)
        inGame <- FALSE
      }
    } else {
      choice <- 1
      inGame <- FALSE
    }
  }
  if (choice == 1) {
    # win
    df[df$path == path,][turns, "win"] <- df[df$path == path,][turns, "win"] + 1
  } else {
    # lose
    df[df$path == path,][turns, "lose"] <- df[df$path == path,][turns, "lose"] + 1
  }
  
}

df$ratio <- (df$win / (df$win +  df$lose)) * 100
