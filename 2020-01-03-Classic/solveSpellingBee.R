library(stringr)
words <- scan("words.txt", what="char", skipNul=TRUE)

words <- words[nchar(words) >= 4]
uniqueLetters <- function(word) {sum(!!str_count(word, letters))}

words <- data.frame(words, sapply(words, uniqueLetters), nchar(words), stringsAsFactors = FALSE)
colnames(words) <- c("word", "letters", "score")
rownames(words) <- 1:nrow(words)

# The words "true" and "false" are read as logicals.
words[words$word==TRUE,] <- c("true",4,4) 
words[words$word==FALSE,] <- c("false",5,5)

words$letters <- as.numeric(words$letters)
words <- words[words$letters <= 7,]

words$pangram <- FALSE
words[words$letters == 7,"pangram"] <- TRUE

words[words$score == 4,"score"] <- 1
words[words$pangram == TRUE,"score"] <- as.numeric(words[words$pangram == TRUE,"score"]) + 7

words$score <- as.numeric(words$score)

for (letter in letters) {
  words[,letter] <- str_detect(words$word,letter)
}
rm(letter)

getWords <- function(centerLetter, otherLetters) {
  validWords <- words[words[,centerLetter]==TRUE,]
  tempLetters <- letters
  tempLetters <- tempLetters[tempLetters != centerLetter]
  for (letter in otherLetters) {
    tempLetters <- tempLetters[tempLetters != letter]
  }
  validWords$otherLetters <- validWords[,tempLetters]
  validWords <- validWords[(rowSums(validWords[,tempLetters]) == 0),]
  validWords <<- validWords
}
