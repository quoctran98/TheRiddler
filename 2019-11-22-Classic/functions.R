isPrime <- function (number) {
  if (number == 2) {
    return(TRUE)
  }
  for (num1 in 2:(number-1)) {
    for (num2 in 2:(number-1)) {
      if (num1*num2 == number) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

findPrimes <- function (numbers) {
  primes <- numbers[sapply(numbers, isPrime)]
  return(primes)
}

findPrimeFactors <- function (number) {
  smallerPrimes <- findPrimes(1:number)
  return(smallerPrimes[number %% smallerPrimes == 0])
}

findProduct <- function (numbers) {
  finalProduct <- 1
  for (index in 1:length(numbers)) {
    finalProduct <- finalProduct*numbers[index]
  }
  return(finalProduct)
}