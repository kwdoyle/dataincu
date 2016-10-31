Dice <- function(n, m) {
  for (i in 1000000000000) {
    x <- floor( runif(n, min=1, max=6) )
    if ( sum(x) == m) {
      return(prod(x))
    }
    if (sum(x) == m) {break}
  }
}

prods1 <- NULL
while( length(prods1) < 100000 ) {
  res <- Dice(8, 24)
  if (class(res) != "NULL") {
    prods1 <- c(prods1, res)
  }
}

mean(prods1) # 2398.071
sd(prods1) # 899.2823


# For n=8, m=24:
# expected value = 2398.071
# sd = 899.2823






prods <- NULL
while( length(prods) < 100000 ) {
  res <- Dice(50, 150)
  if (class(res) != "NULL") {
    prods <- c(prods, res)
  }
}

mean(prods)
sd(prods)

# For n=50, m=150:

# expected value = 9.990499e+20
# sd = 1.110251e+21


