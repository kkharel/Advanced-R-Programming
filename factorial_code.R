#Part 1: Factorial Function

# using for loop
Factorial_loop = function(n) {
  factorial = 1
  if (n < 0) {
    print("Factorial is not available for negative numbers")
  } else if (n == 0) {
    1
  } else {
    for (i in 1:n) {
      factorial = factorial * i
    }
    
  }
  return(factorial)
}

Factorial_loop(100)

#using Reduce from purr_package

library(purrr)

Factorial_reduce = function(n){
  if (n == 0) return(1)
  else {
    reduce(as.numeric(c(1:n)), function(n,y){
      n * y
    })
  }
}

Factorial_reduce(5)

#using recursion

Factorial_func = function(n) {
  if (n == 0) {
    1
  } else
    n*Factorial_func(n-1)
}

Factorial_func(100)


#using memoization

fact_table = c(rep(NA, 100))
Factorial_mem = function(n) {
  if (n == 0) {
    return(1)
  } else
    fact_table[n] <<- n* Factorial_mem(n-1)
  fact_table[n]
}

Factorial_mem(5)

library(microbenchmark)
n=50
Comparision = (microbenchmark(loop = Factorial_loop(n), 
                              reduce = Factorial_reduce(n), 
                              recursive = Factorial_func(n),
                              memoization = Factorial_mem(n)))
Comparision

myplot = autoplot(Comparision)
myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"))

sink(file = "factorial_output.txt")

