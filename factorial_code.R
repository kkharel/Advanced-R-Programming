#Part 1: Factorial Function

sink(file = "factorial_output.txt")

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

Factorial_loop(5)

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

Factorial_func(5)


#Memoization
Factorial_mem <- function(n) {
  inner_fact <- function(m) {
    if(!is.na(fact_table[m])) {
      fact_table[m]
    } else {
      fact_table[m] = m * inner_fact(m-1)
      fact_table[m]
    }
  }
  if (n == 0) {
    return(1)
  }
  if (!exists("fact_table")) {
    fact_table <<- c(1, rep(NA,n-1))
  } else if (length(fact_table) < n) {
    fact_calc <<- c(fact_table, rep(NA, n-length(fact_table)))
  }
  inner_fact(n)
}

Factorial_mem1(5)

library(microbenchmark)
library(ggplot2)
n=1
Comparision = (microbenchmark(loop = Factorial_loop(n), 
                              reduce = Factorial_reduce(n), 
                              recursive = Factorial_func(n),
                              memoization = Factorial_mem(n)))
Comparision

myplot = autoplot(Comparision)

myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"))


n=5
Comparision = (microbenchmark(loop = Factorial_loop(n), 
                              reduce = Factorial_reduce(n), 
                              recursive = Factorial_func(n),
                              memoization = Factorial_mem(n),
                              memoization2 = Factorial_mem1(n)))
Comparision

myplot = autoplot(Comparision)

myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"))

n=25
Comparision = (microbenchmark(loop = Factorial_loop(n), 
                              reduce = Factorial_reduce(n), 
                              recursive = Factorial_func(n),
                              memoization = Factorial_mem(n)))
Comparision

myplot = autoplot(Comparision)

myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"))


n=100
Comparision = (microbenchmark(loop = Factorial_loop(n), 
                              reduce = Factorial_reduce(n), 
                              recursive = Factorial_func(n),
                              memoization = Factorial_mem(n)))
Comparision

myplot = autoplot(Comparision)

myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"))

n=500
Comparision = (microbenchmark(loop = Factorial_loop(n), 
                              reduce = Factorial_reduce(n), 
                              recursive = Factorial_func(n),
                              memoization = Factorial_mem(n)))
Comparision

myplot = autoplot(Comparision)

myplot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"))

sink()
# recursion error for n = 5000
unlink("factorial_output.txt")
getwd()


## Not a good Algo ##
fact_table = c(rep(NA, 100))
Factorial_mem = function(n) {
  if (n == 0) {
    return(1)
  } else
    fact_table[n] <<- n* Factorial_mem(n-1)
  fact_table[n]
}

Factorial_mem(5)
