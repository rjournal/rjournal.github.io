
## Section: Build your own source()

# Example at top of page 5
run <- function(file){
  expressions <- parse(file)
  runtime <- new.env(parent=.GlobalEnv)

  n <- 0
  for (e in expressions){ 
    eval(e, envir=runtime)
    n <- n + 1
  }
  message(sprintf("Counted %d expressions",n))
  runtime
}

# create a script
write("
  # contents of script.R
  x <- 10
  y <- 2*x
", file="script.R")

# run the script
e <- run("script.R")
e$x

# Masking functions, still page 5

start_counting <- function() TRUE
capture <- function(fun, envir){
  function(...){
    out <- fun(...)
    envir$counting <- out
    out
  }
}

store <- new.env()
f <- capture(start_counting, store)
f()
store$counting

# New version of 'run', page 6.
run <- function(file){
  expressions <- parse(file)
  store <- new.env()
  runtime <- new.env(parent=.GlobalEnv)
  runtime$start_counting <- capture(start_counting, store)
  n <- 0
  for (e in expressions){ 
    eval(e, envir=runtime)
    if ( isTRUE(store$counting) ) n <- n + 1
  }
  message(sprintf("Counted %d expressions",n))
  runtime
}

# create script file
write("
# contents of script1.r
x <- 10
start_counting()
y <- 2*x
", file="script1.R")

# run the script
e <- run("script1.R")
e$x
e$y

## Section: build your own pipe operator

# examples at top of page 7
`%p>%` <- function(lhs, rhs) rhs(lhs)

3 %p>% sin %p>% cos

# Counting examples, page 7.
start_counting <- function(data){
  attr(data, "n") <- 0
  data
}
end_counting <- function(data){
  message(sprintf("Counted %d expressions", attr(data,"n")-1))
  attr(data, "n") <- NULL
  data
}

`%p>%` <- function(lhs, rhs){
  if ( !is.null(attr(lhs,"n")) ){
    attr(lhs,"n") <- attr(lhs,"n") + 1
  }
  rhs(lhs)
}

# use the functions
out <- 3 %p>% 
 start_counting %p>% 
   sin %p>% 
   cos %p>% 
 end_counting

out

# Section: Application 1: tracking changes in data

# First example: interactive use of 'lumberjack'
library(lumberjack)
out <- women %L>%
  start_log(simple$new()) %L>%
  transform(height = height * 2.54) %L>%
  identity() %L>%
  dump_log()

read.csv("simple.csv")

# Second example, lumberjack::run_file

# create the script
# contents of test_script.R
write("
# contents of script2.R
start_log(women, simple$new())
women$height <- women$height * 2.54/100
women$weight <- women$weight * 0.453592
women$bmi    <- women$weight/(women$height)^2
", file="script2.R")

# run the script
e <- run_file("script2.R")
read.csv("women_simple.csv")
head(e$women,3)


# Section: Application 2: unit testing

# create files.

write("
# contents of bmi.R
bmi <- function(weight, height) weight/(height^2)
", file="bmi.R")

write("
# contents of test_script.R
data(women)
women$height <- women$height * 2.54/100
women$weight <- women$weight * 0.453592
BMI    <- with(women, bmi(weight,height) )
expect_true( all(BMI >= 10) )
expect_true( all(BMI <= 30) )
", file="test_script.R")

# source bmi.R and run the test file
source("bmi.R")
library(tinytest)
out <- run_test_file("test_script.R")
print(out, passes=TRUE)



