library(magrittr)
library(rmonad)

# minimal example
# ---------------

"a cat" %>>% log %>>% sqrt


# examples from figure 1
# ----------------------

"x" %>>% paste("a") %>>%
         paste("b") %>>%
         log %>% plot(label="value")

"x" %>>% paste("a") %>% tag("a1") %>>%
         paste("b") %>>%
         log %>% view("a1") %>>%
         paste("c") %>% plot(label="value")


# docstrings and metadata
# -----------------------

foo <- function(x){
  "This is a docstring"
  list(sysinfo = sessionInfo())
  return(x)
}
5 %>>% foo %>% get_meta
5 %>>% foo %>% get_doc

# fancy log
# ---------

fancy_log <- function(x){
  list(
    format_warnings = function(x, xs) {
      sprintf("%s NaNs produced", sum(is.na(x)))
    },
    format_log = function(x, passing) {
      if(passing){
        cat("pass\n")
      } else {
        cat("fail\n")
      }
    },
    summarize = list(len = length)
  )
  log(x)
}

"a cat" %>>% fancy_log -> m
c(-2,-1,0,1,2) %>>% fancy_log -> m
m


# Case study
# ----------

m <- {
  "iris dataset"
  as_monad(iris, tag="indata")
} %>>% {
  "anova"
  res.aov <- aov(Petal.Length ~ Species, data = .)
  summary(res.aov)
} 

m <- {
  view(m, "indata")
} %>>% {
  "Kruskal-Wallis"
  res.kr <- kruskal.test(Petal.Length ~ Species, data = iris)
  res.kr
}

m <- {
  view(m, "indata")
} %>>% {
  "t-test"
  t.test(Petal.Length~Species, data=iris)
}

plot(m, label = function(m){paste(get_id(m),
                                  get_doc(m), 
                                  get_time(m),
                                  gsub("character\\(0\\)", "", get_error(m)), 
                                  sep=":")})

missues(m)

id=c(2,3)
get_value(m)[id]
