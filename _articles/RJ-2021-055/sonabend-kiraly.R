library(distr6)
library(magrittr)
set.seed(1)

b <- Binomial$new(prob = 0.1, size = 5)
b$parameters()
b$properties

b$traits

b$mean()

b$mean()
b$entropy()
b$skewness()
b$kurtosis()
plot(b, fun = "all", main = "B(0.1, 5)")

E <- Empirical$new(samples = rexp(10000))
summary(E)
par(mfrow = c(1, 2))
qqplot(E, Normal$new(), xlab = "Empirical", ylab = "Normal")
qqplot(E, Exponential$new(), xlab = "Empirical", ylab = "Exponential")
plot(EmpiricalMV$new(data.frame(rnorm(100, mean = 3), rnorm(100))), fun = "cdf")


U <- Distribution$new(name = "Discrete Uniform",
                      type = set6::Integers$new(), support = set6::Set$new(1:10),
                      pdf = function(x) ifelse(x < 1 | x > 10, 0, rep(1/10,length(x))),
                      decorators = c("CoreStatistics", "ExoticStatistics", "FunctionImputation"))
summary(U)
U$cdf(1:10)
U$rand(10)
U$mean()
U$hazard(2)
U$kthmoment(2)

TB <- truncate(
  Binomial$new(size = 20, prob = 0.5),
  lower = 1,
  upper = 5
)
round(TB$cdf(0:6), 4)

VD <- VectorDistribution$new(
  distribution = "Normal",
  params = data.frame(mean = 2:3)
)
VD$parameters()
VD$mean()
VD$pdf(1:2, 3:4)

MD <- MixtureDistribution$new(
  list(Normal$new(mean = 2, sd = 1),
       Exponential$new(rate = 1)
  )
)
MD$pdf(1:5)
MD$cdf(1:5)
MD$rand(5)

Normal$new(mean = 2) %>%
  setParameterValue(mean = 0) %>%
  getParameterValue("mean")

Normal$new()$pdf(1:2)
Binomial$new()$cdf(1:2, lower.tail = FALSE, log.p = TRUE, simplify = FALSE)

N <- Normal$new(mean = 2, var = 4)
N$parameters()
N$setParameterValue(prec = 1/7)
N$parameters()

N <- Normal$new(decorators = c("CoreStatistics", "ExoticStatistics"))

N <- Normal$new()
decorate(N, c("CoreStatistics", "ExoticStatistics"))

N <- Normal$new()
ExoticStatistics$new()$decorate(N)

M <- MixtureDistribution$new(list(Degenerate$new(),StudentT$new()),
                             weights = c(0.1, 0.9))
M$parameters()
M$setParameterValue(Degen_mean = 2, T_df = 5, mix_weights = "uniform")
M$parameters()

M <- MixtureDistribution$new(list(
  truncate(StudentT$new(), lower = -1, upper = 1),
  huberize(Exponential$new(), upper = 4)
))
M$parameters()

B <- Binomial$new()
B$pdf(-1)


TN <- truncate(Normal$new(), lower = -1, upper = 1)
TN$cdf(-2:2)
class(TN)

HB <- huberize(Binomial$new(), lower = 2, upper = 5)
HB$cdf(1:6)
HB$median()

V <- VectorDistribution$new(distribution = "Normal",
                            params = data.frame(mean = 1:2))
V$pdf(1:2)
V$pdf(1, 2)
V$pdf(1:2, 3:4)

M <- MixtureDistribution$new(distribution = "Degenerate",
                             params = data.frame(mean = 1:10))
M$cdf(1:5)
class(M)

P <- ProductDistribution$new(list(Normal$new(), Exponential$new(), Gamma$new()))
P$cdf(1:5)

DistributionDecorator
CoreStatistics

C <- ExoticStatistics$new()
B <- Binomial$new()
B$hazard(1)
C$decorate(B)
B$hazard(1)

decorate(B, "CoreStatistics")

DistributionWrapper
TruncatedDistribution

N <- Normal$new(mean = 2)
N %>%
  setParameterValue(mean = 1) %>%
  getParameterValue("mean")
pdf(N, 1:4)

d <- Distribution$new(name = "Custom Distribution", type = set6::Integers$new(),
                      support = set6::Set$new(1:10),
                      pdf = function(x) rep(1/10, length(x)))
d$pdf(1:3)

Normal$new(var = 1, sd = 2, prec = 1/3)$getParameterValue("var")
Normal$new(var = 1, sd = 2)$getParameterValue("var")
Normal$new(var = 1)$getParameterValue("var")

G <- Gamma$new(shape = 1, rate = 1)
G$getParameterValue("shape")
G$setParameterValue(shape = 2, rate = 2)
G$getParameterValue("rate")
G$setParameterValue(scale = 2)
G$getParameterValue("rate")

A <- Arcsine$new()
A$properties
A$traits

#---------
# benchmark
#---------
set.seed(1)

quack1 <- function(x) UseMethod("quack1", x)
duck1 <- function(name) return(structure(list(name = name), class = "duck1"))
quack1.duck1 <- function(x) cat(x$name, "quack1!")
quack1(duck1("Arthur"))

setClass("duck2", slots = c(name = "character"))
setGeneric("quack2", function(x) {
  standardGeneric("quack2")
})
setGeneric("duck2", function(name) {
  standardGeneric("duck2")
})
setMethod("duck2", signature(name = "character"),
          definition = function(name){
            new("duck2", name = name)
          })
setMethod("quack2",
          definition = function(x) {
            cat(x@name, "quack2!")
          })
quack2(duck2("Ford"))

duck3 <- R6::R6Class("duck3", public = list(
  initialize = function(name) private$.name = name,
  quack3 = function() cat(private$.name, "quack3!")),
  private = list(.name = character(0)))
duck3$new("Zaphod")$quack3()
d3 <- duck3$new("Zaphod")

microbenchmark::microbenchmark(
  duck1("Arthur"),
  duck2("Ford"),
  duck3$new("Zaphod")
)

microbenchmark::microbenchmark(
  quack1(duck1("Arthur")),
  quack2(duck2("Ford")),
  duck3$new("Zaphod")$quack3(),
  d3$quack3(),
  times = 500,
  setup = set.seed(130720)
)
