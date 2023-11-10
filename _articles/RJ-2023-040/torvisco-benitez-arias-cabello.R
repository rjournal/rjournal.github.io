library(nlstac)
library(ggplot2)
library(doParallel)
library(mosaicData)
library(dplyr)


#Example 1
data <- CoolingWater[40:222, ]
tol <- 1e-7
N <- 10
form <- 'temp ~ a1*exp(-k1*time) + a2'
nlparam <- list(k1 = c(1e-7, 1))
tacfit <- nls_tac(
  formula = form,
  data = data,
  nlparam = nlparam,
  N = N,
  tol = tol,
  parallel = FALSE
)

nlsfit1 <- nls(
  formula = form,
  data = data,
  start = list(k1 = 0.1, a1 = 50, a2 = 20),
  control = nls.control(maxiter = 1000, tol = tol)
)

nlsfit2 <- nls(
  formula = form,
  data = data,
  start = coef(tacfit),
  control = nls.control(maxiter = 1000, tol = tol)
)

df1 <-
  data.frame(Time = data$time,
             Temperature = data$temp,
             Method = "00-Measurements")
df2 <-
  data.frame(
    Time = data$time,
    Temperature = predict(tacfit),
    Method = "01-TAC and NLS with TAC"
  )
df <- rbind(df1, df2)

ggplot(data = df,
       aes(
         x = Time,
         y = Temperature,
         colour = Method,
         shape = Method,
         linetype = Method
       )) +
  geom_point(alpha = 0.65,  bg = "gray") +
  geom_line(linewidth = .85) +
  scale_color_manual(labels = c("Measurements", "TAC and NLS with TAC"),
                     values = c(`00-Measurements` = "black",
                                `01-TAC and NLS with TAC` = "green"
                                )
                     ) +
  scale_linetype_manual(labels = c("Measurements", "TAC and NLS with TAC"),
                        values = c(0, 1)) +
  scale_shape_manual(labels = c("Measurements", "TAC and NLS with TAC"),
                     values = c(21, NA)) +
  theme_bw()

ggsave("./example1.pdf", height = 5 , width = 5 * 2)


#Example 2
data <- Indometh[Indometh$Subject == 3,]
tol <- 1e-7
N <- 10
form <- 'conc ~ a1*exp(-k1*time) + a2*exp(-k2*time) + a3'
nlparam <- list(k1 = c(1e-7, 10), k2 = c(1e-7, 10))
tacfit <-   nls_tac(formula = form,
                    data = data,
                    nlparam = nlparam,
                    N = N,
                    tol = tol,
                    parallel = FALSE
                    )
nlsfit1 <- nls(formula = form,
               data = data,
               start = list(k1 = 1, k2 = 1, a1 = 1, a2 = 1, a3 = 1),
               control = nls.control(maxiter = 1000, tol = tol)
               )
nlsfit2 <- nls(formula = form,
               data = data,
               start = coef(tacfit),
               control = nls.control(maxiter = 1000, tol = tol)
               )

df1 <- data.frame(Time = data$time,
                  Concentration = data$conc,
                  Method = "00-Measurements"
                  )
df2 <-data.frame(Time = data$time,
                 Concentration = predict(tacfit),
                 Method = "01-TAC"
                 )
df3 <- data.frame(Time = data$time,
                  Concentration = predict(nlsfit2),
                  Method = "02-NLS with TAC"
                  )
df <- rbind(df1, df2, df3)

ggplot(data = df,
       aes(
         x = Time,
         y = Concentration,
         colour = Method,
         shape = Method,
         linetype = Method
       )) +
  geom_point(alpha = 0.65,  bg = "gray") +
  geom_line(linewidth = .85) +
  scale_color_manual(labels = c("Measurements", "TAC", "NLS with TAC"),
                     values = c(`00-Measurements` = "black",
                                `01-TAC` = "green",
                                `02-NLS with TAC` = "red"
                     )) +
  scale_linetype_manual(labels = c("Measurements", "TAC", "NLS with TAC"),
                        values = c(0, 1, 2)) +
  scale_shape_manual(labels = c("Measurements", "TAC", "NLS with TAC"),
                     values = c(21, NA, NA)) +
  theme_bw()

ggsave("./example2.pdf", height = 5 , width = 5 * 2)


#Example 3 This example may take a while, be patient! 
set.seed(12345)

x <- seq(from = 0, to = 20, length.out = 65)
y <-  2 * exp(-10 * (x - 0.5) ^ 2) + 3 * exp(-1 * (x - 2) ^ 2) + 
  4 * exp(-0.1 * (x - 5) ^ 2) + 1 + .05 * rnorm(65)
data <- data.frame(x, y)
tol <- 1e-5
N <- 6
form <-
  'y ~ a2*exp(-b1*(x-d1)^2) + a3*exp(-b2*(x-d2)^2)+ a4*exp(-b3*(x-d3)^2)+ a1'
nlparam <- list(
  b1 = c(7.7, 15),
  b2 = c(0, 5.1),
  b3 = c(1e-4, 1.1),
  d1 = c(1e-2, 1.5),
  d2 = c(0.1, 4),
  d3 = c(0.11, 11)
)
tacfit <- nls_tac(formula = form,
                  data = data,
                  nlparam = nlparam,
                  N = N,
                  tol = tol,
                  parallel = FALSE
                  )
nlsfit1 <- nls(formula = form,
               data = data,
               start = list(a1 = 0.5, a2 = 1, a3 = 3,
                            a4 = 5, b1 = 10, b2 = 0.5,
                            b3 = 0.1, d1 = 0, d2 = 1, 
                            d3 = 1),
               control = nls.control(maxiter = 1000, tol = tol)
               )

nlsfit2 <- nls(formula = form,
                data = data,
                start = coef(tacfit),
                control = nls.control(maxiter = 1000, tol = tol)
                )

df1 <- data.frame(x = data$x, y = data$y, Method = "00-Data")
df2 <- data.frame(x = data$x,
                  y = predict(tacfit),
                  Method = "01-TAC")
df3 <- data.frame(x = data$x,
                  y = predict(nlsfit2),
                  Method = "02-NLS with TAC")
df <- rbind(df1, df2, df3)

ggplot(data = df, aes(x = x, 
                      y = y, 
                      colour = Method, 
                      shape = Method, 
                      linetype = Method)
       ) +
  geom_point(alpha = 0.65,  bg = "gray") +
  geom_line(linewidth = .85) +
  scale_color_manual(labels = c("Data", "TAC", "NLS with TAC"),
                     values = c(`00-Data` = "black",
                                `01-TAC` = "green",
                                `02-NLS with TAC` = "red")
                     ) +
  scale_linetype_manual(labels = c("Data", "TAC", "NLS with TAC"),
                        values = c(0, 1, 2)
                        ) +
  scale_shape_manual(labels = c("Data", "TAC", "NLS with TAC"),
                     values = c(21, NA, NA)
                     ) +
  theme_bw() + labs(x = "", y = "")


ggsave("./example3.pdf", height = 5 , width = 5 * 2)


#Example 4
set.seed(12345)
x <- seq(from = 0,
         to = 10,
         length.out = 500)
y <- 3 * exp(-0.85 * x) + 1.5 * sin(2 * x) + 1 + 
  rnorm(length(x), mean = 0, sd = 0.3)
data <- data.frame(x, y)
tol <- 1e-7
N <- 10
form <- 'y ~ a1*exp(-k1*x) + a2*sin(b1*x) + a3'
nlparam <- list(k1 = c(0.1, 1), b1 = c(1.1, 5))

tacfit <- nls_tac(formula = form,
                  data = data,
                  nlparam = nlparam,
                  N = N,
                  tol = tol,
                  parallel = FALSE)

nlsfit1 <- nls(formula = form,
               data = data,
               start = list(k1 = 1,
                            b1 = 1,
                            a1 = 1,
                            a2 = 1,
                            a3 = 1) ,
               control = nls.control(maxiter = 1000, tol = tol)
               )
nlsfit2 <- nls(formula = form,
               data = data,
               start = coef(tacfit),
               control =
                 nls.control(maxiter = 1000, tol = tol)  
               )

df1 <- data.frame(x = data$x, y = data$y, Method = "00-Data")
df2 <- data.frame(x = data$x,
                  y = predict(tacfit),
                  Method = "01-TAC")
df3 <-
  data.frame(x = data$x,
             y = predict(nlsfit1),
             Method = "02-NLS without TAC")
df4 <-
  data.frame(x = data$x,
             y = predict(nlsfit2),
             Method = "03-NLS with TAC")
df <- rbind(df1, df2, df3, df4)

ggplot(data = df, 
       aes(x = x, y = y, colour = Method, shape = Method, linetype = Method)) +
  geom_point(alpha = 0.65,  bg = "gray") +
  geom_line(linewidth = .85) +
  scale_color_manual(labels = c("Data", "TAC", "NLS without TAC", "NLS with TAC"),
                     values = c(`00-Data` = "black",
                                `01-TAC` = "green",
                                `02-NLS without TAC` = "blue",
                                `03-NLS with TAC` = "red"
                                )) +
  scale_linetype_manual(labels = c("Data", "TAC", "NLS without TAC", "NLS with TAC"),
                        values = c(0, 1, 1, 2)) +
  scale_shape_manual(labels = c("Data", "TAC", "NLS without TAC", "NLS with TAC"),
                     values = c(21, NA, NA, NA)) +
  theme_bw() + labs(x = "", y = "")


ggsave("./example4.pdf", height = 5 , width = 5 * 2)


#Example 5
seed <- c('12', '123', '1234')
tol <- 1e-5
a0 <- -1.45
a1 <- 1.66
b1 <- -0.47
a2 <- 0.543
b2 <- -0.82
c <- 1.27
z1 <- 2.53
z2 <- 3.85
x <- numeric(100)
x[1] <- 2.7
x[2] <- 3.12
form <-
  'y ~ a0+ a1*v1 + b1*v1*exp(-c*(v2-z1)^2) + a2*v2 + b2*v2*exp(-c*(v2-z2)^2)'
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(no_cores)
df <- NULL
for (j in 1:3) {
  set.seed(seed[j])
  for (i in 3:100) {
    x[i] <- a0 + (a1 + b1 * exp(-c * (x[i - 2] - z1) ^ 2)) * x[i - 1] +
      (a2 + b2 * exp(-c * (x[i - 2] - z2) ^ 2)) * x[i - 2] + rnorm(1, mean =
                                                                     0, sd = 0.1)
  }
  y <- x[3:100]
  v1 <- x[2:99]
  v2 <- x[1:98]
  data <- data.frame(v1 = v1, v2 = v2, y = y)
  tacfit <- nls_tac(
    form,
    data = data,
    nlparam = list(
      c = c(0, 2),
      z1 = c(1, 3.5),
      z2 = c(3, 5)
    ),
    N = 10,
    tol = tol,
    parallel = TRUE
  )
  tryCatch(
    nlsfit1 <-nls( formula = form, data = data, 
                   start = list(c = 1,
                                z1 = 2.25,
                                z2 = 4,
                                a0 = -1,
                                a1 = 1,
                                b1 = -1,
                                a2 = 1,
                                b2 = -1),
                   control = nls.control(maxiter = 1000,  tol = tol , minFactor = 1e-5)),
    error = function(e) {
      nlsfit1 <<- NULL
      }
  )
  tryCatch(
    nlsfit2 <- nls(
      formula = form,
      data = data,
      start = coef(tacfit),
      control = nls.control(maxiter = 1000,  tol = tol , minFactor = 1e-5)
    ),
    error = function(e) {
      nlsfit2 <<- NULL
    }
  )

  df1 <-
    data.frame(
      x = 3:100,
      y = data$y,
      Method = "00-Data",
      Dataset = paste0("Dataset ", j)
    )
  df2 <-
    data.frame(
      x = 3:100,
      y = predict(tacfit),
      Method = "01-TAC",
      Dataset = paste0("Dataset ", j)
    )
  df3 <-
    tryCatch(
      data.frame(
        x = 3:100,
        y = predict(nlsfit1),
        Method = "02-NLS without TAC",
        Dataset = paste0("Dataset ", j)
      ),
      error = function(e) {
      }
    )
  df4 <-
    tryCatch(
      data.frame(
        x = 3:100,
        y = predict(nlsfit2),
        Method = "03-NLS with TAC",
        Dataset = paste0("Dataset ", j)
      ),
      error = function(e) {
      }
    )
  df <- rbind(df, df1, df2, df3, df4)
}
stopImplicitCluster()
ggplot(data = df,
       aes(
         x = x,
         y = y,
         colour = Method,
         shape = Method,
         linetype = Method
       )) +
  geom_point(alpha = 0.65,  bg = "gray") +
  geom_line(linewidth = .75) +
  scale_color_manual(
    labels = c("Data", "TAC", "NLS without TAC", "NLS with TAC"),
    values = c(
      `00-Data` = "black",
      `01-TAC` = "green",
      `02-NLS without TAC` = "blue",
      `03-NLS with TAC` = "red"
    )
  ) +
  scale_linetype_manual(
    labels = c("Data", "TAC", "NLS without TAC", "NLS with TAC"),
    values = c(0, 1, 1, 2)
  ) +
  scale_shape_manual(
    labels = c("Data", "TAC", "NLS without TAC", "NLS with TAC"),
    values = c(21, NA, NA, NA)
  ) +
  theme_bw() + labs(x = "", y = "") + facet_wrap( ~ Dataset)


ggsave("./example5.pdf", height = 5, width = 5 * 3)


#Example 6
x <- EuStockMarkets[, 4]
x <- diff(x) / x[-length(x)]
tol <- 1e-7
y <- x[3:length(x)]
v1 <- x[2:(length(x) - 1)]
v2 <- x[1:(length(x) - 2)]
data <- data.frame(v1 = v1, v2 = v2, y = y)
form <-
  'y ~ a0+ a1*v1 + b1*v1*exp(-c*(v2-z1)^2) + a2*v2 + b2*v2*exp(-c*(v2-z2)^2)'
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(no_cores)
tacfit <- nls_tac(
  form,
  data = data,
  nlparam = list(
    c = c(1e-7, 5),
    z1 = c(1e-7, 5),
    z2 = c(1e-7, 5)
  ),
  N = 15,
  tol = tol,
  parallel = TRUE
)
stopImplicitCluster()
ggplot(data = data, aes(x = c(3:length(x)), y = y)) +
  theme_bw() +
  geom_line() +
  geom_line(data = data.frame(c(3:length(x)), predict(tacfit)),
            aes(x = c(3:length(x)), y = predict(tacfit)) ,
            color = 'green') +
  labs(x = "Days", y = "Returns")
ggsave("./example6.pdf", height = 7 , width = 7 * 1.5)


#Example 7
set.seed(12345)
x <- seq(from = 0,
         to = 10,
         length.out = 500)
y <-
  3 * exp(-0.85 * x) + 1.5 * sin(2 * x) + 1 + rnorm(length(x), mean = 0, sd = 0.3)
data <- data.frame(x, y)
tol <- 1e-7
N <- 10
form <- 'y ~ a1*exp(-k1*x) + a2*sin(b1*x) + a3'
nlparam <- list(k1 = c(0.1, 1), b1 = c(1.1, 5))
tacfit <-
  nls_tac(
    formula = form,
    data = data,
    functions = c('exp(-k1*x)', 'sin(b1*x)', '1'),
    nlparam = nlparam,
    N = N,
    tol = tol,
    parallel = FALSE
  )



##  Example 8
##  This code reproduces Figure 8 with the benchmarking of the parallelization
##  This code will take a lot to process (several hours)
##  EXECUTE WITH CAUTION

## Data
x <- seq(from = 0,
         to = 50,
         length.out = 1000)
y2 <- 3 * exp(-0.12 * x) + 0.6 * exp(-3.05 * x) + 5 + 0.1 * rnorm(length(x))
y3 <-
  3 * exp(-0.12 * x) + 0.6 * exp(-3.05 * x) + 10.5 * exp(-11.4 * x) +   5 + 0.1 *
  rnorm(length(x))
df2 <- data.frame(time = x, Temp = y2)
df3 <- data.frame(time = x, Temp = y3)
# The nonlinear parameter list (with lower and upper values)
nlparam2 <- list(b1 = c(0, 2), b2 = c(0, 8))

nlparam3 <- list(b1 = c(0, 2),
                 b2 = c(0, 8),
                 b3 = c(5, 20))

Ns <- seq(from = 5, to = 70, by = 5)

times2 <- numeric(length(Ns) + 1)
timesparal2 <- numeric(length(Ns) + 1)
times3 <- numeric(length(Ns) + 1)
timesparal3 <- numeric(length(Ns) + 1)

for (i in 1:length(Ns)) {
  print(Ns[i])
  tt <-
    system.time(
      nls_tac(
        'Temp ~ a1*exp(-b1*time) +  a2*exp(-b2*time)  + a4',
        data = df2,
        nlparam = nlparam2,
        N = Ns[i],
        tol = 1e-4,
        parallel = TRUE
      )
    )
  print(tt)
  timesparal2[i + 1] <- tt[3]
  tt <-
    system.time(
      nls_tac(
        'Temp ~ a1*exp(-b1*time) +  a2*exp(-b2*time) + a4',
        data = df2,
        nlparam = nlparam2,
        N = Ns[i],
        tol = 1e-4,
        parallel = FALSE
      )
    )
  print(tt)
  times2[i + 1] <- tt[3]
  tt <-
    system.time(
      nls_tac(
        'Temp ~ a1*exp(-b1*time) +  a2*exp(-b2*time) + a3*exp(-b3*time) + a4',
        data = df3,
        nlparam = nlparam3,
        N = Ns[i],
        tol = 1e-4,
        parallel = TRUE
      )
    )
  print(tt)
  timesparal3[i + 1] <- tt[3]
  tt <-
    system.time(
      nls_tac(
        'Temp ~ a1*exp(-b1*time) +  a2*exp(-b2*time) + a3*exp(-b3*time) + a4',
        data = df3,
        nlparam = nlparam3,
        N = Ns[i],
        tol = 1e-4,
        parallel = FALSE
      )
    )
  print(tt)
  times3[i + 1] <- tt[3]
}

parbench <-
  rbind(
    data.frame(
      N = c(0, Ns),
      Time = times2,
      Mode = "Non-Parallel",
      Param = "2 Exponentials"
    ),
    data.frame(
      N = c(0, Ns),
      Time = timesparal2,
      Mode = "Parallel",
      Param = "2 Exponentials"
    ),
    data.frame(
      N = c(0, Ns),
      Time = times3,
      Mode = "Non-Parallel",
      Param = "3 Exponentials"
    ),
    data.frame(
      N = c(0, Ns),
      Time = timesparal3,
      Mode = "Parallel",
      Param = "3 Exponentials"
    )
  )


parbench %>% ggplot(aes(x = N, y = Time)) + geom_point(aes(col = Mode)) +
  geom_line(aes(col = Mode)) + theme_bw() + xlab("N") + ylab("Time (s)") +
  facet_wrap( ~ Param, scales = "free")

ggsave(
  "./bechmark23.pdf",
  width = 10,
  height = 5,
  units = "in",
  dpi = 300
)
