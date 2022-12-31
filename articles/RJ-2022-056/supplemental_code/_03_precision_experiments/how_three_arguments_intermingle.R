
## try running this code for different decimal places.
## demonstrates that for abs.tol.si to be tought of
## as the desired decimal place
## and that decision requires abseps.pmvnorm to be
## set to 1e-2 times smaller
## and then that decision requires maxpts.pmvnorm
## to be increased by 10 for every additional
## digit abseps.pmvnorm is made smaller.
## the comments below show this code ran
## for several different decimal places.  The computational times are
##
##
## decimal.place abseps.pmnvorm   maxpts         Answer
## 1e-01         1e-03            25000
## 1e-02         1e-04            25000*10       (0.68    , 0.70    )
## 1e-03         1e-05            25000*100      (0.689   , 0.691   )
## 1e-04         1e-06            25000*1000     (0.6903  , 0.6905  )
## 1e-05         1e-07            25000*10000    (0.69043 , 0.69045 )
## 1e-06         1e-08            25000*100000   Error -- lower maxpts and retry
## 1e-06         1e-08            25000*85000    (0.690440, 0.690442)
shape_matrix <- structure(c(1, 0.9, 0.9, 0.9, 0.9,
                            0.9, 1, 0.9, 0.9, 0.9,
                            0.9, 0.9, 1, 0.9, 0.9,
                            0.9, 0.9, 0.9, 1, 0.9,
                            0.9, 0.9, 0.9, 0.9, 1),
                          .Dim = c(5L, 5L))


decimal.place <- 1e-5
 begg <- Sys.time()
 PD_general <-
    mvpd::pmvss(lower=rep(-2,5),
                  upper=rep( 2,5),
                  alpha=1.7,
                  Q=shape_matrix,
                  abseps.pmvnorm = decimal.place*1e-2,
                  maxpts.pmvnorm = 25000 * (1e-3 / (decimal.place*1e-2) ),
                  abs.tol.si = decimal.place)
 endd <- Sys.time()
 endd-begg
 PD_general
 trunc(PD_general$value* 1/decimal.place)*decimal.place + c(-1,1)*decimal.place



## >  decimal.place <- 1e-2
## >  begg <- Sys.time()
## >  PD_general <-
## +     mvpd::pmvss(lower=rep(-2,5),
## +                   upper=rep( 2,5),
## +                   alpha=1.7,
## +                   Q=shape_matrix,
## +                   abseps.pmvnorm = decimal.place*1e-2,
## +                   maxpts.pmvnorm = 25000 * (1e-3 / (decimal.place*1e-2) ),
## +                   abs.tol.si = decimal.place)
## >  endd <- Sys.time()
## >  endd-begg
## Time difference of 3.175949 secs
## >  PD_general
## 0.6904418 with absolute error < 0.0096
## >  trunc(PD_general$value* 1/decimal.place)*decimal.place + c(-1,1)*decimal.place
## [1] 0.68 0.70

## > decimal.place <- 1e-3
## >  begg <- Sys.time()
## >  PD_general <-
## +     mvpd::pmvss(lower=rep(-2,5),
## +                   upper=rep( 2,5),
## +                   alpha=1.7,
## +                   Q=shape_matrix,
## +                   abseps.pmvnorm = decimal.place*1e-2,
## +                   maxpts.pmvnorm = 25000 * (1e-3 / (decimal.place*1e-2) ),
## +                   abs.tol.si = decimal.place)
## >  endd <- Sys.time()
## >  endd-begg
## Time difference of 22.0104 secs
## >  PD_general
## 0.6904413 with absolute error < 0.00016
## >  trunc(PD_general$value* 1/decimal.place)*decimal.place + c(-1,1)*decimal.place
## [1] 0.689 0.691

## > decimal.place <- 1e-4
## >  begg <- Sys.time()
## >  PD_general <-
## +     mvpd::pmvss(lower=rep(-2,5),
## +                   upper=rep( 2,5),
## +                   alpha=1.7,
## +                   Q=shape_matrix,
## +                   abseps.pmvnorm = decimal.place*1e-2,
## +                   maxpts.pmvnorm = 25000 * (1e-3 / (decimal.place*1e-2) ),
## +                   abs.tol.si = decimal.place)
## >  endd <- Sys.time()
## >  endd-begg
## Time difference of 3.721679 mins
## >  PD_general
## 0.6904413 with absolute error < 1.6e-07
## >  trunc(PD_general$value* 1/decimal.place)*decimal.place + c(-1,1)*decimal.place
## [1] 0.6903 0.6905

## > decimal.place <- 1e-5
## >  begg <- Sys.time()
## >  PD_general <-
## +     mvpd::pmvss(lower=rep(-2,5),
## +                   upper=rep( 2,5),
## +                   alpha=1.7,
## +                   Q=shape_matrix,
## +                   abseps.pmvnorm = decimal.place*1e-2,
## +                   maxpts.pmvnorm = 25000 * (1e-3 / (decimal.place*1e-2) ),
## +                   abs.tol.si = decimal.place)
## >  endd <- Sys.time()
## >  endd-begg
## Time difference of 25.42902 mins
## >  PD_general
## 0.6904413 with absolute error < 6.1e-08
## >  trunc(PD_general$value* 1/decimal.place)*decimal.place + c(-1,1)*decimal.place
## [1] 0.69043 0.69045

## > decimal.place <- 1e-6
## >  begg <- Sys.time()
## >  PD_general <-
## +     mvpd::pmvss(lower=rep(-2,5),
## +                   upper=rep( 2,5),
## +                   alpha=1.7,
## +                   Q=shape_matrix,
## +                   abseps.pmvnorm = decimal.place*1e-2,
## +                   maxpts.pmvnorm = 25000 * (1e-3 / (decimal.place*1e-2) ),
## +                   abs.tol.si = decimal.place)
## Error in probval.GenzBretz(algorithm, n, df, lower, upper, infin, corr,  :
##   NAs in foreign function call (arg 8)
## In addition: Warning message:
## In probval.GenzBretz(algorithm, n, df, lower, upper, infin, corr,  :

##  Error in probval.GenzBretz(algorithm, n, df, lower, upper, infin, corr,  :
##   NAs in foreign function call (arg 8) >  endd <- Sys.time()
## >  endd-begg
## Time difference of 0.00813508 secs



## decimal.place <- 1e-6
##  begg <- Sys.time()
##  PD_general <-
##     mvpd::pmvss(lower=rep(-2,5),
##                   upper=rep( 2,5),
##                   alpha=1.7,
##                   Q=shape_matrix,
##                   abseps.pmvnorm = decimal.place*1e-2,
##                   ##maxpts.pmvnorm = 25000 * (1e-3 / (decimal.place*1e-2) ),
##                   maxpts.pmvnorm = 25000 * 85000,
##                   abs.tol.si = decimal.place)
## >  endd <- Sys.time()
## >  endd-begg
## Time difference of 4.310425 hours
## >  PD_general
## 0.6904413 with absolute error < 4.3e-08
## >  trunc(PD_general$value* 1/decimal.place)*decimal.place + c(-1,1)*decimal.place
## [1] 0.690440 0.690442
