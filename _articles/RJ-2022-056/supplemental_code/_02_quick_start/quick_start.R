################################################################################
## Quick start code (with an extra `pmvss_mc` run as in the body of the paper)##
################################################################################
library(mvpd)
set.seed(10)
shape_matrix <- structure(c(1, 0.9, 0.9, 0.9, 0.9,
                            0.9, 1, 0.9, 0.9, 0.9,
                            0.9, 0.9, 1, 0.9, 0.9,
                            0.9, 0.9, 0.9, 1, 0.9,
                            0.9, 0.9, 0.9, 0.9, 1),
                          .Dim = c(5L, 5L))
X <- mvpd::rmvss(n=5000, alpha=1.7, Q=shape_matrix)
copula::pairs2(X)
fitmv <- mvpd::fit_mvss(X)
fitmv
mvpd::dmvss(x=fitmv$univ_deltas,
            alpha=fitmv$mult_alpha,
            Q=fitmv$mult_Q_posdef,
            delta=fitmv$univ_deltas)[1]
mvpd::pmvss_mc(lower=rep(-2,5),
               upper=rep( 2,5),
               alpha=fitmv$mult_alpha,
               Q=fitmv$mult_Q_posdef,
               delta=fitmv$univ_deltas,
               n=10000)
mvpd::pmvss_mc(lower=rep(-2,5),
               upper=rep( 2,5),
               alpha=fitmv$mult_alpha,
               Q=fitmv$mult_Q_posdef,
               delta=fitmv$univ_deltas,
               n=10000)
mvpd::pmvss(lower=rep(-2,5),
            upper=rep( 2,5),
            alpha=fitmv$mult_alpha,
            Q=fitmv$mult_Q_posdef,
            delta=fitmv$univ_deltas,
            abseps.pmvnorm = 1e-4,
            maxpts.pmvnorm = 25000*10,
            abs.tol.si = 1e-2)[1]
################################################################################
## Bonus Example: mvpd vs. mvgb                                               ##
################################################################################
set.seed(321)
library(mvgb)
tictoc::tic() ## mvgb takes about 10 secs.
gb_4digits <-
  mvgb::pmvss(lower=rep(-2,5),
              upper=rep( 2,5),
              alpha=fitmv$mult_alpha,
              Q=fitmv$mult_Q_posdef,
              delta=fitmv$univ_deltas,
              abseps = 1e-4,
              maxpts = 25000*350)
tictoc::toc()
gb_4digits[c("value","inform","error")]
tictoc::tic() ## mvpd takes about 10 MINUTES.
pd_4digits <-
  mvpd::pmvss(lower=rep(-2,5),
              upper=rep( 2,5),
              alpha=fitmv$mult_alpha,
              Q=fitmv$mult_Q_posdef,
              delta=fitmv$univ_deltas,
              abseps.pmvnorm = 1e-6,
              maxpts.pmvnorm = 25000*1000,
              abs.tol.si = 1e-4)
tictoc::toc()
pd_4digits[1]

gb_4digits[c("value","inform","error")]
pd_4digits[1]
