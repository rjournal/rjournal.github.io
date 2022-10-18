# install.packages("wavScalogram")
library(wavScalogram)

# Code for obtaining Figure 1 ------------

t <- seq(from = -4, to = 4, l = 1000)
w0 <- 6
m <- 4
morlet <- pi^(-0.25)*exp(complex(real = 0, imaginary = 1)*w0*t)*exp(-0.5*t^2)
paul <- ((2*complex(real = 0, imaginary = 1))^m*factorial(m))/(sqrt(pi*factorial(2*m)))*(1-complex(real = 0, imaginary = 1)*t)^(-(m+1))
dog <- -1/(sqrt(gamma(2.5))) * (t^2-1)*exp(-0.5*t^2)
cexcoef <- 2
cexaxis <- 1.75
par(mfrow = c(1,3))
plot(t, Re(morlet), type = "l", lwd = 2, xlab = "Time", ylab = "", ylim = range(c(Re(morlet),Im(morlet))), axes = FALSE, main = "Morlet",cex.main =cexcoef, cex.lab =cexcoef)
lines(t, Im(morlet), lty = 2, lwd = 2)
axis(1, seq(-4,4,2), cex.axis = cexaxis, lwd = 2)
axis(2, seq(-0.5,0.5,0.5), cex.axis = cexaxis, lwd = 2)
box(lwd = 2)
plot(t, Re(paul), type = "l", lwd = 2, xlab = "Time", ylab = "", ylim = range(c(Re(paul),Im(paul))), axes = FALSE, main = "Paul",cex.main =cexcoef,  cex.lab =cexcoef)
lines(t, Im(paul), lty = 2, lwd = 2)
axis(1, seq(-4,4,2), cex.axis = cexaxis, lwd = 2)
axis(2, seq(-0.5,1,0.5), cex.axis = cexaxis, lwd = 2)
box(lwd = 2)
plot(t, Re(dog), type = "l", lwd = 2, xlab = "Time", ylab = "", ylim = range(c(Re(dog),Im(dog))), axes = FALSE, main = "DoG",cex.main =cexcoef, cex.lab =cexcoef)
axis(1, seq(-4,4,2), cex.axis = cexaxis, lwd = 2)
axis(2, seq(-0.4,0.8,0.4), cex.axis = cexaxis, lwd = 2)
box(lwd = 2)
par(mfrow = c(1,1))
# Computing the CWT of a time series x at a given set of scales ----

h <- 0.1
N <- 1000
time <- seq(from = 0, to = N * h, by = h)
signal <- sin(pi * time)
scales <- seq(from = 0.5, to = 4, by = 0.05)
cwt <- cwt_wst(signal = signal, dt = h,
               scales = scales, powerscales = FALSE,
               wname = "DOG", wparam = 6)

# Computing the CWT with base 2 power scales ----

scales <- c(0.5, 4, 16)
cwt <- cwt_wst(signal = signal, dt = h, 
               scales = scales, powerscales = TRUE)

# Code for obtaining Figure 2 ---------

x <- seq(from = -3*pi-1, to = 3*pi+1, l = 200)
y1 <- sin(x)*(x>=-pi & x<= pi)
y0 = y1
y0[x< -pi | x> pi] <- NA
y2 <- sin(x)
y3 <- sin(x)*(x>=-pi & x<= pi) + sin(-x) * ((x< -pi & x>= -3*pi) | (x> pi & x<= 3*pi)) + sin(x)* (x < -3*pi | x > 3*pi)
xlim <- c(-3,-1,1,3)*pi
cexmain <- 2.5
cexaxis <- 1.75
par(mfrow = c(3,1))
plot(x, y1, type  = "l", lwd = 2, xlab = "", ylab = "", main = "Zero padding", lty = 3, axes = FALSE, cex.main =cexmain)
axis(1, seq(-3 * pi, 3 * pi, 2 * pi),
     c(expression(-3 * pi), expression(-pi), expression(pi), expression(3 * pi)),
     cex.axis = cexaxis, lwd = 2)
axis(2, -1:1, cex.axis = cexaxis, lwd = 2)
box(lwd = 2)
lines(x,y0, col = "red", lwd = 3)
abline(v = xlim, lty = 2, lwd = 2)
plot(x,y2, type = "l", lwd = 2,xlab = "", ylab = "",main = "Periodization", lty = 3, axes = FALSE, cex.main =cexmain)
axis(1, seq(-3 * pi, 3 * pi, 2 * pi),
     c(expression(-3 * pi), expression(-pi), expression(pi), expression(3 * pi)),
     cex.axis = cexaxis, lwd = 2)
axis(2, -1:1, cex.axis = cexaxis, lwd = 2)
box(lwd = 2)
lines(x,y0, col = "red", lwd = 3)
abline(v = xlim, lty = 2, lwd = 2)
plot(x,y3, type = "l", lwd = 2,xlab = "", ylab = "", main = "Symmetric catenation", lty = 3, axes = FALSE, cex.main =cexmain)
axis(1, seq(-3 * pi, 3 * pi, 2 * pi),
     c(expression(-3 * pi), expression(-pi), expression(pi), expression(3 * pi)),
     cex.axis = cexaxis, lwd = 2)
axis(2, -1:1, cex.axis = cexaxis, lwd = 2)
box(lwd = 2)
title(xlab = "Time", line = 4, cex.lab = cexmain)
lines(x,y0, col = "red", lwd = 3)
abline(v = xlim, lty = 2, lwd = 2)
par(mfrow = c(1,1))

# Computing the CWT with custom wavelet radius ----

cwt <- cwt_wst(signal = signal, dt = h,
               wname = "DOG", wparam = 6, waverad = 2)

# Code for obtaining Figure 3 (a) and Figure 3 (b)----------

h <-  1 / 12
time1 <- seq(from = 1920, to = 1970 - h, by = h)
time2 <- seq(from = 1970, to = 2020, by = h)
signal <- c(sin(pi * time1), sin(pi * time2 / 2))
cwt_a <- cwt_wst(signal = signal, dt = h, time_values = c(time1, time2))
cwt_b <- cwt_wst(signal = signal, dt = h, time_values = c(time1, time2),
                 energy_density = TRUE)

# Code for obtaining Figure 4 (a) and Figure 4 (b) ----------

sc_a <- scalogram(signal = signal, dt = h, 
                  energy_density = FALSE)
sc_b <- scalogram(signal = signal, dt = h)

# Code for obtaining Figure 5 (a) and Figure 5 (b) ------------

wsc <- windowed_scalogram(signal = signal, dt = h,
                          windowrad = 72, delta_t = 6,
                          time_values = c(time1, time2))

wsc <- windowed_scalogram(signal = signal, dt = h,
                          windowrad = 72, delta_t = 6,
                          border_effects = "INNER",
                          time_values = c(time1, time2))

# Code for obtaining Figure 6 ---------

N <- 1500
time <- 0:N
signal1 <- rnorm(n = N + 1, mean = 0, sd = 0.2) + sin(time / 10)
signal2 <- rnorm(n = N + 1, mean = 0, sd = 0.2) + sin(time / 10)
signal2[500:1000] = signal2[500:1000] + sin((500:1000) / 2)
sc1 <- scalogram(signal1)
sc2 <- scalogram(signal2)
Period <- sc2$scales * sc2$fourierfactor
C <- 0.2
sc2 <- scalogram(signal2, main = "Compensation Method")
lines(log2(Period), C + (1 - C / max(sc2$scalog)) * sc2$scalog, col = "red", lty = 2, lwd = 2)
abline(h = C, lty = 3, lwd = 2)

# Computing commutative WSD of two signals  -------
# The result is Figure 7 but without the Monte Carlo significant contours
set.seed(12345)

N <- 1500
time <- 0:N
signal1 <- rnorm(n = N + 1, mean = 0, sd = 0.2) + sin(time / 10)
signal2 <- rnorm(n = N + 1, mean = 0, sd = 0.2) + sin(time / 10)
signal2[500:1000] = signal2[500:1000] + sin((500:1000) / 2)
wsd <- wsd(signal1 = signal1, signal2 = signal2,
           windowrad = 75, rdist = 14)

# Code for obtaining Figure 7 (with significant contours) ---------
# This make take a while (few minutes)

wsd <- wsd(signal1 = signal1, signal2 = signal2,
           mc_nrand = 100, parallel = TRUE)

# Computation of the Scale Index ----
set.seed(12345) # For reproducibility
N <- 999
h <- 1 / 8
time <- seq(from = 0, to = N * h, by = h)
signal_si <- sin(pi * time) + rnorm(n = N + 1, mean = 0, sd = 2)
s0 <- 1
s1 <- 4
si <- scale_index(signal = signal_si, dt = h,
                  scales = c(s0, 2 * s1, 24), s1 = s1,
                  border_effects = "INNER", makefigure = FALSE)

# Computation of the Scale Index with custom  base 2 power scale interval ----

maxs1 <- 4
si <- scale_index(signal = signal_si, dt = h, scales = c(s0, 2 * maxs1, 24),
                  s1 = pow2scales(c(s0, maxs1, 24)),
                  border_effects = "INNER")

# Code for obtaining a figure like Figure 8 (there is a random noise that will ----
# change the actual figure)

si <- scale_index(signal = signal_si, dt = h, border_effects = "INNER")

# Computation of the Scale Index with custom linear scale interval ----

si <- scale_index(signal = signal_si, dt = h,
                  s1 = seq(from = s0, to = maxs1, by = 0.1),
                  powerscales = FALSE, border_effects = "INNER") 

# Code for obtaining Figure 9 (a) and Figure 9 (b) -----
set.seed(12345)

N <- 1000
nrand <- 100
X <- matrix(rnorm(N * nrand), nrow = N, ncol = nrand)
scales = pow2scales(c(2, 128, 24))
ns = length(scales)
sc_list <- apply(X, 2, scalogram, scales = scales, border_effects = "INNER",
                 energy_density = FALSE, makefigure = FALSE)
sc_matrix <- matrix(unlist(lapply(sc_list, "[[", "scalog")),
                    nrow = ns, ncol = nrand)
sc_mean <- apply(sc_matrix, 1, mean)
s1 = pow2scales(c(2, 64, 24))
si_mean <- scale_index(scalog = sc_mean, scales = scales, s1 = s1,
                       figureperiod = FALSE, plot_scalog = TRUE)

# Code for obtaining Figure 10 -----
set.seed(12345)

s0 <- 1
s1 <- 4
signal1_wsi <- sin(pi * time[1:500]) + rnorm(n = 500, mean = 0, sd = 2)
signal2_wsi <- sin(pi * time[501:1000] / 2) + rnorm(n = 500, mean = 0, sd = 0.5)
signal_wsi <- c(signal1_wsi, signal2_wsi)
wsi <- windowed_scale_index(signal = signal_wsi, dt = h,
                            scales = c(s0, 2 * s1, 24), s1 = s1,
                            windowrad = 50,
                            time_values = time)

# Code for obtaining Figure 11 ----

wsi <- windowed_scale_index(signal = signal_wsi, dt = h,
                            time_values = time)

# Code for obtaining Figure 12 (a) and Figure 12 (b) -----
set.seed(12345)

N <- 1000
nrand <- 100
X <- matrix(rnorm(N * nrand), nrow = N, ncol = nrand)
scales = pow2scales(c(2, 128, 24))
ns = length(scales)
wsc_list <- apply(X, 2, windowed_scalogram, scales = scales,
                  energy_density = FALSE, makefigure = FALSE)
tcentral <- wsc_list[[1]]$tcentral
ntc <- length(tcentral)
wsc_matrix <- array(unlist(lapply(wsc_list, "[[", "wsc")), c(ntc, ns, nrand))
wsc_mean <- apply(wsc_matrix, 1:2, mean)
wsc_coi <- wsc_list[[1]]$coi_maxscale
wsi_mean <- windowed_scale_index(wsc = wsc_mean, wsc_coi = wsc_coi,
                                 scales = scales, time_values = tcentral,
                                 figureperiod = FALSE, plot_wsc = TRUE)

# Code for obtaining the plots in Figure 13 ----

library(TSclust)
data("interest.rates")
returns <- apply(interest.rates, MARGIN = 2, function(x) diff(log(x)))
Nsignals <- ncol(returns)
countries <- colnames(returns)

wsd(signal1 = returns[, 6],
    signal2 = returns[, 13], time_values = time(interest.rates)[-1],
    main = "-log2(WSD) Netherlands - Finland", zlim = c(-5, 5.7))
wsd(signal1 = returns[, 6],
    signal2 = returns[, 2], time_values = time(interest.rates)[-1],
    main = "-log2(WSD) Netherlands - Spain", zlim = c(-5, 5.7))
wsd(signal1 = returns[, 6],
    signal2 = returns[, 18], time_values = time(interest.rates)[-1],
    main = "-log2(WSD) Netherlands - Japan", zlim = c(-5, 5.7))

# Code for obtaining  Figure 14 -----

M <- Nsignals * (Nsignals - 1) / 2 # Number of pairings
auxpair <- vector(mode = "list", M)
k <- 1
for (i in 1:(Nsignals - 1)) {
  for (j in (i + 1):Nsignals) {
    auxpair[[k]] <- c(i, j)
    k <- k + 1
  }
}
fwsd <- function(x) wsd(signal1 = returns[, x[1]],
                        signal2 = returns[, x[2]],
                        makefigure = FALSE)
Allwsd <- lapply(auxpair, FUN = fwsd) # Takes a while
ntimes <- length(Allwsd[[1]]$tcentral)
nscales <- length(Allwsd[[1]]$scales)
area <- ntimes * nscales
meanwsd <- rep(0, M)
for (i in 1:M) {
  meanwsd[i] <- sum(Allwsd[[i]]$wsd) / area
}
d1 <- matrix(0, Nsignals, Nsignals)
d1[lower.tri(d1, diag = FALSE)] <- log2(meanwsd + 1)
dm1 <- as.dist(t(d1) + d1)
names(dm1) <- countries
plot(hclust(dm1), main = "Interest rates 1995-2012", xlab = "", sub = "")

# Code for obtaining Figure 15 (a) ----

cwt_wst(signal = sunspot.month, dt = 1/12,
        time_values = time(sunspot.month),
        energy_density = TRUE)

# Code for obtaining Figure 15 (b) ----

scalogram(signal = sunspot.month, dt = 1/12)

# Code for obtaining Figure 15 (c)----

windowed_scalogram(signal = sunspot.month, dt = 1/12,
                   time_values = time(sunspot.month))

# Code for obtaining Figure 16 (a)----

cwt_wst(signal = sunspot.month, dt = 1/12,
        time_values = time(sunspot.month))

# Code for obtaining Figure 16 (b)----

scalogram(signal = sunspot.month, dt = 1/12,
          energy_density = FALSE)

# Code for obtaining Figure 16 (c)----

windowed_scalogram(signal = sunspot.month, dt = 1/12,
                   time_values = time(sunspot.month),
                   energy_density = FALSE)

# Code for obtaining Figure 17 (a)----

scale_index(signal = sunspot.month, dt = 1/12)

# Code for obtaining Figure 17 (b)----

windowed_scale_index(signal = sunspot.month, dt = 1/12,
                     time_values = time(sunspot.month))


