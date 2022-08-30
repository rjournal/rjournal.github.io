library(etrm)
library(gridExtra)

# code examples used in the submitted paper
################################################################################
# 1. Section 'The "MSFC" class with examples'
# 2. Section 'The strategy classes with examples'
################################################################################

# start 1. Section 'The "MSFC" class with examples'
################################################################################

# load etrm package data
data(powfutures130513)
data(powpriors130513)

# instance of MSFC class with prior
fwd.fut.wpri <- msfc(tdate = as.Date("2013-05-13"),
                     include = powfutures130513$Include,
                     contract = powfutures130513$Contract,
                     sdate = powfutures130513$Start,
                     edate = powfutures130513$End,
                     f = powfutures130513$Closing,
                     prior = powpriors130513$mod.prior)

# instance of MSFC class without prior
fwd.fut.npri <- msfc(tdate = as.Date("2013-05-13"),
                     include = powfutures130513$Include,
                     contract = powfutures130513$Contract,
                     sdate = powfutures130513$Start,
                     edate = powfutures130513$End,
                     f = powfutures130513$Closing,
                     prior = 0)

# the generic plot() method
pw <- plot(fwd.fut.wpri, ylab = "EUR/MWh", legend = "")
pn <- plot(fwd.fut.npri, ylab = "EUR/MWh", legend = "")

# combine plots
gridExtra::grid.arrange(pw, pn)

# msfc summary() method
summary(fwd.fut.wpri)

# msfc show() method
head(show(fwd.fut.wpri), 20)[1:5]

# msfc slots
slotNames(fwd.fut.wpri)

# msfc spline knot points
fwd.fut.npri@KnotPoints

# coefficients for the first spline polynomial
fwd.fut.npri@SplineCoef[[1]]

# end 1. Section 'The "MSFC" class with examples'
################################################################################


# start 2. Section 'The strategy classes with examples'
################################################################################

# load etrm package data
data(powcal)

# data frame with final 500 trading days for CAL-06 contract

dat06 <- data.frame(Date = tail(powcal$Date[!is.na(powcal$Date)], 500),
                    CAL06 = tail(powcal$`CAL-06`[!is.na(powcal$`CAL-06`)], 500))

# instance of the OBPI class, buyer
cal06_obpi_b <- obpi(q = 30,
                     tdate = dat06$Date,
                     f = dat06$CAL06,
                     k = dat06$CAL06[1],
                     vol = 0.2,
                     r = 0,
                     tdays = 250,
                     daysleft = 500,
                     tcost = 0,
                     int = TRUE)

# the generic plot() method
plot(cal06_obpi_b, title = "", legend = "right", ylab.1 = "EUR/MWh")

# strategy class summary() method
summary(cal06_obpi_b)

# strategy class show() method
head(show(cal06_obpi_b))

# instance of the OBPI class, seller
cal06_obpi_s <- obpi(q = - 30,
                     tdate = dat06$Date,
                     f = dat06$CAL06,
                     k = dat06$CAL06[1],
                     vol = 0.2,r = 0,
                     tdays = 250,
                     daysleft = 500,
                     tcost = 0,
                     int = TRUE)

# the generic plot() method
plot(cal06_obpi_s, title = "", legend = "right", ylab.1 = "EUR/MWh")

# strategy class summary() method
summary(cal06_obpi_s)

# strategy class show() method
head(show(cal06_obpi_s))


# end 2. Section 'The strategy classes with examples'
################################################################################
