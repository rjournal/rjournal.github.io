#######################################################################################################
#  R-code for reproducing the simulation study (Section Simulation study) and the analysis (Section Examples and applications) of the article
#
#  Package wsbackfit for Smooth Backfitting Estimation of Generalized Structured Models
#
#  by J Roca Pardiñas, MX Rodriguez Alvarez, S Sperlich
#
#  Contact: J Roca Pardiñas - roca@uvigo.es
#####################################################################################################
# Load the packake (v 1.0-4)
library(wsbackfit)

#####################################################################################################
#####################################################################################################
# Section: Simulation study
#####################################################################################################
#####################################################################################################
	# Number of runs
	n.sim <- 500

	# Sample size
	n <- 500

	# Kernel function, either "Gaussian" or "Epanechnikov"
	kernel <- "Gaussian"

	##############################################################################################
	# Additive model: Gaussian
	##############################################################################################
		# Grid and data.frame for predictions (to obtain the estimated function at particular values)
		n.p <- 100	
		df.n <- data.frame(x1 = seq(0, 1, l = n.p), x2 = seq(-10, 1.5, l = n.p))

		# To save results
		fit.ws.1 <- fit.ws.2  <- matrix(ncol = n.sim, nrow = n.p)
		mse.ws <- ctime.ws <- vector(l = n.sim)
		
		set.seed(123)	
		for (i in 1:n.sim) {
			print(i)
			x1 <- runif(n, 0, 1)
			x2 <- runif(n,-10, 1.5)
			y <- 2 + 3*x1^2 + 0.01*x2^3+ rnorm(n, 0, 0.5)
			df <- data.frame(x1 = x1, x2 = x2, y = y)
			
			start <- proc.time()[3]
			res.sback <- sback(y ~ sb(x1) + sb(x2), data = df, kbin = 30, kernel = kernel)
			p.res.sback <- predict(res.sback, newdata = df.n)
			end <- proc.time()[3]

			ctime.ws[i] <- end - start
			fit.ws.1[,i] <- p.res.sback$peffects[,1] + res.sback$coeff[2]*df.n$x1
			fit.ws.2[,i] <- p.res.sback$peffects[,2] + res.sback$coeff[3]*df.n$x2
			mse.ws[i] <- mean((y-res.sback$fitted.values)^2)

		}
		##############################################################################
		# Plot the results
		# We first center the estimates to compare with the true functions
		##############################################################################
			fit.ws.1.c <- fit.ws.1
			fit.ws.2.c <- fit.ws.2	
			for(i in 1:n.sim) {
				fit.ws.1.c[,i] <- fit.ws.1[,i] - mean(fit.ws.1[,i])
				fit.ws.2.c[,i] <- fit.ws.2[,i] - mean(fit.ws.2[,i])
			}			
			op <- par(mfrow = c(1,2))
			# Function 1
				# Mean
				fit.ws.m <- apply(fit.ws.1.c, 1, mean)
				# Quantiles
				fit.ws.25 <- apply(fit.ws.1.c, 1, quantile, 0.025)
				fit.ws.975 <- apply(fit.ws.1.c, 1, quantile, 0.975)
				
				plot(df.n$x1, fit.ws.m, type = "l", col = 1, lty = 2, lwd = 2, 
					xlab = expression(x[1]), ylab = "", 
					main = expression("Additive effect for"~x[1]),
					cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
				lines(df.n$x1, fit.ws.25, lty = 2, col = 1)
				lines(df.n$x1, fit.ws.975, lty = 2, col = 1)

				# Theoretical
				lines(df.n$x1, 3*df.n$x1^2 - mean(3*df.n$x1^2), lwd = 2)
			
			# Function 2
				# Mean
				fit.ws.m <- apply(fit.ws.2.c, 1, mean)
				# Quantiles
				fit.ws.25 <- apply(fit.ws.2.c, 1, quantile, 0.025)
				fit.ws.975 <- apply(fit.ws.2.c, 1, quantile, 0.975)

				plot(df.n$x2, fit.ws.m, type = "l", col = 1, lty = 2, lwd = 2, 
					xlab = expression(x[2]), ylab = "", 
					main = expression("Additive effect for"~x[2]),
					cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5)
				lines(df.n$x2, fit.ws.25, lty = 2, col = 1)
				lines(df.n$x2, fit.ws.975, lty = 2, col = 1)

				# Theoretical
				lines(df.n$x2, 0.01*df.n$x2^3 - mean(0.01*df.n$x2^3), lwd = 2)
			par(op)

	##############################################################################################
	# Additive model: Bernoulli
	##############################################################################################
		# Grid and data.frame for predictions (to obtain the estimated function at particular values)
		n.p <- 100	
		df.n <- data.frame(x1 = seq(0, 1, l = n.p), x2 = seq(-10, 1.5, l = n.p))

		# To save results
		fit.ws.1 <- fit.ws.2  <- matrix(ncol = n.sim, nrow = n.p)
		mse.ws <- ctime.ws <- vector(l = n.sim)
		
		set.seed(123)	
		for (i in 1:n.sim) {
			print(i)
			x1 <- runif(n, 0, 1)
			x2 <- runif(n,-10, 1.5)
			eta <- (2 + 3*x1^2 + 0.01*x2^3)/4
			y <- rbinom(n, 1, binomial()$linkinv(eta))
			df <- data.frame(x1 = x1, x2 = x2, y = y)
			
			start <- proc.time()[3]
			res.sback <- sback(y ~ sb(x1) + sb(x2), data = df, kbin = 30, kernel = kernel, family = "binomial")
			p.res.sback <- predict(res.sback, newdata = df.n)
			end <- proc.time()[3]

			ctime.ws[i] <- end - start
			fit.ws.1[,i] <- p.res.sback$peffects[,1] + res.sback$coeff[2]*df.n$x1
			fit.ws.2[,i] <- p.res.sback$peffects[,2] + res.sback$coeff[3]*df.n$x2
			mse.ws[i] <- mean((binomial()$linkinv(eta)-res.sback$fitted.values)^2)

		}
		##############################################################################
		# Plot the results
		# We first center the estimates to compare with the true functions
		##############################################################################
			fit.ws.1.c <- fit.ws.1
			fit.ws.2.c <- fit.ws.2	
			for(i in 1:n.sim) {
				fit.ws.1.c[,i] <- fit.ws.1[,i] - mean(fit.ws.1[,i])
				fit.ws.2.c[,i] <- fit.ws.2[,i] - mean(fit.ws.2[,i])
			}

			op <- par(mfrow = c(1,2))
			# Function 1
				# Mean
				fit.ws.m <- apply(fit.ws.1.c, 1, mean)
				# Quantiles
				fit.ws.25 <- apply(fit.ws.1.c, 1, quantile, 0.025)
				fit.ws.975 <- apply(fit.ws.1.c, 1, quantile, 0.975)
				
				plot(df.n$x1, fit.ws.m, type = "l", col = 1, lty = 2, lwd = 2, 
					xlab = expression(x[1]), ylab = "", 
					main = expression("Additive effect for"~x[1]),
					cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5, ylim = c(-1, 1))
				lines(df.n$x1, fit.ws.25, lty = 2, col = 1)
				lines(df.n$x1, fit.ws.975, lty = 2, col = 1)

				# Theoretical
				lines(df.n$x1, 3*df.n$x1^2/4 - mean(3*df.n$x1^2/4), lwd = 2)
			
			# Function 2
				# Mean
				fit.ws.m <- apply(fit.ws.2.c, 1, mean)
				# Quantiles
				fit.ws.25 <- apply(fit.ws.2.c, 1, quantile, 0.025)
				fit.ws.975 <- apply(fit.ws.2.c, 1, quantile, 0.975)

				plot(df.n$x2, fit.ws.m, type = "l", col = 1, lty = 2, lwd = 2, 
					xlab = expression(x[2]), ylab = "", 
					main = expression("Additive effect for"~x[2]),
					cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5, ylim = c(-2, 1))
				lines(df.n$x2, fit.ws.25, lty = 2, col = 1)
				lines(df.n$x2, fit.ws.975, lty = 2, col = 1)

				# Theoretical
				lines(df.n$x2, 0.01*df.n$x2^3/4 - mean(0.01*df.n$x2^3/4), lwd = 2)
			par(op)

	##############################################################################################
	# Varying coefficient model: Gaussian
	##############################################################################################
		# Grid and data.frame for predictions (to obtain the estimated function at particular values)
		n.p <- 100	
		df.n <- data.frame(z1 = 1, x1 = seq(0, 1, l = n.p), z2 = 1, x2 = seq(0, 1, l = n.p))

		# To save results
		fit.ws.1 <- fit.ws.2 <- matrix(ncol = n.sim, nrow = n.p)
		mse.ws <- ctime.ws <- vector(l = n.sim)
		
		set.seed(123)	
		for (i in 1:n.sim) {
			print(i)
			z1 <- runif(n,0,1)
			x1 <- runif(n,0,1)
			z2 <- runif(n,0,1)
			x2 <- runif(n,0,1)
			
			y <- 5*z1*sin(2*pi*x1)  + z2*x2 + rnorm(n, 0, 0.5)
			df <- data.frame(x1 = x1, x2 = x2, z1 = z1, z2 = z2, y = y)
			
			# wsbackfit
			start <- proc.time()[3]
			res.sback <- sback(y ~ sb(x1, by = z1) + sb(x2, by = z2), data = df, k = 30, kernel = kernel)
			p.res.sback <- predict(res.sback, newdata = df.n) 
			end <- proc.time()[3]
			ctime.ws[i] <- end - start
			mse.ws[i] <- mean((y-res.sback$fitted.values)^2)

			fit.ws.1[,i] <- p.res.sback$peffects[,1] + res.sback$coeff[4]*df.n$x1
			fit.ws.2[,i] <- p.res.sback$peffects[,2] + res.sback$coeff[5]*df.n$x2

		}
		##############################################################################
		# Plot the results
		# We first center the estimates to compare with the true functions
		##############################################################################

			fit.ws.1.c <- fit.ws.1
			fit.ws.2.c <- fit.ws.2	
			for(i in 1:n.sim) {
				fit.ws.1.c[,i] <- fit.ws.1.c[,i] - mean(fit.ws.1.c[,i])
				fit.ws.2.c[,i] <- fit.ws.2.c[,i] - mean(fit.ws.2.c[,i])
			}

			op <- par(mfrow = c(1,2))
			# Function 1
				# Mean
				fit.ws.m <- apply(fit.ws.1.c, 1, mean)
				
				# Quantiles
				fit.ws.25 <- apply(fit.ws.1.c, 1, quantile, 0.025)
				fit.ws.975 <- apply(fit.ws.1.c, 1, quantile, 0.975)

				plot(df.n$x1, fit.ws.m, type = "l",  col = 1, lty = 2, lwd = 2, 
					xlab = expression(x[1]), ylab = "", 
					main = expression("Varying coefficient as a function of"~x[1]),
					cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5, ylim = c(-5,5))
				lines(df.n$x1, fit.ws.25, lty = 2, col = 1)
				lines(df.n$x1, fit.ws.975, lty = 2, col = 1)

				lines(df.n$x1, 5*sin(2*pi*df.n$x1) - mean(5*sin(2*pi*df.n$x1)), lwd = 2)
			
			# Function 2
				# Mean
				fit.ws.m <- apply(fit.ws.2.c, 1, mean)

				# Quantiles
				fit.ws.25 <- apply(fit.ws.2.c, 1, quantile, 0.025)
				fit.ws.975 <- apply(fit.ws.2.c, 1, quantile, 0.975)
				
				plot(df.n$x2, fit.ws.m, type = "l",  col = 1, lty = 2, lwd = 2, 
					xlab = expression(x[2]), ylab = "", 
					main = expression("Varying coefficient as a function of"~x[2]),
					cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5, ylim = c(-1,1))
				lines(df.n$x2, fit.ws.25, lty = 2, col = 1)
				lines(df.n$x2, fit.ws.975, lty = 2, col = 1)
				
				lines(df.n$x2, df.n$x2 - mean(df.n$x2), lwd = 2)
			par(op)

	##############################################################################################
	# Varying coefficient model: binomial
	##############################################################################################
		# Grid and data.frame for predictions (to obtain the estimated function at particular values)
		n.p <- 100	
		df.n <- data.frame(z1 = 1, x1 = seq(0, 1, l = n.p), z2 = 1, x2 = seq(0, 1, l = n.p))

		# To save results
		fit.ws.1 <- fit.ws.2 <- matrix(ncol = n.sim, nrow = n.p)
		mse.ws <- ctime.ws <- vector(l = n.sim)
		
		set.seed(123)	
		for (i in 1:n.sim) {
			print(i)
			z1 <- runif(n,0,1)
			x1 <- runif(n,0,1)
			z2 <- runif(n,0,1)
			x2 <- runif(n,0,1)
			
			eta <- 5*z1*sin(2*pi*x1)  + z2*x2
			y <- rbinom(n, 1, binomial()$linkinv(eta))			
			df <- data.frame(x1 = x1, x2 = x2, z1 = z1, z2 = z2, y = y)
			
			# wsbackfit
			start <- proc.time()[3]
			res.sback <- sback(y ~ sb(x1, by = z1) + sb(x2, by = z2), data = df, k = 30, kernel = kernel, family = "binomial")
			p.res.sback <- predict(res.sback, newdata = df.n) 
			end <- proc.time()[3]
			ctime.ws[i] <- end - start
			mse.ws[i] <- mean((y-res.sback$fitted.values)^2)

			fit.ws.1[,i] <- p.res.sback$peffects[,1] + res.sback$coeff[4]*df.n$x1
			fit.ws.2[,i] <- p.res.sback$peffects[,2] + res.sback$coeff[5]*df.n$x2

		}
		##############################################################################
		# Plot the results
		# We first center the estimates to compare with the true functions
		##############################################################################
			fit.ws.1.c <- fit.ws.1
			fit.ws.2.c <- fit.ws.2	
			for(i in 1:n.sim) {
				fit.ws.1.c[,i] <- fit.ws.1.c[,i] - mean(fit.ws.1.c[,i])
				fit.ws.2.c[,i] <- fit.ws.2.c[,i] - mean(fit.ws.2.c[,i])
			}

			op <- par(mfrow = c(1,2))
			# Function 1
				# Mean
				fit.ws.m <- apply(fit.ws.1.c, 1, mean)
				
				# Quantiles
				fit.ws.25 <- apply(fit.ws.1.c, 1, quantile, 0.025)
				fit.ws.975 <- apply(fit.ws.1.c, 1, quantile, 0.975)

				plot(df.n$x1, fit.ws.m, type = "l",  col = 1, lty = 2, lwd = 2, 
					xlab = expression(x[1]), ylab = "", 
					main = expression("Varying coefficient as a function of"~x[1]),
					cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5, ylim = c(-6,6))
				lines(df.n$x1, fit.ws.25, lty = 2, col = 1)
				lines(df.n$x1, fit.ws.975, lty = 2, col = 1)

				lines(df.n$x1, 5*sin(2*pi*df.n$x1) - mean(5*sin(2*pi*df.n$x1)), lwd = 2)
			
			# Function 2
				# Mean
				fit.ws.m <- apply(fit.ws.2.c, 1, mean)

				# Quantiles
				fit.ws.25 <- apply(fit.ws.2.c, 1, quantile, 0.025)
				fit.ws.975 <- apply(fit.ws.2.c, 1, quantile, 0.975)
				
				plot(df.n$x2, fit.ws.m, type = "l",  col = 1, lty = 2, lwd = 2, 
					xlab = expression(x[2]), ylab = "", 
					main = expression("Varying coefficient as a function of"~x[2]),
					cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5, ylim = c(-2,2))
				lines(df.n$x2, fit.ws.25, lty = 2, col = 1)
				lines(df.n$x2, fit.ws.975, lty = 2, col = 1)
				
				lines(df.n$x2, df.n$x2 - mean(df.n$x2), lwd = 2)
			par(op)



#####################################################################################################
#####################################################################################################
# Section: Examples and applications
#####################################################################################################
#####################################################################################################
	#######################################################################################
	# Gaussian simulated data
	#######################################################################################
		set.seed(123)

		# Generate the data
		n <- 1000
		x1 <- runif(n)*4-2
		x2 <- runif(n)*4-2
		x3 <- runif(n)*4-2
		x4 <- runif(n)*4-2
		x5 <- as.numeric(runif(n)>0.6)

		g1 <- 2*sin(2*x1)
		g2 <- x2^2
		g3 <- 0
		g4 <- x4

		mu <- g1 + g2 + g3 + g4 + 1.5*x5
		err <- (0.5 + 0.5*x5)*rnorm(n)
		y <- mu + err

		df_gauss <- data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = as.factor(x5), y = y)

		# Fit the model with a fixed bandwidth for each covariate
		m0 <- sback(formula = y ~ x5 + sb(x1, h = 0.1) + sb(x2, h = 0.1) + sb(x3, h = 0.1) + sb(x4, h = 0.1), kbin = 30, data = df_gauss)

		names(m0)
		
		summary(m0)

		op <- par(mfrow = c(2,2))
		plot(m0, select = 1, ylim = c(-2.5,2.5))
		plot(m0, select = 2, ylim = c(-2.5,2.5))
		plot(m0, select = 3, ylim = c(-2.5,2.5))
		plot(m0, select = 4, ylim = c(-2.5,2.5))
		par(op)
		
		op <- par(mfrow = c(2,2))
		plot(m0, select = 1, composed = FALSE, ylim = c(-2.5,2.5))
		plot(m0, select = 2, composed = FALSE, ylim = c(-2.5,2.5))
		plot(m0, select = 3, composed = FALSE, ylim = c(-2.5,2.5))
		plot(m0, select = 4, composed = FALSE, ylim = c(-2.5,2.5))
		par(op)


		# Estimate the variance as a function of the binary covariate x5
		resid <- y - m0$fitted.values
		sig0 <- var(resid[x5 == 0])
		sig1 <- var(resid[x5 == 1])

		# Re-estimate the model but with weights inverse to the predicted variance.
		w <- x5/sig1 + (1-x5)/sig0

		m1 <- sback(formula = y ~ x5 + sb(x1, h = 0.1) + sb(x2, h = 0.1) + sb(x3, h = 0.1) + sb(x4, h = 0.1), weights = w, kbin = 30, data = df_gauss)

		summary(m1)

		# Fit the model selecting the bandwidths of each covariate via k-fold cross-validation
		m1cv <- sback(formula = y ~ x5 + sb(x1, h = -1) + sb(x2, h = -1) + sb(x3, h = -1) + sb(x4, h = -1), weights = w, kbin = 30, bw.grid = seq(0.01, 0.99, length = 30), KfoldCV = 5, data = df_gauss)

		summary(m1cv)

		# Fit the model selecting fixing the bandwidths for some covarate and selecting them for the remaining via k-fold cross-validation
		m2cv <- sback(formula = y ~ x5 + sb(x1, h = 0.1) + sb(x2, h = -1) + sb(x3, h = 0.1) + sb(x4, h = 0.1), weights = w, kbin = 30, KfoldCV = 5, data = df_gauss)
		summary(m2cv)

	#######################################################################################
	# Postoperative infection data
	#######################################################################################
		# Load the data
		data(infect)

		# Generalized varying coefficient model with binary respons
		m2 <- sback(formula = inf ~ sb(gluc, h = 10) + sb(gluc, by = linf, h = 10), data = infect, family = "binomial", kbin = 15)
		 
		summary(m2)

		
		op <- par(mfrow = c(1,3))
		plot(m2, ask = FALSE, cex.main = 2, cex = 2, cex.lab = 1.5, cex.axis = 2)
		par(op)
		
		op <- par(mfrow = c(1,3))
		plot(m2, composed = FALSE, ask = FALSE, cex.main = 2, cex = 2, cex.lab = 1.5, cex.axis = 2)
		par(op)
		
		# Countour plot of the probability of post-opererational infection
		# Create newdata for prediction
		ngrid <- 30
		gluc0 <- seq(50,190, length.out=ngrid)
		linf0 <- seq(0,45, length.out=ngrid)
		infect_pred  <- expand.grid(gluc = gluc0, linf = linf0)

		m2p <- predict(m2, newdata = infect_pred)
		n <- sqrt(nrow(infect_pred))
		Z <- matrix(m2p$pfitted.values, n, n)
		filled.contour(z = Z, x = gluc0, y = linf0, xlab = "Glucose (mg/dl)", ylab = "Lymphocytes (%)", 
			col = cm.colors(21))

	#######################################################################################
	# Poisson regression with offset
	#######################################################################################
		set.seed(123)
		
		# Generate the data
		n <- 1000
		x1 <- runif(n,-1,1)
		x2 <- runif(n,-1,1)
		eta <- 2 + 3*x1^2 + 5*x2^3
		exposure <- round(runif(n, 50, 500))
		y <- rpois(n, exposure*exp(eta))
		df_poiss <- data.frame(y = y, x1 = x1, x2 = x2)

		# Fit the model
		m4 <- sback(formula = y ~ sb(x1, h = 0.1) + sb(x2, h = 0.1), 
		data = df_poiss, offset = log(exposure), 
		kbin = 30, family = "poisson")

		summary(m4)

		op <- par(mfrow = c(1,2))
		plot(m4, ask = FALSE)
		par(op)

		# Dataframe and offset for prediction (example)
		n.p <- 100
		newoffset <- rep(0, n.p)
		df_poiss_pred <- data.frame(x1 = seq(-1,1,l=n.p), x2 = seq(-1,1,l=n.p))

		m4p <- predict(m4, newdata = df_poiss_pred, newoffset = newoffset)			

