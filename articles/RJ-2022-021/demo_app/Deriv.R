################################################
## Functions for derivatives of GAM(M) models ##
## https://fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/
################################################
Deriv <- function(mod, n = 200, eps = 1e-7, newdata, term) {
    if(inherits(mod, "gamm"))
        mod <- mod$gam
    m.terms <- attr(terms(mod), "term.labels")
    if(missing(newdata)) {
        newD <- sapply(model.frame(mod)[, m.terms, drop = FALSE],
                       function(x) seq(min(x), max(x), length = n))
        names(newD) <- m.terms
    } else {
        newD <- newdata
    }
    X0 <- predict(mod, data.frame(newD), type = "lpmatrix")
    newD <- newD + eps
    X1 <- predict(mod, data.frame(newD), type = "lpmatrix")
    Xp <- (X1 - X0) / eps
    Xp.r <- NROW(Xp)
    Xp.c <- NCOL(Xp)
    ## dims of bs
    bs.dims <- sapply(mod$smooth, "[[", "bs.dim") - 1
    ## number of smooth terms
    t.labs <- attr(mod$terms, "term.labels")
    ## match the term with the the terms in the model
    if(!missing(term)) {
        want <- grep(term, t.labs)
        if(!identical(length(want), length(term)))
            stop("One or more 'term's not found in model!")
        t.labs <- t.labs[want]
    }
    nt <- length(t.labs)
    ## list to hold the derivatives
    lD <- vector(mode = "list", length = nt)
    names(lD) <- t.labs
    for(i in seq_len(nt)) {
        Xi <- Xp * 0
        want <- grep(t.labs[i], colnames(X1))
        Xi[, want] <- Xp[, want]
        df <- Xi %*% coef(mod)
        df.sd <- rowSums(Xi %*% mod$Vp * Xi)^.5
        lD[[i]] <- list(deriv = df, se.deriv = df.sd)
    }
    class(lD) <- "Deriv"
    lD$gamModel <- mod
    lD$eps <- eps
    lD$eval <- newD - eps
    lD ##return
}

confint.Deriv <- function(object, term, alpha = 0.05, ...) {
    l <- length(object) - 3
    term.labs <- names(object[seq_len(l)])
    if(missing(term)) {
        term <- term.labs
    } else { ## how many attempts to get this right!?!?
        ##term <- match(term, term.labs)
        ##term <- term[match(term, term.labs)]
        term <- term.labs[match(term, term.labs)]
    }
    if(any(miss <- is.na(term)))
        stop(paste("'term'", term[miss], "not a valid model term."))
    res <- vector(mode = "list", length = length(term))
    names(res) <- term
    residual.df <- df.residual(object$gamModel)
    tVal <- qt(1 - (alpha/2), residual.df)
    ##for(i in term.labs[term]) {
    for(i in term) {
        upr <- object[[i]]$deriv + tVal * object[[i]]$se.deriv
        lwr <- object[[i]]$deriv - tVal * object[[i]]$se.deriv
        res[[i]] <- list(upper = drop(upr), lower = drop(lwr))
    }
    res$alpha = alpha
    res
}

signifD <- function(x, d, upper, lower, eval = 0) {
    miss <- upper > eval & lower < eval
    incr <- decr <- x
    want <- d > eval
    incr[!want | miss] <- NA
    want <- d < eval
    decr[!want | miss] <- NA
    list(incr = incr, decr = decr)
}

plot.Deriv <- function(x, alpha = 0.05, polygon = TRUE,
                       sizer = FALSE, term,
                       eval = 0, lwd = 3,
                       col = "lightgrey", border = col,
                       ylab, xlab, main, ...) {
    l <- length(x) - 3
    ## get terms and check specified (if any) are in model
    term.labs <- names(x[seq_len(l)])
    if(missing(term)) {
        term <- term.labs
    } else {
        term <- term.labs[match(term, term.labs)]
    }
    if(any(miss <- is.na(term)))
        stop(paste("'term'", term[miss], "not a valid model term."))
    if(all(miss))
        stop("All terms in 'term' not found in model.")
    l <- sum(!miss)
    nplt <- n2mfrow(l)
    tVal <- qt(1 - (alpha/2), df.residual(x$gamModel))
    if(missing(ylab))
        ylab <- expression(italic(hat(f)*"'"*(x)))
    if(missing(xlab)) {
        xlab <- attr(terms(x$gamModel), "term.labels")
        names(xlab) <- xlab
    }
    if (missing(main)) {
        main <- term
        names(main) <- term
    }
    ## compute confidence interval
    CI <- confint(x, term = term)
    ## plots
    layout(matrix(seq_len(l), nrow = nplt[1], ncol = nplt[2]))
    for(i in term) {
        upr <- CI[[i]]$upper
        lwr <- CI[[i]]$lower
        ylim <- range(upr, lwr)
        plot(x$eval[,i], x[[i]]$deriv, type = "n",
             ylim = ylim, ylab = ylab, xlab = xlab[i], main = main[i], ...)
        if(isTRUE(polygon)) {
            polygon(c(x$eval[,i], rev(x$eval[,i])),
                    c(upr, rev(lwr)), col = col, border = border)
        } else {
            lines(x$eval[,i], upr, lty = "dashed")
            lines(x$eval[,i], lwr, lty = "dashed")
        }
        abline(h = 0, ...)
        if(isTRUE(sizer)) {
            lines(x$eval[,i], x[[i]]$deriv, lwd = 1)
            S <- signifD(x[[i]]$deriv, x[[i]]$deriv, upr, lwr,
                         eval = eval)
            lines(x$eval[,i], S$incr, lwd = lwd, col = "blue")
            lines(x$eval[,i], S$decr, lwd = lwd, col = "red")
        } else {
            lines(x$eval[,i], x[[i]]$deriv, lwd = 2)
        }
    }
    layout(1)
    invisible(x)
}
