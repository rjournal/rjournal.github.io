library(PINstimation)

## NOTE: Running the application on high-frequency data containing 5 411 401 observations
## takes a prohibitively long time. Therefore, we provide the intermediate results for the
## creation of the tables and results

## Download intermediate results of data from the Internet
## The list 'app2' stores the different tables, and figures in the paper
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
app2 <- readRDS(file = url("https://www.pinstimation.com/data/rpaper_app_2.RDS"), "rb")
announcements <- readRDS(file = url("https://www.pinstimation.com/data/announcements.RDS"),"rb")


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## FUNCTIONS
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 'getheaders' returns the headers of tables given the table name
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
getheaders <- function(tab) {
  headers <- switch(
    EXPR=tab,
    "tab1" = c("Setting", "Sample", "N", "mean", "min", "Q01", "Q05", "Q10", "Q25", "Q50",
               "Q75", "Q90", "Q95", "Q99", "max", "sd"),
    "tab2" =  c("-","0.25", "0.5", "0.75", "1", "1.25", "1.5", "1.75", "2", ">2.00"),
  )
  return(headers)
}

## 'getSummaryStatistics' creates the table 1 in the R paper of vpin summary statistics
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
getSummaryStatistics <- function(largeestimates, smallestimates, allestimates) {

  getsumstats <- function(tab) {
    xtab <- lapply(tab, function(x) unname(do.call(c, x)))
    sumstats <- lapply(xtab, function(x) 100 * unname(c(
      length(x)/100, # N
      mean(x), # mean
      min(x),  # minimum
      quantile(x, probs = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)),
      max(x),  # maximum
      sd(x)      # sd
    )))
  }

  lstats <- getsumstats(largeestimates)
  sstats <- getsumstats(smallestimates)
  allstats <- getsumstats(allestimates)

  sumstats <- lapply(seq_along(smallestimates), function(x) {
    y <- data.frame(rbind(lstats[[x]], sstats[[x]], allstats[[x]]));
    y <- cbind(sample = c("large", "small", "All"), y);
    y <- cbind(Setting = rep(names(lstats)[x], 3), y); y})

  tab <- data.frame(do.call(rbind, sumstats))
  colnames(tab) <- getheaders("tab1")

  return(tab)

}


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## + 1 + LOAD INTERMEDIATE RESULTS FROM THE FILE 'APP2'
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    largevpin <- app2$largevpin
    smallvpin <- app2$smallvpin
    tab.vpin <- app2$allvpin
    tab.buckets <- app2$tab.buckets
    retvpin <- app2$retvpin
    vnames <- names(tab.vpin[[1]])
    lnames <- names(largevpin[[1]])
    snames <- names(smallvpin[[1]])

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## + 2 + CREATE R PAPER TABLES: TABLE 6 - SUMMARY STATISTICS OF VPIN
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    tab1 <- getSummaryStatistics(largevpin, smallvpin, tab.vpin)
    app2$table1 <- tab1
    show(knitr::kable(tab1, format="simple", digits = 1,
                      caption = "Descriptive statistics for VPIN measures"))

## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## + 3 + CREATE R PAPER TABLES: TABLE 7, AND TABLE 8 - ABSOLUTE RETURNS
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Create the absolute leading return vector
    # =======================================================================================
    retvpin <- retvpin[order(retvpin$vpin), ]
    retvpin$a.return <- abs(retvpin$l.return)


    # Find the group of the vpin values (20 quantiles)
    # =======================================================================================
    vpinqtles <- quantile(retvpin$vpin, probs = ((1:19)/20))
    retvpin$vgrp <- findInterval(retvpin$vpin, vpinqtles)

    # Find the group of the absolute returns (0.25 to > 2.00)
    # =======================================================================================
    retbreaks <- (1:8)/400
    retvpin$rgrp <- findInterval(retvpin$a.return, retbreaks)

    # Headers of the tables
    # =======================================================================================
    vpin.headers <- round(c(vpinqtles, 1), 3)
    rtn.headers <- c(100 * retbreaks, ">2.00")

    # =======================================================================================
    # = Table 7 = Distribution of the absolute post-returns conditional on VPIN
    # =======================================================================================

    vpingroups <- split(retvpin, retvpin$vgrp)
    rtnpervpin <- lapply(vpingroups, function(x){
      lrtn <- nrow(x)
      rtn <- split(x$a.return, x$rgrp, drop = FALSE)
      mrtn <- lapply(rtn, function(x) length(x))
      mrtn <- unlist(mrtn)
    })

    tab2 <- data.frame(do.call(rbind, rtnpervpin))
    tab2 <- sweep(tab2, MARGIN=1, FUN="/", STATS=rowSums(tab2))
    tab2 <- 100 * tab2
    colnames(tab2) <- rtn.headers
    row.names(tab2) <- vpin.headers
    app2$table2 <- tab2

    show(knitr::kable(tab2, format="simple", digits = 2,
                  caption = "Distribution of the absolute post-returns conditional on VPIN"))

    # =======================================================================================
    # = Table 8 = Distribution of VPIN conditional on absolute post-returns
    # =======================================================================================

    rtngroups <- split(retvpin, retvpin$rgrp)
    vpinperrtn <- lapply(rtngroups, function(x){
      lvpin <- nrow(x)
      vpins <- split(x$vpin, x$vgrp)
      mvpin <- lapply(vpins, function(x) length(x))
      mvpin <- unlist(mvpin)
    })
    tab8 <- data.frame(do.call(rbind, vpinperrtn))
    tab8 <- sweep(tab8, MARGIN=1, FUN="/", STATS=rowSums(tab8))
    tab8 <- 100 * tab8
    tab8 <- t(tab8)
    colnames(tab8) <- rtn.headers
    row.names(tab8) <- vpin.headers
    app2$table8 <- tab8

    show(knitr::kable(tab8, format="simple", digits = 2,
              caption = "Distribution of VPIN conditional on absolute post-returns"))


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## CREATE R PAPER TABLES: TABLE 9 - POSITIVE AND NEGATIVE RETURNS
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Create the positive, and negative leading return vectors
    # =======================================================================================
    posretvpin <- retvpin[which(retvpin$l.return > 0), ]
    negretvpin <- retvpin[which(retvpin$l.return < 0), ]
    tab9 <- data.frame()

    # Find the group of the vpin values (20 quantiles)
    # =======================================================================================
    pvpinqtles <- quantile(posretvpin$vpin, probs = ((1:19)/20))
    posretvpin$vgrp <- findInterval(posretvpin$vpin, pvpinqtles)

    nvpinqtles <- quantile(negretvpin$vpin, probs = ((1:19)/20))
    negretvpin$vgrp <- findInterval(negretvpin$vpin, nvpinqtles)

    # Find the group of the absolute returns (0.25 to > 2.00)
    # =======================================================================================
    retbreaks <- (1:8)/400
    posretvpin$rgrp <- findInterval(posretvpin$l.return, retbreaks)
    negretvpin$rgrp <- findInterval(negretvpin$a.return, retbreaks)

    # Headers of the tables
    # =======================================================================================
    pvpin.headers <- round(c(pvpinqtles, 1), 3)
    nvpin.headers <- round(c(nvpinqtles, 1), 3)
    prtn.headers <- c(100 * retbreaks, ">2.00")
    nrtn.headers <- c(-100 * retbreaks, "<-2.00")

    # =======================================================================================
    # = Table 9.1 = Distribution of the positive post-returns conditional on VPIN
    # =======================================================================================

    vpingroups <- split(posretvpin, posretvpin$vgrp)
    rtnpervpin <- lapply(vpingroups, function(x){
      lrtn <- nrow(x)
      rtn <- split(x$a.return, x$rgrp, drop = FALSE)
      mrtn <- lapply(rtn, function(x) length(x))
      mrtn <- unlist(mrtn)
    })

    tab9.1 <- data.frame(do.call(rbind, rtnpervpin))
    tab9.1 <- sweep(tab9.1, MARGIN=1, FUN="/", STATS=rowSums(tab9.1))
    tab9.1 <- 100 * tab9.1
    colnames(tab9.1) <- prtn.headers
    rownames(tab9.1) <- pvpin.headers
    app2$table91 <- tab9.1


    # =======================================================================================
    # = Table 9.2 = Distribution of VPIN conditional on positive post-returns
    # =======================================================================================

    rtngroups <- split(posretvpin, posretvpin$rgrp)
    vpinperrtn <- lapply(rtngroups, function(x){
      vpins <- split(x$vpin, x$vgrp)
      mvpin <- lapply(vpins, function(x) length(x))
      mvpin <- unlist(mvpin)
    })
    tab9.2 <-data.frame(do.call(rbind, vpinperrtn))
    tab9.2 <- sweep(tab9.2, MARGIN=1, FUN="/", STATS=rowSums(tab9.2))
    tab9.2 <- 100 * tab9.2
    tab9.2 <- data.frame(t(tab9.2))
    colnames(tab9.2) <- prtn.headers
    rownames(tab9.2) <- pvpin.headers
    app2$table92 <- tab9.2


    # =======================================================================================
    # = Table 9.3 = Distribution of the negative post-returns conditional on VPIN
    # =======================================================================================

    vpingroups <- split(negretvpin, negretvpin$vgrp)
    rtnpervpin <- lapply(vpingroups, function(x){
      lrtn <- nrow(x)
      rtn <- split(x$a.return, x$rgrp, drop = FALSE)
      mrtn <- lapply(rtn, function(x) length(x))
      mrtn <- unlist(mrtn)
    })

    tab9.3 <- data.frame(do.call(rbind, rtnpervpin))
    tab9.3 <- sweep(tab9.3, MARGIN=1, FUN="/", STATS=rowSums(tab9.3))
    tab9.3 <- 100 * tab9.3
    colnames(tab9.3) <- nrtn.headers
    rownames(tab9.3) <- nvpin.headers
    app2$table93 <- tab9.3

    # =======================================================================================
    # = Table 9.4 = Distribution of VPIN conditional on negative post-returns
    # =======================================================================================

    rtngroups <- split(negretvpin, negretvpin$rgrp)
    vpinperrtn <- lapply(rtngroups, function(x){
      lvpin <- nrow(x)
      vpins <- split(x$vpin, x$vgrp)
      mvpin <- lapply(vpins, function(x) length(x))
      mvpin <- unlist(mvpin)
    })
    tab9.4 <- data.frame(do.call(rbind, vpinperrtn))
    tab9.4 <- sweep(tab9.4, MARGIN=1, FUN="/", STATS=rowSums(tab9.4))
    tab9.4 <- 100 * tab9.4
    tab9.4 <- data.frame(t(tab9.4))
    colnames(tab9.4) <- nrtn.headers
    rownames(tab9.4) <- nvpin.headers
    app2$table94 <- tab9.4


    show(knitr::kable(tab9.1[c(1, 10, 20), ], format="simple", digits = 2,
                      caption = "Distribution of VPIN conditional on positive post-returns"))

    show(knitr::kable(tab9.3[c(1, 10, 20), ], format="simple", digits = 2,
                      caption = "Distribution of VPIN conditional on negative post-returns"))

    show(knitr::kable(tab9.2[c(1, 10, 20), ], format="simple", digits = 2,
                      caption = "Distribution of positive post-returns conditional on VPIN"))

    show(knitr::kable(tab9.4[c(1, 10, 20), ], format="simple", digits = 2,
                      caption = "Distribution of negative post-returns conditional on VPIN"))


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## COMBINE VPINS AND ANNOUNCEMENTS
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Define the function meanna
    meanna <- function(x) mean(x, na.rm = TRUE)

    ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## FIGURE B1 - Intraday pattern of mean VPIN
    ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    tider <- paste0(c("09:00", "09:15", "09:30", "09:45", "10:00", "10:15", "10:30", "10:45",
                      "11:00", "11:15", "11:30", "11:45", "12:00", "12:15", "12:30", "12:45",
                      "13:00", "13:15", "13:30", "13:45", "14:00", "14:15", "14:30", "14:45",
                      "15:00", "15:15", "15:30", "15:45", "16:00", "16:15", "16:30", "16:45",
                      "17:00", "17:15", "17:25"),
                    ":00")

    ##### find what is VPIN for each of above times. Each stock and each day.

    showProgress <- function(x, total) {cat("\rCalculating intraday mean vpin: ",
                                            round(100 * (x/total), 2), "%               ", sep = "")}
    cat("\n\n")
    intra <- c()
    for (k in 1:length(tider)) {
      showProgress(k, length(tider))
      tm <- tider[k]
      tidvpin <- lapply(tab.buckets, function(x) {
        x$starttid <- substr(x$starttime, 12, 19);
        x$sluttid <- substr(x$endtime, 12, 19);
        days <- unique(substr(x$starttime, 1, 11));
        daytimes <- as.POSIXct(paste(days, tm));
        points <- unlist(lapply(daytimes, function(z) sum(x$starttime <= z)))
        points[points == 0] <- 1
        vpins <- x$vpin[points]
      })
      intra <- c(intra, mean(do.call(c, tidvpin)))
    }

    tider <- substr(tider, 1, 5)
    plot(intra, ylim = c(0.22,0.26), cex=1.5, pch=16, xaxt="none", las=1, xlab = "", ylab = "VPIN", type="p")
    axis(1, 1:length(tider), factor(tider), las=2, cex.axis=1)

    ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## FIGURE 1 - VPIN around announcements
    ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    # Construct a span for +100 buckets before and after the announcements.
    # =======================================================================================

    span <- 100
    ann <- announcements

    rem <- c() # to label which announcements have nonNA VPIN in announcement bucket
    ann.vp <- data.frame()

    for(anc in seq_len(nrow(ann))) {

      thisann <- ann[anc, ]

      if (thisann$ticker %in% vnames) {

        tbd <- thisbucketdata <- tab.buckets[[thisann$ticker]]
        timing <- tm <- paste(thisann$date, thisann$time)

        before_ann <- which(tbd$starttime < tm & tbd$endtime <= tm)
        at_ann <- c(which(tbd$starttime == tm & tbd$endtime == tm),
                    which(tbd$starttime <= tm & tbd$endtime > tm))
        after_ann <- which(tbd$starttime > tm)

        if (length(at_ann) > 1) {
          # after_ann <- c(at_ann[-1], after_ann)
          at_ann <- at_ann[1]
        }

        ex_before <- rep(NA, max(span - length(before_ann), 0))
        ex_after <- rep(NA, max(span - length(after_ann), 0))

        vps <- c(ex_before, tbd$vpin[tail(before_ann, span)], tbd$vpin[at_ann],
                 tbd$vpin[head(after_ann, span)], ex_after)

        if(length(at_ann) != 0 && !is.na(tbd$vpin[at_ann])) {
          ann.vp <- rbind(ann.vp, vps)
          rem <- c(rem, anc)
        }
        colnames(ann.vp) <- paste("C", 1:201, sep="")
        rm(vps,before_ann, at_ann, after_ann, tbd, tm, timing)
      }

    }

    ann.vp <- data.frame(ann.vp)
    ann <- ann[rem,]
    rownames(ann) <- NULL


    ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ## FIGURE 2 - VPIN around announcements
    ## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    ## FIGURE 2 - PANEL A: WHOLE SAMPLE
    ## +++++++++++++++++++++++++++++++++++++++++++

    cat("\n\n\rPrinting Figure 2A")

    meas <- apply(ann.vp, 2, meanna)
    plot(meas, type = "l",xaxt = "none",
         xlab = "bucket no. wrt. announcement", ylab = "VPIN", las=2)
    axis(1, 1:(2*span+1), -100:100, las=2, cex.axis=1)

    ## FIGURE 2 - PANEL B: LARGE/SMALL CAP STOCKS
    ## +++++++++++++++++++++++++++++++++++++++++++

    cat("\n\n\rPrinting Figure 2B\n\n")

    meas.large <- apply(ann.vp[ann$ticker %in% lnames,], 2, meanna)
    meas.small <- apply(ann.vp[ann$ticker %in% snames,], 2, meanna)

    plot(meas.large, type = "l", col=2, lty=3, lwd=2, ylim = c(0.25, 0.33),
         xaxt = "none", xlab = "bucket no. wrt. announcement", ylab = "VPIN", las=2)
    axis(1, 1:(2*span+1), -100:100, las=2, cex.axis=1)
    lines(meas.small, col=4, lty=2, lwd=2)
    legend(x = "topleft", lty = c(3, 2), col = c(2, 4), lwd = 2,
           legend = c("Large Cap Stocks", "Small Cap Stocks"))


## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## SAVE THE LIST IN AN RDS VARIABLE 'rpaper_app2.RDS'
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

saveRDS(object = app2, file="rpaper_app_2.RDS")
