# Helper function
show_gain_fun <- function(
  filter_w,
  lags,
  freq_step,
  step,
  mode,
  overwrite_title = NULL
) {
  freqs <- seq(0, pi, freq_step)

  i <- 0 + 1i # complex number i

  lags <- -lags
  l_fr <- length(freqs)

  Fa <- rep(NA, l_fr)

  for (j in seq_along(Fa)) {
    Fa[[j]] <- sum(filter_w * exp(i * freqs[[j]] * lags))
  }

  Ga <- abs(Fa)

  stepd2 <- step / 2

  mid_text <- switch(
    mode,
    quarterly = character(0),
    monthly = paste0(2:5, paste0("*pi/", stepd2))
  )

  freq_cuts <- seq(0, pi, pi / stepd2)
  lab_text <- c("0", paste0("pi/", stepd2), mid_text, "pi")
  lab_expr <- parse(text = lab_text)

  plot(
    freqs,
    Ga,
    type = "n",
    xlab = "Frequency",
    ylab = "Gain",
    main = "",
    xaxt = "n",
    panel.first = {
      abline(h = seq(-0.4, 1.4, 0.2), v = freq_cuts, col = "lightgray", lty = 3)
    }
  )
  axis(side = 1, at = freq_cuts, labels = lab_expr)
  abline(h = 0, col = "grey60", lty = 1)
  lines(freqs, Ga, type = "l")
}

###################################################################
#                                                                 #
#         Computation and Plotting of X13 Filter Weights          #
#                                                                 #
###################################################################

# Author: Dominik Schulz
# Version: July 16, 2025

# Arguments:
#
# k:         the k in a (3xk)-seasonal filter; corresponding SI ratios
#            will be used for SI_{t+s*j} for s = 12 for monthly data,
#            s = 4 for quarterly data, and with running variable
#            j = -n, -n+1, ..., n-1, n, where n = (k-1)/2.
# mode:      either "quarterly" or "monthly" depending on the observation
#            frequency of the data.
# plot:      a logical; if TRUE, the final filter weights are automatically
#            plotted.
# show_gain: a logical; if plot = TRUE and show_gain = TRUE, the gain function
#            of the filter will be plotted instead of the filter weights.
# freq_step: the step size between 0 and pi to use for the frequencies for the gain plot.

# Note: The function returns the numeric vector with the filter weights invisibly,
#       i.e. they can only be accessed when the output of the function is saved
#       to an object (similar to the acf() function).

x13_sym_seasonal_filter <- function(
  k,
  mode = c("quarterly", "monthly"),
  plot = TRUE,
  show_gain = FALSE,
  freq_step = 0.01
) {
  stopifnot("k must be odd" = ((k %% 2) == 1))
  stopifnot("k must be >= 3" = (k >= 3))

  mode <- match.arg(mode)

  step <- switch(
    mode,
    "quarterly" = 4,
    "monthly" = 12
  )

  n <- (k - 1) / 2
  div <- k

  len <- k + 2

  filt_sub_left <- seq(from = 1 / div, to = 2 / div, by = 1 / div)
  filt_sub_right <- rev(filt_sub_left)
  filt_sub_mid <- rep(3 / div, len - 2 * length(filt_sub_left))

  filt_sub <- c(filt_sub_left, filt_sub_mid, filt_sub_right)

  filt_complete <- (1 / 3) * filt_sub

  filt_Sh <- rep(0, step * (length(filt_complete) - 1) + 1)

  for (i in seq_along(filt_complete)) {
    filt_Sh[[(i - 1) * step + 1]] <- filt_complete[[i]]
  }

  lSh1 <- length(filt_Sh)
  lSh <- (lSh1 - 1) / 2

  filt_mat <- matrix(0, ncol = lSh1 + step, nrow = step + 1)

  j <- 0
  for (i in -(step / 2):(step / 2)) {
    j <- j + 1
    filt_mat[j, j:(lSh1 + j - 1)] <- filt_Sh
  }
  filt_mat[1, ] <- filt_mat[1, ] * (1 / (2 * step))
  filt_mat[step + 1, ] <- filt_mat[step + 1, ] * (1 / (2 * step))
  filt_mat[2:step, ] <- filt_mat[2:step, ] * (1 / step)

  filt_trend <- c(apply(filt_mat, MARGIN = 2, FUN = sum))

  lt <- length(filt_trend)
  filt_out <- rep(0, lt)
  mid_out <- (lt - 1) / 2 + 1
  filt_out[(mid_out - lSh):(mid_out + lSh)] <- filt_Sh
  filt_out <- filt_out - filt_trend

  if (plot) {
    l <- length(filt_out)
    mid <- (l - 1) / 2 + 1

    lags <- (1:l) - mid

    if (show_gain) {
      show_gain_fun(
        filter_w = filt_out,
        lags = lags,
        freq_step = freq_step,
        step = step,
        mode = mode
      )
      title(
        main = paste0(
          "Gain function of the (3x",
          k,
          ")-seasonality filter of X13 for ",
          mode,
          " data"
        )
      )
      title(
        sub = "For additive component model; symmetric filter for interior points",
        adj = 1
      )
    } else {
      plot(
        lags,
        filt_out,
        xlab = "Lag",
        ylab = "Filter weight",
        main = paste0(
          "Filter weights of the (3x",
          k,
          ")-seasonality filter of X13 for ",
          mode,
          " data"
        ),
        panel.first = {
          grid()
        },
        type = "n"
      )
      title(
        sub = "For additive component model; symmetric filter for interior points",
        adj = 1
      )
      abline(h = 0, col = "grey60", lty = 1)
      lines(lags, filt_out, type = "h")
    }
  }

  invisible(filt_out)
}

# Arguments:
#
# m:         the total number of obs. to use in the Henderson filter; should
#            be an odd number; m = 2H + 1, where H is as in Findley et al. (1998).
# mode:      either "quarterly" or "monthly" depending on the observation
#            frequency of the data; doesn't have an effect on the Henderson
#            filter weights, but is used in gain function plotting.
# plot:      a logical; if TRUE, the final filter weights are automatically
#            plotted.
# show_gain: a logical; if plot = TRUE and show_gain = TRUE, the gain function
#            of the filter will be plotted instead of the filter weights.
# freq_step: the step size between 0 and pi to use for the frequencies for the gain plot.

# Note: The function returns the numeric vector with the filter weights invisibly,
#       i.e. they can only be accessed when the output of the function is saved
#       to an object (similar to the acf() function).

x13_sym_henderson_trend_filter <- function(
  m,
  mode = c("quarterly", "monthly"),
  plot = TRUE,
  show_gain = FALSE,
  freq_step = 0.01
) {
  stopifnot("m must be odd" = ((m %% 2) == 1))
  stopifnot("m must be >= 5" = (m >= 5))

  mode <- match.arg(mode)

  H <- (m - 1) / 2

  j <- -H:H

  j2 <- j^2

  qj <- ((H + 1)^2 - j2) * ((H + 2)^2 - j2) * ((H + 3)^2 - j2)

  C1 <- sum(qj)
  C2 <- sum(qj * j2)
  C4 <- sum(qj * j^4)

  b <- C2 / (C2^2 - C1 * C4)
  a <- -b * C4 / C2

  hj <- qj * (a + b * j2)

  if (plot) {
    if (show_gain) {
      step <- switch(
        mode,
        "quarterly" = 4,
        "monthly" = 12
      )

      l <- length(hj)
      mid <- (l - 1) / 2 + 1

      lags <- (1:l) - mid

      show_gain_fun(
        filter_w = hj,
        lags = lags,
        freq_step = freq_step,
        step = step,
        mode = mode
      )
      title(
        main = paste0(
          "Gain function of the ",
          m,
          "-term Henderson trend filter of X13"
        )
      )
      title(
        sub = "For additive component model; symmetric filter for interior points",
        adj = 1
      )
    } else {
      plot(
        j,
        hj,
        xlab = "Lag",
        ylab = "Filter weight",
        main = paste0(
          "Filter weights of the ",
          m,
          "-term Henderson trend filter of X13"
        ),
        panel.first = {
          grid()
        },
        type = "n"
      )
      title(
        sub = "For additive component model; symmetric filter for interior points",
        adj = 1
      )
      abline(h = 0, col = "grey60", lty = 1)
      lines(j, hj, type = "h")
    }
  }

  invisible(hj)
}

# Iterated final trend and seasonality filters under full use
# of symmetric filters throughout and the same henderson filter
x13_sym_final <- function(
  m,
  k,
  mode = c("quarterly", "monthly"),
  plot = TRUE,
  plot_which = c("trend", "deseason"),
  show_gain = FALSE,
  freq_step = 0.01
) {
  mode <- match.arg(mode)
  plot_which <- match.arg(plot_which)

  henderson_w <- x13_sym_henderson_trend_filter(
    m = m,
    mode = mode,
    plot = FALSE
  )

  seas_w <- x13_sym_seasonal_filter(k = k, mode = mode, plot = FALSE)

  lh <- length(henderson_w)
  ls <- length(seas_w)

  # Stage I. Initial Estimates

  q <- (lh - 1) / 2
  q1 <- q + 1
  wi <- 1 / (lh - 1)
  wo <- 1 / (2 * (lh - 1))

  ### Initial trend weights
  T1_w <- c(wo, rep(wi, lh - 2), wo)
  detrend1_w <- -T1_w
  detrend1_w[[q1]] <- detrend1_w[[q1]] + 1

  ### Initial seasonality weights

  seas_sub_w <- x13_sym_seasonal_filter(k = 3, mode = mode, plot = FALSE)

  mat <- matrix(0, ncol = length(seas_sub_w) + 2 * q, nrow = length(seas_sub_w))
  for (i in 1:length(seas_sub_w)) {
    mat[i, i:(lh - 1 + i)] <- detrend1_w * seas_sub_w[[i]]
  }
  S1_w <- apply(mat, 2, sum)

  deseas1_w <- -S1_w
  sel <- (length(S1_w) - 1) / 2 + 1
  deseas1_w[[sel]] <- deseas1_w[[sel]] + 1

  # Stage II.

  ### Second trend estimation weights

  ln <- (length(S1_w) - 1) / 2

  mat <- matrix(0, ncol = lh + 2 * ln, nrow = lh)
  for (i in 1:lh) {
    mat[i, i:(length(S1_w) - 1 + i)] <- deseas1_w * henderson_w[[i]]
  }
  T2_w <- apply(mat, 2, sum)
  sel <- (length(T2_w) - 1) / 2 + 1
  detrend2_w <- -T2_w
  detrend2_w[[sel]] <- detrend2_w[[sel]] + 1

  ### Second and final seasonality estimation weights

  ln <- (length(T2_w) - 1) / 2

  mat <- matrix(0, ncol = ls + 2 * ln, nrow = ls)
  for (i in 1:ls) {
    mat[i, i:(length(T2_w) - 1 + i)] <- detrend2_w * seas_w[[i]]
  }
  S_final_w <- apply(mat, 2, sum)
  sel <- (length(S_final_w) - 1) / 2 + 1
  deseason_final_w <- -S_final_w
  deseason_final_w[[sel]] <- deseason_final_w[[sel]] + 1

  # Stage III.

  ### Third and final trend estimation

  ln <- (length(S_final_w) - 1) / 2

  mat <- matrix(0, ncol = lh + 2 * ln, nrow = lh)
  for (i in 1:lh) {
    mat[i, i:(length(S_final_w) - 1 + i)] <- deseason_final_w * henderson_w[[i]]
  }
  T_final_w <- apply(mat, 2, sum)

  if (plot) {
    hj <- switch(
      plot_which,
      "trend" = T_final_w,
      "deseason" = deseason_final_w
    )

    if (show_gain) {
      n1 <- switch(
        plot_which,
        "trend" = paste0(
          "Gain function of the cumulated trend filter following a ",
          m,
          "-term Henderson trend filter of X13"
        ),
        "deseason" = paste0(
          "Gain function of the cumulated seasonal adjustment filter following a (3x",
          k,
          ")-seasonal filter of X13"
        )
      )

      step <- switch(
        mode,
        "quarterly" = 4,
        "monthly" = 12
      )

      l <- length(hj)
      mid <- (l - 1) / 2 + 1

      lags <- (1:l) - mid

      show_gain_fun(
        filter_w = hj,
        lags = lags,
        freq_step = freq_step,
        step = step,
        mode = mode,
        overwrite_title = n1
      )
      title(main = n1)
      title(
        sub = "For additive component model; symmetric filter for interior points",
        adj = 1
      )
    } else {
      n1 <- switch(
        plot_which,
        "trend" = paste0(
          "Cumulated trend filter weights following a ",
          m,
          "-term Henderson trend filter of X13"
        ),
        "deseason" = paste0(
          "Cumulated seasonal adjustment filter weights following a (3x",
          k,
          ")-seasonal filter of X13"
        )
      )

      len <- length(hj)
      js <- (len - 1) / 2
      j <- -js:js

      plot(
        j,
        hj,
        xlab = "Lag",
        ylab = "Filter weight",
        main = n1,
        panel.first = {
          grid()
        },
        type = "n"
      )
      title(
        sub = "For additive component model; symmetric filter for interior points",
        adj = 1
      )
      abline(h = 0, col = "grey60", lty = 1)
      lines(j, hj, type = "h")
    }
  }

  invisible(list(trend_w = T_final_w, deseason_w = deseason_final_w))
}

# Helper function
compute_asy_weights <- function(sym_weights, m, d, Rconst) {
  filter_mat <- matrix(0, ncol = m, nrow = length(d))

  j <- 0
  for (d0 in d) {
    j <- j + 1
    M <- m - d0
    filt <- rev(
      head(sym_weights, M) +
        (1 / M) * sum(tail(sym_weights, d0)) +
        (((1:M) - (M + 1) / 2) * Rconst) /
          (1 + Rconst * M * (M - 1) * (M + 1) / 12) *
          sum(tail(sym_weights, d0) * ((M + 1):m - (M + 1) / 2))
    )
    filter_mat[length(d) + 1 - j, 1:M] <- filt
  }

  filter_mat
}

x13_asym_henderson_trend_filters <- function(
  m,
  tp = 1:((m - 1) / 2),
  R_np1 = NULL,
  mode = c("quarterly", "monthly"),
  plot = TRUE,
  show_gain = FALSE,
  freq_step = 0.01,
  plot_tp = tp
) {
  check_tp <- (tp >= 1) & (tp <= ((m - 1) / 2))
  tp <- tp[check_tp]
  check_plot_tp <- (plot_tp >= 1) & (plot_tp <= ((m - 1) / 2))
  plot_tp <- plot_tp[check_plot_tp]

  d <- rev((m - 1) / 2 - tp + 1)

  stopifnot("m must be odd" = ((m %% 2) == 1))
  stopifnot("m must be >= 5" = (m >= 5))

  mode <- match.arg(mode)

  if (is.null(R_np1)) {
    # Default values for the constant taken from Findley et al. (1998) and from the X13 source code
    R_np1 <- if (mode == "monthly") {
      if (m <= 9) {
        0.99
      } else if (m == 13) {
        3.5
      } else if (m > 13) {
        4.5
      }
    } else if (mode == "quarterly") {
      if (m <= 5) {
        0.001
      } else if (m >= 7) {
        4.5
      }
    }
  }

  Rconst <- 4 / (R_np1^2 * pi)
  # Symmetric weights are the basis for the corresponding asymmetric ones
  sym_weights <- x13_sym_henderson_trend_filter(
    m = m,
    plot = FALSE
  )

  w_out <- compute_asy_weights(
    sym_weights = sym_weights,
    m = m,
    d = d,
    Rconst = Rconst
  )

  if (plot) {
    l <- length(w_out[1, ])

    if (show_gain) {
      step <- switch(
        mode,
        "quarterly" = 4,
        "monthly" = 12
      )

      for (u in 1:dim(w_out)[[1]]) {
        w <- w_out[u, ]

        lags <- (1:l) - tp[[u]]

        show_gain_fun(
          filter_w = w,
          lags = lags,
          freq_step = freq_step,
          step = step,
          mode = mode
        )
        title(
          main = paste0(
            "Gain function of the ",
            m,
            "-term Henderson trend filter of X13"
          )
        )
        title(
          sub = paste0(
            "For additive component model; asymmetric filter at time point ",
            tp[[u]]
          ),
          adj = 1
        )
      }
    } else {
      for (u in 1:dim(w_out)[[1]]) {
        plot(
          (1:l) - tp[[u]],
          w_out[u, ],
          xlab = "Lag",
          ylab = "Filter weight",
          main = paste0(
            "Filter weights of the ",
            m,
            "-term Henderson trend filter of X13"
          ),
          panel.first = {
            grid()
          },
          type = "n"
        )
        title(
          sub = paste0(
            "For additive component model; asymmetric filter at time point ",
            tp[[u]]
          ),
          adj = 1
        )
        abline(h = 0, col = "grey60", lty = 1)
        lines((1:l) - tp[[u]], w_out[u, ], type = "h")
      }
    }
  }

  invisible(w_out)
}

#=============================================================#
#
# # Example for a (3x5)-seasonality filter for a monthly series
#
# ### Shows filter weights
# test1 <- x13_seasonal_filter(k = 15, mode = "monthly", plot = TRUE)
# test1
#
# ### Shows filter gain function
# x13_seasonal_filter(k = 15, mode = "monthly", plot = TRUE, show_gain = TRUE, freq_step = 0.001)
#
# # Example for a 7-term Henderson trend filter
#
# ### Shows filter weights
# test2 <- x13_henderson_trend_filter(m = 23, plot = TRUE)
# test2
#
# ### Shows filter gain function
# x13_henderson_trend_filter(m = 23, plot = TRUE, show_gain = TRUE, mode = "monthly", freq_step = 0.001)
