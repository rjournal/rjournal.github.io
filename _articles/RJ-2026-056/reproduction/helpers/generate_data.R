# ------------------------------------------------------------------------------
# LassoHiDFastGibbs: data generators used for benchmarks / paper reproducibility
# CRAN-safe: uses system.file() for inst/extdata and requireNamespace() for
# optional suggested packages.
# ------------------------------------------------------------------------------

# Internal helper: locate a file shipped in inst/extdata
.get_extdata_path <- function(fname, subdir = NULL) {
  f <- if (is.null(subdir)) {
    system.file("extdata", fname, package = "LassoHiDFastGibbs")
  } else {
    system.file("extdata", subdir, fname, package = "LassoHiDFastGibbs")
  }

  if (identical(f, "")) {
    stop(
      "Cannot find file in inst/extdata: ", fname, "\n",
      "Expected location: inst/extdata/",
      if (!is.null(subdir)) paste0(subdir, "/") else "",
      fname,
      call. = FALSE
    )
  }
  f
}

# Internal helper: safe model.matrix power expansion (2-way or 3-way interactions)
.mm_power <- function(df, power = 2) {
  df <- as.data.frame(df)

  if (power == 2) {
    stats::model.matrix(~ .^2, data = df)[, -1, drop = FALSE]
  } else if (power == 3) {
    stats::model.matrix(~ .^3, data = df)[, -1, drop = FALSE]
  } else {
    stop("power must be 2 or 3.", call. = FALSE)
  }
}

generate_data <- function(dataset_name) {
  # Validate input early
  if (!is.character(dataset_name) || length(dataset_name) != 1L || is.na(dataset_name)) {
    stop("dataset_name must be a single, non-missing character string.", call. = FALSE)
  }

  # We'll fill these and normalize at the end
  x <- NULL
  y <- NULL

  # ---- simulated datasets ----
  if (dataset_name == "sim1") {
    n <- 30000; p <- 30
    x <- matrix(stats::rnorm(n * p), n, p)
    y <- stats::rnorm(n)
    vbeta0 <- rep(c(-3, 3), 10)
    inds <- sort(sample.int(p, length(vbeta0)))
    y <- drop(y + x[, inds, drop = FALSE] %*% vbeta0)

  } else if (dataset_name == "sim2") {
    n <- 500; p <- 400
    x <- matrix(stats::rnorm(n * p), n, p)
    y <- stats::rnorm(n)
    vbeta0 <- rep(c(-3, 3), 10)
    inds <- sort(sample.int(p, length(vbeta0)))
    y <- drop(y + x[, inds, drop = FALSE] %*% vbeta0)

  } else if (dataset_name == "sim3") {
    n <- 100; p <- 500
    x <- matrix(stats::rnorm(n * p), n, p)
    y <- stats::rnorm(n)
    vbeta0 <- rep(c(-3, 3), 10)
    inds <- sort(sample.int(p, length(vbeta0)))
    y <- drop(y + x[, inds, drop = FALSE] %*% vbeta0)

  } else if (dataset_name == "sim4") {
    n <- 50; p <- 5000
    x <- matrix(stats::rnorm(n * p), n, p)
    y <- stats::rnorm(n)
    vbeta0 <- rep(c(-3, 3), 10)
    inds <- sort(sample.int(p, length(vbeta0)))
    y <- drop(y + x[, inds, drop = FALSE] %*% vbeta0)

    # ---- external-data generators ----
  } else if (dataset_name == "qtl") {
    res <- generate_qtl_data()
    y <- res$y
    x <- res$x

  } else if (dataset_name == "covid") {
    res <- generate_covid_data()
    y <- res$y
    x <- res$x
    # remove any all-zero columns (if present)
    s <- colSums(x)
    inds <- which(s == 0)
    if (length(inds) > 0) x <- x[, -inds, drop = FALSE]

  } else if (dataset_name == "riboflavin") {
    res <- generate_riboflavin_data()
    y <- res$y
    x <- res$x

  } else if (dataset_name == "cookie") {
    res <- generate_cookie_data()
    y <- res$y
    x <- res$x

    # ---- Kakadu (your shipped CSV) ----
  } else if (dataset_name == "Kakadu2") {
    f <- .get_extdata_path("Kakadu.csv")
    dat <- utils::read.csv(f)

    if (!("income" %in% names(dat))) {
      stop("Kakadu.csv must contain a column named 'income'.", call. = FALSE)
    }

    y <- as.numeric(dat$income)
    x0 <- dat[, c(2:21, 23), drop = FALSE]
    x <- .mm_power(x0, power = 2)

  } else if (dataset_name == "Kakadu3") {
    f <- .get_extdata_path("Kakadu.csv")
    dat <- utils::read.csv(f)

    if (!("income" %in% names(dat))) {
      stop("Kakadu.csv must contain a column named 'income'.", call. = FALSE)
    }

    y <- as.numeric(dat$income)
    x0 <- dat[, c(2:21, 23), drop = FALSE]
    x <- .mm_power(x0, power = 3)

    # ---- diabetes (CRAN dataset from lars) ----
  } else if (dataset_name == "diabetes") {
    if (!requireNamespace("lars", quietly = TRUE)) knitr::knit_exit()
    env <- new.env()
    utils::data("diabetes", package = "lars", envir = env)
    y <- env$diabetes$y
    x <- env$diabetes$x

  } else if (dataset_name == "diabetes2") {
    if (!requireNamespace("lars", quietly = TRUE)) {
      stop("Dataset 'diabetes2' requires the 'lars' package.", call. = FALSE)
    }
    env <- new.env()
    utils::data("diabetes", package = "lars", envir = env)
    y0 <- env$diabetes$y
    x0 <- env$diabetes$x

    norm0 <- normalize(y0, x0, scale = TRUE)
    x <- .mm_power(norm0$mX, power = 2)
    y <- norm0$vy

    # ---- energy (your shipped CSV) ----
  } else if (dataset_name == "energy2") {
    f <- .get_extdata_path("energydata_complete.csv")
    dat <- utils::read.csv(f)

    if (!("Appliances" %in% names(dat))) {
      stop("energydata_complete.csv must contain a column named 'Appliances'.", call. = FALSE)
    }

    y0 <- dat$Appliances
    x0 <- dat[, -c(1, 2), drop = FALSE]

    norm0 <- normalize(y0, x0, scale = TRUE)
    x <- .mm_power(norm0$mX, power = 2)
    y <- norm0$vy

  } else if (dataset_name == "energy3") {
    f <- .get_extdata_path("energydata_complete.csv")
    dat <- utils::read.csv(f)

    if (!("Appliances" %in% names(dat))) {
      stop("energydata_complete.csv must contain a column named 'Appliances'.", call. = FALSE)
    }

    y0 <- dat$Appliances
    x0 <- dat[, -c(1, 2), drop = FALSE]

    norm0 <- normalize(y0, x0, scale = TRUE)
    x <- .mm_power(norm0$mX, power = 3)
    y <- norm0$vy

    # ---- Crime (your shipped RData) ----
  } else if (dataset_name %in% c("Crime", "Crime2")) {
    f <- .get_extdata_path("comData.Rdata")
    e <- new.env(parent = emptyenv())
    base::load(f, envir = e)

    if (!exists("X", envir = e, inherits = FALSE) || !exists("Y", envir = e, inherits = FALSE)) {
      stop("comData.Rdata must contain objects named 'X' and 'Y'.", call. = FALSE)
    }

    X <- get("X", envir = e, inherits = FALSE)
    Y <- get("Y", envir = e, inherits = FALSE)

    if (!("murders" %in% colnames(Y))) {
      stop("Y must contain a column named 'murders'.", call. = FALSE)
    }

    # IMPORTANT: keep X and y aligned (your old code could misalign them)
    y0 <- Y[, "murders"]
    ok <- stats::complete.cases(X) & !is.na(y0)

    mX <- X[ok, , drop = FALSE]
    y  <- as.numeric(y0[ok])

    drop_cols <- intersect(colnames(mX), c("ownHousQrange", "rentUpperQ"))
    if (length(drop_cols) > 0) {
      mX <- mX[, setdiff(colnames(mX), drop_cols), drop = FALSE]
    }

    if (dataset_name == "Crime") {
      x <- mX
    } else {
      x <- .mm_power(mX, power = 2)
    }

    # ---- Hitters (CRAN dataset from ISLR) ----
  } else if (dataset_name %in% c("Hitters", "Hitters2")) {
    if (!requireNamespace("ISLR", quietly = TRUE)) {
      stop("Dataset 'Hitters' requires the 'ISLR' package.", call. = FALSE)
    }
    data("Hitters", package = "ISLR")
    Hitters <- stats::na.omit(ISLR::Hitters)

    if (dataset_name == "Hitters") {
      x <- stats::model.matrix(Salary ~ ., data = Hitters)[, -1, drop = FALSE]
    } else {
      x <- stats::model.matrix(Salary ~ .^2, data = Hitters)[, -1, drop = FALSE]
    }
    y <- as.numeric(Hitters[, "Salary"])

    # ---- BostonHousing2 (CRAN dataset from mlbench) ----
  } else if (dataset_name == "BostonHousing2") {
    if (!requireNamespace("mlbench", quietly = TRUE)) {
      stop("Dataset 'BostonHousing2' requires the 'mlbench' package.", call. = FALSE)
    }
    data("BostonHousing", package = "mlbench")
    BostonHousing <- stats::na.omit(BostonHousing)
    x <- stats::model.matrix(medv ~ .^2, data = BostonHousing)[, -1, drop = FALSE]
    y <- as.numeric(BostonHousing[, "medv"])

    # ---- flare eyedata (CRAN dataset from flare) ----
  } else if (dataset_name == "eyedata") {
    if (!requireNamespace("flare", quietly = TRUE)) {
      stop("Dataset 'eyedata' requires the 'flare' package.", call. = FALSE)
    }
    ey <- new.env()
    data("eyedata", package = "flare", envir = ey)

    if (exists("eyedata", envir = ey)) {
      x <- ey$eyedata$x
      y <- ey$eyedata$y
    } else {
      x <- ey$x
      y <- ey$y
    }

  } else {
    stop("Unknown dataset_name: '", dataset_name, "'.", call. = FALSE)
  }

  # ---- final normalize ----
  norm <- normalize(y, x, scale = TRUE)

  vy <- norm$vy
  mX <- norm$mX

  n <- length(vy)
  p <- ncol(mX)

  list(mX = mX, vy = vy, n = n, p = p)
}

# ---- external dataset helpers ----

generate_covid_data <- function() {
  meta_path <- .get_extdata_path("all_metadata.rds")
  dat_path  <- .get_extdata_path("all_bulk.rds")

  meta <- readRDS(meta_path)
  dat  <- readRDS(dat_path)

  if (!("meta_WHO_scores" %in% names(meta))) {
    stop("all_metadata.rds must contain meta_WHO_scores.", call. = FALSE)
  }

  inds_na <- which(is.na(meta$meta_WHO_scores))

  # dat is assumed features x samples; your previous logic used transpose
  x <- t(dat[, -inds_na, drop = FALSE])

  y <- meta$meta_WHO_scores[-inds_na]
  y[y == "1 or 2"] <- 1.5
  y <- as.numeric(y)

  list(x = x, y = y, dataset_name = "covid")
}

generate_cookie_data <- function() {
  cookie_path <- .get_extdata_path("cookie_data.csv")
  cookie <- utils::read.csv(cookie_path)

  if (ncol(cookie) < 704) {
    stop("cookie_data.csv must have at least 704 columns (700 X + 4 Y).", call. = FALSE)
  }

  x <- as.matrix(cookie[, 1:700, drop = FALSE])
  ymat <- as.matrix(cookie[, 701:704, drop = FALSE])
  y <- as.numeric(ymat[, 2])

  list(x = x, y = y, dataset_name = "cookie")
}

generate_riboflavin_data <- function() {

  if (!requireNamespace("hdi", quietly = TRUE)) {
    stop("Dataset 'riboflavin' requires the 'hdi' package.", call. = FALSE)
  }

  dat_env <- new.env()
  data("riboflavin", package = "hdi", envir = dat_env)

  dat <- dat_env$riboflavin

  list(
    x = as.matrix(dat$x),
    y = as.numeric(dat$y),
    dataset_name = "riboflavin"
  )
}


generate_qtl_data <- function() {
  resp_path <- .get_extdata_path("phe_simulat.csv")
  cov_path  <- .get_extdata_path("gen_simulat.csv")

  response   <- utils::read.table(resp_path, header = FALSE, sep = ",")
  covariates <- utils::read.table(cov_path, header = FALSE, sep = ",")

  n <- nrow(response)
  p <- ncol(covariates)

  # (p*(p+1))/2 columns for 2-way interactions incl squares
  P <- as.integer(p * (p + 1) / 2)
  X <- matrix(0, n, P)
  vbeta <- numeric(P)

  count <- 1L
  for (i in 1:p) {
    for (j in i:p) {
      if (i == j) {
        X[, count] <- covariates[, i]
      } else {
        X[, count] <- covariates[, i] * covariates[, j]
      }

      # signal pattern (unchanged)
      b <- 0
      if ((i == 1)   & (j == 1))     b <- 4.47
      if ((i == 21)  & (j == 21))    b <- 3.16
      if ((i == 31)  & (j == 31))    b <- 2.24
      if ((i == 51)  & (j == 51))    b <- 1.58
      if ((i == 71)  & (j == 71))    b <- 1.58
      if ((i == 91)  & (j == 91))    b <- 1.10
      if ((i == 101) & (j == 101))   b <- 1.10
      if ((i == 111) & (j == 111))   b <- 0.77
      if ((i == 121) & (j == 121))   b <- 0.77
      if ((i == 1)   & (j == 11))    b <- 1.00
      if ((i == 2)   & (j == 119))   b <- 3.87
      if ((i == 10)  & (j == 91))    b <- 1.30
      if ((i == 15)  & (j == 75))    b <- 1.73
      if ((i == 20)  & (j == 46))    b <- 1.00
      if ((i == 21)  & (j == 22))    b <- 1.00
      if ((i == 26)  & (j == 91))    b <- 1.00
      if ((i == 41)  & (j == 61))    b <- 0.71
      if ((i == 56)  & (j == 91))    b <- 3.16
      if ((i == 65)  & (j == 85))    b <- 2.24
      if ((i == 86)  & (j == 96))    b <- 0.89
      if ((i == 101) & (j == 105))   b <- 1.00
      if ((i == 111) & (j == 121))   b <- 2.24

      vbeta[count] <- b
      count <- count + 1L
    }
  }

  sigma2.true <- 20
  y_mu <- X %*% vbeta
  y <- as.vector(y_mu + stats::rnorm(n, 0, sqrt(sigma2.true)))

  X_std <- scale(X, center = TRUE, scale = TRUE)

  list(y = y, x = X_std, beta = vbeta)
}
