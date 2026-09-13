# ==========================
# ==========================
## Data sets
# ==========================
# ==========================


# Internal helper: locate a file shipped in inst/extdata
.get_extdata_path <- function(fname, subdir = NULL) {
  f <- if (is.null(subdir)) {
    system.file("extdata", fname, package = "BayesianLasso")
  } else {
    system.file("extdata", subdir, fname, package = "BayesianLasso")
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
  
  # ---------- diabetes2 dataset ---------------------
  
  if (dataset_name=="diabetes2") 
  {
    if (!requireNamespace("lars", quietly = TRUE)) {
      install.packages("lars")
    }
    data(diabetes, package = "lars")
    
    y = diabetes$y
    x = diabetes$x
    inds = 1:ncol(x)
    
    
    # Normalizing and scaling the dataset by function normalize()
    norm = BayesianLasso::normalize(y,x, scale = TRUE)
    x = norm$mX
    x <- model.matrix(~.^2, data=data.frame(x=x))[,-1]
    y <- norm$vy
    
    n = nrow(x)
    p = ncol(x)
    
  }
  
  
  
  # ---------------- Kakadu2 dataset --------------------------
  if (dataset_name=="Kakadu2") 
  {
    if (!requireNamespace("Ecdat", quietly = TRUE)) {
      install.packages("Ecdat")
    }
    
    data("Kakadu", package = "Ecdat")
    
    y <- as.vector(Kakadu$income)
    x <- Kakadu[,c(1:20,22)]  
    
    x <- model.matrix(~.^2,data=x)[,-1]
    
    n = nrow(x)
    p = ncol(x)
  }
  
  # -------------- Crime dataset ---------------
  if (dataset_name == "Crime") {
    
    # ---- Load dataset from reproduction/extdata ----
    rdata_path <- file.path("extdata", "comData.Rdata")
    
    if (!file.exists(rdata_path)) {
      stop("Cannot find comData.Rdata in reproduction/extdata/. ",
           "Make sure you are running reproduce_all.R from the 'reproduction/' directory.")
    }
    
    load(rdata_path)  # should create objects like X and Y
    
    # ---- Remove rows with NA while keeping X and Y aligned ----
    datXY <- na.omit(cbind(as.data.frame(X), as.data.frame(Y)))
    X2 <- as.matrix(datXY[, colnames(X), drop = FALSE])
    Y2 <- as.matrix(datXY[, colnames(Y), drop = FALSE])
    
    # ---- Drop unwanted columns safely ----
    drop_cols <- c("ownHousQrange", "rentUpperQ")
    X2 <- X2[, !colnames(X2) %in% drop_cols, drop = FALSE]
    
    # ---- Define regression inputs ----
    x <- X2
    y <- as.vector(Y2[, "murders"])
    varnames <- colnames(x)
    inds <- seq_len(ncol(x))
    
    n = nrow(x)
    p = ncol(x)
  }
  
  # ---- Eyedata (CRAN dataset from flare) ----
  if (dataset_name == "eyedata") {
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
    n = nrow(x)
    p = ncol(x)
  }
  
  # ---------- Cookie dataset ---------------------------------
  if (dataset_name == "cookie") {
    cookie_path <- file.path("extdata", "cookie_data.csv")
    if (!file.exists(cookie_path))
      stop("Cannot find cookie_data.csv in reproduction/extdata/")
    cookie <- utils::read.csv(cookie_path)
    x <- as.matrix(cookie[, 1:700, drop = FALSE])
    y <- as.numeric(cookie[, 702])   # column 702 = second Y column
    n <- nrow(x); p <- ncol(x)
  }
  
  # ---------- riboflavin dataset ---------------------------------
  if (dataset_name == "riboflavin") {
    if (!requireNamespace("hdi", quietly = TRUE)) {
      stop("Dataset 'riboflavin' requires the 'hdi' package.", call. = FALSE)
    }
    
    dat_env <- new.env()
    data("riboflavin", package = "hdi", envir = dat_env)
    
    dat <- dat_env$riboflavin
    x <- as.matrix(dat$x)
    y <- as.numeric(dat$y)
    n = nrow(x)
    p = ncol(x)
    
    # list(
    #   x = x,
    #   y = y,
    #   dataset_name = "riboflavin",
    #   n = nrow(x),
    #   p = ncol(x)
    # )
    
  }
  
  list(mX = x, vy = y, n = n, p = p)
  
}



