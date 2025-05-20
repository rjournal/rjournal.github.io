library(SIHR)
library(ggplot2)
library(reshape)
library(BGLR)

# Motif Regression --------------------------------------------------------

# Load data
load("../data/fornicolai-subset.RData")
colnames(X) <- paste0("X",1:ncol(X))
head(cbind(y, X[,1:4]))

# Run SIHR and store results
Mat <- matrix(NA, nrow = 666, ncol = 4)
Est <- LF(X = X, y = y, loading.mat = diag(ncol(X)))
Mat[, 1] <- Est$est.debias.vec
Mat[, 2] <- Est$se.vec
Mat[, 3] <- Est$est.debias.vec - qnorm(0.975) * Est$se.vec
Mat[, 4] <- Est$est.debias.vec + qnorm(0.975) * Est$se.vec

# Prepare data for visualization
CI.mat <- Mat[, 3:4]
colnames(CI.mat) <- c("lower", "upper")
df <- data.frame(motif = 1:666, CI.mat, point = Mat[, 1])
ind.pos <- (1:666)[(CI.mat[, 1] > 0)]
ind.neg <- (1:666)[(CI.mat[, 2] < 0)]
ind.neu <- (1:666)[(CI.mat[, 1] < 0) & (CI.mat[, 2] > 0)]

# Plot
ggplot() +
  # neutral points
  geom_linerange(
    data = df[ind.neu, ],
    mapping = aes(x = motif, ymin = lower, ymax = upper), linewidth = 0.2, color = "lightgray"
  ) +
  # positive points
  geom_point(
    data = df[ind.pos, ],
    mapping = aes(x = motif, y = point), color = "#E41A1C", size = 2
  ) +
  geom_errorbar(
    data = df[ind.pos, ],
    mapping = aes(x = motif, ymin = lower, ymax = upper), width = 8, size = 0.8, color = "#E41A1C"
  ) +
  # negative points
  geom_point(
    data = df[ind.neg, ],
    mapping = aes(x = motif, y = point), color = "#377EB8", size = 2
  ) +
  geom_errorbar(
    data = df[ind.neg, ],
    mapping = aes(x = motif, ymin = lower, ymax = upper), width = 8, size = 0.8, color = "#377EB8"
  ) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 1) +
  labs(x = "Motif Index", y = "Regression Coefs") +
  xlim(c(-5, 671)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", colour = "black"),
    axis.title.x = element_text(size = 11.5, colour = "black"),
    axis.title.y = element_text(size = 11.5, colour = "black")
  )


# Glutose ---------------------------------------------------------------

# Prepare data
data.chem <- read.table(file = "../data/biochemistry.txt", header = T, sep = "\t", fill = T)
data(mice)
glu.merge <- data.chem
coord.int <- colnames(data.chem)[c(9:11, 17)]
subset1 <- rownames(mice.X)[which(rownames(mice.X) %in% glu.merge$SUBJECT.NAME)]
glu.merge <- glu.merge[glu.merge$SUBJECT.NAME %in% subset1, ]
notna <- which(rowSums(is.na(glu.merge[, coord.int])) == 0 &
  !is.na(glu.merge$EndNormalBW))
glu.merge <- glu.merge[notna, ]
outcome.c <- (glu.merge$Biochem.Glucose >= 11.1)
geno <- mice.X[rownames(mice.X) %in% subset1[notna], ]
geno <- scale(geno)
glu.merge$GENDER <- ifelse(glu.merge$GENDER == "M", 1, 0)
x <- cbind(as.numeric(glu.merge$GENDER), glu.merge$Age)
colnames(x) <- c("gender", "age")
x.scaled <- scale(x)
Z <- geno
X <- x.scaled
length(which(apply(Z, 2, var) > quantile(apply(Z, 2, var), 0.75)))
Z <- Z[, which(apply(Z, 2, var) > quantile(apply(Z, 2, var), 0.75))]
ZX <- cbind(Z, X)
head(cbind(y, ZX[,1:4]))

# Run SIHR and store results
Mat <- matrix(NA, nrow = 2341, ncol = 4)
Est <- LF(X = ZX, y = as.numeric(outcome.c), model = "logistic", loading.mat = diag(2343))
Mat[, 1] <- Est$est.debias.vec[1:2341]
Mat[, 2] <- Est$se.vec[1:2341]
Mat[, 3] <- Est$est.debias.vec[1:2341] - qnorm(0.975) * Est$se.vec[1:2341]
Mat[, 4] <- Est$est.debias.vec[1:2341] + qnorm(0.975) * Est$se.vec[1:2341]

# Prepare data for visualization
CI.mat <- Mat[, 3:4]
colnames(CI.mat) <- c("lower", "upper")
df <- data.frame(gene = 1:2341, CI.mat, point = Mat[, 1])
ind.pos <- (1:2341)[(CI.mat[, 1] > 0)]
ind.neg <- (1:2341)[(CI.mat[, 2] < 0)]
ind.neu <- (1:2341)[(CI.mat[, 1] < 0) & (CI.mat[, 2] > 0)]

# Plot
ggplot() +
  # neutral points
  geom_linerange(
    data = df[ind.neu, ],
    mapping = aes(x = gene, ymin = lower, ymax = upper), linewidth = 0.2, color = "lightgray"
  ) +
  # positive points
  geom_point(
    data = df[ind.pos, ],
    mapping = aes(x = gene, y = point), color = "#E41A1C", size = 2
  ) +
  geom_errorbar(
    data = df[ind.pos, ],
    mapping = aes(x = gene, ymin = lower, ymax = upper), width = 30, size = 0.8, color = "#E41A1C"
  ) +
  # negative points
  geom_point(
    data = df[ind.neg, ],
    mapping = aes(x = gene, y = point), color = "#377EB8", size = 2
  ) +
  geom_errorbar(
    data = df[ind.neg, ],
    mapping = aes(x = gene, ymin = lower, ymax = upper), width = 30, size = 0.8, color = "#377EB8"
  ) +
  geom_hline(yintercept = 0, linetype = 2, linewidth = 1) +
  labs(x = "Gene Index", y = "Regression Coefs") +
  xlim(0, 2341) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", colour = "black"),
    axis.title.x = element_text(size = 11.5, colour = "black"),
    axis.title.y = element_text(size = 11.5, colour = "black")
  )
