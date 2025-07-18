# Generated by `rjournal_pdf_article()` using `knitr::purl()`: do not edit by hand
# Please edit GPUmatrixPaper.Rmd to modify this file

## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
library(plotly)
library(ggplot2)
library(palmerpenguins)
library(kableExtra)
library(Matrix)


## ----echo=TRUE,message=TRUE,warning=TRUE--------------------------------------
library(GPUmatrix)
#R matrix initialization
m <- matrix(c(1:10)+40,5,2)
#Show CPU matrix
m

#GPU matrix initialization
Gm <- gpu.matrix(c(1:10)+40,5,2)
#Show GPU matrix
Gm


## ----echo=TRUE----------------------------------------------------------------
Gm[c(2,3),1]

Gm[,2]
 
Gm2 <- cbind(Gm[c(1,2),], Gm[c(3,4),])
Gm2

Gm2[1,3] <- 0
Gm2


## ----echo=TRUE----------------------------------------------------------------
Gm3 <- gpu.matrix(nrow = 2,ncol=3)
Gm3[,2]
Gm3[1,2] <- 1 
Gm3
Gm3[1,3] <- 0
Gm3


## ----echo=FALSE---------------------------------------------------------------
df <- data.frame(
  MatrixClass = c("matrix", "data.frame", "integer", "numeric", "dgeMatrix", "ddiMatrix", "dpoMatrix", "dgCMatrix", "float32", "torch_tensor", "tensorflow.tensor"),
  Package = c("base", "base", "base", "base", "Matrix", "Matrix", "Matrix", "Matrix", "float", "torch", "tensorflow"),
  DataTypeDefault = c("float64", "float64", "float64", "float64", "float64", "float64", "float64", "float64", "float32", "float64", "float64"),
  SPARSE = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, "Depends of tensor type", "Depends of tensor type"),
  BackCast = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
)


## ----cast-packages-latex, eval = knitr::is_latex_output()---------------------
 knitr::kable(df, format = "latex",caption = "Cast options from other packages. If back cast is TRUE, then it is possible to convert a gpu.matrix to this object and vice versa. If is FALSE, it is possible to convert these objects to gpu.matrix but not vice versa.")


## ----cast-packages-html, eval = knitr::is_html_output(), layout = "l-body-outset"----
#  knitr::kable(df, format = "html",caption = "Cast options from other packages. If back cast is TRUE, then it is possible to convert a gpu.matrix to this object and vice versa. If is FALSE, it is possible to convert these objects to gpu.matrix but not vice versa.")


## ----echo=TRUE----------------------------------------------------------------
m <- matrix(c(1:10)+40,5,2)
Gm <- gpu.matrix(m)
Gm


## ----echo=TRUE----------------------------------------------------------------
library(Matrix)
M <- Matrix(c(1:10)+40,5,2)
Gm <- gpu.matrix(M)
Gm


## ----echo=TRUE----------------------------------------------------------------
library(float)
mfloat32 <- fl(m)
Gm <- gpu.matrix(mfloat32)
Gm


## ----echo=TRUE----------------------------------------------------------------
Ms <- Matrix(sample(0:1, 10, replace = TRUE), nrow=5, ncol=2, sparse=TRUE)
Ms
 
Gms <- gpu.matrix(Ms)
Gms


## ----echo=TRUE----------------------------------------------------------------
Gm32 <- gpu.matrix(c(1:10)+40,5,2, dtype = "float32")
Gm32


## ----echo=TRUE----------------------------------------------------------------
Ms <- Matrix(sample(0:1, 10, replace = TRUE), nrow=5, ncol=2, sparse=TRUE)
Gm32 <- gpu.matrix(Ms, dtype = "float32", sparse = F)
Gm32


## ----echo=TRUE----------------------------------------------------------------
Gms32 <- to_sparse(Gm32)
Gms32


## ----echo=TRUE----------------------------------------------------------------
Gms64 <- Gms32
dtype(Gms64) <- "float64"
Gms64


## ----echo=TRUE----------------------------------------------------------------
(Gm + Gm) == (m + m)

(Gm + M) == (mfloat32 + Gm)

(M + M) == (mfloat32 + Gm)


## ----echo=FALSE---------------------------------------------------------------
df_operators <- data.frame(
  MathematicalOperators = c("log", "log2", "log10", "cos", "cosh", "acos", "acosh", "sin", "sinh", "asin", "asinh", "tan", "atan", "tanh", "atanh", "sqrt", "abs", "sign", "ceiling", "floor", "cumsum", "cumprod", "exp", "expm1"),
  Usage = c("log(Gm)", "log2(Gm)", "log10(Gm)", "cos(Gm)", "cosh(Gm)", "acos(Gm)", "acosh(Gm)", "sin(Gm)", "sinh(Gm)", "asin(Gm)", "asinh(Gm)", "tan(Gm)", "atan(Gm)", "tanh(Gm)", "atanh(Gm)", "sqrt(Gm)", "abs(Gm)", "sign(Gm)", "ceiling(Gm)", "floor(Gm)", "cumsum(Gm)", "cumprod(Gm)", "exp(Gm)", "expm1(Gm)")
)


## ----math-operators, eval = knitr::is_latex_output()--------------------------
 knitr::kable(df_operators, format = "latex",caption = "Mathematical operators that accept a gpu.matrix as input.")


## ----math-operators-html, eval = knitr::is_html_output(), layout = "l-body-outset"----
#  knitr::kable(df_operators, format = "html",caption = "Mathematical operators that accept a gpu.matrix as input.")


## ----echo=FALSE---------------------------------------------------------------
df_complex <- data.frame(
  MathematicalOperators = c("Re", "Im", "Conj", "Arg", "Mod"),
  Usage = c("Re(Gm)", "Im(Gm)", "Conj(Gm)", "Arg(Gm)", "Mod(Gm)")
)



## ----complex-operators, eval = knitr::is_latex_output()-----------------------
 knitr::kable(df_complex, format = "latex",caption = "Complex operators that accept a gpu.matrix with complex type as input.")


## ----complex-operators-html, eval = knitr::is_html_output(), layout = "l-body-outset"----
#  knitr::kable(df_complex, format = "html",caption = "Complex operators that accept a gpu.matrix with complex type as input.")


## ----echo=TRUE----------------------------------------------------------------
library(GPUmatrix)
m <- matrix(c(1:10)+40,5,2)
Gm <- gpu.matrix(c(1:10)+40,5,2)

head(tcrossprod(m),2)

head(tcrossprod(Gm),2)

Gm <- tail(Gm,3)
rownames(Gm) <- c("a","b","c")
tail(Gm,2)

colMaxs(Gm)


## ----echo=TRUE----------------------------------------------------------------
#GPUmatrix initialization with CPU option
Gm <- gpu.matrix(c(1:10)+40,5,2,device="cpu")
#Show CPU matrix from GPUmatrix
Gm


## ----plotNMFgpumatrix, out.width = "100%", out.height = "50%", fig.cap = "Computation time (in seconds) of non-negative factorization for MKL-R (i.e. R with the optimized MKL BLAS library, solid green), solid lines for CPU, dashed lines for GPU with CUDA, pink lines for GPUmatrix with float64, and blue lines for GPUmatrix with float32. Time shown in y-axis is in logarithmic scale. All calculations are performed on square matrices. The x-axis represents the number of rows in the matrices. The internal size of the factorization is 10."----
knitr::include_graphics("figures/plotNMFgpumatrix.png")


## ----LRGC, out.width = "100%", out.height = "50%", fig.cap = "Computation time (in seconds) of the logistic regression using the conjugate gradient method for MKL-R (i.e. R with the optimized MKL BLAS library, solid green), solid lines for CPU, dashed lines for GPU with CUDA, pink lines for GPUmatrix with float64, and blue lines for GPUmatrix with float32. Time shown in y-axis is in logarithmic scale. The calculations are performed on random matrices whose size are n x (n/100). Therefore, the leftmost part of the graph shows the computing time for a 10,000 x 100 matrix and the rightmost part a 50,000 x 500 matrix."----
knitr::include_graphics("figures/LRGC_glm.png")


## ----LRGC-sparse, out.width = "100%", out.height = "50%", fig.cap = "Computation time (in seconds) of the logistic regression using the conjugate gradient method in a sparse matrix. Solid green for MKL-R dense case  (i.e. the computation is performed without any consideration of the sparsity of the matrix). Solid lines for CPU, dashed lines for GPU with CUDA, pink lines for GPUmatrix dense with float64, blue lines for GPUmatrix dense with float32, yellow lines for GPUmatrix sparse with float32, orange lines for GPUmatrix sparse with float64. Violet line, using Matrix package(that implicitly considers the matrix to be sparse). Time shown in y-axis is in logarithmic scale. The calculations are performed on random matrices whose size are n x (n/100). Therefore, the leftmost part of the graph shows the computing time for a 10,000 x 100 matrix and the rightmost part a 50,000 x 500 matrix."----

knitr::include_graphics("figures/LRGC.png")


## ----echo=TRUE----------------------------------------------------------------

# Standard glm

counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
summary(glm.D93)


# Using speedglm
library(speedglm)
sglm.D93 <- speedglm(counts ~ outcome + treatment, family = poisson())
summary(sglm.D93)

# GPU glm
library(GPUmatrix)
gpu.glm.D93 <- GPUglm(counts ~ outcome + treatment, family = poisson())
summary(gpu.glm.D93)


## ----GLMplot, out.width = "100%", out.height = "50%", fig.cap = "Computation time (in seconds) of general linear model using *speedglm* function with MKL-R matrix (i.e. R with the optimized MKL BLAS library, solid green), solid black line for glm function, solid lines for CPU, dashed lines for GPU with CUDA, pink lines for GPUmatrix with float64, and blue lines for GPUmatrix with float32. Time shown in y-axis is in logarithmic scale. The calculations are performed on random matrices whose size are n x (n/10). Therefore, the leftmost part of the graph shows the computing time for a 1,000 x 100 matrix and the rightmost part a 10,000 x 1000 matrix."----
knitr::include_graphics("figures/plotGLMRes.png")


## ----FunctionTime, out.width = "100%", out.height = "80%", fig.cap = "Computation time (in seconds) where MKL-R solid green, solid lines for CPU, dashed lines for GPU with CUDA, pink lines for GPUmatrix with float64 and blue lines for GPUmatrix with float32. All calculations are performed on square matrices. The x-axis represents the number of rows in the matrices. The operations are the element-wise or Hadamard product of two matrices, the exponential of each element of a matrix, the mean of the rows of a matrix, the standard matrix product, the inverse of a matrix, and the singular value decomposition of a matrix (SVD)."----
knitr::include_graphics("figures/performanceComparison.png")


## ----SparseFunctionTime, out.width = "100%", out.height = "30%", fig.cap = "Computation time (in seconds) for the Matrix package (solid violet), yellow lines for GPUmatrix with float32, orange lines for GPUmatrix with float64, solid lines for CPU and dashed lines for GPU with CUDA. Time shown in y-axis is in logarithmic scale. The small model is a random square matrix of size 2,000 x 2,000. The proportion of non-zero elements is either 0.001 or 0.01. The large model is a 20,000 x 20,000 matrix with the same proportion of non-zero elements. The element-wise multiplication is performed on the sparse matrix. The right panel shows the time required to multiply these matrices by dense matrices whose sizes are 2,000 x 500 and 20,000 x 500, respectively."----
knitr::include_graphics("figures/SparsePerformanceComparison.png")

