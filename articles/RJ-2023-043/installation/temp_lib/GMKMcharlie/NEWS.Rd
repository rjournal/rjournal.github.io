\name{NEWS}
\alias{NEWS}
\title{FLSSS updates}


\section{Changes in GMKMcharlie version 1.1.5 (2020525)}{
  For the classical GMM trainer,
  \itemize{
    \item Improved the interface and overall computing accuracy and efficiency for extremely high dimensional problems.
    \item Added the option of embedding small positive constants on the diagonal entries of covariance matrices.
    \item Added the option of running K-means and K-means++ beforehand.
  }
}


\section{Changes in GMKMcharlie version 1.1.0 (20201023)}{
  \itemize{
    \item Adjusted the method of copying bits to avoid warning messages from 32-bit compilers.
    \item Enabled classical GMM trainer to fix any of the model parameters, including mixture weights, means and covariances of the Gaussian components, during training.
  }
}
