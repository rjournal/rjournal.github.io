\name{NEWS} \title{News for Package \pkg{rebmix}}
\newcommand{\cpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}
\section{Changes in version 2.15.0}{ 
\itemize{
\item Method \code{"labelmoments"} is added.
\item Method \code{"mergelabels"} is added.
\item Adjacency matrix \code{A} slot is added to \code{"RCLRMIX"} class.
\item Improved C++ functions \code{CombineComponentsEntropy()} and \code{CombineComponentsDemp()}.
}
}
\section{Changes in version 2.14.2}{ 
\itemize{
\item Valgrind problems have been resolved.
}
}
\section{Changes in version 2.14.1}{ 
\itemize{
\item Class \code{"Histogram"} is added.
\item Methods \code{"chistogram"}, \code{"fhistogram"} and \code{mapclusters} are added.
\item Argument \code{y0} is removed from \code{REBMIX}, \code{bins}, \code{optbins} methods and from class \code{"REBMIX"}.
\item The C++ function \code{REBMIXK()} is added. It is called by the \code{REBMIX} method if the argument \code{Dataset} contains objects of type \code{"Histogram"}.
\item Numerous minor changes and debugging in C++ and R codes required due to the above changes.
\item Methods \code{"demix"}, \code{"dfmix"}, \code{"pemix"}, \code{"pfmix"} and all information criterion methods are modified for class \code{"Histogram"}.
}
}
\section{Changes in version 2.14.0}{ 
\itemize{
\item The package has been extended to three parametric family types.
\item Improved C++ functions \code{REBMIXKNN()}, \code{REBMIXKDE()} and \code{REBMIXH()}.
\item Method \code{plot} is debugged and improved.
\item Value \code{"cdf"} is added to \code{what} argument.
\item Class \code{"EMMIX.Theta"} is added.
\item Method \code{EMMIX} is added.
}
}
\section{Changes in version 2.13.1}{ 
\itemize{
\item Reduced size of the vignette.
\item Conditional jump or move depends on uninitialised value(s.
}
}
\section{Changes in version 2.13.0}{ 
\itemize{
\item Method \code{plot} is debugged and improved.
\item Improved C++ functions \code{REBMIXKNN()}, \code{REBMIXKDE()} and \code{REBMIXH()}.
\item EM algorithm is added to the C++ code for the improved estimation of all mixture model parameters.
\item Updated examples.
}
}
\section{Changes in version 2.12.0}{ 
\itemize{
\item Value \code{"density"} is replaced by \code{"pdf"} in \code{what} argument.
\item Value \code{"marginal"} is replaced by \code{"marginal pdf"} in \code{what} argument.
\item Value \code{"distribution"} is replaced by \code{"marginal cdf"} in \code{what} argument.
\item Improved C++ functions regarding memory allocation and efficiency.
\item Improved outlier detection.
\item \code{"Knuth equal"} and \code{"Knuth unequal"} binning rules are added to the existing \code{"Sturges"}, \code{"Log10"} and \code{"RootN"} rules.
\item Method \code{plot} is improved.
\item Method \code{RCLRMIX} is debugged and improved.
\item Method \code{REBMIX} is debugged and improved.
\item Method \code{bins} is added.
\item Method \code{optbins} is added.
\item Fixed bugs in C++ and R code.
}
}
\section{Changes in version 2.11.0}{ 
\itemize{
\item Method \code{split} is improved.
\item Improved C++ functions \code{REBMIXKNN()}, \code{REBMIXKDE()} and \code{REBMIXH()}.
\item Method \code{RCLSMIX} is improved.
\item Method \code{plot} is improved.
\item Continuous parametric family \code{"Gumbel"} is added.  
\item EM algorithm is added to the C++ code for the improved estimation of the Gaussian mixture model parameters.
\item Class \code{EM.Control} is added for EM algorithm control parameters handling.
}
}
\section{Changes in version 2.10.3}{ 
\itemize{
\item Method \code{Zp} is replaced by \code{a.Zp} accessor method.
\item Method \code{coef} is replaced by \code{a.theta1.all} and \code{a.theta2.all} accessor methods.
\item Accessor methods are added for all slots.
\item Class \code{"RNGMIX.Theta"} is added.
\item Method \code{show} is added for \code{"RCLS.chunk"} class.
\item Minimum number of component \code{cmin} is added to \code{REBMIX} arguments.
\item Minimum number of component \code{cmin} slot is added to \code{"REBMIX"} class.
\item Improved C++ functions \code{REBMIXKNN()}, \code{REBMIXKDE()} and \code{REBMIXH()}.
\item Preprocessing \code{"Parzen window"} is renamed to \code{"kernel density estimation"}.
\item Rough parameter estimation for binomial and Poisson parametric families is improved.
\item Latent class analysis is added.
}
}
\section{Changes in version 2.10.2}{ 
\itemize{
\item Method \code{split} is improved.
\item Fixed GCC 8.1 notes and warnings in C++ functions.
}
}
\section{Changes in version 2.10.1}{ 
\itemize{
\item Fixed numerical problems related to high dimensional datasets.
\item Fixed bug in \code{REBMIX} method.
\item Improved C++ functions \code{REBMIXKNN()}, \code{REBMIXPW()} and \code{REBMIXH()}.
\item Improved C++ functions \code{RoughEstimationKNN()}, \code{RoughEstimationPW()} and \code{RoughEstimationH()}.
}
}
\section{Changes in version 2.10.0}{ 
\itemize{
\item Fixed bug in \code{plot} method.
\item Additional argument \code{rseed} enters \code{boot} method.
\item Maximum number of nonempty bins has been introduced in C++ to avoid histograms with excessive number of nonempty bins.
}
}
\section{Changes in version 2.9.3}{ 
\itemize{
\item Acceleration rate has been improved. It is now progressively increasing.
\item The Newton-Raphson root finding in C++ functions has been improved. It affects only \code{"Weibull"}, \code{"gamma"} and \code{"vonMises"} parametric families.
}
}
\section{Changes in version 2.9.2}{ 
\itemize{
\item Continuous circular parametric family \code{"vonMises"} is added.
\item Fixed bug in C++ function \code{REBMIXH}.
\item Improved C++ functions \code{InformationCriterionKNN()}, \code{InformationCriterionPW()} and \code{InformationCriterionH()}.
\item Improved C++ functions \code{EnhancedEstimationKNN()}, \code{EnhancedEstimationPW()} and \code{EnhancedEstimationPW()}.
}
}
\section{Changes in version 2.9.1}{ 
\itemize{
\item Properly registered native routines and disabled symbol search in C++ code.
}
}
\section{Changes in version 2.9.0}{ 
\itemize{
\item Fixed bugs in \code{RCLRMIX} method.
}
}
\section{Changes in version 2.8.4}{ 
\itemize{
\item Method \code{Zp} is added.
\item Fixed bugs in \code{plot} method.
\item Fixed bugs in \code{split} method.
\item Fixed bugs in \code{RCLRMIX} method.
\item Fixed bugs in \code{RCLSMIX} method.
\item Improved C++ functions \code{GlobalModeKNN()}, \code{GlobalModePW()} and \code{GlobalModeH()}.
\item Improved C++ functions \code{REBMIXKNN()}, \code{REBMIXPW()} and \code{REBMIXH()}.
}
}
\section{Changes in version 2.8.3}{ 
\itemize{
\item Fixed convergence bug in C++ code.
\item Bayes classification is improved by introducing outliers.
\item Method \code{plot} is improved.
\item Fixed bug in C++ functions \code{RoughEstimationKNN()}, \code{RoughEstimationPW()} and \code{RoughEstimationH()}.
\item Improved C++ functions \code{GlobalModeKNN()}, \code{GlobalModePW()} and \code{GlobalModeH()}.
}
}
\section{Changes in version 2.8.2}{ 
\itemize{
\item Fixed information criterion bug in C++ and R code.
\item Fixed heap block freed with an inappropriate deallocation function bug in C++ code.
\item Method \code{plot} is improved.
\item Methods \code{demix}, \code{dfmix}, \code{pemix} and \code{pfmix} are improved.
\item Methods \code{split}, \code{chunk} and \code{BFSMIX} are added.
\item Renewed vignette.
}
}
\section{Changes in version 2.8.1}{ 
\itemize{
\item Fixed isnan and isinf bug in C++ code.
\item Fixed invalid object bug in vignette.
}
}
\section{Changes in version 2.8.0}{ 
\itemize{
\item Object-oriented C++ code is improved considerably.
\item R code is extended and rewritten in S4 class system.
\item Multivariate normal parametric family with unrestricted variance-covariance matrices is enabled.
\item Classification is improved.
\item Clustering is added.
\item Datasets \code{iris} is added. 
\item Renewed vignette.
\item Fixed bug in C++ functions \code{RoughLognormalParameters()}, \code{RoughWeibullParameters()} and \code{RoughGammaParameters()}.
}
}
\section{Changes in version 2.7.2}{ 
\itemize{
\item Switch \code{_TIME_LEFT_SWITCH} is eliminated from C code.
\item C code is converted into object-oriented C++ code.
\item Fixed no visible global function definition bug in \code{NAMESPACE} file.
\item Renewed vignette.
}
}
\section{Changes in version 2.7.1}{ 
\itemize{
\item Demos \code{rebmix.galaxy}, \code{rebmix.univariate.normal}, \code{rebmix.mixed.continuous.discrete} and 
\code{rebmix.multivariate.normal} are changed. 
}
}
\section{Changes in version 2.7.0}{ 
\itemize{
\item Fixed title field in \code{DESCRIPTION} file.
\item Fixed S3 generic/method consistency bug in \code{NAMESPACE} file.
\item Fixed line endings in \code{Makevars} file.
\item Fixed \code{\\VignetteKeyword} entries bug in vignette.
\item Method \code{predict.list} is converted to function \code{RCLSMIX}.
\item Method \code{boot} is converted to function \code{boot.REBMIX}.
\item Method \code{plot} is improved.
\item Function \code{kseq} is improved.
\item Function \code{REBMIX} is improved.
\item Total of positive relative deviations \code{D} is eliminated from \code{REBMIX} arguments.
\item Value \code{"c"} is eliminated from \code{what} argument.
\item Value \code{"K"} is added to \code{what} argument.
\item Fixed bug in C functions \code{InformationCriterionKNN()}, \code{InformationCriterionPW()} and \code{InformationCriterionH()}.
\item Fixed bug illegal characters found in URL.
\item Methods \code{AIC}, \code{AIC3}, \code{AIC4}, \code{AICc}, \code{CAIC}, \code{BIC}, \code{HQC}, \code{MDL2}, \code{MDL5}, \code{AWE}, \code{CLC}, \code{ICL}, \code{ICLBIC}, \code{PRD}, \code{SSE} and \code{PC} are converted to functions with the same names.
\item Renewed vignette.
}
}
\section{Changes in version 2.6.2}{ 
\itemize{
\item Demos \code{rebmix.galaxy}, \code{rebmix.univariate.normal}, \code{rebmix.mixed.continuous.discrete}, 
\code{rebmix.multivariate.normal}, \code{rebmix.weibullnormal} and \code{rebmix.truck} are added. 
\item Datasets \code{weibullnormal} and \code{truck} are added. 
\item Method \code{boot.REBMIX} for bootstrap is improved. 
\item Function \code{kseq} for sequence of bins or nearest neighbours generation is added. 
\item Additional argument \code{y0} enters function \code{REBMIX}.
\item Improved treatment of accuracy \code{Eps} in C functions.
\item Fixed bug in C functions \code{EnhancedEstimationKNN()}, \code{EnhancedEstimationPW()} and \code{EnhancedEstimationH()}.
}
}
\section{Changes in version 2.6.1}{ 
\itemize{
\item Renewed vignette. 
\item Renewed references in Reference manual.
\item Method \code{PD} is added.  
\item Fixed bug in C functions \code{REBMIXKNN()}, \code{REBMIXPW()} and \code{REBMIXH()}.
}
}
\section{Changes in version 2.6.0}{ 
\itemize{ 
\item Switch \code{_TIME_LEFT_SWITCH} introduced in C code.
\item Simplified treatment of loose restraints in C functions \code{RoughEstimationKNN()}, \code{RoughEstimationPW()} and \code{RoughEstimationH()}.
\item Fixed bug in C functions \code{REBMIXKNN()}, \code{REBMIXPW()} and \code{REBMIXH()}.
\item Minimum weight multiplier \code{b} is eliminated from \code{REBMIX} arguments.
\item Fixed source and destination overlap in memcpy bug in C code.
}
}
\section{Changes in version 2.5.1}{ 
\itemize{ 
\item Fixed checking top-level files.
\item Fixed checking for old-style vignette sources.
\item Packages mixtools and mclust excluded from vignette.
}
}
\section{Changes in version 2.5.0}{ 
\itemize{ 
\item Rough parameter estimation is improved by introducing constrained entropy maximization.
\item Fixed bug in C functions \code{RoughEstimationKNN()}, \code{RoughEstimationPW()} and \code{RoughEstimationH()}.
\item Fixed bug in \code{boot.REBMIX} method.
}
}
\section{Changes in version 2.4.2}{ 
\itemize{ 
\item Fixed symbol eating bug for Mac OS X.  
}
}
\section{Changes in version 2.4.1}{ 
\itemize{ 
\item Continuous parametric family \code{"gamma"} is added.  
}
}
\section{Changes in version 2.4.0}{ 
\itemize{ 
\item Methods \code{AIC}, \code{AIC3}, \code{AIC4}, \code{AICc} and \code{CAIC} are added. 
\item Method \code{AWE} is added. 
\item Method \code{BIC} is added. 
\item Method \code{boot.REBMIX} for bootstrap is added. 
\item Method \code{CLC} is added. 
\item Function \code{demix} for empirical density calculation is added. 
\item Function \code{dfmix} for predictive density calculation is added. 
\item Method \code{HQC} is added. 
\item Method \code{ICL} is added. 
\item Method \code{ICLBIC} is added. 
\item Method \code{logL} is added. 
\item Methods \code{MDL2} and \code{MDL5} are added. 
\item Function \code{pemix} for empirical distribution function calculation is added. 
\item Function \code{pfmix} for predictive distribution function calculation is added. 
\item Function \code{plot.REBMIX} is extended. It enables besides \code{"pdf"} plots also \code{"marginal pdf"}, 
\code{"c"}, \code{"IC"}, \code{"logL"}, \code{"D"} and \code{"marginal cdf"} plots.
\item Additional arguments \code{what} enters method \code{plot.REBMIX}.
\item Method \code{PRD} is added. 
\item Methods \code{print.boot.REBMIX} and \code{summary.boot.REBMIX} are added.
\item Method \code{SSE} is added. 
\item Dataset \code{weibull} is added. 
}
}
\section{Changes in version 2.3.0}{ 
\itemize{ 
\item Discrete parametric families \code{"binomial"}, \code{"Poisson"} and \code{"Dirac"} are added. 
\item Information criteria \code{"D"} and \code{"SSE"} are added.
\item Methods \code{print.RNGMIX}, \code{print.REBMIX}, \code{coef.REBMIX} and \code{summary.REBMIX} are added.
\item Method \code{predict.list} for class membership prediction is added.
\item The package does not write to stdout and read from stdout any more.
\item The package is protected against wrong input arguments.
\item Additional arguments \code{Variables}, \code{theta1}, \code{theta2}, \code{ymin}, \code{ymax} and \code{b} 
    enter function \code{REBMIX}.
\item Minimum radius \code{RMIN} is eliminated.
\item Simplified treatment of global modes with equal probabilities in C functions \code{GlobalModeKNN()}, 
    \code{GlobalModePW()} and \code{GlobalModeH()}.
\item Improved treatment of potentially singular solutions in C functions \code{EnhancedEstimationKNN()}, 
    \code{EnhancedEstimationPW()} and \code{EnhancedEstimationH()}.
\item Improved convergence criteria in C functions \code{REBMIXKNN()}, \code{REBMIXPW()} and \code{REBMIXH()}.
\item Fixed bug in C function \code{PreprocessingPW()}.
\item Fixed bug in C functions \code{RoughEstimationKNN()}, \code{RoughEstimationPW()} and 
    \code{RoughEstimationH()}.
\item Fixed bug in \code{plot.REBMIX} method. 
\item Dataset \code{adult} is added. 
}
}
