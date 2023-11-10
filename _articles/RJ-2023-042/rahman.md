---
title: "bqror: An R package for Bayesian Quantile Regression in Ordinal Models"
date: "2023-03-19"
abstract: >
  This article describes an R package $\CRANpkg{bqror}$ that estimates Bayesian quantile regression in ordinal models introduced in @Rahman-2016. The paper classifies ordinal models into two types and offers computationally efficient, yet simple, Markov chain Monte Carlo (MCMC) algorithms for estimating ordinal quantile regression. The generic ordinal model with 3 or more outcomes (labeled $OR_{I}$ model) is estimated by a combination of Gibbs sampling and Metropolis-Hastings algorithm. Whereas an ordinal model with exactly 3 outcomes (labeled $OR_{II}$ model) is estimated using a Gibbs sampling algorithm only. In line with the Bayesian literature, we suggest using the marginal likelihood for comparing alternative quantile regression models and explain how to compute the same. The models and their estimation procedures are illustrated via multiple simulation studies and implemented in two applications. The article also describes several functions contained within the $\CRANpkg{bqror}$ package, and illustrates their usage for estimation, inference, and assessing model fit. 

draft: true
author:
  # see ?rjournal_article for more information
  - name: Prajual Maheshwari
    affiliation: Quantitative Researcher, Ogha Research
    address:
      - 2123, 14th Main Road, HAL 3rd Stage, Kodihalli, Bengaluru, Karnataka, India
    url: https://prajual.netlify.app
    email:  prajual1391@gmail.com

  - name: Mohammad Arshad Rahman
    affiliation: Department of Economic Sciences, Indian Institute of Technology Kanpur
    address: Room 672, Faculty Building, Indian Institute of Technology Kanpur, India
    url: https://www.arshadrahman.com
    orcid: 0000-0001-8434-0042
    email: marshad@iitk.ac.in, arshadrahman25@gmail.com
                 
type: package
output:
  rjtools::rjournal_web_article:
    self_contained: yes
    toc: no
    html_document: 
    mathjax: true
bibliography: rahman.bib
---



<!--%------------------------------------------------------------------------------>
<!--%------------------------------------------------------------------------------>
# Introduction{#sec:intro}

Quantile regression defines the conditional quantiles of a continuous dependent variable as a function of the covariates without assuming any error distribution [@Koenker-Basset-1978]. The method is robust and offers several advantages over least squares regression such as desirable equivariance properties, invariance to monotone transformation of the dependent variable, and robustness against outliers [@KoenkerBook-2005; @Davino-etal-2014;@Furno-Vistocco-2018]. However, quantile regression with discrete outcomes is more complex because quantiles of discrete data cannot be obtained through a simple inverse operation of the cumulative distribution function (${cdf}$). Besides, discrete outcome (binary and ordinal) modeling requires location and scale restrictions to uniquely identify the parameters (see [Section 2](#sec:QROrdinal) for details). @Kordas-2006 estimated quantile regression with binary outcomes using simulated annealing, while @Benoit-Poel-2010 proposed Bayesian binary quantile regression where a working likelihood for the latent variable was constructed by assuming the error follows an asymmetric Laplace (AL) distribution [@Yu-Moyeed-2001]. The estimation procedure for the latter is available in the \CRANpkg{bayesQR} package of R software [@Benoit-Poel-Rpackage-2017]. A couple of recent works on Bayesian quantile regression with binary longitudinal (panel) outcomes are @Rahman-Vossmeyer-2019 and @Bresson-etal-2021. Extending the quantile framework to ordinal outcomes is more intricate due to the difficulty in satisfying the ordering of cut-points while sampling. @Rahman-2016 introduced Bayesian quantile analysis of ordinal data and proposed two efficient MCMC algorithms. Since @Rahman-2016, ordinal quantile regression has attracted some attention which includes @Alhamzawi-2016, @Alhamzawi-Ali-Longitudinal2018, @Ghasemzadeh-etal-2018-METRON, @Rahman-Karnawat-2019, @Ghasemzadeh-etal-2020-Comm, and @Tian-etal-2021.

Ordinal outcomes frequently occur in a wide class of applications in economics, finance, marketing, and the social sciences. Here, ordinal regression (e.g. ordinal probit, ordinal logit) is an important tool for modeling, analysis, and inference. Given the prevalence of ordinal models in applications and the recent theoretical developments surrounding ordinal quantile regression, an estimation package is essential so that applied econometricians and statisticians can benefit from a more comprehensive data analysis. At present, no statistical software (such as R, MATLAB, Python, Stata, SPSS, and SAS) have any package for estimating quantile regression with ordinal outcomes. The current paper fills this gap in the literature and describes the implementation of \CRANpkg{bqror} package (version 1.6.0) for estimation and inference in Bayesian ordinal quantile regression.

The \CRANpkg{bqror} package offers two MCMC algorithms. Ordinal model with 3 or more outcomes is estimated through a combination of Gibbs sampling [@Casella-George-1992] and Metropolis-Hastings (MH) algorithm [@Chib-Greenberg-1995]. The method is implemented in the function $\texttt{quantregOR1}$. For ordinal models with exactly 3 outcomes, the package presents a Gibbs sampling algorithm that is implemented in the function $\texttt{quantregOR2}$. We recommend using this procedure for an ordinal model with 3 outcomes, since its simpler and faster. Both functions, $\texttt{quantregOR1}$ and $\texttt{quantregOR2}$, report the posterior mean, posterior standard deviation, 95\% posterior credible interval, and inefficiency factor of the model parameters. To compare alternative quantile regression models, we recommend using the marginal likelihood over the deviance information criterion (DIC). This is because the "Bayesian approach" to compare models is via the marginal likelihood [@Chib-1995;@Chib-Jeliazkov-2001]. So, the \CRANpkg{bqror} package also provides the marginal likelihood with technical details for computation described in this paper. Additionally, the package includes functions to calculate the covariate effects and example codes to produce trace plots of MCMC draws. Lastly, this paper utilizes the \CRANpkg{bqror} package to demonstrate the estimation of quantile ordinal models on simulated data and real-life applications.

<!--%------------------------------------------------------------------------------>
<!--%------------------------------------------------------------------------------>
# Quantile regression in ordinal models{#sec:QROrdinal}

Ordinal outcomes are common in a wide class of applications in economics, finance, marketing, social sciences, statistics in medicine, and transportation. In a typical study, the observed outcomes are ordered and categorical; so for the purpose of analysis scores/numbers are assigned to each outcome. For example, in a study on public opinion about offshore drilling [@Mukherjee-Rahman-2016], responses may be recorded as follows: 1 for 'strongly oppose', 2 for 'somewhat oppose', 3 for 'somewhat support', and 4 for 'strongly support'. The numbers have an ordinal meaning but have no cardinal interpretation. We cannot interpret a score of 2 as twice the support compared to a score of 1, or the difference in support between 2 and 3 is the same as that between 3 and 4. With ordinal outcomes, the primary modeling objective is to express the probability of outcomes as a function of the covariates. Ordinal models that have been extensively studied and employed in applications include the ordinal probit and ordinal logit models [@Johnson-Albert-2000; @Greene-Hensher-2010], but they only give information about the average probability of outcomes conditional on the covariates.

Quantile regression with ordinal outcomes can be estimated using the monotone equivariance property and provides information on the probability of outcomes at different quantiles. In the spirit of @Albert-Chib-1993, the ordinal quantile regression model can be presented in terms of an underlying latent (or unobserved) variable $z_{i}$ as follows:
<!--%------------------------------------------------------------------------------>
\begin{equation} 
z_{i} = x'_{i} \beta_{p} + \epsilon_{i}, \hspace{0.75in} \forall \; i=1, \cdots, n, 
(\#eq:latentreg) 
\end{equation}
<!--%------------------------------------------------------------------------------>
where $x'_{i}$ is a $1 \times k$ vector of covariates, $\beta_{p}$ is a $k \times 1$ vector of unknown parameters at the $p$-th quantile, $\epsilon_{i}$ follows an AL distribution i.e., $\epsilon_{i} \sim AL(0,1,p)$, and $n$ denotes the number of observations. Note that unlike the Classical (or Frequentist) quantile regression, the error is assumed to follow an AL distribution in order to construct a (working) likelihood [@Yu-Moyeed-2001]. The latent variable $z_{i}$ is related to the observed discrete response $y_{i}$ through the following relationship,
<!--%------------------------------------------------------------------------------>
\begin{equation} 
\gamma_{p,j-1} < z_{i} \le \gamma_{p,j} \; \Rightarrow\; y_{i} = j, \hspace{0.75in} \forall \; i=1,\cdots, n; \; j=1,\cdots, J,
(\#eq:cutpoints) 
\end{equation}
<!--%------------------------------------------------------------------------------>
where $\gamma_{p} = (\gamma_{p,0}=-\infty, \gamma_{p,1},\ldots, \gamma_{p,J-1}, \gamma_{p,J}=\infty)$ is the cut-point vector and $\textit{J}$ denotes the number of outcomes or categories. Typically, the cut-point $\gamma_{p,1}$ is fixed at 0 to anchor the location of the distribution required for parameter identification [@Jeliazkov-Rahman-2012]. Given the observed data $y$ = $(y_{1}, \cdots, y_{n})'$, the joint density (or likelihood when viewed as a function of the parameters) for the ordinal quantile model can be written as,
<!--%------------------------------------------------------------------------------>
\begin{equation} 
%\begin{split} 
f(y|\Theta_{p}) =  \prod_{i=1}^{n} \prod_{j=1}^{J} P(y_{i} = j | \Theta_{p})^{ I(y_{i} = j)}  %\\ =  \prod_{i=1}^{n}  \prod_{j=1}^{J} \bigg[ F_{AL}(\gamma_{p,j} - x'_{i}\beta_{p}) - F_{AL}(\gamma_{p,j-1} - x'_{i}\beta_{p})   \bigg]^{ I(y_{i} = j)},
(\#eq:likelihood)
%\end{split} 
\end{equation}
<!--%------------------------------------------------------------------------------>
where $\Theta_{p} = (\beta_{p}, \gamma_{p})$, $F_{AL}(\cdot)$ denotes the ${cdf}$ of an AL distribution and $I(y_{i}=j)$ is an indicator function, which equals 1 if $y_{i}=j$ and 0 otherwise.

Working directly with the AL likelihood \@ref(eq:likelihood) is not convenient for MCMC sampling. Therefore, the latent formulation of the ordinal quantile model \@ref(eq:latentreg), following @Kozumi-Kobayashi-2011, is expressed in the normal-exponential mixture form as follows,
<!--%------------------------------------------------------------------------------>
\begin{equation}
z_{i} = x'_{i} \beta_{p} + \theta w_{i} + \tau \sqrt{w_{i}} \,u_{i}, \hspace{0.5in} \forall \; i=1, \cdots, n, (\#eq:latentregNE)
\end{equation}
<!--%------------------------------------------------------------------------------>
where $\epsilon_{i} = \theta w_{i} + \tau \sqrt{w_{i}} \, u_{i} \sim AL(0,1,p)$, $w_{i} \sim\mathcal{E}(1)$ is mutually independent of $u_{i} \sim N(0,1)$, $N$ and $\mathcal{E}$ denotes normal and exponential distributions, respectively; $\theta = (1-2p)/[p(1-p)]$ and $\tau = \sqrt{2/[p(1-p)]}$. Based on this formulation, we can write the conditional distribution of the latent variable as $z_{i}|\beta_{p},w_{i} \sim N( x'_{i}\beta_{p} + \theta w_{i}, \tau^{2} w_{i})$ for $i=1,\ldots,n$. This allows access to the properties of normal distribution which helps in constructing efficient MCMC algorithms.

<!--%------------------------------------------------------------------------------>
### $\mathrm{OR_{I}}$ model{#sec:ORI}

The term "$\mathrm{OR_{I}}$ model" is ascribed to an ordinal model in which the number of outcomes ($J$) is equal to or greater than 3, location restriction is imposed by setting $\gamma_{p,1} = 0$, and scale restriction is achieved via constant variance (See @Rahman-2016; for a given value of $p$, variance of a standard AL distribution is constant). Note that in contrast to @Rahman-2016, our definition of $OR_{I}$ model includes an ordinal model with exactly 3 outcomes. The location and scale restrictions are necessary to uniquely identify the parameters (see @Jeliazkov-etal-2008 and @Jeliazkov-Rahman-2012 for further details and a pictorial representation).

During the MCMC sampling of the $\mathrm{OR_{I}}$ model, we need to preserve the ordering of cut-points ($\gamma_{p,0}=-\infty < \gamma_{p,1} < \gamma_{p,2} < \ldots < \gamma_{p,J-1} < \gamma_{p,J}=\infty$). This is achieved by using a monotone transformation from a compact set to the real line. Many such transformations are available (e.g., log-ratios of category bin widths, arctan, arcsin), but the \CRANpkg{bqror} package utilizes the logarithmic transformation [@Albert-Chib-2001; @Rahman-2016],
<!--%------------------------------------------------------------------------------>
\begin{equation} 
\delta_{p,j} = \ln ( \gamma_{p,j} - \gamma_{p,j-1} ), \qquad 2 \le j \le J-1. 
(\#eq:logtransformation)
\end{equation}
<!--%------------------------------------------------------------------------------>
The cut-points $(\gamma_{p,1}, \gamma_{p,2}, \cdots, \gamma_{p,J-1})$ can be obtained from a one-to-one mapping to $(\delta_{p,2}, \cdots, \delta_{p,J-1})$.

With all the modeling ingredients in place, we employ the Bayes' theorem and express the joint posterior distribution as proportional to the product of the likelihood and the prior distributions. As in @Rahman-2016, we employ independent normal priors: $\beta_{p} \sim N(\beta_{p0}, B_{p0})$, $\delta_{p} \sim N(\delta_{p0}, D_{p0})$ in the \CRANpkg{bqror} package. The augmented joint posterior distribution for the $\mathrm{OR_{I}}$ model can thus be written as,
<!--%------------------------------------------------------------------------------>
\begin{equation} 
\begin{split} 
\pi(z,\beta_{p}, \delta_{p},w|y) & \propto f(y|z,\beta_{p},\delta_{p},w) \; \pi(z | \beta_{p}, w) \; \pi(w) \; \pi(\beta_{p}) \; \pi(\delta_{p}), \\
   & \propto \Big\{ \prod_{i=1}^{n} f(y_{i}|z_{i},\delta_{p}) \Big\} \; \pi(z | \beta_{p},w) \; \pi(w) \; \pi(\beta_{p}) \; \pi(\delta_{p}), \\
    & \propto  \prod_{i=1}^{n} \bigg\{ \prod_{j=1}^{J} 1\{\gamma_{p,j-1} < z_{i} \le \gamma_{p,j} \} \; N(z_{i}|x'_{i}\beta_{p} + \theta w_{i}, \tau^{2} w_{i}) \; \mathcal{E}(w_{i}|1) \bigg\} \\
    & \qquad \times \; N(\beta_{p}|\beta_{p0}, B_{p0}) \; N(\delta_{p}|\delta_{p0}, D_{p0}).
\end{split}
(\#eq:JointPostORI)
\end{equation}



































