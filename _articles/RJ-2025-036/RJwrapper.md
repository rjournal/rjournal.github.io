---
abstract: |
  The combination of diagnostic tests has become a crucial area of
  research, aiming to improve the accuracy and robustness of medical
  diagnostics. While existing tools focus primarily on linear
  combination methods, there is a lack of comprehensive tools that
  integrate diverse methodologies. In this study, we present
  [`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html),
  a comprehensive R package and web tool designed to address the
  limitations of existing diagnostic test combination platforms. One of
  the unique contributions of
  [`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
  is offering a range of 142 methods to combine two diagnostic tests,
  including linear, non-linear, machine learning algorithms, and
  mathematical operators. Another significant contribution of
  [`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
  is its inclusion of advanced tools for ROC analysis, diagnostic
  performance metrics, and visual outputs such as
  sensitivity-specificity curves. Furthermore,
  [`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
  offers classification functions for new observations, making it an
  easy-to-use tool for clinicians and researchers. The web-based version
  is also available at <https://biotools.erciyes.edu.tr/dtComb/> for
  non-R users, providing an intuitive interface for test combination and
  model training.
address:
- |
  S. Ilayda Yerlitaş Taştan\
  Department of Biostatistics\
  Erciyes University\
  Türkiye\
  (ORCiD: 0000-0003-2830-3006)\
  [ilaydayerlitas340@gmail.com](ilaydayerlitas340@gmail.com){.uri}
- |
  Serra Bersan Gengeç\
  Department of Biostatistics\
  Erciyes University\
  Türkiye\
  [serrabersan@gmail.com](serrabersan@gmail.com){.uri}
- |
  Necla Koçhan\
  Department of Mathematics\
  Izmir University of Economics\
  Türkiye\
  (ORCiD: 0000-0003-2355-4826)\
  [necla.kayaalp@gmail.com](necla.kayaalp@gmail.com){.uri}
- |
  Ertürk Zararsız\
  Department of Biostatistics\
  Erciyes University\
  Türkiye\
  (ORCiD if desired)\
  [gozdeerturk9@gmail.com](gozdeerturk9@gmail.com){.uri}
- |
  Selçuk Korkmaz\
  Department of Biostatistics\
  Trakya University\
  Türkiye\
  (ORCiD if desired)\
  [selcukorkmaz@gmail.com](selcukorkmaz@gmail.com){.uri}
- |
  Zararsız\
  Department of Biostatistics\
  Erciyes University\
  Türkiye\
  (ORCiD: 0000-0001-5801-1835)\
  [gokmen.zararsiz@gmail.com](gokmen.zararsiz@gmail.com){.uri}
author:
- S. Ilayda Yerlitaş Taştan, Serra Bersan Gengeç, Necla Koçhan, Ertürk
  Zararsız, Selçuk Korkmaz and Zararsız
bibliography:
- dtCombreferences.bib
title: "dtComb: A Comprehensive R Library and Web Tool for Combining
  Diagnostic Tests"
---

::::::::: article
## Introduction

A typical scenario often encountered in combining diagnostic tests is
when the gold standard method combines two-category and two continuous
diagnostic tests. In such cases, clinicians usually seek to compare
these two diagnostic tests and improve the performance of these
diagnostic test results by dividing the results into proportional
results [@muller2019amyloid; @faria2016neutrophil; @nyblom2006ast].
However, this technique is straightforward and may not fully capture all
potential interactions and relationships between the diagnostic tests.
Linear combination methods have been developed to overcome such problems
[@erturkzararsiz2023linear].\
Linear methods combine two diagnostic tests into a single score/index by
assigning weights to each test, optimizing their performance in
diagnosing the condition of interest [@neumann2023combining]. Such
methods improve accuracy by leveraging the strengths of both tests
[@aznar2022stepwise; @bansal2013does]. For instance, Su and Liu
[@su1993linear] found that Fisher's linear discriminant function
generates a linear combination of markers with either proportional or
disproportional covariance matrices, aiming to maximize sensitivity
consistently across the entire selectivity spectrum under a multivariate
normal distribution model. In contrast, another approach introduced by
Pepe and Thomson [@pepe2000combining] relies on ranking scores,
eliminating the need for linear distributional assumptions when
combining diagnostic tests. Despite the theoretical advances, when
existing tools were examined, it was seen that they contained a limited
number of methods. For instance, Kramar et al. developed a computer
program called **mROC** that includes only the Su and Liu method
[@kramar2001mroc]. Pérez-Fernández et al. presented a
[`movieROC`](https://cran.r-project.org/web/packages/movieROC/index.html)
R package that includes methods such as Su and Liu, min-max, and
logistic regression methods [@perez2021visualizing]. An R package called
[`maxmzpAUC`](https://github.com/wbaopaul/MaxmzpAUC-R) that includes
similar methods was developed by Yu and Park [@yu2015two].

On the other hand, non-linear approaches incorporating the non-linearity
between the diagnostic tests have been developed and employed to
integrate the diagnostic tests
[@du2024likelihood; @ghosh2005classification]. These approaches
incorporate the non-linear structure of tests into the model, which
might improve the accuracy and reliability of the diagnosis. In contrast
to some existing packages, which permit the use of non-linear approaches
such as splines[^1], lasso[^2] and ridge regression, there is currently
no package that employs these methods directly for combination and
offers diagnostic performance. Machine-learning (ML) algorithms have
recently been adopted to combine diagnostic tests
[@ahsan2024advancements; @sewak2024construction; @agarwal2023artificial; @prinzi2023explainable].
Many publications/studies focus on implementing ML algorithms in
diagnostic tests
[@salvetat2022game; @salvetat2024ai; @ganapathy2023comparison; @alzyoud2024diagnosing; @zararsiz2016statistical].
For instance, DeGroat et al. performed four different classification
algorithms (Random Forest, Support Vector Machine, Extreme Gradient
Boosting Decision Trees, and k-Nearest Neighbors) to combine markers for
the diagnosis of cardiovascular disease [@degroat2024discovering]. The
results showed that patients with cardiovascular disease can be
diagnosed with up to 96% accuracy using these ML techniques. There are
numerous applications where ML methods can be implemented
([`scikit-learn`](https://scikit-learn.org/stable/)
[@pedregosa2011scikit],
[`TensorFlow`](https://www.tensorflow.org/learn?hl=tr)
[@tensorflow2015-whitepaper],
[`caret`](https://cran.r-project.org/web/packages/caret/index.html)
[@kuhn2008building]). The
[`caret`](https://cran.r-project.org/web/packages/caret/index.html)
library is one of the most comprehensive tools developed in the R
language[@kuhn2008building]. However, these are general tools developed
only for ML algorithms and do not directly combine two diagnostic tests
or provide diagnostic performance measures.

Apart from the aforementioned methods, several basic mathematical
operations such as addition, multiplication, subtraction, and division
can also be used to combine markers
[@svart2024neurofilament; @luo2024ast; @serban2024significance]. For
instance, addition can enhance diagnostic sensitivity by combining the
effects of markers, whereas subtraction can more distinctly
differentiate disease states by illustrating the variance across
markers. On the other hand, there are several commercial (e.g. IBM SPSS,
MedCalc, Stata, etc.) and open source (R) software packages
([`ROCR`](https://cran.r-project.org/web/packages/ROCR/index.html)
[@sing2005rocr],
([`pROC`](https://cran.r-project.org/web/packages/pROC/index.html)
[@robin2011proc],
[`PRROC`](https://cran.r-project.org/web/packages/PRROC/index.html)
[@grau2015prroc],
[`plotROC`](https://cran.r-project.org/web/packages/plotROC/index.html)
[@sachs2017plotroc]) that researchers can use for Receiver operating
characteristic (ROC) curve analysis. However, these tools are designed
to perform a single marker ROC analysis. As a result, there is currently
no software tool that covers almost all combination methods.

In this study, we developed
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html),
an R package encompassing nearly all existing combination approaches in
the literature.
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
has two key advantages, making it easy to apply and superior to the
other packages: (1) it provides users with a comprehensive 142 methods,
including linear and non-linear approaches, ML approaches and
mathematical operators; (2) it produces turnkey solutions to users from
the stage of uploading data to the stage of performing analyses,
performance evaluation and reporting. Furthermore, it is the only
package that illustrates linear approaches such as Minimax and Todor &
Saplacan [@sameera2016binary; @todor2014tools]. In addition, it allows
for the classification of new, previously unseen observations using
trained models. To our knowledge, no other tools were designed and
developed to combine two diagnostic tests on a single platform with 142
different methods. In other words,
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
has made more effective and robust combination methods ready for
application instead of traditional approaches such as simple ratio-based
methods. First, we review the theoretical basis of the related
combination methods; then, we present an example implementation to
demonstrate the applicability of the package. Finally, we present a
user-friendly, up-to-date, and comprehensive web tool developed to
facilitate
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
for physicians and healthcare professionals who do not use the R
programming language. The
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
package is freely available on the CRAN network, the web application is
freely available at <https://biotools.erciyes.edu.tr/dtComb/>, and all
source code is available on GitHub[^3].

## Material and methods

This section will provide an overview of the combination methods
implemented in the literature. Before applying these methods, we will
also discuss the standardization techniques available for the markers,
the resampling methods during model training, and, ultimately, the
metrics used to evaluate the model's performance.

### Combination approaches

#### Linear combination methods

The
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
package comprises eight distinct linear combination methods, which will
be elaborated in this section. Before investigating these methods, we
briefly introduce some notations which will be used throughout this
section.\
Notations:\
Let $D_{i}, i = 1, 2, …, n_1$ be the marker values of the $i$th
individual in the diseased group, where $D_i=(D_{i1},D_{i2})$, and
$H_j, j=1,2,…,n_2$ be the marker values of the $j$th individual in the
healthy group, where $H_j=(H_{j1},H_{j2})$. Let
$x_{i1}=c(D_{{i1}},H_{j1})$ be the values of the first marker, and
$x_{i2}=c(D_{i2},H_{j2})$ be values of the second marker for the $i$th
individual $i=1,2,...,n$. Let $D_{i,min}=\min(D_{i1},D_{i2})$,
$D_{i,max}=\max(D_{i1},D_{i2})$, $H_{j,min}=\min(H_{j1},H_{j2})$,
$H_{j,max}=\max(H_{j1},H_{j2})$ and $c_i$ be the resulting combination
score of the $i$th individual.

-   *Logistic regression:* Logistic regression is a statistical method
    used for binary classification. The logistic regression model
    estimates the probability of the binary outcome occurring based on
    the values of the independent variables. It is one of the most
    commonly applied methods in diagnostic tests, and it generates a
    linear combination of markers that can distinguish between control
    and diseased individuals. Logistic regression is generally less
    effective than normal-based discriminant analysis, like Su and Liu's
    multivariate normality-based method, when the normal assumption is
    met [@ruiz1991asymptotic; @efron1975efficiency]. On the other hand,
    others have argued that logistic regression is more robust because
    it does not require any assumptions about the joint distribution of
    multiple markers [@cox1989analysis]. Therefore, it is essential to
    investigate the performance of linear combination methods derived
    from the logistic regression approach with non-normally distributed
    data.\
    The objective of the logistic regression model is to maximize the
    logistic likelihood function. In other words, the logistic
    likelihood function is maximized to estimate the logistic regression
    model coefficients.\

    $$\label{eq:1}
    c=\frac{exp(\beta_0 + \beta_1 x_{i1} + \beta_2 x_{i2}}{1+exp(\beta_0 + \beta_1 x_{i1} + \beta_2 x_{i2}}   (\#eq:1)$$
    The logistic regression coefficients can provide the maximum
    likelihood estimation of the model, producing an easily
    interpretable value for distinguishing between the two groups.

-   *Scoring based on logistic regression:* The method primarily uses a
    binary logistic regression model, with slight modifications to
    enhance the combination score. The regression coefficients, as
    predicted in Eq \@ref(eq:1), are rounded to a user-specified number
    of decimal places and subsequently used to calculate the combination
    score [@leon2006bedside].
    $$c= \beta_1 x_{i1}+\beta_2 x_{i2}$$

-   *Pepe & Thompson's method:* Pepe & Thompson have aimed to maximize
    the AUC or partial AUC to combine diagnostic tests, regardless of
    the distribution of markers [@pepe2000combining]. They developed an
    empirical solution of optimal linear combinations that maximize the
    Mann-Whitney U statistic, an empirical estimate of the ROC curve.
    Notably, this approach is distribution-free. Mathematically, they
    maximized the following objective function:
    $$\text{maximize} \; U(a)= \frac{1}{n_1 n_2} \sum_{i=1}^{n_1} \sum_{j=1}^{n_2} I\left[D_{i1}+\alpha D_{i2}\geq H_{j1}+\alpha H_{j2}\right]$$

    $$c= x_{i1}+\alpha x_{i2}
    \label{eq:4}   (\#eq:4)$$
    where $a \in [-1,1]$ is interpreted as the relative weight of
    $x_{i2}$ to $x_{i1}$ in the combination, the weight of the second
    marker. This formula aims to find $\alpha$ to maximize $U(a)$.
    Readers are referred to see (Pepe and Thomson) [@pepe2000combining].

-   *Pepe, Cai & Langton's method:* Pepe et al. observed that when the
    disease status and the levels of markers conform to a generalized
    linear model, the regression coefficients represent the optimal
    linear combinations that maximize the area under the ROC curves
    [@pepe2006combining]. The following objective function is maximized
    to achieve a higher AUC value:
    $$\text{maximize} \; U(a)= \frac{1}{n_1 n_2} \sum_{i=1}^{n_1} \sum_{j=1}^{n_2} I\left[D_{i1}+\alpha D_{i2}> H_{j1}+\alpha H_{j2}\right] + \frac{1}{2}I\left[D_{i1}+\alpha = H_{j1} + \alpha H_{j2}\right]$$
    Before calculating the combination score using Eq \@ref(eq:4), the
    marker values are normalized or scaled to be constrained within the
    scale of 0 to 1. In addition, it is noted that the estimate obtained
    by maximizing the empirical AUC can be considered as a particular
    case of the maximum rank correlation estimator from which the
    general asymptotic distribution theory was developed. Readers are
    referred to Pepe (2003, Chapters 4--6) for a review of the ROC curve
    approach and more details [@pepe2003statistical].

-   *Min-Max method:* The Pepe & Thomson method is straightforward if
    there are two markers. It is computationally challenging if we have
    more than two markers to be combined. To overcome the computational
    complexity issue of this method, Liu et al. [@liu2011min] proposed a
    non-parametric approach that linearly combines the minimum and
    maximum values of the observed markers of each subject. This
    approach, which does not rely on the normality assumption of data
    distributions (i.e., distribution-free), is known as the Min-Max
    method and may provide higher sensitivity than any single marker.
    The objective function of the Min-Max method is as follows:
    $$\text{maximize} \; U(a)= \frac{1}{n_1 n_2} \sum_{i=1}^{n_1} \sum_{j=1}^{n_2} I[D_{i,max}+\alpha D_{i,min}> H_{j,max}+\alpha H_{j,min}]$$

    $$c= x_{i,max}+\alpha x_{i,min}$$
    \
    where $x_{i,max}=\max⁡(x_{i1},x_{i2})$ and
    $x_{i,min}=\min⁡(x_{i1},x_{i2})$.\
    The Min-Max method aims to combine repeated measurements of a single
    marker over time or multiple markers that are measured with the same
    unit. While the Min-Max method is relatively simple to implement, it
    has some limitations. For example, markers may have different units
    of measurement, so standardization can be needed to ensure
    uniformity during the combination process. Furthermore, it is
    unclear whether all available information is fully utilized when
    combining markers, as this method incorporates only the markers'
    minimum and maximum values into the model [@kang2016linear].

-   *Su & Liu's method:* Su and Liu examined the combination score
    separately under the assumption of two multivariate normal
    distributions when the covariance matrices were proportional or
    disproportionate [@su1993linear]. Multivariate normal distributions
    with different covariances were first utilized in classification
    problems [@anderson1962classification]. Then, Su and Liu also
    developed a linear combination method by extending the idea of using
    multivariate distributions to the AUC, showing that the best
    coefficients that maximize AUC are Fisher's discriminant
    coefficients. Assuming that $D~N(\mu_D, \sum_D)$ and
    $H~N(\mu_H, \sum_H)$ represent the multivariate normal distributions
    for the diseased and non-diseased groups, respectively. The Fisher's
    coefficients are as follows:
    $$(\alpha, \beta) = (\sum_{D} + \sum_{H})^{-1} \mu \label{eq:alpha_beta}   (\#eq:alpha-beta)$$
    where $\mu=\mu_D-\mu_H$. The combination score in this case is:
    $$c= \alpha x_{i1}+ \beta x_{i2} 
    \label{eq:9}   (\#eq:9)$$

-   *The Minimax method:* The Minimax method is an extension of Su &
    Liu's method [@sameera2016binary]. Suppose that D follows a
    multivariate normal distribution $D\sim N(\mu_D, \sum_D)$,
    representing the diseased group, and H follows a multivariate normal
    distribution $H\sim N(\mu_H, \sum_H)$, representing the non-diseased
    group. Then Fisher's coefficients are as follows:
    $$(\alpha, \beta) = \left[t\sum_{D} + (1-t)\sum_{H}\right]^{-1} (\mu_D - \mu_H) \label{eq:alpha_beta_expression}   (\#eq:alpha-beta-expression)$$

    Given these coefficients, the combination score is calculated using
    Eq \@ref(eq:9). In this formula, *t* is a constant with values
    ranging from 0 to 1. This value can be hyper-tuned by maximizing the
    AUC.

-   *Todor & Saplacan's method:* Todor and Saplacan's method uses the
    sine and cosine trigonometric functions to calculate the combination
    score [@todor2014tools]. The combination score is calculated using
    $\theta \in[-\frac{\pi}{2},\frac{\pi}{2}]$, which maximizes the AUC
    within this interval. The formula for the combination score is given
    as follows:
    $$c= \sin{(\theta)}x_{i1}+\cos{(\theta)}x_{i2}$$

#### Non-linear combination methods

In addition to linear combination methods, the
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
package includes seven non-linear approaches, which will be discussed in
this subsection. In this subsection, we will use the following
notations: $x_{ij}$: the value of the *j*th marker for the *i*th
individual, $i=1,2,...,n$ and $j=1,2$ *d*: degree of polynomial
regressions and splines, $d = 1,2,…,p$.

-   *Logistic Regression with Polynomial Feature Space:* This approach
    extends the logistic regression model by adding extra predictors
    created by raising the original predictor variables to a certain
    power. This transformation enables the model to capture and model
    non-linear relationships in the data by including polynomial terms
    in the feature space [@james2013introduction]. The combination score
    is calculated as follows:
    $$c=\frac{exp\left(\beta_0 + \beta_1 x_{ij} + \beta_2 x_{ij}^2+...+\beta_p x_{ij}^p\right)}{1+exp\left(\beta_0 + \beta_1 x_{ij} + \beta_2 x_{ij}^2+...+\beta_p x_{ij}^p\right)}$$
    where $c_i$ is the combination score for the *i*th individual and
    represents the posterior probabilities.

-   *Ridge Regression with Polynomial Feature Space:* This method
    combines Ridge regression with expanded feature space created by
    adding polynomial terms to the original predictor variables. It is a
    widely used shrinkage method when we have multicollinearity between
    the variables, which may be an issue for least squares regression.
    This method aims to estimate the coefficients of these correlated
    variables by minimizing the residual sum of squares (RSS) while
    adding a term (referred to as a regularization term) to prevent
    overfitting. The objective function is based on the L2 norm of the
    coefficient vector, which prevents overfitting in the model (Eq
    \@ref(eq:beta-hat-r)). The Ridge estimate is defined as follows:
    $$\hat{\beta}^R = \text{argmin}_{\beta} \text{RSS} + \lambda \sum_{j=1}^{2} \sum_{d=1}^{p} \beta_j^{d^2} \label{eq:beta_hat_r}   (\#eq:beta-hat-r)$$

    where
    $$RSS=\sum_{i=1}^{n}\left(y_i-\beta_0-\sum_{j=1}^{2}\sum_{d=1}^{p} \beta_j^d x_{ij}^d\right)$$
    and $\hat{\beta}^R$ denotes the estimates of the coefficients of the
    Ridge regression, and the second term is called a penalty term where
    $\lambda \geq 0$ is a shrinkage parameter. The shrinkage parameter,
    $\lambda$, controls the amount of shrinkage applied to regression
    coefficients. A cross-validation is implemented to find the
    shrinkage parameter. We used the
    [`glmnet`](https://cran.r-project.org/web/packages/glmnet/index.html)
    package [@friedman2010regularization] to implement the Ridge
    regression in combining the diagnostic tests.

-   *Lasso Regression with Polynomial Feature Space:* Similar to Ridge
    regression, Lasso regression is also a shrinkage method that adds a
    penalty term to the objective function of the least square
    regression. The objective function, in this case, is based on the L1
    norm of the coefficient vector, which leads to the sparsity in the
    model. Some of the regression coefficients are precisely zero when
    the tuning parameter $\lambda$ is sufficiently large. This property
    of the Lasso method allows the model to automatically identify and
    remove less relevant variables and reduce the algorithm's
    complexity. The Lasso estimates are defined as follows:

    $$\hat{\beta}^L = \text{argmin}_{\beta} \text{RSS} + \lambda \sum_{j=1}^{2} \sum_{d=1}^{d} | \beta_j^d | \label{eq:beta_hat_l}   (\#eq:beta-hat-l)$$

    To implement the Lasso regression in combining the diagnostic tests,
    we used the
    [`glmnet`](https://cran.r-project.org/web/packages/glmnet/index.html)
    package [@friedman2010regularization].

-   *Elastic-Net Regression with Polynomial Feature Space:* Elastic-Net
    Regression is a method that combines Lasso (L1 regularization) and
    Ridge (L2 regularization) penalties to address some of the
    limitations of each technique. The combination of the two penalties
    is controlled by two hyperparameters, $\alpha\in$\[0,1\] and
    $\lambda$, which enable you to adjust the trade-off between the L1
    and L2 regularization terms [@james2013introduction]. For the
    implementation of the method, the
    [`glmnet`](https://cran.r-project.org/web/packages/glmnet/index.html)
    package is used [@friedman2010regularization].

-   *Splines:* Another non-linear combination technique frequently
    applied in diagnostic tests is the splines. Splines are a versatile
    mathematical and computational technique that has a wide range of
    applications. These splines are piecewise functions that make
    interpolating or approximating data points possible. There are
    several types of splines, such as cubic splines. Smooth curves are
    created by approximating a set of control points using cubic
    polynomial functions. When implementing splines, two critical
    parameters come into play: degrees of freedom and the choice of
    polynomial degrees (i.e., degrees of the fitted polynomials). These
    user-adjustable parameters, which influence the flexibility and
    smoothness of the resulting curve, are critical for controlling the
    behavior of splines. We used the
    [`splines`](https://rdocumentation.org/packages/splines/versions/3.6.2)
    package in the R programming language to implement splines.

-   *Generalized Additive Models with Smoothing Splines and Generalized
    Additive Models with Natural Cubic Splines:* Regression models are
    of great interest in many fields to understand the importance of
    different inputs. Even though regression is widely used, the
    traditional linear models often fail in real life as effects may not
    be linear. Another method called generalized additive models was
    introduced to identify and characterize non-linear regression
    [@james2013introduction]. Smoothing Splines and Natural Cubic
    Splines are two standard methods used within GAMs to model
    non-linear relationships. To implement these two methods, we used
    the [`gam`](https://cran.r-project.org/web/packages/gam/index.html)
    package in R [@Trevor2015gam]. The method of GAMs with Smoothing
    Splines is a more data-driven and adaptive approach where smoothing
    splines can automatically capture non-linear relationships without
    specifying the number of knots (specific points where two or more
    polynomial segments are joined together to create a
    piecewise-defined curve or surface) or the shape of the spline in
    advance. On the other hand, natural cubic splines are preferred when
    we have prior knowledge or assumptions about the shape of the
    non-linear relationship. Natural cubic splines are more
    interpretable and can be controlled by the number of knots
    [@elhakeem2022using].

#### Mathematical Operators

This section will mention four arithmetic operators, eight distance
measurements, and the exponential approach. Also, unlike other
approaches, in this section, users can apply logarithmic, exponential,
and trigonometric (sinus and cosine) transformations on the markers. Let
$x_{ij}$ represent the value of the *j*th variable for the *i*th
observation, with $i=1,2,...,n$ and $j=1,2$. Let the resulting
combination score for the *i*th individual be $c_i$.

-   *Arithmetic Operators:* Arithmetic operators such as addition,
    multiplication, division, and subtraction can also be used in
    diagnostic tests to optimize the AUC, a measure of diagnostic test
    performance. These mathematical operations can potentially increase
    the AUC and improve the efficacy of diagnostic tests by combining
    markers in specific ways. For example, if high values in one test
    indicate risk, while low values in the other indicate risk,
    subtraction or division can effectively combine these markers.

-   *Distance Measurements:* While combining markers with mathematical
    operators, a distance measure is used to evaluate the relationships
    or similarities between marker values. It's worth noting that, as
    far as we know, no studies have integrated various distinct distance
    measures with arithmetic operators in this context. Euclidean
    distance is the most commonly used distance measure, which may not
    accurately reflect the relationship between markers. Therefore, we
    incorporated a variety of distances into the package we developed.
    These distances are given as follows
    [@minaev2018distance; @pandit2011comparative; @cha2007comprehensive]:\
    *Euclidean:*
    $$c = \sqrt{(x_{i1} - 0)^2 + (x_{i2} - 0)^2} \label{eq:euclidean_distance}   (\#eq:euclidean-distance)$$
    \
    *Manhattan:*
    $$c = |x_{i1} - 0| + |x_{i2} - 0| \label{eq:manhattan_distance}   (\#eq:manhattan-distance)$$
    \
    *Chebyshev:*
    $$c = \max\{|x_{i1} - 0|, |x_{i2} - 0|\} \label{eq:max_absolute}   (\#eq:max-absolute)$$
    \
    *Kulczynskid:*
    $$c = \frac{|x_{i1} - 0| + |x_{i2} - 0|}{\min\{x_{i1}, x_{i2}\}} \label{eq:custom_expression}   (\#eq:custom-expression)$$
    \
    *Lorentzian:*
    $$c = \ln(1 + |x_{i1} - 0|) + \ln(1 + |x_{i2} - 0|) \label{eq:ln_expression}   (\#eq:ln-expression)$$
    \
    *Taneja:*
    $$c = z_1 \left( \log \left( \frac{z_1}{\sqrt{x_{i1} \epsilon}} \right) \right) + z_2 \left( \log \left( \frac{z_2}{\sqrt{x_{i2} \epsilon}} \right) \right) \label{eq:log_expression}   (\#eq:log-expression)$$
    \
    where
    $z_1 = \frac{x_{i1} - 0}{2}, \quad z_2 = \frac{x_{i2} - 0}{2}$\
    *Kumar-Johnson:*
    $$c = \frac{{(x_{i1}^2 - 0)^2}}{{2(x_{i1} \epsilon)^{\frac{3}{2}}}} + \frac{{(x_{i2}^2 - 0)^2}}{{2(x_{i2} \epsilon)^{\frac{3}{2}}}}, \quad  \epsilon=0.0000) \label{eq:c_expression}   (\#eq:c-expression)$$
    \
    *Avg:*
    $$c = \frac{{|x_{i1} - 0| + |x_{i2} - 0| + \max\{(x_{i1} - 0),(x_{i2} - 0)\}}}{2} \label{eq:c_expression}   (\#eq:c-expression)$$
    \

-   *Exponential approach:* The exponential approach is another
    technique to explore different relationships between the diagnostic
    measurements. The methods in which one of the two diagnostic tests
    is considered as the base and the other as an exponent can be
    represented as $x_{i1}^{(x_{i2})}$ and $x_{i2}^{(x_{i1})}$. The
    specific goals or hypothesis of the analysis, as well as the
    characteristics of the diagnostic tests, will determine which method
    to use.

#### Machine-Learning algorithms

Machine-learning algorithms have been increasingly implemented in
various fields, including the medical field, to combine diagnostic
tests. Integrating diagnostic tests through ML can lead to more
accurate, timely, and personalized diagnoses, which are particularly
valuable in complex medical cases where multiple factors must be
considered. In this study, we aimed to incorporate almost all ML
algorithms in the package we developed. We took advantage of the
[`caret`](https://cran.r-project.org/web/packages/caret/index.html)
package in R [@kuhn2008building] to achieve this goal. This package
includes 190 classification algorithms that could be used to train
models and make predictions. Our study focused on models that use
numerical inputs and produce binary responses depending on the
variables/features and the desired outcome. This selection process
resulted in 113 models we further implemented in our study. We then
classified these 113 models into five classes using the same idea given
in [@zararsiz2016statistical]: (i) discriminant classifiers, (ii)
decision tree models, (iii) kernel-based classifiers, (iv) ensemble
classifiers, and (v) others. Like in the
[`caret`](https://cran.r-project.org/web/packages/caret/index.html)
package, `mlComb()` sets up a grid of tuning parameters for a number of
classification routines, fits each model, and calculates a performance
measure based on resampling. After the model fitting, it uses the
`predict()` function to calculate the probability of the \"event\"
occurring for each observation. Finally, it performs ROC analysis based
on the probabilities obtained from the prediction step.

### Standardization

Standardization is converting/transforming data into a standard scale to
facilitate meaningful comparisons and statistical inference. Many
statistical techniques frequently employ standardization to improve the
interpretability and comparability of data. We implemented five
different standardization methods that can be applied for each marker,
the formulas of which are listed below:

-   Z-score: $\frac{{x - \text{mean}(x)}}{{\text{sd}(x)}}$

-   T-score:
    $\left( \frac{{x - \text{mean}(x)}}{{\text{sd}(x)}} \times 10 \right) + 50$

-   min_max_scale: $\frac{{x - \min(x)}}{{\max(x) - \min(x)}}$

-   scale_mean_to_one: $\frac{x}{{\text{mean}(x)}}$

-   scale_sd_to_one: $\frac{x}{{\text{sd}(x)}}$

### Model building

After specifying a combination method from the
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
package, users can build and optimize model parameters using functions
like `mlComb()`, `linComb()`, `nonlinComb()`, and `mathComb()`,
depending on the specific model selected. Parameter optimization is done
using n-fold cross-validation, repeated n-fold cross-validation, and
bootstrapping methods for linear and non-linear approaches (i.e.,
`linComb()`, `nonlinComb()`). Additionally, for machine-learning
approaches (i.e., `mlComb()`), all of the resampling methods from the
[`caret`](https://cran.r-project.org/web/packages/caret/index.html)
package are used to optimize the model parameters. The total number of
parameters being optimized varies across models, and these parameters
are fine-tuned to maximize the AUC. The returned object stores input
data, preprocessed and transformed data, trained model, and resampling
results.

### Evaluation of model performances

A confusion matrix, as shown in Table [1](#tab:T1){reference-type="ref"
reference="tab:confusion_matrix"}, is a table used to evaluate the
performance of a classification model and shows the number of correct
and incorrect predictions. It compares predicted and actual

::: {#tab:confusion_matrix}
  -----------------------------------------------------------
  Predicted labels   Actual class labels              Total
  ------------------ --------------------- ---------- -------
                     Positive              Negative   

  Positive           TP                    FP         TP+FP

  Negative           FN                    TN         FN+TN

  Total              TP+FN                 FP+TN      n
  -----------------------------------------------------------

  : (#tab:T1) Confusion Matrix
:::

::: flush
TP: True Positive, TN: True Negative, FP: False Positive, FN: False
Negative, n: Sample size
:::

class labels, with diagonal elements representing the correct
predictions and off-diagonal elements representing the number of
incorrect predictions. The
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
package uses the
[`OptimalCutpoints`](https://cran.r-project.org/web/packages/OptimalCutpoints/index.html)
package [@yin2014optimal] to generate the confusion matrix and then
[`epiR`](https://cran.r-project.org/web/packages/epiR/index.html)
[@stevenson2017epir], including different performance metrics, to
evaluate the performances. Various performance metrics, accuracy rate
(ACC), Kappa statistic ($\kappa$), sensitivity (SE), specificity (SP),
apparent and true prevalence (AP, TP), positive and negative predictive
values (PPV, NPV), positive and negative likelihood ratio (PLR, NLR),
the proportion of true outcome negative subjects that test positive
(False T+ proportion for true D-), the proportion of true outcome
positive subjects that test negative (False T- proportion for true D+),
the proportion of test-positive subjects that are outcome negative
(False T+ proportion for T+), the proportion of test negative subjects
(False T- proportion for T-) that are outcome positive measures are
available in the
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
package. These metrics are summarized in Table
[2](#tab:T2){reference-type="ref" reference="tab:performance_metrics"}.

::: {#tab:performance_metrics}
  -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  **Performance Metric**                                                **Formula**
  --------------------------------------------------------------------- -------------------------------------------------------------------------------------------------------------------------
  Accuracy                                                              $\text{ACC} = \frac{{\text{TP} + \text{TN}}}{2}$

  Kappa                                                                 $\kappa = \frac{{\text{ACC} - P_e}}{{1 - P_e}}$

                                                                        $P_e = \frac{{(\text{TN} + \text{FN})(\text{TP} + \text{FP}) + (\text{FP} + \text{TN})(\text{FN} + \text{TN})}}{{n^2}}$

  Sensitivity (Recall)                                                  $\text{SE} = \frac{{\text{TP}}}{{\text{TP} + \text{FN}}}$

  Specificity                                                           $\text{SP} = \frac{{\text{TN}}}{{\text{TN} + \text{FP}}}$

  Apparent Prevalence                                                   $\text{AP} = \frac{{\text{TP}}}{{n}} + \frac{{\text{FP}}}{{n}}$

  True Prevalence                                                       $\text{TP} = \frac{{\text{AP} + \text{SP} - 1}}{{\text{SE} + \text{SP} - 1}}$

  Positive Predictive Value (Precision)                                 $\text{PPV} = \frac{{\text{TP}}}{{\text{TP} + \text{FP}}}$

  Negative Predictive Value                                             $\text{NPV} = \frac{{\text{TN}}}{{\text{TN} + \text{FN}}}$

  Positive Likelihood Ratio                                             $\text{PLR} = \frac{{\text{SE}}}{{1 - \text{SP}}}$

  Negative Likelihood Ratio                                             $\text{NLR} = \frac{{1 - \text{SE}}}{{\text{SP}}}$

  The Proportion of True Outcome Negative Subjects That Test Positive   $\frac{{\text{FP}}}{{\text{FP} + \text{TN}}}$

  The Proportion of True Outcome Positive Subjects That Test Negative   $\frac{{\text{FN}}}{{\text{TP} + \text{FN}}}$

  The Proportion of Test Positive Subjects That Are Outcome Negative    $\frac{{\text{FP}}}{{\text{TP} + \text{FN}}}$

  The Proportion of Test Negative Subjects That Are Outcome Positive    $\frac{{\text{FN}}}{{\text{FN} + \text{TN}}}$
  -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  : (#tab:T2) Performance Metrics and Formulas
:::

### Prediction of the test cases

The class labels of the observations in the test set are predicted with
the model parameters derived from the training phase. It is critical to
emphasize that the same analytical procedures employed during the
training phase have also been applied to the test set, such as
normalization, transformation, or standardization. More specifically, if
the training set underwent Z-standardization, the test set would
similarly be standardized using the mean and standard deviation derived
from the training set. The class labels of the test set are then
estimated based on the cut-off value established during the training
phase and using the model's parameters that are trained using the
training set.

### Technical details and the structure of dtComb

The
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
package is implemented using the R programming language
(<https://www.r-project.org/>) version 4.2.0. Package development was
facilitated with
[`devtools`](https://cran.r-project.org/web/packages/devtools/index.html)
[@wickham2016devtools] and documented with
[`roxygen2`](https://cran.r-project.org/web/packages/roxygen2/index.html)
[@wickham2013roxygen2]. Package testing was performed using 271 unit
tests [@wickham2011testthat]. Double programming was performed using
Python (<https://www.python.org/>) to validate the implemented functions
[@shiralkarprogramming].\
To combine diagnostic tests, the
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
package allows the integration of eight linear combination methods,
seven non-linear combination methods, arithmetic operators, and, in
addition to these, eight distance metrics within the scope of
mathematical operators and a total of 113 machine-learning algorithms
from the
[`caret`](https://cran.r-project.org/web/packages/caret/index.html)
package [@kuhn2008building]. These are summarized in Table
[3](#tab:T3){reference-type="ref" reference="tab:dtComb_features"}.

::: {#tab:dtComb_features}
+--------------------------+------------------------------------------+
| **Modules (Tab Panels)** | **Features**                             |
+:=========================+:=========================================+
| Combination Methods      | -   Linear Combination Approach (8       |
|                          |     Different methods)                   |
|                          |                                          |
|                          | -   Non-linear Combination Approach (7   |
|                          |     Different Methods)                   |
|                          |                                          |
|                          | -   Mathematical Operators (14 Different |
|                          |     methods)                             |
|                          |                                          |
|                          | -   Machine-Learning Algorithms (113     |
|                          |     Different Methods)                   |
|                          |     [@kuhn2008building]                  |
+--------------------------+------------------------------------------+
|                          | -   Five standardization methods         |
|                          |     applicable to linear, non-linear,    |
|                          |     mathematical methods                 |
|                          |                                          |
|                          | -   16 preprocessing methods applicable  |
|                          |     to ML [@kuhn2008building]            |
+--------------------------+------------------------------------------+
|                          | -   Three different methods for linear   |
|                          |     and non-linear combination methods   |
|                          |                                          |
|                          |     -   Bootstrapping                    |
|                          |                                          |
|                          |     -   Cross-validation                 |
|                          |                                          |
|                          |     -   Repeated cross-validation        |
|                          |                                          |
|                          | -   12 different resampling methods for  |
|                          |     ML [@kuhn2008building]               |
+--------------------------+------------------------------------------+
|                          | -   34 different methods for optimum     |
|                          |     cutpoints [@yin2014optimal]          |
+--------------------------+------------------------------------------+

: (#tab:T3) Features of dtComb
:::

## Results

Table [4](#tab:T4){reference-type="ref" reference="tab:exist_pck"}
summarizes the existing packages and programs, including
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html),
along with the number of combination methods included in each package.
While **mROC** offers only one linear combination method,
[`maxmzpAUC`](https://github.com/wbaopaul/MaxmzpAUC-R) and
[`movieROC`](https://cran.r-project.org/web/packages/movieROC/index.html)
provide five linear combination techniques each, and
[`SLModels`](https://cran.r-project.org/web/packages/SLModels/index.html)
includes four. However, these existing packages primarily focus on
linear combination approaches. In contrast,
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
goes beyond these limitations by integrating not only linear methods but
also non-linear approaches, machine learning algorithms, and
mathematical operators.

::: {#tab:exist_pck}
  --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  **Packages&Programs**                                                                                     **Linear Comb.**   **Non-linear Comb.**   **Math. Operators**   **ML algorithms**
  -------------------------------------------------------------------------------------------------------- ------------------ ---------------------- --------------------- -------------------
  **mROC** [@kramar2001mroc]                                                                                       1                    \-                    \-                   \-

  [`maxmzpAUC`](https://github.com/wbaopaul/MaxmzpAUC-R) [@yu2015two]                                              5                    \-                    \-                   \-

  [`movieROC`](https://cran.r-project.org/web/packages/movieROC/index.html) [@perez2021visualizing]                5                    \-                    \-                   \-

  [`SLModels`](https://cran.r-project.org/web/packages/SLModels/index.html) [@aznar-gimeno2023comparing]           4                    \-                    \-                   \-

  [`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)                                            8                    7                     14                   113
  --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  : (#tab:T4) Comparison of dtComb vs. existing packages and
  programs
:::

### Dataset

To demonstrate the functionality of the
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
package, we conduct a case study using four different combination
methods. The data used in this study were obtained from patients who
presented at Erciyes University Faculty of Medicine, Department of
General Surgery, with complaints of abdominal pain
[@zararsiz2016statistical; @akyildiz2010value]. The dataset comprised
D-dimer levels (*D_dimer*) and leukocyte counts (*log_leukocyte*) of 225
patients, divided into two groups (*Group*): the first group consisted
of 110 patients who required an immediate laparotomy (*needed*). In
comparison, the second group comprised 115 patients who did not
(*not_needed*). After the evaluation of conventional treatment, the
patients who underwent surgery due to their postoperative pathologies
are placed in the first group. In contrast, those with a negative result
from their laparotomy were assigned to the second group. All the
analyses were performed by following a workflow given in Fig.
[1](#figure:workflow){reference-type="ref" reference="figure:workflow"}.
First of all, the
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
package should be loaded in order to use related functions.

![Figure 1: **Combination steps of two diagnostic tests.** The figure
presents a schematic representation of the sequential steps involved in
combining two diagnostic tests using a combination
method.](Figure/Figure_1.png){#figure:workflow width="81.0%"
alt="graphic without alt text"}

``` r
# load dtComb package
library(dtComb)
```

Similarly, the laparotomy data can be loaded from the R database by
using the following R code:

``` r

# load laparotomy data
data(laparotomy)
```

### Implementation of the dtComb package

In order to demonstrate the applicability of the
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
package, the implementation of an arbitrarily chosen method from each of
the linear, non-linear, mathematical operator and machine learning
approaches is demonstrated and their performance is compared. These
methods are Pepe, Cai & Langton for linear combination, Splines for
non-linear, Addition for mathematical operator and SVM for
machine-learning. Before applying the methods, we split the data into
two parts: a training set comprising 70% of the data and a test set
comprising the remaining 30%.

``` r
# Splitting the data set into train and test (70%-30%)
set.seed(2128)
inTrain <- caret::createDataPartition(laparotomy$group, p = 0.7, list = FALSE)
trainData <- laparotomy[inTrain, ]
colnames(trainData) <- c("Group", "D_dimer", "log_leukocyte")
testData <- laparotomy[-inTrain, -1]

# define marker and status for combination function
markers <- trainData[, -1]
status <- factor(trainData$Group, levels = c("not_needed", "needed"))
```

The model is trained on `trainData` and the resampling parameters used
in the training phase are chosen as ten repeat five fold repeated
cross-validation. Direction = '\<' is chosen, as higher values indicate
higher risks. The Youden index was chosen among the cut-off methods. We
note that markers are not standardised and results are presented at the
confidence level (CI 95%). Four main combination functions are run with
the selected methods as follows.

``` r

# PCL method
fit.lin.PCL <- linComb(markers = markers,  status = status, event = "needed", 
                       method = "PCL", resample = "repeatedcv", nfolds = 5,
                       nrepeats = 10, direction = "<", cutoff.method = "Youden")

# splines method (degree = 3 and degrees of freedom = 3)
fit.nonlin.splines <- nonlinComb(markers = markers, status = status, event = "needed", 
                                 method = "splines", resample = "repeatedcv", nfolds = 5, 
                                 nrepeats = 10, cutoff.method = "Youden", direction = "<", 
                                 df1 = 3, df2 = 3)
#add operator
 fit.add <- mathComb(markers = markers, status = status, event = "needed",
                     method = "add", direction = "<", cutoff.method = "Youden")
#SVM
fit.svm <- mlComb(markers = markers, status = status, event = "needed", method = "svmLinear", 
                 resample = "repeatedcv", nfolds  = 5,nrepeats = 10, direction = "<", 
                 cutoff.method = "Youden")
```

Various measures were considered to compare model performances,
including AUC, ACC, SEN, SPE, PPV, and NPV. AUC statistics, with 95% CI,
have been calculated for each marker and method. The resulting
statistics are as follows: 0.816 (0.751--0.880), 0.802 (0.728--0.877),
0.888 (0.825--0.930), 0.911 (0.868--0.954), 0.877 (0.824-0.929), and
0.875 (0.821-0.930) for D-dimer, Log(leukocyte), Pepe, Cai & Langton,
Splines, Addition, and Support Vector Machine (SVM). The results
revealed that the predictive performances of markers and the combination
of markers are significantly higher than random chance in determining
the use of laparotomy ($p<0.05$). The highest sensitivity and NPV were
observed with the Addition method, while the highest specificity and PPV
were observed with the Splines method. According to the overall AUC and
accuracies, the combined approach fitted with the Splines method
performed better than the other methods (Fig.
[2](#figure:radar){reference-type="ref" reference="figure:radar"}).
Therefore, the Splines method will be used in the subsequent analysis of
the findings.

![Figure 2: **Radar plots of trained models and performance measures of
two markers.** Radar plots summarize the diagnostic performances of two
markers and various combination methods in the training dataset. These
plots illustrate the performance metrics such as AUC, ACC, SEN, SPE,
PPV, and NPV measurements. In these plots, the width of the polygon
formed by connecting each point indicates the model's performance in
terms of AUC, ACC, SEN, SPE, PPV, and NPV metrics. It can be observed
that the polygon associated with the Splines method occupies the most
extensive area, which means that the Splines method performed better
than the other methods.](Figure/Figure_4.png){#figure:radar width="100%"
alt="graphic without alt text"}

For the AUC of markers and the spline model:

``` r
fit.nonlin.splines$AUC_table
                    AUC     SE.AUC LowerLimit UpperLimit         z      p.value
D_dimer       0.8156966 0.03303310  0.7509530  0.8804403  9.556979 1.212446e-21
log_leukocyte 0.8022286 0.03791768  0.7279113  0.8765459  7.970652 1.578391e-15
Combination   0.9111752 0.02189588  0.8682601  0.9540904 18.778659 1.128958e-78
```

Here:\
`SE`: Standard Error.\
The area under ROC curves for D-dimer levels and leukocyte counts on the
logarithmic scale and combination score were 0.816, 0.802, and 0.911,
respectively. The ROC curves generated with the combination score from
the splines model, D-dimer levels, and leukocyte count markers are also
given in Fig. [3](#figure:roc){reference-type="ref"
reference="figure:roc"}, showing that the combination score has the
highest AUC. It is observed that the splines method significantly
improved between 9.5% and 10.9% in AUC statistics compared to D-dimer
level and leukocyte counts, respectively.

![Figure 3: **ROC curves.** ROC curves for combined diagnostic tests,
with sensitivity displayed on the y-axis and 1-specificity displayed on
the x-axis. As can be observed, the combination score produced the
highest AUC value, indicating that the combined strategy performs the
best overall.](Figure/Figure_2.png){#figure:roc width="70.0%"
alt="graphic without alt text"}

\
To see the results of the binary comparison between the combination
score and markers:

``` r
fit.nonlin.splines$MultComp_table

Marker1 (A)   Marker2 (B)   AUC (A)   AUC (B)      |A-B|  SE(|A-B|)         z      p-value
1 Combination       D_dimer 0.9079686 0.8156966 0.09227193 0.02223904 4.1490971 3.337893e-05
2 Combination log_leukocyte 0.9079686 0.8022286 0.10573994 0.03466544 3.0502981 2.286144e-03
3     D_dimer log_leukocyte 0.8156966 0.8022286 0.01346801 0.04847560 0.2778308 7.811423e-01
```

Controlling Type I error using Bonferroni correction, comparison of
combination score with markers yielded significant results ($p<0.05$).\
To demonstrate the diagnostic test results and performance measures for
non-linear combination approach, the following code can be used:

``` r
fit.nonlin.splines$DiagStatCombined
          Outcome +    Outcome -      Total
Test +           66           13         79
Test -           11           68         79
Total            77           81        158

Point estimates and 95% CIs:
--------------------------------------------------------------
Apparent prevalence *                  0.50 (0.42, 0.58)
True prevalence *                      0.49 (0.41, 0.57)
Sensitivity *                          0.86 (0.76, 0.93)
Specificity *                          0.84 (0.74, 0.91)
Positive predictive value *            0.84 (0.74, 0.91)
Negative predictive value *            0.86 (0.76, 0.93)
Positive likelihood ratio              5.34 (3.22, 8.86)
Negative likelihood ratio              0.17 (0.10, 0.30)
False T+ proportion for true D- *      0.16 (0.09, 0.26)
False T- proportion for true D+ *      0.14 (0.07, 0.24)
False T+ proportion for T+ *           0.16 (0.09, 0.26)
False T- proportion for T- *           0.14 (0.07, 0.24)
Correctly classified proportion *      0.85 (0.78, 0.90)
--------------------------------------------------------------
* Exact CIs
```

Furthermore, if the diagnostic test results and performance measures of
the combination score are compared with the results of the single
markers, it can be observed that the TN value of the combination score
is higher than that of the single markers, and the combination of
markers has higher specificity and positive-negative predictive value
than the log-transformed leukocyte counts and D-dimer level (Table
[5](#tab:T5){reference-type="ref" reference="tab:diagnostic_measures"}).
Conversely, D-dimer has a higher sensitivity than the others. Optimal
cut-off values for both markers and the combined approach are also given
in this table.

::: {#tab:diagnostic_measures}
  ---------------------------------------------------------------------------------------------------------------------------------------
  **Diagnostic Measures (95% CI)**    **D-dimer level ($>1.6$)**   **Log(leukocyte count) ($>4.16$)**   **Combination score ($>0.448$)**
  ---------------------------------- ---------------------------- ------------------------------------ ----------------------------------
  TP                                              66                               61                                  65

  TN                                              53                               60                                  69

  FP                                              28                               21                                  12

  FN                                              11                               16                                  12

  Apparent prevalence                      0.59 (0.51-0.67)                 0.52 (0.44-0.60)                    0.49 (0.41-0.57)

  True prevalence                          0.49 (0.41-0.57)                 0.49 (0.41-0.57)                    0.49 (0.41-0.57)

  Sensitivity                              0.86 (0.76-0.93)                 0.79 (0.68-0.88)                    0.84 (0.74-0.92)

  Specificity                              0.65 (0.54-0.76)                 0.74 (0.63-0.83)                    0.85 (0.76-0.92)

  Positive predictive value                0.70 (0.60-0.79)                 0.74 (0.64-0.83)                    0.84 (0.74-0.92)

  Negative predictive value                0.83 (0.71-0.91)                 0.79 (0.68-0.87)                    0.85 (0.76-0.92)

  Positive likelihood ratio                2.48 (1.81-3.39)                 3.06 (2.08-4.49)                    5.70 (3.35-9.69)

  Negative likelihood ratio                0.22 (0.12-0.39)                 0.28 (0.18-0.44)                    0.18 (0.11-0.31)

  False T+ proportion for true D-          0.35 (0.24-0.46)                 0.26 (0.17-0.37)                    0.15 (0.08-0.24)

  False T- proportion for true D+          0.14 (0.07-0.24)                 0.21 (0.12-0.32)                    0.16 (0.08-0.26)

  False T+ proportion for T+               0.30 (0.21-0.40)                 0.26 (0.17-0.36)                    0.16 (0.08-0.26)

  False T- proportion for T-               0.17 (0.09-0.29)                 0.21 (0.13-0.32)                    0.15 (0.08-0.24)

  Accuracy                                 0.75 (0.68-0.82)                 0.77 (0.69-0.83)                    0.85 (0.78-0.90)
  ---------------------------------------------------------------------------------------------------------------------------------------

  : (#tab:T5) Statistical diagnostic measures with 95% confidence
  intervals for each marker and the combination score
:::

For a comprehensive analysis, the `plotComb` function in
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
can be used to generate plots of the kernel density and individual-value
of combination scores of each group and the specificity and sensitivity
corresponding to different cut-off point values Fig.
[4](#figure:scatter){reference-type="ref" reference="figure:scatter"}.
This function requires the result of the `nonlinComb` function, which is
an object of the "dtComb" class and `status` which is of factor type.

``` r
# draw distribution, dispersion, and specificity and sensitivity plots
plotComb(fit.nonlin.splines, status)
```

![Figure 4: **Kernel density, individual-value, and sens&spe plots of
the combination score acquired with the training model.** Kernel density
of the combination score for two groups: needed and not needed (a).
Individual-value graph with classes on the x-axis and combination score
on the y-axis (b). Sensitivity and specificity graph of the combination
score c. While colors show each class in Figures (a) and (b), in Figure
(c), the colors represent the sensitivity and specificity of the
combination score.](Figure/Figure_3.png){#figure:scatter width="100%"
alt="graphic without alt text"}

If the model trained with Splines is to be tested, the generically
written predict function is used. This function requires the test set
and the result of the `nonlinComb` function, which is an object of the
"dtComb" class. As a result of prediction, the output for each
observation consisted of the combination score and the predicted label
determined by the cut-off value derived from the model.

``` r
# To predict the test set 
pred <- predict(fit.nonlin.splines, testData)
head(pred)

   comb.score labels
1   0.6133884 needed
7   0.9946474 needed
10  0.9972347 needed
11  0.9925040 needed
13  0.9257699 needed
14  0.9847090 needed
```

Above, it can be seen that the estimated combination scores for the
first six observations in the test set were labelled as **needed**
because they were higher than the cut-off value of 0.448.

### Web interface for the dtComb package

The primary goal of developing the
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
package is to combine numerous distinct combination methods and make
them easily accessible to researchers. Furthermore, the package includes
diagnostic statistics and visualization tools for diagnostic tests and
the combination score generated by the chosen method. Nevertheless, it
is worth noting that using R code may pose challenges for physicians and
those unfamiliar with R programming. We have also developed a
user-friendly web application for
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
using
[`Shiny`](https://cran.r-project.org/web/packages/shiny/index.html)
[@chang2017shiny] to address this. This web-based tool is publicly
accessible and provides an interactive interface with all the
functionalities found in the
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
package.\
To initiate the analysis, users must upload their data by following the
instructions outlined in the \"Data upload\" tab of the web tool. For
convenience, we have provided three example datasets on this page to
assist researchers in practicing the tool's functionality and to guide
them in formatting their own data (as illustrated in Fig.
[5](#figure:web){reference-type="ref" reference="figure:web"}a). We also
note that ROC analysis for a single marker can be performed within the
'ROC Analysis for Single Marker(s)' tab in the data upload section of
the web interface.

In the \"Analysis\" tab, one can find two crucial subpanels:

-   Plots (Fig. [5](#figure:web){reference-type="ref"
    reference="figure:web"}b): This section offers various visual
    representations, such as ROC curves, kernel density plots,
    individual-value plots, and sensitivity and specificity plots. These
    visualizations help users assess single diagnostic tests and the
    combination score generated using user-defined combination methods.

-   Results (Fig. [5](#figure:web){reference-type="ref"
    reference="figure:web"}c): In this subpanel, one can access a range
    of statistics. It provides insights into the combination score and
    single diagnostic tests, AUC statistics, and comparisons to evaluate
    how the combination score fares against individual diagnostic tests,
    and various diagnostic measures. One can also predict new data based
    on the model parameters set previously and stored in the \"Predict\"
    tab (Fig. [5](#figure:web){reference-type="ref"
    reference="figure:web"}d). If needed, one can download the model
    created during the analysis to keep the parameters of the fitted
    model. This lets users make new predictions by reloading the model
    from the \"Predict\" tab. Additionally, all the results can easily
    be downloaded using the dedicated download buttons in their
    respective tabs.

![Figure 5: **Web interface of the dtComb package.** The figure
illustrates the web interface of the
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
package, which demonstrates the steps involved in combining two
diagnostic tests. a) Data Upload: The user is able to upload the dataset
and select relevant markers, a gold standard test, and an event factor
for analysis.b) Combination Analysis: This panel allows the selection of
the combination method, method-specific parameters, and resampling
options to refine the analysis. c) Combination Analysis Output: Displays
the results generated by the selected combination method, providing the
user with key metrics and visualizations for interpretation. d) Predict:
Displays the prediction results of the trained model when applied to the
test set.](Figure/Figure_5.png){#figure:web width="100%"
alt="graphic without alt text"}

## Summary and further research

In clinical practice, multiple diagnostic tests are possible for disease
diagnosis [@yu2015two]. Combining these tests to enhance diagnostic
accuracy is a widely accepted approach
[@su1993linear; @pepe2000combining; @liu2011min; @sameera2016binary; @pepe2006combining; @todor2014tools].
As far as we know, the tools in Table [4](#tab:T4){reference-type="ref"
reference="tab:exist_pck"} have been designed to combine diagnostic
tests but only contain at most five different combination methods. As a
result, despite the existence of numerous advanced combination methods,
there is currently no extensive tool available for integrating
diagnostic tests.\
In this study, we presented
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html), a
comprehensive R package designed to combine diagnostic tests using
various methods, including linear, non-linear, mathematical operators,
and machine learning algorithms. The package integrates 142 different
methods for combining two diagnostic markers to improve the accuracy of
diagnosis. The package also provides ROC curve analysis, various
graphical approaches, diagnostic performance scores, and binary
comparison results. In the given example, one can determine whether
patients with abdominal pain require laparotomy by combining the D-dimer
levels and white blood cell counts of those patients. Various methods,
such as linear and non-linear combinations, were tested, and the results
showed that the Splines method performed better than the others,
particularly in terms of AUC and accuracy compared to single tests. This
shows that diagnostic accuracy can be improved with combination
methods.\
Future work can focus on extending the capabilities of the
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html)
package. While some studies focus on combining multiple markers, our
study aimed to combine two markers using nearly all existing methods and
develop a tool and package for clinical practice [@kang2016linear].

### R Software

The R package
[`dtComb`](https://cran.r-project.org/web/packages/dtComb/index.html) is
now available on the CRAN website
<https://cran.r-project.org/web/packages/dtComb/index.html>.

### Acknowledgment

We would like to thank the Proofreading & Editing Office of the Dean for
Research at Erciyes University for the copyediting and proofreading
service for this manuscript.
:::::::::

[^1]: [https://cran.r-project.org/web/
    packages/splines/index.html](https://cran.r-project.org/web/
    packages/splines/index.html){.uri}

[^2]: []{#note2
    label="note2"}<https://cran.r-project.org/web/packages/glmnet/index.html>

[^3]: <https://github.com/gokmenzararsiz/dtComb>,
    <https://github.com/gokmenzararsiz/dtComb_Shiny>
