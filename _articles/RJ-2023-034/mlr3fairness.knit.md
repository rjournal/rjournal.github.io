---
title: Fairness Audits and Debiasing Using \pkg{mlr3fairness}
abstract: >
  Given an increase in data-driven automated decision-making based on machine learning models, it is
  imperative that along with tools to develop and improve such models there are sufficient capabilities
  to analyze and assess models with respect to potential biases. We present the package \CRANpkg{mlr3fairness},
  a collection of metrics and methods that allow for the assessment of bias in machine learning models.
  Our package implements a variety of widely used fairness metrics that can be used to audit models for potential biases along with a set of visualizations that can help to provide additional insights into such biases. \CRANpkg{mlr3fairness} furthermore integrates bias mitigation methods that can help alleviate biases in Machine Learning (ML) models through data preprocessing or post-processing of predictions. These allow practitioners to trade off performance and fairness metric that are appropriate for their use case.
date: "2022-11-09"
draft: true
author:
  - name: Florian Pfisterer
    orcid: 0000-0001-8867-762X
    email:  florian.pfisterer@stat.uni-muenchen.de
    affiliation: Ludwig-Maximilians-Universität München
    address: Munich, Germany
    affiliation2: Munich Center for Machine Learning
    address2: Munich, Germany
  - name: Siyi Wei
    email: weisiyi2@gmail.com
    affiliation: University of Toronto
    address: Toronto, Canada
  - name: Sebastian Vollmer
    orcid: 0000-0002-9025-0753
    email: svollmer@stat.uni-muenchen.de
    affiliation: Deutsches Forschungszentrum für Künstliche Intelligenz
    address: Kaiserslautern, Germany
    affiliation2: University of Kaiserslautern
    address2: Kaiserslautern, Germany
  - name: Michel Lang
    email: michel.lang@stat.uni-muenchen.de
    orcid: 0000-0001-9754-0393
    affiliation: Research Center Trustworthy Data Science and Security
    address: Dortmund, Germany
    affiliation2: TU Dortmund University
    address2: Dortmund, Germany
  - name: Bernd Bischl
    email: bernd.bischl@stat.uni-muenchen.de
    orcid: 0000-0001-6002-6980
    affiliation: Ludwig-Maximilians-Universität München
    address: Munich, Germany
    affiliation2: Munich Center for Machine Learning
    address2: Munich, Germany
preamble: |
  \usepackage{longtable}
  \usepackage{bbm}
bibliography: mlr3fairness.bib
output: 
  rjtools::rjournal_web_article
---




# Introduction

Humans are increasingly subject to data-driven automated decision-making.
Those automated procedures such as credit risk assessments are often applied using predictive models [@kozodoi2022fairness;@galindo2000credit] often profoundly affecting individual's lives.
It is therefore important that, along with tools to develop and improve such models, we also develop sufficient capabilities to analyze and assess models not only with respect to their robustness and predictive performance but also with respect to potential biases.
This is highlighted, e.g., by the European General Data Protection Regulation (GDPR) which requires data to be processed fairly.
Popular R modelling frameworks such as \CRANpkg{caret} [@caret], \CRANpkg{tidymodels} [@tidymodels], \CRANpkg{SuperLearner} [@superlearner], or \CRANpkg{mlr} [@mlr] implement a plethora of metrics to measure performance, but fairness metrics are widely missing.
This lack of availability can be detrimental to obtaining fair and unbiased models if the result is to forgo bias audits due to the considerable complexity of implementing such metrics.
Consequently, there exists a considerable necessity for R packages to (a) implement such metrics, and (b) to connect these metrics to existing ML frameworks.
If biases are detected and need to be mitigated, we might furthermore want to employ bias mitigation techniques that tightly integrate with the fitting and evaluation of the resulting models in order to obtain trade-offs between a model's fairness and utility (e.g., predictive accuracy).

In this article, we present the \CRANpkg{mlr3fairness} package which builds upon the ML framework \CRANpkg{mlr3} [@mlr3].
Our extension contains fairness metrics, fairness visualizations, and model-agnostic pre- and postprocessing operators that aim to reduce biases in ML models.
Additionally, \CRANpkg{mlr3fairness} comes with reporting functionality that assists the user in documenting data and ML models, as well as to perform fairness audits.

In the remainder of the article, we first provide an introduction to fairness in ML to raise awareness for biases that can arise due to the use of ML models.
Next, we introduce the \CRANpkg{mlr3fairness} package, followed by an extensive case study, showcasing the capabilities of \CRANpkg{mlr3fairness}.
We conclude with a summary.



# Fairness in Machine Learning

Studies have found that data-driven automated decision-making systems often improve over human expertise (@dawes1989clinical) and high-stakes decisions can therefore be enhanced using data-driven systems.
This often does not only improve predictions but can also make decisions more efficient through automation.
Such systems, often without human oversight, are now ubiquitous in everyday life [@o2016weapons;@eubanks2018automating;@noble2018algorithms].
To provide further examples, ML-driven systems are used for highly influential decisions such as loan accommodations [@Chen2018;@Turner2019], job applications [@schumann], healthcare [@Topol2019], and criminal sentencing [@compas;@corbettcompas;@richardcompas].
With this proliferation, such decisions have become subject to scrutiny as a result of prominent inadequacies or failures, for example in the case of the COMPAS recidivism prediction system [@compas].

Without proper auditing, those models can unintentionally result in negative consequences for individuals, often from underprivileged groups [@fairmlbook].
Several sources of such biases are worth mentioning in this context:
Data often contains **historical biases** such as gender or racial stereotypes, that -- if picked up by the model -- will be replicated into the future.
Similarly, unprivileged populations are often not represented in data due to **sampling biases** leading to models that perform well in groups sufficiently represented in the data but worse on others [@gendershades] -- this includes a higher rate of missing data.
Other biases include biases in how *labels* and *data* are measured [@bao2021s] as well as **feedback** loops where repeated decisions affect the population subject to such decisions.
For an in-depth discussion and further sources of biases, the interested reader is referred to available surveys of the field [@fairmlbook;@mehrabi;@mitchell2021algorithmic].

## Quantifying fairness

We now turn to the question of how we can detect whether disparities exist in a model and if so, how they can be quantified.
What constitutes a fair model depends on a society's ethical values and which normative position we take, resulting in different metrics that are applied to a problem at hand.
In this article, we focus on a subgroup of these, so-called *statistical group fairness* metrics.
First, the observations are grouped by a sensitive attribute $A$ ($A = 0$ vs.\ $A = 1$), which, e.g., is an identifier for a person's race or a person's gender.
For the sake of simplicity, we consider a *binary classification* scenario and a *binary sensitive attribute*.
Each observation has an associated label $Y, Y \in \{0, 1\}$, and we aim to predict, e.g., whether a defendant was caught re-offending.
A system then makes a prediction $\hat{Y}, \hat{Y} \in \{0,1\}$, with the goal to predict whether an individual might re-offend.
We assume that $Y = 1$ is the favored outcome in the following exposition.
While we do not describe them in detail, the concepts discussed in the following often extend naturally to more complex scenarios including multi-class classification, regression or survival analysis and similarly to settings with multiple sensitive attributes.
We now provide and discuss several metrics grouped into metrics that require *Separation* and *Independence* [@fairmlbook] to provide further intuition regarding core concepts and possible applications.

### Separation

One group of widely used fairness notions requires **Separation**: $\hat{Y} \perp A | Y$.
In order for separation to hold, the prediction $\hat{Y}$ has to be independent of $A$ given the true label $Y$.
This essentially requires that some notion of model error, e.g., accuracy or false positive rate, is equal across groups $A$.
From this notion, we can derive several metrics that come with different implications.
It is important to note that those metrics can only meaningfully identify biases under the assumption that no disparities exist in the data or that they are legally justified.
For example, if societal biases lead to disparate measurements of an observed quantity (e.g. SAT scores) for individuals with the same underlying ability, *separation* based metrics might not identify existing biases.
For this reason, @wachter-vlr2020 refer to those metrics as *bias-preserving* metrics since underlying disparities are not addressed.
We now provide and discuss several metrics to provide further intuition regarding core concepts and possible applications.

#### Equalized Odds

A predictor $\hat{Y}$ satisfies *equalized odds* with respect to a sensitive attribute $A$ and observed outcome $Y$, if $\hat{Y}$ and $A$ are conditionally independent given $Y$:
\begin{equation}
\mathbb{P}\left(\hat{Y} = 1 \mid A = 0, Y = y\right) = \mathbb{P}\left(\hat{Y} = 1 \mid A = 1, Y = y\right), \quad y \in \{0,1\}.
(\#eq:eod)
\end{equation}
In short, we require that the true positive rates (TPR) and false positive rates (FPR) across both groups $A = 0$ and $A = 1$ are equal.
This intuitively requires, e.g., in the case of university admission, independent of the sensitive attribute, equal chances for qualified individuals to be accepted and unqualified individuals to be rejected.
Similar measures have been proposed based on equalized false positive rates [@chouldechova2017fair] and false omission rates [@richardcompas], depending on the scenario and societal context.

#### Equality of Opportunity

A predictor $\hat{Y}$ satisfies *equality of opportunity* with respect to a sensitive attribute $A$ and observed outcome $Y$, if $\hat{Y}$ and $A$ are conditionally independent for $Y = 1$.
This is a relaxation of the aforementioned *equalized odds* essentially only requiring equal TPRs:
\begin{equation}
\mathbb{P}\left(\hat{Y} = 1 \mid A = 0, Y = 1\right) = \mathbb{P}\left(\hat{Y} = 1 \mid A = 1, Y = 1\right).
(\#eq:eop)
\end{equation}
Intuitively, this only requires that, independent of the sensitive attribute, qualified individuals have the same chance of being accepted.

#### Performance Parity

A more general formulation can be applied when we require parity of some performance metric across groups.
To provide an example, @gendershades compare accuracy across intersectional subgroups, essentially arguing that model performance should be equal across groups:

\begin{equation}
\mathbb{P}\left(\hat{Y} = Y \mid A = 0\right) = \mathbb{P}\left(\hat{Y} = Y \mid A = 1\right).
 (\#eq:accp)
\end{equation}
This intuitively requires that the model should work equally well for all groups, i.e., individuals are correctly accepted or denied at the same rate, independent of the predicted attribute.
This notion can be extended across supervised learning settings and performance metrics, leading to considerations of equal mean squared error, e.g., in a regression setting.


### Independence

The second group of fairness metrics is given by so-called *bias-transforming* metrics [@wachter-vlr2020].
They require that decision rates, such as the positive rate, are equal across groups.
This notion can identify biases, e.g., arising from societal biases that manifest in different base rates across groups.
At the same time, employing such notions poses a considerable risk, as blindly optimizing for demographic parity might result in predictors that, e.g., jail innocent people from an advantaged group in order to achieve parity across both groups [@dwork2012; @richardcompas].
A predictor $\hat{Y}$ satisfies *demographic parity* [@Calders2010] with respect to a sensitive attribute $A$ and observed outcome $Y$, if $\hat{Y}$ and $A$ are conditionally independent:
\begin{equation}
\mathbb{P}\left(\hat{Y} = 1 \mid A = 0\right) = \mathbb{P}\left(\hat{Y} = 1 \mid A = 1\right).
 (\#eq:dp)
\end{equation}
In contrast to the previous definitions, this requires that the chance of being accepted is equal across groups.


### Fairness metrics

In order to encode the requirements in equations \@ref(eq:eod) - \@ref(eq:dp) into a fairness metric, we encode differences between measured quantities in two groups.
For a performance metric $M$, e.g., the true positive rate (TPR), we calculate the difference in the metric across the two groups:

\[
\Delta_{\mathrm{M}} = \mathrm{M}_{A=0} - \mathrm{M}_{A=1}.
\]
When $\Delta_{\mathrm{M}}$ now significantly deviates from $0$, this indicates a fairness violation with respect to the fairness notion described in $M$.
To provide an example, with $\mathbb{P}\left(\hat{Y} = 1 \mid A = \star, Y = 1\right)$ denoted with $\mathrm{TPR}_{A=\star}$, we calculate the difference in TPR between the two groups:
\[
\Delta_{\mathrm{TPR}} = \mathrm{TPR}_{A=0} - \mathrm{TPR}_{A=1}.
\]
When $\Delta_{\mathrm{TPR}}$ now significantly deviates from $0$, the prediction $\hat{Y}$ violates the requirement for *equality of opportunity* formulated above.

It is important to note that in practice, we might not be able to perfectly satisfy a given metric, e.g., due to stochasticity in data and labels.
Instead, to provide a binary conclusion regarding fairness, a model could be considered fair if $|\Delta_{\mathrm{TPR}}| < \epsilon$ for a given threshold $\epsilon > 0$, e.g., $\epsilon = 0.05$.
This allows for small deviations from perfect fairness due to variance in the estimation of $\mathrm{TPR}_{A=\star}$ or additional sources of bias.
However, choosing appropriate thresholds is difficult, and widely used values for $\epsilon$ such as $0.05$ are arbitrary and do not translate to legal doctrines, such as, e.g., disparate impact [@chen22].
A more in-depth treatment of metrics is given in [@fairmlbook; @saleiro2018aequitas; @kim2020fact; @mehrabi; @wachter-vlr2020].


#### Selecting fairness metrics

While the aforementioned metrics are conceptually similar, they encode different beliefs of what constitutes *fair* in a given scenario.
@wachter-vlr2020 differentiate between *bias-preserving* and *bias-transforming* metrics:
Bias-preserving metrics such as equalized odds and equality of opportunity require that errors made by a model are equal across groups.
This can help to detect biases stemming, e.g., from imbalances in the sampling or under- and overfitting in ML models, but might be problematic in cases where labels are biased.
To provide an example, police enforcement and subsequent arrests of violent re-offenders might be different across ZIP code areas, a proxy for race.
This might lead to situations where observed labels $Y$ suffer from differential measurement bias strongly correlated with race [@bao2021s].
Bias-preserving metrics do not take such disparities into account and might, therefore (wrongly) lead to the conclusion, that a given model is fair.

Bias-transforming methods, in contrast, do not depend on labels and might therefore not suffer from this problem.
They can help detect biases arising from different base rates across populations, arising, e.g., from aforementioned biases in the labelling or as a consequence of structural discrimination.
Deciding which metrics to use constitutes a value judgement and requires careful assessment of the societal context a decision-making system is deployed in.
A discussion of different metrics and their applicability can be found in the Aequitas Fairness Toolkit [@saleiro2018aequitas] which also provides guidance towards selecting a metric via the [Aequitas Fairness Tree](http://www.datasciencepublicpolicy.org/our-work/tools-guides/aequitas/).
@wachter-vlr2020 recommend using *bias-transforming* metrics and providing a checklist that can guide the choice of fairness metric.
@corbett2018measure, on the other hand, point out several limitations of available metrics and argue for grounding decisions in real-world quantities in addition to abstract fairness metrics.
Similarly, @friedler16 emphasize the need to differentiate between constructs we aim to measure (e.g., job-related knowledge) and the observed quantity that can be measured in practice (e.g., years in a job) when trying to automate decision, since disparities in how constructs translate to observed quantities might suffer from bias.
To provide an example, individuals with similar abilities might exhibit different measured quantities (grades) due to structural bias, e.g., worse access to after-school tutoring programs.

#### The dangers of fairness metrics

We want to stress that overly trusting in metrics can be dangerous and that fairness metrics can and should not be used to *prove* or *guarantee* fairness.
Whether a selected fairness notion (and a corresponding numerical value) is actually fair depends on the societal context in which a decision is made and which action should be derived from a given prediction.
Therefore, selecting the correct fairness metric requires a thorough understanding of the societal context of a decision, as well as the possible implications of such decisions.
To provide an example, in some cases discrepancies in positive predictions might be justified or even desired, as they, e.g., allow for a more nuanced, gender-specific diagnosis [@cirillo2020sex].
Furthermore, fairness metrics might not detect biases in more fine-grained subgroups, e.g., at the intersection of multiple sensitive attributes.
It is also important to note that fairness metrics merely provide a reduction of the aforementioned fairness notions into mathematical objectives.
As such, they require a variety of abstraction steps that might invalidate the metric [@chen22], as they, e.g., require that the data is a large enough and representative sample of exactly the population that we aim to investigate.
Furthermore, practitioners need to look beyond the model, and also at the data used for training and the process of data and label acquisition.
If the data for example exhibit disparate measurement errors in the features or labels, valid fairness assessments can become impossible.
Similarly, feedback loops might arise from a prediction leading to changes in the data collected in the future.
Even an *initially fair* model might then lead to adverse effects in the long term [@schwobel-facct22a].

Note that the fairness definitions presented above serve a dual purpose [@wachter-vlr2020]:
First, as a *diagnostic tool* to detect disparities.
This allows for assessing whether a model has inherited biases, e.g., from historical disparities reflected in the data.
The second purpose is as a basis for *model selection* and making fair decisions in practice.
In this setting, fairness notions are employed to audit ML models or to select which model should be used in practice.
In this setting, it is important to note that fairness metrics should not be used as the sole basis for making decisions about whether to employ a given ML model or to assess whether a given system is fair.
We therefore explicitly encourage using the presented metrics for exploratory purposes.

#### Other notions of fairness

In addition to *statistical group fairness notions* introduced above, several additional fairness notions exist.
The notion of *individual fairness* was proposed by @dwork2012.
Its core idea comes from the principle of *treating similar cases similarly and different cases differently*.
In contrast to statistical group fairness notions, this notion allows assessing *fairness* at an individual level and
would therefore allow determining, whether an individual is treated fairly.
A more in-depth treatment of individual fairness notions is, e.g., given in @binns2020apparent.
Similarly, a variety of *causal* fairness notions exist (c.f. @kilbertus2017avoiding).
They argue that assessing fairness requires incorporating causal relationships in the data and propose a variety of causal fairness metrics based on a *directed acyclic graph* describing relationships in the data.

#### Fairness constraints

Statistical group fairness notions suffer from two further problems in practice:
First, it might be hard to exactly satisfy the required fairness notions, e.g., due to a limited amount of data available for evaluation.
Secondly, only requiring fairness might lead to degenerate solutions [@corbett2018measure] or models that have low utility, e.g., in separating *good* and *bad* credit risk.
One approach to take this into account is to employ models which maximize utility but satisfy some maximum constraint on potential unfairness.
This can be achieved via constraints on the employed fairness measure, e.g. $|\Delta_M| \leq \epsilon$ requiring that the absolute difference in a metric $M$ between groups
is smaller than a chosen value $\epsilon$.
In the following, we denote the fairness metric we want to minimize with $\Delta_M$ and a performance metric with $\rho$. 

\[
  \rho_{|\Delta_M| \leq \epsilon} = \left\{
\begin{array}{ll}
\rho         & |\Delta_M| \leq \epsilon      \\
- |\Delta_M| &  \textrm{else.}                \\
\end{array}
\right.
\]

Note, that this assumes that the fairness metric $\rho$ is strictly positive and should be maximized.
This approach is similar in spirit to the approach of [@perrone2021fair] who optimize the constrained expected improvement $cEI = \mathbb{P}(|\Delta_M| \leq \epsilon) \cdot \rho$.

However, it is not immediately clear, how the constraint $\epsilon$ should be chosen.
An alternative, therefore, is to employ *multi-objective optimization* to investigate available trade-offs between performance and accuracy metrics.
This can be done via \CRANpkg{mlr3tuning} which contains functionality to tune models for multiple metrics, e.g., described in more detail in the [mlr3book](https://mlr3book.mlr-org.com/optimization.html#mult-measures-tuning) [@mlr3book].
The result of multi-objective optimization then is the *Pareto-set*: A list of models which optimally trade off the specified objectives.

## Bias mitigation

If biases are detected in a model, we might now be interested in improving models in order to potentially mitigate such biases.
Bias in models might arise from a variety of sources, so a careful understanding of the data, data quality and distribution might lead to approaches that can help in decreasing biases, e.g. through the collection of better or additional data or a better balancing of sensitive groups.
Similarly, biases might arise from the model, e.g., through under- or overfitting and more careful tuning of model hyperparameters might help with improving fairness.
Especially if the goal is to satisfy *bias-transforming* metrics, a better solution might often be to address fairness problems in the real world instead of relying on algorithmic interventions to solve fairness. This might lead to more robust, long-term solutions instead of temporarily addressing issues via algorithmic interventions.
In addition, a variety of algorithmic bias mitigation techniques, that might help with obtaining fairer models have been proposed.
Their goal is to reduce measured gaps in fairness, either via data pre-processing, employing models that incorporate fairness, or by applying post-processing techniques to a model's predictions.
Popular examples of such techniques include computing instance weights before training [@kamiran2012data], where each observation is weighted proportional to the inverse frequency of its label and sensitive attribute.
Other methods work by directly learning fair models that incorporate fairness constraints into the fitting procedure [@Zafar2017] or by adapting model predictions, e.g., [@hardt2016equality] propose to randomly flip a small fraction of predictions in each group given by $\hat{Y}$ and $A$, such that fairness metrics are satisfied in expectation.
Since bias mitigation techniques are often tailored towards a particular fairness metric, the optimal choice is often not trivial and a combination of algorithms and bias mitigation techniques, e.g. determined via tuning might result in an optimal model.

Bias-mitigation techniques, as proposed above, have the goal of mitigating fairness issues, as measured by fairness metrics.
In practice, this usually comes with several drawbacks:
First, bias-mitigation strategies often lead to a decrease in a classifier's predictive performance [@corbett2018measure].
In addition, processing schemes can worsen interpretability or introduce stochasticity during prediction (see, e.g., @hardt2016equality).
Furthermore, we want to caution against favouring bias-mitigation techniques over policy interventions that tackle biases at their root cause.
A different set of risks is posed by *fairwashing* [@fairwashing], i.e., finding fair explanations or satisfying fairness metrics for otherwise unfair models.
If biases are only addressed at a given moment and without regard for downstream effects, they might simultaneously lead to a decrease in predictive performance in the near term and to negative consequences for the sensitive group in the long term [@schwobel-facct22a].

# The \pkg{mlr3fairness} package {#main}

In this section, we first give an overview of related software.
Next, we give a very briefly introduce to the \CRANpkg{mlr3} ecosystem of packages.
Finally, the implemented extensions for fairness are presented.


## Related software {#related}

Several R packages provide similar capabilities to our software, but mostly focus on fairness metrics and visualization.
The \CRANpkg{fairness} package [@fairness] allows for the calculation of a variety of fairness metrics, while \CRANpkg{aif360} [@aif360] wraps the Python \pkg{aif360} module allowing for the computation of fairness metrics and several bias mitigation techniques but has only limited interoperability with R objects such as \code{data.frame}s.
The \CRANpkg{fairmodels} [@fairmodels] package again allows for the computation of fairness metrics for classification and regression settings as well as several bias mitigation techniques.
It tightly integrates with \CRANpkg{DALEX} [@dalex] to gain further insight using interpretability techniques.

Outside R, in Python, the \pkg{fairlearn} module [@fairlearn] provides ample functionality to study a wide variety of metrics, bias mitigation with respect to a variety of pre-, in- and postprocessing methods as well as to visualize differences.
It furthermore provides a *fairlearn dashboard* providing a comprehensive fairness report.
The \pkg{aif360} [@aif360] module similarly provides metrics as well as bias mitigation techniques while the \pkg{aequitas} fairness toolkit [@saleiro2018aequitas] provides similar capabilities.
Interoperability with the \pkg{scikit-learn} [@sklearn] ML framework allows for bias mitigation for a wide variety of ML models in all aforementioned systems.
Similar capabilities are also available in Julia's \pkg{Fairness.jl} [@fairnessjl] library.


## The mlr3 ecosystem {#mlr3}

\CRANpkg{mlr3fairness} is tightly integrated into the ecosystem of packages around the ML framework \CRANpkg{mlr3} [@mlr3].
\CRANpkg{mlr3} provides the infrastructure to fit, resample, and evaluate over 100 ML algorithms using a unified API.
Multiple extension packages bring numerous additional advantages and extra functionality.
In the context of fairness, the following extension packages deserve special mention:

* \CRANpkg{mlr3pipelines} [@mlr3pipelines] for pre- and postprocessing via pipelining.
  This allows composing bias mitigation techniques with arbitrary ML algorithms shipped with \CRANpkg{mlr3} as well as fusing ML algorithms with preprocessing steps such as imputation or class balancing.
  It furthermore integrates with \CRANpkg{mcboost} [@mcboost], which implements additional bias mitigation methods.
  We present an example in the supplementary material.
* \CRANpkg{mlr3tuning} for its extensive tuning capabilities.
* \pkg{mlr3proba} [@mlr3proba] for survival analysis.
* \CRANpkg{mlr3benchmark} for post-hoc analysis of benchmarked approaches.
* \CRANpkg{mlr3oml} as a connector to OpenML [@Vanschoren2014], an online scientific platform for collaborative ML.

In order to provide the required understanding for \CRANpkg{mlr3}, we briefly introduce some terminology and syntax.
A full introduction can be found in the mlr3 book [@mlr3book].

A `Task` in \CRANpkg{mlr3} is a basic building block holding the data, storing covariates and the target variable along with some meta-information.
The shorthand constructor function `tsk()` can be used to quickly access example tasks shipped with \CRANpkg{mlr3} or \CRANpkg{mlr3fairness}.
In the following chunk, we retrieve the binary classification task with id `"adult_train"` from the package.
It contains a part of the `Adult` data set [@uci].
The task is to predict whether an individual earns more than $50.000 per year.
The column `"sex"` is set as a binary sensitive attribute with levels `"Female"` and `"Male"`.


```r
library("mlr3verse")
library("mlr3fairness")

task = tsk("adult_train")
print(task)
```

```
#> <TaskClassif:adult_train> (30718 x 13)
#> * Target: target
#> * Properties: twoclass
#> * Features (12):
#>   - fct (7): education, marital_status, occupation, race, relationship,
#>     sex, workclass
#>   - int (5): age, capital_gain, capital_loss, education_num,
#>     hours_per_week
#> * Protected attribute: sex
```

The second building block is the `Learner`.
It is a wrapper around an ML algorithm, e.g., an implementation of logistic regression or a decision tree.
It can be trained on a `Task` and used for obtaining a `Prediction` on an independent test set which can subsequently be scored using a `Measure` to get an estimate for the predictive performance on new data.
The shorthand constructors `lrn()` and `msr()` allow for the instantiation of implemented `Learner`s and `Measure`s, respectively.
In the following example, we will first instantiate a learner, then split our data into a train and test set, afterwards train it on the train set of the dataset and finally evaluate predictions on held-out test data.
The train-test split in this case is given by row indices, here stored in the `idx` variable.


```r
learner = lrn("classif.rpart", predict_type = "prob")
idx = partition(task)
learner$train(task, idx$train)
prediction = learner$predict(task, idx$test)
```

We then employ the `classif.acc` measure which measures the accuracy of a prediction compared to the true label:


```r
measure = msr("classif.acc")
prediction$score(measure)
```

```
#> classif.acc 
#>      0.8382
```

In the example above, we obtain an accuracy score of 0.8382, meaning our ML model correctly classifies roughly 84 \% of the samples in the test data.
As the split into training set and test set is stochastic, the procedure should be repeated multiple times for smaller datasets [@bischl2012resampling] and the resulting performance values should be aggregated.
This process is called resampling, and can easily be performed with the `resample()` function, yielding a `ResampleResult` object.
In the following, we employ 10-fold cross-validation as a resampling strategy:


```r
resampling = rsmp("cv", folds = 10)
rr = resample(task, learner, resampling)
```

We can call the `aggregate` method on the `ResampleResult` to obtain the accuracy aggregated across all $10$ replications.


```r
rr$aggregate(measure)
```

```
#> classif.acc 
#>      0.8408
```

Here, we obtain an accuracy of 0.8408, so slightly higher than previous scores, due to using a larger fraction of the data.
Furthermore, this estimate has a lower variance (as it is an aggregate) at the cost of additional computation time.
To properly compare competing modelling approaches, candidates can be benchmarked against each other using the `benchmark()` function (yielding a `BenchmarkResult`).
In the following, we compare the decision tree from above to a logistic regression model.
To do this, we use the `benchmark_grid` function to compare the two `Learners` across the same `Task` and resampling procedure.
Finally, we aggregate the measured scores each learner obtains on each cross-validation split using the `$aggregate()` function.


```r
learner2 = lrn("classif.log_reg", predict_type = "prob")

grid = benchmark_grid(task, list(learner, learner2), resampling)
bmr = benchmark(grid)

bmr$aggregate(measure)[, .(learner_id, classif.acc)]
```

```
#>         learner_id classif.acc
#> 1:   classif.rpart      0.8408
#> 2: classif.log_reg      0.8467
```
After running the benchmark, we can again call `.$aggregate` to obtain aggregated scores.
The \CRANpkg{mlr3viz} package comes with several ready-made visualizations for objects from `mlr3` via \CRANpkg{ggplot2}'s [@ggplot2] `autoplot` function.
For a `BenchmarkResult`, the `autoplot` function provides a Box-plot comparison of performances across the cross-validation folds for each `Learner`.
Figure \@ref(fig:bmrbox) contains the box-plot comparison.
We can see that `log_reg` has higher accuracy and lower interquartile range across the 10 folds, and we might therefore want to prefer the `log_reg` model.



\begin{figure}

{\centering \includegraphics{mlr3fairness_files/figure-latex/bmrbox-1} 

}

\caption{Model comparison based on accuracy for decision trees (rpart) and logistic regression (log\_reg) across resampling splits.}(\#fig:bmrbox)
\end{figure}

### Selecting the sensitive attribute

For a given task, we can select one or multiple sensitive attributes.
In \CRANpkg{mlr3}, the sensitive attribute is identified by the column role `pta` and can be set as follows:


```r
task$set_col_roles("marital_status", add_to = "pta")
```

In the example above, we add the `"martial_status"` as an additional sensitive attribute.
This information is then automatically passed on when the task is used, e.g., when computing fairness metrics.
If more than one sensitive attribute is specified, metrics will be computed based on intersecting groups formed by the columns.

### Quantifying fairness

With the \CRANpkg{mlr3fairness} package loaded, fairness measures can be constructed via `msr()` like any other measure in \CRANpkg{mlr3}.
They are listed with prefix *fairness*, and simply calling `msr()` without any arguments will return a list of all available measures.
Table \@ref(tab:metrics) provides an overview over some popular fairness measures which are readily available.


Table: (\#tab:metrics) Overview of fairness metrics available with mlr3fairness.

|key                   |description                                                                                  |
|:---------------------|:--------------------------------------------------------------------------------------------|
|fairness.acc          | Accuracy equality \citep{gendershades}                                                      |
|fairness.mse          | Mean squared error equality (Regression)                                                    |
|fairness.eod          | Equalized odds \citep{hardt2016equality}                                                    |
|fairness.tpr          | True positive rate equality / Equality of opportunity \citep{hardt2016equality}             |
|fairness.fpr          | False positive rate equality / Predictive equality \citep{chouldechova2017fair}             |
|fairness.tnr          | True negative rate equality                                                                 |
|fairness.fnr          | False negative rate equality \citep{richardcompas}                                          |
|fairness.fomr         | False omission rate equality \citep{richardcompas}                                          |
|fairness.tnr          | Negative predictive value equality                                                          |
|fairness.tnr          | Positive predictive value equality                                                          |
|fairness.cv           | Demographic parity / Equalized positive rates \citep{Calders2010}                           |
|fairness.pp           | Predictive parity / Equalized precision \citep{chouldechova2017fair}                        |
|fairness.\{tp, fp, tn, fn\}         | Equal true positives, false positives, true negatives, false negatives        |
|fairness.acc\_eod=.05 | Accuracy under equalized odds constraint \citep{perrone2021fair}                            |
|fairness.acc\_ppv=.05 | Accuracy under ppv constraint \citep{perrone2021fair}                                       |

Furthermore, new custom fairness measures can be easily implemented, either by implementing them directly or by composing them from existing metrics.
This process is extensively documented in an accompanying [measures vignette](https://mlr3fairness.mlr-org.com/articles/measures-vignette.html) available with the package.

Here we choose the binary accuracy measure `"classif.acc"` and the equalized odds metric from above using `"fairness.eod"`:
The constructed list of measures can then be used to score a `Prediction`, a `ResampleResult` or `BenchmarkResult`, e.g.


```r
measures = list(msr("classif.acc"), msr("fairness.eod"))
rr$aggregate(measures)
```

```
#>             classif.acc fairness.equalized_odds 
#>                 0.84078                 0.07939
```


We can clearly see a comparatively large difference in equalized odds at around 0.08.
This means, that in total, the false positive rates (FPR) and true positive rates (TPR) on average differ by ~0.08, indicating that our model might exhibit a bias.
Looking at the individual components yields a clearer picture.
Here, we are looking at the confusion matrices of the combined predictions of the 10 folds, grouped by sensitive attribute:


```r
fairness_tensor(rr)
```

```
#> $Male
#>         truth
#> response   <=50K    >50K
#>    <=50K 0.43030 0.10033
#>    >50K  0.03408 0.11202
#> 
#> $Female
#>         truth
#> response    <=50K     >50K
#>    <=50K 0.282668 0.020900
#>    >50K  0.003907 0.015789
```

Plotting the prediction density or comparing measures graphically often provides additional insights:
For example, in Figure \@ref(fig:predplots), we can see that Females are more often predicted to earn below $50.000.
Similarly, we can see that both equality in FPR and TPR differ considerably.


```r
fairness_prediction_density(prediction, task)
compare_metrics(prediction, msrs(c("fairness.fpr", "fairness.tpr", "fairness.eod")), task)
```

\begin{figure}

{\centering \includegraphics{mlr3fairness_files/figure-latex/predplots-1} 

}

\caption{Visualizing predictions of the decision tree model. Left: Prediction densities for the negative class for Female and Male. Right: Fairness metrics comparison for FPR, TPR, EOd metrics. Plots show a higher likelihood for the '<50k' class for females resulting in fairness metrics different from 0.}(\#fig:predplots)
\end{figure}


### Bias mitigation

As mentioned above, several ways to improve a model's fairness exist.
While non-technical interventions, such as e.g. collecting more data should be preferred,
\CRANpkg{mlr3fairness} provides several bias mitigation techniques that can be used together with a `Learner` to obtain fairer models.
Table \@ref(tab:biasmitigation) provides an overview of implemented bias mitigation techniques.
They are implemented as `PipeOps` from the \CRANpkg{mlr3pipelines} package and can be
combined with arbitrary learners using the `%>>%` operator to build a pipeline that can later be trained.
In the following example, we show how to combine a learner with a reweighing scheme (`reweighing_wts`) or alternatively how to post-process predictions using the equalized odds debiasing (`EOd`) strategy.
An introduction to \CRANpkg{mlr3pipelines} is available in the corresponding [mlr3book chapter](https://mlr3book.mlr-org.com/pipelines.html) [@mlr3book].


```r
po("reweighing_wts") %>>% lrn("classif.glmnet")
po("learner_cv", lrn("classif.glmnet")) %>>% po("EOd")
```

 Table: (\#tab:biasmitigation) Overview of bias mitigation techniques available in mlr3fairness.

|Key                | Description                      | Type              | Reference          |
|:------------------|:---------------------------------|:------------------|:-------------------|
| EOd               |Equalized-Odds Debiasing          |Postprocessing     | @hardt2016equality |
| reweighing_os     |Reweighing (Oversampling)         |Preprocessing      | @kamiran2012data   |
| reweighing_wts    |Reweighing (Instance weights)     |Preprocessing      | @kamiran2012data   |


It is simple for users or package developers to extend \CRANpkg{mlr3fairness} with additional
bias mitigation methods -- as an example, the \CRANpkg{mcboost} package adds further postprocessing methods
that can improve fairness.<br>
Along with pipeline operators, \CRANpkg{mlr3fairness} contains several machine learning algorithms listed in table \@ref{tab:fairlearns} that can directly incorporate
fairness constraints. They can similarly be constructed using the `lrn()` shorthand.

Table: (\#tab:fairlearns) Overview of fair ML algorithms available with mlr3fairness.

| Key               | Package | Reference              |
| :---------------- | :------ | :--------------------  |
| regr.fairfrrm     | fairml  | @scutari       |
| classif.fairfgrrm | fairml  | @scutari       |
| regr.fairzlm      | fairml  | @Zafar2017     |
| classif.fairzlrm  | fairml  | @Zafar2017     |
| regr.fairnclm     | fairml  | @komiyama      |


###  Reports

Because fairness aspects can not always be investigated based on the fairness definitions above (e.g., due to biased sampling or labelling procedures), it is important to document data collection and the resulting data as well as the models resulting from this data.
Informing auditors about those aspects of a deployed model can lead to better assessments of a model's fairness.
Questionnaires for ML models [@modelcards] and data sets [@datasheets] have been proposed in literature.
We further add automated report templates using R markdown [@rmarkdown] for data sets and ML models.
In addition, we provide a template for a *fairness report* which includes many fairness metrics and visualizations to provide a good starting point for generating a fairness report inspired by the *Aequitas Toolkit* [@saleiro2018aequitas].
A preview for the different reports can be obtained from the [Reports vignette](https://mlr3fairness.mlr-org.com/articles/reports-vignette.html) in the package documentation.


Table: (\#tab:reports) Overview of reports generated by mlr3fairness.

| Report      |  Description             | Reference                   |
|--------------------|--------------------------|-----------------------------|
| [`report_modelcard()`](https://mlr3fairness.mlr-org.com/articles/modelcard/modelcard.html) | Modelcard for ML models  | @modelcards          |
| [`report_datasheet()`](https://mlr3fairness.mlr-org.com/articles/datasheet/datasheet.html) | Datasheet for data sets  | @datasheets          |
| [`report_fairness()`](https://mlr3fairness.mlr-org.com/articles/fairness/fairness.html)    | Fairness Report          | --                   |


# Case study

In order to demonstrate a full workflow, we conduct full bias assessment and bias mitigation on the popular adult data set [@uci].
The goal is to predict whether an individual's income is larger than \$$50.000$ with the sensitive attribute being *gender*.
The data set ships with \CRANpkg{mlr3fairness}, separated into a *train* and *test* task and can be instantiated using `tsk("adult_train")` and `tsk("adult_test")`, respectively.
As a fairness metric, we consider *true positive parity* which calls for equality in the true positive rates across groups, in this case the `sex` variable.
We furthermore are interested in the model's utility, here measured as its classification accuracy.


```r
library("mlr3verse")
library("mlr3fairness")

task = tsk("adult_train")
print(task)
```

```
#> <TaskClassif:adult_train> (30718 x 13)
#> * Target: target
#> * Properties: twoclass
#> * Features (12):
#>   - fct (7): education, marital_status, occupation, race, relationship,
#>     sex, workclass
#>   - int (5): age, capital_gain, capital_loss, education_num,
#>     hours_per_week
#> * Protected attribute: sex
```

```r
measures = msrs(c("fairness.tpr", "classif.acc"))
```

In order to get an initial perspective, we benchmark three models using 3-fold cross-validation each:

* a classification tree from the \CRANpkg{rpart} package,
* a penalized logistic regression from the \CRANpkg{glmnet} package and
* a penalized logistic regression from the \CRANpkg{glmnet} package, but with reweighing preprocessing.

The logistic regression in the latter two approaches do not support operating on factor features natively, therefore we pre-process the data with a feature encoder from \CRANpkg{mlr3pipelines}.
To achieve this, we connect the feature encoder `po("encode")` with the learner using the `%>>%` operator. 
This encodes factor variables into integers using dummy encoding.
We then evaluate all three learners on the `adult_train` data using 3-fold cross-validation by building up a grid of experiments we want to run using `benchmark_grid`.
This grid is then executed using the `benchmark` function, and we can aggregate the performance and fairness metric scores via the `$aggregate()` function.


```r
set.seed(4321)
learners = list(
    lrn("classif.rpart"),
    po("encode") %>>% lrn("classif.glmnet"),
    po("encode") %>>% po("reweighing_wts") %>>% lrn("classif.glmnet")
)

grid = benchmark_grid(
  tasks = tsks("adult_train"),
  learners = learners,
  resamplings = rsmp("cv", folds = 3)
)

bmr1 = benchmark(grid)
bmr1$aggregate(measures)[, c(4, 7, 8)]
```

```
#>                              learner_id fairness.tpr classif.acc
#> 1:                        classif.rpart     0.059767      0.8408
#> 2:                encode.classif.glmnet     0.070781      0.8411
#> 3: encode.reweighing_wts.classif.glmnet     0.004732      0.8351
```

The preprocessing step of reweighing already improved the fairness while sacrificing only a tiny bit of performance.
To see if we can further improve, we use \CRANpkg{mlr3tuning} to jointly tune all hyperparameters of the *glmnet* model as well as our reweighing hyperparameter.
In order to do this, we use an `AutoTuner` from \CRANpkg{mlr3tuning}; a model that tunes its own hyperparameters during training.
The full code for setting up this model can be found in the appendix.
An `AutoTuner` requires a specific metric to tune for.
Here, we define a fairness-thresholded accuracy metric. We set $\epsilon = 0.01$ as a threshold:

\[
  if \; |\Delta_{EOd}| \leq \epsilon: \textrm{accuracy} \;\; else: \;  - |\Delta_{EOd}|.
\]


```r
metric = msr("fairness.constraint",
    performance_measure = msr("classif.acc"),
    fairness_measure = msr("fairness.eod"),
    epsilon = 0.01
)
```


We then design the pipeline and the hyperparameters we want to tune over.
In the following example, we choose `tuning_iters = 3` and set a small range for the hyperparameters in `vals` to shorten the run time of the tuning procedure.
In real settings, this parameter would be set to a larger number, such as $100$.
To construct a self-tuning learner, we construct an `AutoTuner` that takes as input a learner, the resampling procedure and metric used for tuning as well as the tuning strategy along with a termination criterion (here how many tuning iterations should be run). 
In addition, we provide a new `id` for the learner to beautify subsequent printing and visualization.
We can then use this self-tuning learner like any other learner and benchmark it using `benchmark` as described above.


```r
tuning_iters = 3
at = AutoTuner$new(lrn, rsmp("holdout"),
    metric,
    tuner = mlr3tuning::tnr("random_search"),
    terminator = trm("evals", n_evals = tuning_iters)
)
at$id = "glmnet_weighted_tuned"

grd = benchmark_grid(
  tasks = tsks("adult_train"),
  learners = list(at),
  resamplings = rsmp("cv", folds = 3)
)

bmr2 = benchmark(grd, store_models = TRUE)
bmr2$aggregate(measures)[, c(4, 7, 8)]
```

```
#>               learner_id fairness.tpr classif.acc
#> 1: glmnet_weighted_tuned     0.009486      0.8385
```

The result improves w.r.t.\ accuracy while only slightly decreasing the measured fairness.
Note that the generalization error is estimated using a holdout strategy during training and slight violations of the desired threshold $\epsilon$ should therefore be considered [@feurer23].
The results of both benchmark experiments can then be collected and jointly visualized in Figure \@ref(fig:fat) visualizing accuracy and fairness of models in our benchmark. 
In addition to aggregate scores (denoted by a cross) individual iterations of the 3-fold Cross-Validation (represented by points) are shown to visualize variations in the individual results.




\begin{figure}

{\centering \includegraphics{mlr3fairness_files/figure-latex/fat-1} 

}

\caption{Fairness-Accuracy tradeoff for 3-fold CV on the adult train set.}(\#fig:fat)
\end{figure}


```r
bmr$aggregate(measures)[, c(4, 7, 8)]
```

```
#>                              learner_id fairness.tpr classif.acc
#> 1:                        classif.rpart     0.059767      0.8408
#> 2:                encode.classif.glmnet     0.070781      0.8411
#> 3: encode.reweighing_wts.classif.glmnet     0.004732      0.8351
#> 4:                glmnet_weighted_tuned     0.009486      0.8385
```

Especially when considering optimizing accuracy while still retaining a fair model, tuning can be helpful and further improve upon available trade-offs.
In this example, the `AutoTuner` improves w.r.t. the fairness metric while offering accuracy comparable with the simple `glmnet` model.
This can e.g. be observed from the fairness accuracy tradeoff shown in Figure \@ref(fig:fat).
Whether the achieved accuracy is sufficient, needs to be determined, e.g. from a business context.
For now, we assume that the model obtained from the `AutoTuner` is the model we might want to use going forward.
Having decided for a final model, we can now train the final model on the full training data


```r
at_lrn = bmr$learners$learner[[4]]
at_lrn$train(tsk("adult_train"))
```

and predict on the held out *test* set available for the `Adult` dataset to obtain a final estimate.
This is important since estimating fairness metrics often incurs significant variance [@agrawal2020debiasing] and evaluation of the test-set provides us with an unbiased estimate of model performance after the previous model selection step.


```r
test = tsk("adult_test")
at_lrn$predict(test)$score(measures, test)
```

```
#> fairness.tpr  classif.acc 
#>      0.07141      0.84375
```

On the held-out test set, the fairness constraint is slightly violated which can happen due to the comparatively large variance in the estimation of fairness metrics.

# Summary

The large-scale availability and use of automated decision making systems have resulted in growing concerns for a lack of fairness in the decisions made by such systems. As a result, fairness auditing methods that allow for investigating (un-)fairness in such systems, especially ones that provide interoperability with machine learning toolkits that allows for ease of use and integration into model evaluation and tuning.
In future work we plan on implementing several tools that further support the user w.r.t. pinpointing potential fairness issues in the data, especially through the help of interpretability tools, such as the \CRANpkg{iml} [@iml] package. We furthermore aim to implement additional fairness metrics from the realm of \`individual fairness' [@dwork2012] and \`conditional demographic parity' [@wachter-vlr2020].

\pagebreak

# Appendix

## Tuning the ML pipeline

We include the full code to construct the `AutoTuner` with additional details
and comments below.
We first load all required packages and use \pkg{mlr3}'s interaction with the \CRANpkg{future} [@future] package
to automatically distribute the tuning to all available cores in parallel by setting a `plan`.
See the documentation of \CRANpkg{future} for platform-specific hints regarding parallelization.


```r
library(mlr3misc)
library(mlr3)
library(mlr3pipelines)
library(mlr3fairness)
library(mlr3tuning)

# Enable parallelization utilizing all cores
future::plan("multicore")
```



We then instantiate an ML pipeline using \pkg{mlr3pipelines}. 
This connects several modelling steps, in our case **categorical encoding**, **reweighing** and a final **learner** using the
`%>>%` (double caret) operator, ultimately forming a new learner.
This learner can then subsequently be fit on a `Task`. We use the `po(<key>)` shorthand to construct a new
pipeline operator from a dictionary of implemented operators. 
We conduct **categorical encoding** because \pkg{glmnet} can not naturally handle categorical variables, and we therefore have
to encode them (in our case using `one-hot` encoding).


```r
# Define the learner pipeline.
lrn = as_learner(po("encode") %>>% po("reweighing_wts") %>>%
  po("learner", lrn("classif.glmnet")))
```

In addition, we have to specify the hyperparameter space our `Tuner` should tune over.
We do this by defining a list of values with a `to_tune()` token specifying the range.
Note, that hyperparameter names are prefixed with the respective operation's `id`.


```r
# Define the parameter space to optimize over
vals = list(
  reweighing_wts.alpha = to_tune(0.75, 1),
  classif.glmnet.alpha = to_tune(0.5, 1),
  classif.glmnet.s = to_tune(1e-4, 1e-2, logscale = TRUE)
)

# Add search space to the learner
lrn$param_set$values = insert_named(lrn$param_set$values, vals)
```

Before we now train the model, we again specify a metric we aim to satisfy, here we would like the equalized odds difference to be smaller than $0.1$.
In this case, we set a constraint on the *equalized odds difference* comprised of the differences in true positive rate (TPR) and false positive rate (FPR):

$$
\Delta_{EOd} = \frac{|\textrm{TPR}_{sex = M} - \textrm{TPR}_{sex = F}| + |\textrm{FPR}_{sex = M} - \textrm{FPR}_{sex = F}|}{2}.
$$

This can be done using the `fairness.constraint` measure.


```r
metric = msr("fairness.constraint",
    performance_measure = msr("classif.acc"),
    fairness_measure = msr("fairness.eod"),
    epsilon = 0.1
)
```

We can now instantiate a new `AutoTuner` using `lrn` defined above by additionally providing arguments specifying the tuning strategy, in our case random search, the measure to optimize for as well as the number of tuning steps.


```r
metric = msr("fairness.constraint",
    performance_measure = msr("classif.acc"),
    fairness_measure = msr("fairness.eod"),
    epsilon = 0.1
)

at = AutoTuner$new(
  learner = lrn, # The learner
  resampling = rsmp("holdout"), # inner resampling strategy
  measure = metric, # the metric to optimize for
  tuner = mlr3tuning::tnr("random_search"), # tuning strategy
  terminator = trm("evals", n_evals = 30) # number of tuning steps
)
```

The so-constructed `AutoTuner` can now be used on any classification Task!
Additional information regarding the `AutoTuner` is again available in the corresponding [mlr3book chapter](https://mlr3book.mlr-org.com/optimization.html#autotuner).
In the following example, we will apply it to the `Adult` task and train our model.
This will perform a tuning loop for the specified number of evaluations and
automatically retrain the best found parameters on the full data.


```r
at$train(tsk("adult_train"))
```

After training, we can look at the best models found, here ordered by our metric.
Note, that our metric reports the negative constraint violation if the constraint is violated and the accuracy in case the constraint is satisfied.


```r
head(at$archive$data[order(fairness.acc_equalized_odds_cstrt), 1:4])
```

We can then use the tuned model to assess our metric on the held out data:


```r
prd = at$predict(tsk("adult_test"))
prd$score(c(metric, msr("classif.acc"), msr("fairness.eod")),  tsk("adult_test"))
```

So our tuned model manages to obtain an accuracy of `~0.84` while satisfying the specified constraint of $\Delta_{EOd} < 0.1$.
So to summarize, we have tuned a model to optimize accuracy with respect to a constraint on a selected fairness metric using an `AutoTuner`.

# References

