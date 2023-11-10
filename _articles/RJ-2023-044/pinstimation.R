
### PINstimation: An R package for estimating models of probability of informed trading
### by Montasser Ghachem and Oguz Ersan (2022)

### Important: Note that some outputs are randomly generated so the output that you obtain
### might differ from the one obtained in the paper.

library(PINstimation)

### This code concerns the section "PINstimation package" in the R paper

## +++++ ++++++++++++++++++++++++
## + 1 + ++ Original PIN Model ++
## +++++ ++++++++++++++++++++++++

    # [1] Estimate the PIN model using the function pin_ea(), and the dataset 'dailytrades'
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    model_ea <- pin_ea(dailytrades)
    show(model_ea)


    # [2] Display the optimal parameter estimates, the dataframe of initial parameter sets as
    # well as the probability of informed trading PIN.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    model_ea@parameters

    model_ea@initialsets

    model_ea@pin


    # [3] Using the slot details of the 'estimate.pin' object, we display the ML-estimated
    # parameters for each initial parameter set. The prefix 'in.' concerns parameters in an
    # initial parameter set and the prefix 'op.' concerns parameters in the optimal parameter
    # set (estimated values).
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    model_ea@details[ ,1:5]

    model_ea@details[, 6:10]


## +++++ ++++++++++++++++++++++++
## + 2 + ++     MPIN Model     ++
## +++++ ++++++++++++++++++++++++


    # [1] Estimate the MPIN model using the function 'mpin_ecm()'. The argument 'layers' is
    # omitted so the number of layers in the data is detected, using the information criterion.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    model_empin <- mpin_ecm(dailytrades, verbose = FALSE)
    show(model_empin)


    # [2] Estimate the MPIN model using the function 'mpin_ml()' on the preloaded dataset
    # 'dailytrades'.The argument 'layers' is omitted so the function detects the number of
    # layers in the data, using the algorithm in Ersan and Ghachem (2022a).
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    model_mpin <- mpin_ml(dailytrades, verbose = FALSE)
    show(model_mpin)


    # [3] Display the Multilayer probability of informed trading per layer from both estimation
    # methods.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    model_mpin@mpinJ

    model_empin@mpinJ


    # [4] Display the alpha estimates per layer from both estimation methods.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    model_mpin@parameters$alpha

    model_empin@parameters$alpha


    # [5] The function 'getSummary()' gives a useful summary of the main information contained in
    # these models. The column 'layers' contains the initial number of layers in the ECM estimation,
    # while the column 'em.layers' contains the number of layers in the optimal estimate obtained
    # through the ECM algorithm.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    getSummary(model_empin)


## +++++ ++++++++++++++++++++++++
## + 3 + ++    AdjPIN Model    ++
## +++++ ++++++++++++++++++++++++


    # [1] Using the function 'initials_adjpin()' and the dataset 'dailytrades', generate
    # initial parameter sets for the estimation of the AdjPIN model.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    init.sets <- initials_adjpin(dailytrades)

    # [2] Use the function 'adjpin()' to estimate the AdjPIN model using the initial sets in
    # 'init.sets', and the ECM algorithm (used by default).
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    model <- adjpin(data = dailytrades, initialsets = init.sets)
    show(model)


    # [3] Use the slot '@parameters' to display probability estimates, trading intensity
    # estimates, the adjusted probability of informed trading (adjpin), and the probability
    # of symmetric-order flow shock (psos).
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    model@parameters[1:4]

    model@parameters[5:10]

    model@adjpin

    model@psos


    # [4] Use the function adjpin() to estimate a restricted AdjPIN model using the standard
    # maximum-likelihood method, where it is assumed that the liquidity shock rates are equal on
    # the buy and sell side, i.e., d.b = d.s. By default, adjpin() uses initials_adjpin()
    # to generate 20 initial parameter sets for the estimation of the AdjPIN model. Note that
    # the number of initial parameters set can be altered using the argument 'num_init'.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    model <- adjpin(data = dailytrades, method = "ML", restricted = list(d = TRUE))
    show(model)


## +++++ ++++++++++++++++++++++++
## + 4 + ++     VPIN Model     ++
## +++++ ++++++++++++++++++++++++


    # [1] Use the function vpin() on the preloaded dataset 'hfdata' to estimate the volume
    # -synchronized probability of informed trading. The model_vpin is an 'estimate.vpin'
    # S4 object that contains the result of VPIN estimation.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    model_vpin <- vpin(hfdata)

    show(model_vpin)


    # [2] The following command shows the last ten rows of the data frame containing
    # selected information details about buckets.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    tail(model_vpin@bucketdata[, c(1,4:7)], 5)


    # [3] The following code shows the summary statistics of the daily vpin vectors, and uses
    # it to plot a line graph.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    summary(model_vpin@dailyvpin$dvpin)

    plot(model_vpin@dailyvpin[,"dvpin"], type="l", col = "blue", xlab="day", ylab="daily VPIN")


## +++++ ++++++++++++++++++++++++
## + 5 + ++  Data Simulation   ++
## +++++ ++++++++++++++++++++++++


    # [1] We generate a series of 100 simulated semi-annually datasets, having 3 layers
    # and 125 days each, before running an  MPIN estimation using mpin_ml() on the
    # dataset number 73.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    dataseries <- generatedata_mpin(series = 100, days = 125, layers = 3)
    show(dataseries)

    model_mpin <- mpin_ml(dataseries@datasets[[73]]@data)


    # [2] Use generatedata_mpin() with no arguments to generate a dataset of 60 days and
    # whose number of layers is uniformly drawn from {1,...,5}.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    sdata <- generatedata_mpin()

    print(sdata@layers)

    detectlayers_eg(sdata@data)


    # [3] Generate a single MPIN dataset with one information layer and the simulation
    # parameters (alpha ,delta ,mu ,eb, es) = (0.3, 0.7, 8000, 1500, 2000). This can be
    # achieved using either of the arguments 'parameters' or 'ranges'. If the argument
    # 'parameters' is used, The argument 'layers' can be omitted, as the function will
    # detect it from the size of the vector parameters.[*] If the argument 'layers' is
    # not compatible with the number of layers detected from the argument 'parameters',
    # then the latter will be used and a warning is issued.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    sdata <- generatedata_mpin(parameters = c(0.3,0.7,8000,1500,2000))
    show(sdata)

    sdata <- generatedata_mpin(
      layers = 1,
      ranges = list(alpha=0.3, delta=0.7, eps.b=1500, eps.s=1800, mu=8000))
    show(sdata)


    # [4] We generate a series of 500 datasets with 2 information layers where each layer
    # has a minimum share of 0.1, eps.b is equal to 5000; and mu is between 5000 and 25000.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    dataseries <- generatedata_mpin(
      series = 500, layers = 2,
      ranges = list(alpha = c(0.1,1), eps.b = 5000, mu = c(5000, 25000)))

    show(dataseries)


    # [5] Use generatedata_adjpin() with no arguments to generate a 'dataset' object
    # of 60 days for the adjusted PIN model.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    sdata <- generatedata_adjpin()
    show(sdata)


    # [6] Use generatedata_adjpin() to generate a series of 100 datasets of 60 days for
    # a restricted version of the AdjPIN model, where mu.b = mu.s, and d.b = d.s.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    sdata <- generatedata_adjpin(series=100, restricted=list(mu=TRUE, d=TRUE))
    show(sdata)


    # [7] Check the accuracy of the empirical numbers contained in the slot @empiricals of the
    # object dataset. In virtue of the weak law of large numbers, the empirical parameters in
    # @empiricals should converge to the theoretical parameters in @theoreticals as the number
    # of days in the dataset becomes large. Generate a single dataset with 10 000.000 days, to
    # check whether the empirical parameters get arbitrarily close to the theoretical ones.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    sdata <- generatedata_mpin(days = 10000000, layers = 1)
    show(sdata)


    # [8] We generate a collection of size 100 of dataset objects, whose data sequences span 60
    # days and contain 3 information layers. Note that each generated dataset object has two
    # properties: the probability of informed trading (in slot @emp.pin), computed using the
    # empirical parameters of the generated dataset - as stored in slot @empiricals; and the
    # number of layers used for the generation (in the slot @layers). For each of these dataset
    # objects, we run the function mpin_ml() one the generated pair of sequences of buyer-
    # initiated and seller-initiated trades stored in the slot @data, to obtain an object
    # 'estimate.mpin'. We recover from this object the estimated value of MPIN (stored in the
    # slot @mpin), and the detected number of layers (stored in the slot @layers); and compare
    # them with the actual values of emp.pin and layers (=3) stored in the dataset object.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    collection <- generatedata_mpin(series = 100, layers = 3)
    accuracy <- devmpin <- 0

    for (i in 1:100) {

      sdata <- collection@datasets[[i]]
      model <- mpin_ml(sdata@data, xtraclusters = 3, verbose=FALSE)
      accuracy <- accuracy + (sdata@layers == model@layers)
      devmpin <- devmpin + abs(sdata@emp.pin - model@mpin)

    }

    cat("The accuracy of layer detection: ", paste0(accuracy,"%.\n"), sep="")
    cat("The average error in MPIN estimates: ", devmpin/100, ".\n", sep="")


## +++++ ++++++++++++++++++++++++
## + 6 + ++  Data Aggregation  ++
## +++++ ++++++++++++++++++++++++


    # [1] Delete the variable 'volume' and store the remaining data in the variable
    # 'xdata', then aggregate it using the EMO algorithm with 'timelag' of 50 milliseconds.
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    xdata <- hfdata
    xdata[, "volume"] <- NULL
    aggtrades <- aggregate_trades(xdata, algorithm = "EMO", timelag = 50)


    # [2] Aggregate all observations using the 'LR' algorithm with timelag set to 1 second,
    # and use the aggregated data 'aggtrades' to estimate the PIN model
    # ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    aggtrades <- aggregate_trades(xdata, algorithm = "LR", timelag = 1000)


## +++++ +++++++++++++++++++
## + X + ++  END OF FILE  ++
## +++++ +++++++++++++++++++
