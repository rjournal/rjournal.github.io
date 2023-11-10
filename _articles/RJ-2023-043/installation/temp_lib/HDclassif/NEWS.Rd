\name{NEWS}
\alias{NEWS}

\title{News for \R Package \pkg{HDclassif}}

\section{Changes in version 2.2.1 (2023-08-21)}{
	\itemize{
		\item Update the maintainer's email address.
	}
}

\section{Changes in version 2.2.0 (2019-11-19)}{
	\subsection{User visible changes}{
		\itemize{
			\item New default for the argument 'show' in functions \code{hdda} and \code{hddc}. Now 'show' is set to the value of \code{getHDclassif.show()} which is \code{FALSE} at package loading. To set the value of 'show' permanently, use the new function \code{setHDclassif.show}.
			\item More explicit warnings when the maximum number of iterations is reached in HDDC.
			\item Convergence criterion in HDDC modified to accomodate large numbers.
			\item Algorithm now stops when there is log-likelihood alternation.
		}
	}

	\subsection{Bug Fix}{
		\itemize{
			\item[hdclassif_dim_choice (thanks to Zhenfeng He)] Bug in finding the intrisic dimension with the BIC method could occur when **all** eigenvalues are smaller than 1e-8 (value of noise.ctrl). Now corrected.
			\item[plot.hdc] plot method when selection has been made with the BIC criterion now works appropriately.
			\item[plot.hdc] Now graphical parameters are reset at the end of the plot even in case of an interruption.
		}
	}
}

\section{Changes in version 2.1.0 (2018-05-10)}{
	\subsection{Major Bug Fix}{
		\itemize{
			\item[SlopeHeuristic (thanks to Thibaut Marin-Cudraz)] There was an important issue in the computation of the slope heuristic, undermining its validity in some situations. This is now corrected.
		}
	}
	\subsection{Minor Bug Fixes}{
		\itemize{
			\item[hddc (thanks to Vibhu Agarwal)] There was a problem to handle data sets of less than 2 variables. Now corrected.
			\item[hddc (thanks to Vibhu Agarwal)] There was a problem when performing the Cattell scree test with 2 or less eigenvalues. Now corrected.
		}
	}
	\subsection{New features}{
		\itemize{
			\item[slopeHeuristic] A new option, \code{plot}, displays the slope heuristic: both the fit of the likelihoods and the value of the slope heuristic criterion.
			\item[predict] The likelihood has been added as an output.
			\item[hddc] New argument \dQuote{subset}: it allows to perform HDDC on a subset of the data before computing the posterior on the full sample. Can be useful for having quick results on large datasets.
			\item[hddc] Added a warning when the maximum number of iterations is reached.
		}
	}
	\subsection{Help pages}{
		\itemize{
			\item Some cleanup and rewritting.
		}
	}
}

\section{Changes in version 2.0.2 2016-12-20}{
	\subsection{Bug correction}{
		\itemize{
			\item[hddc] there were problems when the dimension of the data was greater than d_max.
			\item[hddc] now the hddc initialization init="vector" works properly. 
			\item[hdmda] now function hdmda works for any model.
			\item[hdda] problem when model="ALL" in hdda is fixed
		}
	}
	\subsection{Help pages}{
		\itemize{
			\item[hdda] Some rewriting regarding the option d_select.	
		}
	}
}

\section{Changes in version 2.0.1 2016-09-01}{
	\subsection{Bug correction}{
		\itemize{
			\item[demo] now demo(hddc) works properly.
			\item[hddc] now the value of the "noise" b, cannot be lower than the parameter noise.ctrl.
		}
	}
}

\section{Changes in version 2.0 2016-06-30}{
	\subsection{New functions}{
		\itemize{
			\item[slopeHeuristic] Added: the slope heuristic to choose among different models in hddc
		}
	}
	\subsection{New features}{
		\itemize{
			\item[hddc] Added ICL criterion as an output for hddc
			\item[hddc] Added native parallel computing for hddc => new argument mc.cores
			\item[hddc] Controls for the kmeans initialization are more easily handled => there is now a kmeans.control argument in hddc
			\item[hddc] In hddc: The threshold of the Cattel scree test (argument threshold) can now be a vector.
			\item[hddc] Added in hddc: argument nbrep: the number of repetitions for each combination of model/K/threshold, only the best BIC is selected for these combinations
			\item[hddc] Added: an explicit argument "init.vector" for user made initializations
			\item significant improvement in the algorithm for large datasets => new argument d_max that controls the maximal number of intrinsic dimensions that are computed for Cattel's scree test
		}
	}
}
		
\section{Changes in version 1.3 2015-06-30}{
	\subsection{New function}{
		\itemize{
			\item[hdmda] the hdmda method for classification is added (supervised classification by using HDDC within the classes)
		}
	}
	\subsection{Other}{
		\itemize{
			\item slight improvement of error handling
			\item slight changes in the help files
		}
	}
}
		
\section{Changes in version 1.2.3 2015-02-14}{
	\subsection{New features}{
		\itemize{
			\item[hddc] The ARI criterion is introduced in the function predict for hddc objects. It is a criterion to assess the goodness of fit of the clustering. ARI completely replaces the former "best classication rate", as the algorithm used to compute it was flawed.
		}
	}
	\subsection{Other}{
		\itemize{
			\item The readability of help files is improved.
		}
	}
}
		
\section{Changes in version 1.2.2 2013-01-15}{
	\subsection{New Design}{
		\itemize{
			\item[hddc] now hddc will stop if the number of potential individuals in a class is inferior to 2; if so it will give the message 'empty class' which means stricly less than 2 individuals. 
			\item[hddc] changing the name of the argument 'ctrl' in hddc to 'min.individuals' for a clearer meaning. Now the argument 'min.individuals' is the minimum NUMBER of individuals in a class, it is not a PERCENTAGE of the total nomber of observations anymore. Its value cannot be lower than 2.
			\item[hddc & hdda] in hddc and hdda the option 'dim.ctrl' is now named 'noise.ctrl' for a clearer meaning
		}
	}
	\subsection{New features}{
		\itemize{
			\item[hddc] now some errors are now handled properly by the function hddc
		}
	}
	\subsection{Bug correction}{
		\itemize{
			\item[hddc] now some errors are now handled properly by the function hddc
			\item[hddc] corrected a bug when using common dimension models which could lead to a selection of the intrinsic dimension of 0
		}
	}
	\subsection{Help pages}{
		\itemize{
			\item[hddc] imoprtant in hddc.Rd: mismatch between the code and the help files; the stopping criterion (eps) now is correctly described as 'the difference between two successive log likelihoods' (as it is in the code) instead of 'the difference between two successive log likelihood per observation'
			\item some rewritting of help files
		}
	}
}
		
\section{Changes in version 1.2.1 2012-01-05}{
	\itemize{
		\item the citation of the package is updated
		\item new reference in .Rd files
	}
}
		
\section{Changes in version 1.2 2011-07-15}{
	\subsection{Code changes}{
		\itemize{
			\item now the BIC and the log likelihood are not divided by N (the number of observation) anymore
			\item very slight changes in the random initialization of hddc (now the random init cannot begin with an empty class)
		}
	}
	\subsection{New features}{
		\itemize{
			\item[hdda] added some features to hdda, notably the model "all" and the V-fold cross validation for dimension selection
			\item[hdda] a cross-validation option has been added for hdda in order to select the best dimension or threshold with respect to the CV result
			\item[hdda] added a leave-one-out cross-validation option to hdda
			\item[plot.hdc] big changes in the function plot.hdc. Now the dimensions selection using either Cattell's scree-test or the BIC can be plotted. 
			\item[plot.hdc] The graph of the eigenvalues has been removed.
			\item[plot.hdc] Graph scale changed for Cattell's scree-test to see directly the threshold levels
			\item[hddc] now it is possible to select the dimension with the "bic" criterion in hddc
			\item[hddc] added some warnings when the value of the parameter b is very low (inferior to 1e-6)
			\item the callculation trick when N<p is now done since Ni<p, Ni being the number of observations for the class i
		}
	}
	\subsection{Bug correction}{
		\itemize{
			\item changes on the predict.hdc function. It now works all the time.
		}
	}
	\subsection{Help pages}{
		\itemize{
			\item Big rewriting.
		}
	}
}
		
\section{Changes in version 1.1.3 2011-03-30}{
	\subsection{Bug correction}{
		\itemize{
			\item[hddc] hddc can now be initialized with a given class vector 
		}
	}
}

\section{Changes in version 1.1.2 2011-02-10}{
	\itemize{
		\item slight change in the demo functions
		\item the description of the package is changed
	}
}

\section{Changes in version 1.1.1 2010-12-01}{
	\itemize{
		\item the models can now be selected using integers instead of names
		\item the graph of hddc now gives the comparison between different models and different number of clusters
		\item the calculation of the log likelihood has been modified in hddc
	}
}


\section{Changes in version 1.1 2010-10-01}{
	\subsection{New features}{
		\itemize{
			\item When several models are given, HDDA and HDDC now explicitly give the model they select
			\item[hddc] The initialization kmeans can be settled by the user using the dots argument: ... 
			\item[hddc] hddc now handles several models at once
			\item A demo has been built for the methods hdda and hddc
		}
	}
	\subsection{Bug correction}{
		\itemize{
			\item[plot] A plot minor issue is fixed
		}
	}
	\subsection{Design changes}{
		Some names are changed in the functions hdda and hddc : 
		\itemize{
			\item Former name	->	New name
			\item AkiBkQkDk		->	AkjBkQkDk
			\item AkiBQkDk		->	AkjBQkDk
			\item AkiBkQkD		->	AkjBkQkD
			\item AkiBQkD		->	AkjBQkD
			\item AiBQD			->	AjBQD
		}
	}
}
		
		
		
		
		
		