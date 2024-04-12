This README gives details about running the replication code for the glmmPen package submission to The R Journal

"heiling-glmmPen.R" and "heiling-glmmPen.html"

The code in "heiling-glmmPen.R" gives the replication script that can be performed in under an hour. Rmarkdown output using this same code is provided in the html document with the same name (Rmarkdown file not included here). The "heiling-glmmPen.R" code runs the following code:
	The glmmPen variable selection procedure of the basal dataset (10 covariates) 
	All code written in Section "Software" of the paper (illustration of methods, diagnostics) and corresponding output. Output fit object saved as "fit_basal.RData".
	Creation of Figures 1a and 1b (the code saves these figures in the "Figures/" folder)
	A single replicate of one of the simulations discussed in Section "Simulations" 
	Creation of Tables 3-4 using the output from the simulations (output of simulations saved in the "Tables 3 and 4 Row x Full Data.RData" files).

In order to keep the replication script to under an hour, we restricted this code to not contain the "glmm()" example given in the paper (the paper provides code for this example, but no output). The code to run this example is provided in the "heiling-glmmPen_extra.R" file. The output of the code can be seen in the "heiling-glmmPen_extra.html" file. Running this example takes approximately 3 minutes.

Notes: 
(i) As seen in the "heiling-glmmPen.html" document, the glmmPen() code outputs iteration-level information for each iteration of the MCECM algorithm by default. To restrict this output, the argument 'progress' can be set to FALSE in glmmPen() (see glmmPen documentation). 
(ii) In this "heiling-glmmPen" replication script as well as the paper, we only provide a variable selection illustration example using 10 TSP covariates of the basal dataset because running full versions of these items would take well over an hour to run. Running the complete basal data fit with all 50 covariates will take nearly a day to run.
(iii) A "Figures/" folder is provided to provide a location to save the figures produced by the "heiling-glmmPen.R" code. After running the code and producing these figures, one can clearly see that these are the same figures used in the paper. 

The code to run all simulations (100 replicates for 8 different simulation set-ups) in serial is given in the folder "sims_serial". Median times to completion are provided in Table 3 of the paper, which can give the reviewer an idea of how long it would take to run these simulations. Additional code at the end of this script converts the simulation output into the "Tables ... Full Data.RData" files provided in this "heiling-glmmPen" folder.

We ran all of these simulations on a cluster so that we could run each simulation replicate in parallel, which drastically reduces the required time to complete these simulations. Code and additional instructions needed to run these simulations on a cluster is provided in the folder "sims_on_cluster".




######################################################################################################