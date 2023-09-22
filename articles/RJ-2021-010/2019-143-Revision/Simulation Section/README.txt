R files for “A Fast and Scalable Implementation Method for Competing Risks Data with the R Package fastcmprsk”

By Eric S. Kawaguchi
Updated: 06/18/20
#################################################
CONTENTS OF FOLDER:
installNecessaryPackages.R: R file with necessary packages that need to be installed prior.

kawaguchi.R: Reproduces the results that are within the text of the manuscript. Does NOT include results from Section 5 (simulation study).


REPRODUCING RESULTS FROM SECTION 5:
See "section5" folder

Contents of sim_study folder:
	section5.Rproj: R project file. Sets correct file path.
	simulationList.xlsx: xlsx file that lists the simulation scenarios.
	“evaluation” folder: Has R files to reproduce tables and figures from section 5.
	“results” folder: Saves .RData files needed to summarize simulation results.
	“internal” folder: Internal R files to run simulations (Do not edit).
	“runs” folder: R files to run simulations over various scenarios given by the xlsx file.
	“sourceFiles” folder: Additional R files needed for simulation studies.
		crrp_BIC.R: Modified code for crrp to level comparisons.
		getLambdaPath.R: Retrieves path of tuning parameters using crrp’s internal code.
		utils.R: Files to create correlation (and design) matrix needed to simulate design matrix.

#################################################
HOW TO RUN SIMULATION STUDY:
1. Load the fastcmprsk package and make sure necessary packages are installed (see installNecessaryPackages.R file).

2. Without changing or modifying any files, open section5.Rproj

3. go to the “runs” folder and click the R file that corresponds to the simulation study (e.g. runs-s51 corresponds to the study performed in Section 5.1).

4. Run the entire script (results will be saved in the “results” folder).
[MAKE SURE DIRECTORY IS .../RJournal_Rfiles/section5/]

5. To evaluate, go to the evaluation folder and choose the corresponding R file. (e.g. evals-s51 is to evaluate the results from runs-s51)

6. Follow #3 again and change the directory.

7. Run the entire script (results will be stored in corresponding objects).

NOTES:
- Timing comparisons were done on a Mac Pro with an Intel Core i5 2.9 GHz processor and 16GB of memory. Results may vary slightly but should have a consistent trend to what is reported.
