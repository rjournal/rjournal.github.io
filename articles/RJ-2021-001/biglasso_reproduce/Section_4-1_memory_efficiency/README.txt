
To replicate results in Section 4.1, the Python script "Syrupy" needs to be installed first. (See details in https://github.com/jeetsukumaran/Syrupy)

Here is how the maximum RSS is measured and reported. 

1) For a single fit (for example biglasso model), cd to single_fit directory, run the following command at terminal:

	syrupy.py Rscript biglasso_memory.R

this generates four files:

	syrupy_*.ps.log
	syrupy_*.ps.raw	syrupy_*.err.log	syrupy_*.out.log

The .ps.log file contains memory profiles during model fitting.

2) Open file "syrupy_*.ps.log", identify the maximum value in Column RSS. This is the maximum RSS (in KB) during the model fitting.

3) The same procedure applies to measure memory usage for cross-validation.

Note: Folders "single_fit" and "cvfit_serial" contain the raw *.ps.log files - except the names were changed - used for reporting results in Table 2 of the paper.