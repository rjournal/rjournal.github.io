This README gives details about running the code for all simulations in serial.

The "_sim_all_serial.R" code runs each of the 800 simulation replicates (100 replicates for 8 simulation set-ups) one after the other. This will take a long time, particularly the simulations with 50 covariates (the "50Cov" simulations), see Table 3 for median time to completion for these simulations.

At the end of the document, there is additional code to convert the output into the .RData objects given in the "Tables ... Full Data.RData" objects.

If desired, adjust the "prefix0" variable to the path where the user wants the simulation output to go. Currently, this directory is set to the current working directory.




######################################################################################################