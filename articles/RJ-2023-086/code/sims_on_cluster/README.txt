This README gives details about running the simulations in parallel on a cluster.

Each R file is designed to run a single replicate of one of the simulation set-ups (each R file specifies 4 possible simulation set-ups; both files together specify the 8 possible simulation set-ups described in the paper). For each simulation set-up, 100 replicates should be run. By submitting the code on a cluster, one can run these 8x100 replicates in parallel. 

R files: 

If desired, go into .R code files and change path names (the prefix variables). Code should run with the default path names, so changes are not necessary.

A description of the simulation set-ups are given at the top of each R file.

sh files: 

Currently, the header of the sh files is "#!/bin/sh", which is appropriate for a linux environment. If using a different environment, this header may need to be changed. 

The sh files specify that the code (the .R files in this directory) are present in the home directory and .Rout files should be saved in an "Rout/" folder. If necessary, change the locations to the appropriate paths.

Perform the following adjustment to the sh file (if necessary):

dos2unix directory/file_name.sh

Submitting batches on the cluster:

sbatch --array=1-400 directory/file_name.sh
