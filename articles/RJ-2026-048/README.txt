The folder "code" contains scripts to reproduce the results in the four tutorials in the article. Each script tries to install the package (and its dependencies) from CRAN if it is not already installed. The last script requires the package torchvision to download the KMNIST dataset, so it is installed there.

Run the scripts in the following order:

Tutorial 1: 1_simulated_data_linear.R
Tutorial 2: 2_simulated_data_non_linear.R
Tutorial 3: 3_gallstone_data_classification.R
Tutorial 4: 4_kmnist_convolutional_net.R