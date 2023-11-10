**DCEM 2.0.5**

DCEM is published in SoftwareX, https://doi.org/10.1016/j.softx.2021.100944. Use citation("DCEM") to cite the package.

Added the functionality to predict the cluster membership for test data.

Fixed a minor bug in co-variance calculation during maximiztion.

**DCEM 2.0.4**

Added the option to get data membership (maximum posterior probability) from the output.

**DCEM 2.0.3**

Added quick start examples and use cases in the vignettes.

**DCEM 2.0.2**

Added the option to get cluster membership of data directly from the output.
Patched the code for K-Means++ based initialization.

**DCEM 2.0.1**

Bug fix release of the DCEM package.

**Bug Fixes**

Removed the usage of floor in integer division in CPP code. The usage lead to warnings (on Solaris OS) in the previous version - 2.0.0.

**DCEM 2.0.0**

This is the fourth major release of the DCEM package.

**Major Features**

Improves the EM\* implementation for even faster execution. The EM\* is motivated from the ideas published in the Using data to build a better EM: EM* for big data. Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic (2016) <https://doi.org/10.1007/s41060-017-0062-1>.

The package now supports both the EM\* and the traditional EM algorithm for speed-up comparison. The EM\* leverages the max-heap structure to expedite the execution time manifold compared to the conventional EM.

**DCEM 1.0.0**

This is the third stable release of the DCEM package.

**Major Features**

Implements the EM\* algorithm from the ideas published in the Using data to build a better EM: EM* for big data. Hasan Kurban, Mark Jenne, Mehmet M. Dalkilic (2016) <https://doi.org/10.1007/s41060-017-0062-1>.

The package now supports the EM algorithm and the improved version EM\*. The EM\*
leverages a heap structure to expedite the execution time manifold compared to the conventional EM.


**DCEM 0.0.2**

This is the second stable release of the DCEM package.

**Major Features**

Implements the improved initialization schemes (based on the idea published in Kmeans++: The Advantages of Careful Seeding, David Arthur and Sergei Vassilvitskii, http://ilpubs.stanford.edu:8090/778/1/2006-13.pdf.) to 
expedite convergence on big datasets.


**DCEM 0.0.1**

This is the first stable release of the DCEM package.

**Major Features**

Support clustering of both multivariate and univariate data for finite gaussian mixture models.
