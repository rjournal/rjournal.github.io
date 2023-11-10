# clustvarsel 2.3.4 (2020-12)

* Bug fixes and polish.  

# clustvarsel 2.3.3 (2018-11)

* Added the final estimated model to the `clustvarsel` object. 
* Solved a bug that stop execution in the greedy-backward search when no variables could be removed. 

# clustvarsel 2.3.2 (2018-04)

* Package version accompanying JSS paper. 
* Bug fixes in the extreme case no clustering variable is selected using the greedy forward/backward search.

# clustvarsel 2.3.1 (2017-06)

* Fix bug on a `if` executed with a condition that has length greater than 1.

# clustvarsel 2.3 (2017-01)

* Add optional argument `verbose` to `clustvarsel()` for printing steps info during the search.
* New print method for `clustvarsel` objects.
* A parallel cluster is automatically stopped unless a registered parallel back end is provided as argument to `parallel` argument in the `clustvarsel()` function call.
* Add "A quick tour of clustvarsel" vignette.
  
# clustvarsel 2.2 (2015-11)

* Reformat summary output from `clustvarsel` object.
* Add and update references in main help page.

# clustvarsel 2.1 (2014-10)

* Version associated with JSS paper submission.
* Add explicitly stop of clusters if parallel is used.
* Specifically included in the `hc()` function call the argument name `data = ...` so that works with both mclust version 4.4 and upper.
* Other bug fixes and improvements.

# clustvarsel 2.0 (2013-10)

* Partial rewriting of the package.
* "greedy" search has option for forward and backward direction.
* "headlong" search has option only for forward direction in this release.
* In `clustvarsel()` argument `G` is not the maximum number of clusters but it must be a vector of number of cluster to look for.
* No separate code for sampling and no-sampling version of each search algorithm.
* Inclusion of argument `hcModel` to control the initial hierarchical clustering.
* Include subset selection in the regression of proposed variable on the variables already included.
* "greedy" search algorithms can be executed either sequentially or using the parallel computing facilities available in R.
* This version of the package requires R (>= 3.0.0) and mclust (>= 4.0).
  
# clustvarsel 1.3 (2009-08)

* Last version on CRAN available for R-2.14.x and mclust version 3.5

 