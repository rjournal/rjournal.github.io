# 2022-53: TreeSearch: Morphological Phylogenetic Analysis in R

# Referee 1

> I have provided some comments to the manuscript and have checked the package TreeSearch. I feel both the manuscript and the package are well prepared and should be very useful for inferring phylogenetic inferences, in particular when there are “inapplicable characters”.

> There are a few minor issues that may have to be concerned and I have listed them below. I recommend “minor revision”.

I thank the referee for this positive feedback and have taken the suggested
opportunities to improve the clarity of the manuscript.

## Comments for the author

> The manuscript entitled "TreeSearch: Morphological Phylogenetic Analysis in R" by Martin R. Smith described the R package TreeSearch, which will be very useful for inferring phylogenies based on morphological datasets, in particular, when there are 'inapplicable characters'.
> The package is built upon the Morphy” phylogenetic library written in C (Brazeau et al 2019), and has R interfaces for scoring, searching, and visualising the trees using tree similarity indices. I feel both the manuscript and the package are well prepared. However, there are a few sentences/paragraphs that seem confusing to me.

> First of all, the package was cited as “M. R. Smith. TreeDist: calculate and map distances between phylogenetic trees. Comprehensive R
Archive Network, page doi:10.5281/zenodo.3528123, 2020b. doi: 10.5281/zenodo.3528123. [p6]”.
I am not sure this is still appropriate as it may lead issues in citation.

This is the correct citation for the package "TreeDist" (not "TreeSearch").

> Second, A flow chart would help the readers to understand how the package is organised and how to use the functions (for the Section "Implementation"). Please consider.

Thanks for the suggestion -- a flow chart has been added as a new figure.

> 1. Page2, "Implied weighting is an approximate method that treats each additional step in a character as less surprising – and thus requiring less penalty – than the previous step." "additional step" may be confusing for the readers, it is better to provide a citation.

Added citation to Goloboff (1993) and glossed the term "step".

> 2. Page 2. "The minimum length of a tree is one less than the number of unique applicable tokens that must be present.". A citation to "applicable tokens" may help the reader to understand the text.

Reworded to "The minimum length of a tree is one less than the number of unique tokens (excluding the inapplicable token `-`) that must be present."

"token" is defined in the "dataset review" section.

> 3. Page 3. "A visualization of stress is provided by plotting the minimum spanning tree (Gower and Ross, 1969); contortions in this tree indicate that mapping has distorted original distances (Smith, 2022b)". A little bit more explanation would help the readers to understand. If the statement could be supported by the figures, please cite the figure(s) properly.

Clarified and linked to figures, with explicit link to expository figures in
Smith 2022b.

> 4. Page 3. Paragraph "During tree search, ..., each iteration will identify fewer new trees.". I am not sure if this paragraph is really necessary.

Removed.

> 5. Page 3. "trees <- MaximizeParsimony(vinther, concavity = 10, tolerance = 0.05)" and "table(signif(TreeLength(trees, vinther, concavity = 10)))". A little bit more explanation on the arguments may have to be provided as not all the readers may move to the manual page of those functions.

Arguments now detailed in the text.

> 6. Page 3. "The latter feature can be used...". What does "the latter feature" refer to? Could the whole paragraph be restructured to make it more understandable?

- Reworded first sentence
  - "Annotating trees by the iteration in which they were first found allows a user to evaluate whether a continuation of tree search is likely to yield more optimal trees."
- Restructured paragraph for clarity and concision

> 7. Page 5. "tested on all widely available operating systems ". It would be better to list the names of the operating systems explicitly.

Tested operating systems (Windows 10, Mac OS X 10 and Ubuntu 20) are now stated in the text.

> Also, I have noticed that this package uses Valgrind which is only available on the UNIX-like systems.
This means the code may have not been checked thoroughly on Windows. 
If this is true, should this be mentioned in the maintext?

Valgrind is used to check C++ code for memory allocation errors, which are platform independent.
Automated tests run on all listed systems, but I develop and use the software on Windows -- thus TreeSearch has been checked most thoroughly on real datasets using Windows.

> 8. Page 5. 'remotes::install_github("ms609/ape@patch-3")'.
The 'patched' ape could not be installed using the code provided.
Please check and correct.

The patch to ape has now been incorporated into the package on CRAN.
I have removed the note from the manuscript.


# Referee 2

> Dear Editor,
>
>Please find below my comments on the TreeSearch package and its associated manuscript entitled “TreeSearch: Morphological Phylogenetic Analysis in R”. This package allows users to load their own phylogenetic dataset and run some advanced phylogenetic inferences on it.
>
>Tree inference features are already present to some extent in the phangorn and ape packages but not to the extend of what is available in TreeSearch. 
>
>I therefore highly recommend the publication of this manuscript in the R journal since I believe it will be of great interest to readers and might even attract new R users that would be more familiar with running tree inferences in other software which are not community developed (such as PAUP* or TNT).
>
> I’d like also to highlight that the author made a great effort to document the package and even provides a GUI that will be even better at attracting new R users and would hopefully make them prefer this software over other available ones.
>
> I mainly have three suggestions that I detail below:
>
> • Benchmarking the functions speed
>
> • Adding a LICENSE file
>
> • Adding a way to export/save the work on the GUI as a reproducible script


## Comments on the manuscript:

> The manuscript reads really well.
> It contains most information a user would need for knowing the details about the implementation without having to delve deeper into the code or the phylogenetic literature (although the manuscript provides great references to it).
>
> One additional section the author could add would be to benchmark the package performances compared to other R packages (e.g. phangorn) or to other software (PAUP* and TNT).
> This would allow the reader to grasp the order of magnitude of speed of the author’s algorithm (I was positively surprised to see how relatively fast
it ran for basic analyses).
> [omitted] is a benchmark function the author could use using dispRity::sim.morpho
to simulate some morphological data (of course any other function the author would come up with would work perfectly!).

This is a nice suggestion in principle, but is not really practical to implement, because there is not a defined "end" to tree search.  One method may be faster than another to return results, yet these results may be further from the globally optimal tree.  Because different algorithms use different optimality criteria, trees that are optimal under one method may not be under another, and the number of optimal trees may differ wildly.  Finally, run time depends enormously on tree size, the complexity of the underlying dataset, and the nature of the landscape of optimal trees; it is not clear that even order-of-magnitude timing comparisons obtained under a benchmarking exercise would transfer usefully from one dataset to another.  As results are so difficult to extrapolate beyond the specific datasets used to generate them, I don't feel that benchmarking data could ever really provide the reader with much additional value.


## Comments on the package implementation:

> Overall the package implementation seems very solid and serious:
>
> - The author’s code is mostly easy to read with consistent functions and arguments names.
>
> - The author uses a continuous integration although it has not been updated since a while, maybe due to changes in Travis-CI - I suggest switching to GitHub-actions or an other CI or fixing the errors with Travis.
>
> - The functions are unit tested with an implemented test coverage through codecov at 83% which is a decent level (and a good 83 points more than many other packages used in evolutionary biology!).
Looking at the commits history I have confidence that the author will continue updating the test coverage.
>
> - Finally the authors keeps a good record of the updates to the package in the NEWS file which is really useful for future developments of the package (and namely will foster other authors to use this work!).

The package does in fact use GitHub actions -- I have now removed the out-dated
Travis badge from the README file.

The 17% of code not presently covered by unit tests is predominantly legacy or
development code that is not central to the main functioning of the package,
and thus not discussed in the present manuscript.

> The only thing missing there would be a LICENSE file to clearly state the license of the software.

Added, complementing the entry in the `DESCRIPTION` file.

## Comments on the vignettes and manuals:

> The vignettes and manuals are really clearly written with plenty of information for readers with a basic knowledge in phylogenetics.
I actually enjoyed reading the vignettes and I have no doubt that this format would allow the author to add more/modify information as the package grows.

Great! I do indeed plan to continue adding to the vignettes as the package matures.

## Comments on the GUI:

> This GUI is super nice! I’ve only played with the example datasets but it is very intuitive and allows for quiet a few advanced exploratory analysis.
I think it would be very useful for exploring datasets or for R beginners that want to use TreeSearch.
> On that note, I suggest the author adds an additional export button for exporting the results of analyses on the GUI. The author could have a script running in the background recording the users analyses so it could be exported and attached to publications for reproducibility.

This is a great suggestion.  I've added options to export:

1. A script listing all tree searches conducted in the current session, which is exported to a ZIP archive along with the data files used.  The ZIP archive should then contain everything a researcher requires to reproduce a GUI analysis in the R console.

2. A script that will reproduce the contents of the current plotting area (i.e. tree plot / tree space analysis / cluster consensus trees).  This is again provided in a ZIP archive with all files necessary for a standalone reproduction of the plot.
