#' # fcaR, Formal Concept Analysis with R
#'
#' This script accompanies the paper "fcaR, Formal Concept Analysis with R" that presents the fcaR package.
#'
#' The script can be sourced or run line by line, or one can use "knitr::spin('./fcaR.R')" to run everything and obtain an HTML file with all the results.
#'
#' ## Installation and setup
## ---- message = FALSE
library(magrittr)

if (!require(fcaR)) {

  # Choose the version to install as needed.

  # Last version submitted to CRAN
  # install.packages("fcaR")

  # Development version
  remotes::install_github("Malaga-FCA-group/fcaR",
                          dependencies = TRUE)

}

#' In case the package "hasseDiagram" has not been installed (it is necessary for the plot of concept lattices), we can force it with (maybe indicating previously to use Bioconductor repositories with `setRepositories()`):
# install.packages("hasseDiagram")

#' Then, we can load the library:
library(fcaR)

#' ## Section "Background on FCA"


#' The following code defines the formal context that is used as example for in sections "Background on FCA" and "Formal concept analysis with fcaR"

objects <- paste0("O", 1:4)
n_objects <- length(objects)

attributes <- paste0("P", 1:4)
n_attributes <- length(attributes)

I <- matrix(data = c(0, 0.5, 0.5, 0,
                     0.5, 1, 1, 0.5,
                     0.5, 0, 0, 1,
                     0.5, 1, 1, 0.5),
            nrow = n_objects,
            byrow = FALSE)

colnames(I) <- attributes
rownames(I) <- objects

fc <- FormalContext$new(I)

#' The formal context can be printed with
fc

#' In the paper, it is exported to LaTeX, using:
fc$to_latex(fraction = "sfrac")

#' ### Derivation operators

#' The examples in this section were calculated using fcaR. For instance, the sets S and T are defined as follows:
S <- Set$new(fc$objects, O1 = 1, O2 = 1)
T <- Set$new(fc$attributes, P1 = 0.5)

#' intent(S), extent(T) and closure(T) (in the paper the operator was named "phi") are calculated with:
intentS <- fc$intent(S)
intentS
extentT <- fc$extent(T)
extentT
phiT <- fc$closure(T)
phiT

#' The example of the concept that appears in that section requires to compute first the concept lattice.

#' We find the concept lattice with:
fc$find_concepts()

#' Then, we take the 6th concept (for example)
C <- fc$concepts$sub(6)

#' If we call (A, B) to the left- and right- sides of C, we can check that A = extent(B) and B = intent(A). To extract these components of a Concept, we use the "get_extent()" and "get_intent()" methods.
A <- C$get_extent()
A
B <- C$get_intent()
B
fc$extent(B) # This should match A
fc$intent(A) # This should match B

#' Another way to check this is by using
#' the %==% operator:
A %==% fc$extent(B)
B %==% fc$intent(A)

#' In the paper, we make extensive use of exporting variables to LaTeX, just by using the "to_latex()" method:
A$to_latex()

#' ### The concept lattice

#'  The example of the order relationship between concepts uses the second subconcept of the C previously computed:
Csub <- fc$concepts$subconcepts(6)$sub(2)

#' We can check that Csub is a subconcept of C using the operator %<=%:
Csub %<=% C

#' To plot the concept lattice (Figure 1):
fc$concepts$plot()

#' ### Implications and logic

#' We compute the Duquenne-Guigues basis of implications using:
fc$find_implications()

#' These implications are the basis of valid implications:
fc$implications

#' Again, they are exported to LaTeX with:
fc$implications$to_latex()

#' To apply the equivalence rules mentioned in the section, it is advisable to clone the set of implications beforehand to not overwrite it:
imps <- fc$implications$clone()
## ---- message = FALSE
# Here we apply Simplification and RightSimplification
imps$apply_rules(c("simp", "rsimp"))

#' The difference with the previous implications are in items 2 to 6:
imps[2:6]

#' The actual code used in the paper to match the id of the implications is:
# indexes of rules that have changed
id_diff <- which(base::rowSums(abs(fc$implications$size() - imps$size())) > 0)
# Export to LaTeX only those rules with the given numbers
imps[id_diff]$to_latex(numbered = TRUE, numbers = id_diff)

#' The example of computing the closure of a set S uses:
# Definition of S
S <- Set$new(fc$attributes, P2 = 0.5)
# Closure computation
Scl <- fc$implications$closure(S, reduce = TRUE)

#' The closure of S, named S^+ in the paper, is
Scl$closure

#' The reduced set of implications is
Scl$implications

#' The implications, as before, are exported to LaTeX with
Scl$implications$to_latex()


#' # Section "Formal concept analysis with fcaR"

#' ## Derivation operators
#'
#' To plot the heatmap of a formal context, we use (Figure 2 in the paper):
fc$plot()


#' In this section, we begin with examples similar to those of the "Background on FCA" section.
#'
S <- Set$new(fc$objects, O1 = 1, O2 = 1)
S
fc$intent(S)
T <- Set$new(fc$attributes, P1 = 1, P3 = 1)
T
fc$extent(T)
fc$closure(T)

#' In addition, we show how to perform clarification on the formal context fc
fc_cla <- fc$clarify(TRUE)
fc_cla

#' ## Concept lattice

#' Again, to compute the concept lattice
fc$find_concepts()

#' And to show the list of concepts
fc$concepts

#' Size of the concept lattice: number of concepts:
fc$concepts$size()

#' One can take just a subset of the concepts using the standard R notation
fc$concepts[c(1:3, 5, 8)]

#' One could use negative indexes and a boolean vector to subset:
fc$concepts[-c(1:3)]

#' To compute the support of each concept, we use the "support()" method:
fc$concepts$support()

#' ## Sublattices
#'
#' To build a sublattice, we can use the indexes of the "generating" concepts:
fc$concepts$sublattice(5:6)

#' Or we could use a ConceptSet to generate the sublattice:
fc$concepts$sublattice(fc$concepts[5:6])

#' ## Subconcepts, superconcepts, infimum and supremum
#'
#' The concept named L in the paper is obtained with:
L <- fc$concepts$sub(4)

#' The list of subconcepts and superconcepts of L
fc$concepts$subconcepts(L)
fc$concepts$superconcepts(L)

#' To compute the supremum and infimum of a set of concepts, the methods are "supremum()" and "infimum()". Let us build a ConceptSet for an example:
X <- fc$concepts[c(2, 3, 4)]
X
fc$concepts$infimum(X) # the largest common subconcept of all elements in X
fc$concepts$supremum(X) # the smallest common superconcept of all elements in X

#' The irreducible elements are an essential part of lattice theory.
fc$concepts$meet_irreducibles()
fc$concepts$join_irreducibles()

#' ## The standard context

#' The standard context condenses the same knowledge as in the original formal context. It is formed with the join- and meet- irreducible elements of the original context, with the order relationship in the concept lattice, <=.
#'
#' To compute the standard context, we use the "standardize()" method.
fc_std <- fc$standardize()
fc_std

#' In the paper, the table of the standard context was generated with:
fc_std$to_latex(table = FALSE)

#' Its concept lattice, which is isomorphic to that of the original formal context, is computed and plotted with:
fc_std$find_concepts()
fc_std$concepts$plot(object_names = TRUE)

#' ## Implications and logic

#' To compute the Duquenne-Guigues basis of implications for the formal context fc, we use
fc$find_implications()

#' The complete basis can be printed with
fc$implications

#' As with the concepts, we can use standard R subsetting to extract only selected implications:
fc$implications[1:3]

#' To export them to LaTeX, we do.
fc$implications[1:3]$to_latex()

#' Filtering of implications: We can filter those implications that fulfill certain conditions
fc$implications$filter(lhs = c("P1", "P2"),
                       rhs = "P4")

#' The cardinality of the implication set is the number of implications:
fc$implications$cardinality()

#' The size of an implication is the cardinality (as in set theory) of its LHS and RHS. The "size()" method can be applied to sets of implications so it will return the detailed sizes of each:
# For example, take 2 implications and compute their sizes:
my_imps <- fc$implications[4:5]
my_imps
my_imps$size()

#' The supports of a set of implications can be computed as:
fc$implications$support()

#' ## Application of the simplification logic

#' The equivalence rules implemented are in the "equivalenceRegistry" object:
equivalencesRegistry
#' The user can obtain the names of the implmented rules with:
equivalencesRegistry$get_entry_names()

#' The information of one of those entries is retrieved by:
equivalencesRegistry$get_entry("Composition")


#' The application of the equivalence rules to the implication basis can be made with:
## ---- message = FALSE
fc$implications$apply_rules(
  rules = c("reduction",
            "comp",
            "gener",
            "simpl"))
# Let us inspect the result
fc$implications

#' ## Computation of closure and recommendations

#' The example of computing the closure requires to define the Set A:
A <- Set$new(attributes = fc$attributes,
             P2 = 1)

#' The actual computation occurs in this line, and it returns the closure, and the reduced implication set.
fc$implications$closure(A, reduce = TRUE)


#' # Section "Usage example. Fuzzy diagnostic system"

#' Let us load the cobre32 dataset.
#'
#' One can get help about the dataset with ?cobre32
fc <- FormalContext$new(cobre32)

#' This formal context may be plotted with (resulting in Figure 3 in the paper):
fc$plot()

#' Let us compute the implication basis. It is time consuming, depending on the hardware. It may take from 30 seconds to several minutes.
fc$find_implications()

#' Number of implications and concepts:
fc$concepts$size()
fc$implications$cardinality()

#' We are going to check how the sizes of the implications decrease when using the simplification logic.
# Sizes before applying the logic
pre_logic <- colMeans(fc$implications$size())
pre_logic

#' We apply the logic
## ---- message = FALSE
fc$implications$apply_rules(
  rules = c("simplification",
            "rsimplification"))
# Sizes after applying the logic:
post_logic <- colMeans(fc$implications$size())
post_logic

#' This is the function that is used in the paper for the diagnosis system
diagnose <- function(S) {

  fc$implications$recommend(S = S,
                            attribute_filter =
                              c("dx_ss", "dx_other"))

}

#' These are the same examples as in the paper.
#'
#' First, we define a Set and the execute the "diagnose()" function on it.
S1 <- Set$new(attributes = fc$attributes,
              COSAS_1 = 1/2, COSAS_2 = 1, COSAS_3 = 1/2,
              COSAS_4 = 1/6, COSAS_5 = 1/2, COSAS_6 = 1)
diagnose(S1)

S2 <- Set$new(attributes = fc$attributes,
              COSAS_2 = 1, COSAS_6 = 1, FICAL_1 = 1/3, FICAL_3 = 1/3)
diagnose(S2)

S3 <- Set$new(attributes = fc$attributes,
              COSAS_4 = 2/3, FICAL_3 = 1/2, FICAL_5 = 1/2, FICAL_8 = 1/2)
diagnose(S3)

#' Since for S2 there was not a diagnosis, we can use the simplification logic closure to obtain the reduced set of implications:
cl <- fc$implications$closure(S2, reduce = TRUE)
## ---- message = FALSE
# We apply again the equivalence rules to obtain an even more compact implication set.
cl$implications$apply_rules(
  rules = c("simp", "rsimp", "reorder"))
# And, finally, we filter the implications that have a diagnosis in the RHS, and not in the LHS:
cl$implications$filter(rhs = c("dx_ss", "dx_other"),
                       not_lhs = c("dx_ss", "dx_other"),
                       drop = TRUE)


