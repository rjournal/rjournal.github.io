library(GeRnika)


# Generate a tumor instance with 5 clones, 4 samples, k=0.5, and neutral evolution.
I <- create_instance(n = 5, m = 3, k = 0.5, selection = "neutral", seed = 1)
I


# Create a 'Phylotree' class object based on the previously simulated instance.
phylotree <- B_to_phylotree(B = I$B)
phylotree


# Plot the 'Phylotree' class object.
plot(phylotree)


# Plot the 'Phylotree' class object according to the clone proportions
# associated to the previously generated U matrix and using predefined tags
# to label the nodes in the tree.
plot_proportions(phylotree, I$U, labels = TRUE)


# Load the first trio of matrices of the B_mats object offered by GeRnika.
B_mats <- GeRnika::B_mats
B_real <- B_mats[[1]]$B_real
B_alg1 <- B_mats[[1]]$B_alg1
B_alg2 <- B_mats[[1]]$B_alg2


# Create a predefined set of tags for the clones in the phylogenetic trees.
tags <- c("TP53", "KRAS", "PIK3CA", "APC", "EGFR", "BRCA1", "PTEN", 
          "BRAF", "MYC", "CDKN2A")


# Create a 'Phylotree' class object per each B matrix in the loaded trio of B
# matrices.
phylotree_real <- B_to_phylotree(B_real, labels = tags)
phylotree_alg1 <- B_to_phylotree(B_alg1, labels = tags)
phylotree_alg2 <- B_to_phylotree(B_alg2, labels = tags)


# Plot the phylogenetic trees using the predefined set of tags.
plot(phylotree_real, labels = TRUE)
plot(phylotree_alg1, labels = TRUE)
plot(phylotree_alg2, labels = TRUE)


# Check if phylotree_real and phylotree_alg1 are equal.
equals(phylotree_1 = phylotree_real, phylotree_2 = phylotree_alg1)


# Check if phylotree_real and phylotree_real are equal.
equals(phylotree_1 = phylotree_real, phylotree_2 = phylotree_real)


# Find the maximal common subtrees between phylotree_real and phylotree_alg2,
# using the predefined set of tags.
find_common_subtrees(phylotree_1 = phylotree_real, phylotree_2 = phylotree_alg2, 
                     labels = TRUE)


# Load the Lancet palette offered by GeRnika
palette <- GeRnika::palettes$Lancet


# Compute the consensus tree between phylotree_real and phylotree_alg1, using the
# predefined set of tags for the clones in the trees and the previously loaded
# palette
consensus_real_alg1 <- combine_trees(phylotree_1 = phylotree_real, 
                                      phylotree_2 = phylotree_alg1,
                                      labels = TRUE, 
                                      palette = palette)


# Plot the consensus tree between phylotree_real and phylotree_alg1 by means of
# DiagrammeR's render_graph function.
DiagrammeR::render_graph(consensus_real_alg1)
