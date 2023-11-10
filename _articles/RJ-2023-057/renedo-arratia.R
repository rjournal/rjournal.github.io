# This file contains the examples found in the article "clustAnalytics: An R 
# Package for Assessing Stability and Significance of Communities in Networks"

library(clustAnalytics)

# Rewiring algorithm
data(foodwebs, package="igraphdata")
rewired_ChesLower <- rewireCpp(foodwebs$ChesLower, weight_sel = "max_weight")

data(g_forex, package="clustAnalytics")
rewireCpp(g=g_forex, weight_sel="const_var", lower_bound=0, upper_bound=1)

data(karate, package="igraphdata")
rewired_karate <- rewireCpp(karate, weight_sel="max_weight")


# Cluster Stability
data(karate, package="igraphdata")
c1 <- membership(cluster_louvain(karate))
c2 <- V(karate)$Faction
reduced_mutual_information(c1, c2, method="approximation2")
reduced_mutual_information(c1, c2, method="approximation2", normalized=TRUE)


# Graph generators and other utilities
B <- matrix(c(1, 0.2, 0.2, 1), ncol=2)
G <- barabasi_albert_blocks(m=4, p=c(0.5, 0.5), B=B, t_max=100, type="Hajek",
                              sample_with_replacement = FALSE)
plot(G, vertex.color=(V(G)$label), vertex.label=NA, vertex.size=10)

apply_subgraphs(g=karate, com=V(karate)$Faction, f=gorder)        




#############################
## An introductory example ##

data(karate, package="igraphdata")
rewired_karate <- rewireCpp(karate, weight_sel = "max_weight")
par(mfrow=c(1,2), mai=c(0,0.1,0.3,0.1))
plot(karate, main="karate")
plot(rewired_karate, main="rewired_karate")

evaluate_significance(karate,
                      alg_list=list(Louvain=cluster_louvain,
                                    "label prop"= cluster_label_prop,
                                    walktrap=cluster_walktrap),
                      gt_clustering=V(karate)$Faction)

evaluate_significance_r(karate,
                        alg_list=list(Lv=cluster_louvain,
                                      "WT"= cluster_walktrap),
                        weight_sel="max_weight", n_reps=100)


# Applying scoring functions
scoring_functions(karate, V(karate)$Faction, type="local")
scoring_functions(karate, V(karate)$Faction, type="global")

cut_ratio(karate, V(karate)$Faction)
conductance(karate, V(karate)$Faction)
weighted_clustering_coefficient(karate)
apply_subgraphs(karate, V(karate)$Faction, weighted_clustering_coefficient)


# Evaluating cluster stability
boot_alg_list(g=karate, return_data=FALSE, R=99,
              alg_list=list(Louvain=cluster_louvain,
                            "label prop"= cluster_label_prop,
                            walktrap=cluster_walktrap))


# Clustering Assessment on synthetic ground truth networks
pm <- matrix (c(.3, .001, .001, .003,
                .001, .2, .005, .002,
                .001, .005, .2, .001,
                .003, .002, .001, .3), nrow=4, ncol=4)
g_sbm <- igraph::sample_sbm(100, pref.matrix=pm, block.sizes=c(25,25,25,25))
E(g_sbm)$weight <- 1
memb <- c(rep(1,25), rep(2,25), rep(3,25), rep(4,25))
significance_table_sbm <- evaluate_significance(g_sbm, gt_clustering=memb)
significance_table_sbm

b_sbm <- boot_alg_list(g=g_sbm, return_data=FALSE, R=99)
b_sbm
