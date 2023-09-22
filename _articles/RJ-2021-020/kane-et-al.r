library(basket)

# Load the Vemurafenib data, perform a basket analysis, and show the 
# summary, including the cluster analysis.

data(vemu_wide)

vm <- basket(vemu_wide$responders, 
             vemu_wide$evaluable,
             vemu_wide$baskets, p0 = 0.25, 
             cluster_analysis = TRUE)

summary(vm)

# Plot basket-wise and cluster-wise response densities.

plot_density(vm, type = "basket")

plot_density(vm, type = "cluster")

# Show the posterior exchangeability probabilities.

basket_pep(vm)

plot_pep(vm$basket)

plot_pep_graph(vm)
