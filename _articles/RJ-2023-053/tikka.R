# Replication material for
# "Identifying Counterfactual Queries with the R Package cfid"
# Santtu Tikka
# 18.11.2022

# Install the development version of the package
remotes::install_github("santikka/cfid")
# Or install the latest CRAN version
# install.packages("cfid")
library("cfid")

# Examples of Section 4

# DAGs
g <- dag("X -> Z -> Y; X -> Y; X <-> Z")
g <- dag("X -> {Z, Y}; Z -> Y; X <-> Z")
g <- dag("X -> {Z, Y}; X <-> Z -> Y;")
g <- dag("Z <-> X -> {Z -> Y}")

# Counterfactual Variables
v1 <- cf(var = "Y", obs = 0L, sub = c(X = 0L))
v2 <- cf(var = "X", obs = 1L)
v3 <- cf(var = "Z", obs = 0L, sub = c(D = 0L))
v4 <- cf(var = "D", obs = 0L)
list(v1, v2, v3, v4)

# Counterfactual Conjunctions
c1 <- conj(v1, v2, v3, v4)
c1

c2 <- v1 + v2
c3 <- v3 + v4
c2
c3
c2 + c3

c1[c(1, 3)]

# Identifiability
g1 <- dag("Y <-> X -> W -> Y <- Z <- D")
g2 <- dag("Y <-> X -> W -> Y <- Z <- D; X -> Y")
out1 <- identifiable(g1, c1)
out2 <- identifiable(g2, c1)
out1
out2

print(out1[["formula"]], use_do = TRUE)

g3 <- dag("Z <-> X -> {Z -> Y}")
v5 <- cf("Z", 0, c(X = 0))
identifiable(g3, v1, v5 + v2)

identifiable(g3, v1, v5 + v2, data = "observations")

