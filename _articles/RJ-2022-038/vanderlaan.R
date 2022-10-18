library(reclin2)
data("linkexample1", "linkexample2")
(pairs <- pair_blocking(linkexample1, linkexample2, "postcode"))

library(parallel)
cl <- makeCluster(2)
cpairs <- cluster_pair_blocking(cl, linkexample1, linkexample2, "postcode")

(compare_pairs(pairs, on = c("lastname", "firstname", "address", "sex"),
  comparators = list(lastname = jaro_winkler(0.9), firstname = jaro_winkler(0.9),
     address = jaro_winkler(0.9) ), inplace = TRUE))

(compare_pairs(cpairs, on = c("lastname", "firstname", "address", "sex"),
  comparators = list(lastname = jaro_winkler(0.9), firstname = jaro_winkler(0.9),
     address = jaro_winkler(0.9) ), inplace = TRUE))


comp_name <- function(x, y) {
  equal <- identical()
  regular <- equal(x[[1]], y[[1]]) & equal(x[[2]], y[[2]])
  swapped <- equal(x[[1]], y[[2]]) & equal(x[[2]], y[[1]])
  regular | swapped
}
compare_vars(pairs, "name_swap", on_x = c("firstname", "lastname"), 
  comparator = comp_name)

m <- problink_em(~ lastname + firstname + address + sex, data = pairs)
(pairs <- predict(m, pairs = pairs, add = TRUE))

compare_vars(pairs, "true", on_x = "id", on_y = "id", inplace = TRUE)
mglm <- glm(true ~ lastname + firstname, data = pairs,
  family = binomial())
pairs[, pglm := predict(mglm, type = "response")]

(pairs <- select_n_to_m(pairs, "weights", variable = "select", threshold = 0))

(linked_data_set <- link(pairs, selection = "select"))

cpairs <- predict(m, pairs = cpairs, add = TRUE)
select_threshold(cpairs, "weights", variable = "initial", threshold = 0)
local_cpairs <- cluster_collect(cpairs, "initial")
local_cpairs <- select_n_to_m(local_cpairs, "weights", variable = "select")

unlist(cluster_call(cpairs, \(p, ...) nrow(p)))

(cluster_modify_pairs(cpairs, \(p, ...) p[weights > 0, ]))


