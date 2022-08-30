### R code from vignette source '/home/tdhock/projects/nc-article/hocking-edited.Rnw'

###################################################
### code chunk number 1: li
###################################################
if(dir.exists("library-new")).libPaths("library-new")
options(crayon.enabled=FALSE)


###################################################
### code chunk number 2: irisSingle
###################################################
(iris.tall.single <- nc::capture_melt_single(
  iris, part = ".*", "[.]", dim = ".*", value.name = "cm"))


###################################################
### code chunk number 3: hist
###################################################
library(ggplot2)
ggplot(iris.tall.single) + facet_grid(part ~ dim) +
  theme_bw() + theme(panel.spacing = grid::unit(0, "lines")) +
  geom_histogram(aes(cm, fill = Species), color = "black", bins = 40)


###################################################
### code chunk number 4: singleCompare
###################################################
iris.pattern <- "(.*)[.](.*)"
iris.wide <- data.table::as.data.table(iris)
iris.tall <- data.table::melt(
  iris.wide, measure = patterns(iris.pattern), value.name = "cm")
iris.tall[, `:=`(part = sub(iris.pattern, "\\1", variable),
                 dim  = sub(iris.pattern, "\\2", variable))][]


###################################################
### code chunk number 5: singleCompareTidyr
###################################################
tidyr::pivot_longer(iris, matches(iris.pattern), values_to = "cm",
  names_to=c("part", "dim"), names_pattern=iris.pattern)


###################################################
### code chunk number 6: irisMultiple
###################################################
(iris.parts <- nc::capture_melt_multiple(iris, column = ".*", "[.]", dim = ".*"))


###################################################
### code chunk number 7: scatter
###################################################
ggplot(iris.parts) + facet_grid(. ~ dim) +
  theme_bw() + theme(panel.spacing = grid::unit(0, "lines")) +
  coord_equal() + geom_abline(slope = 1, intercept = 0, color = "grey") +
  geom_point(aes(Petal, Sepal, color = Species), shape = 1)


###################################################
### code chunk number 8: multCompare
###################################################
iris.multiple <- data.table::melt(
  iris.wide, measure = patterns(Petal="Petal", Sepal="Sepal"))
iris.multiple[, dim := c("Length", "Width")[variable] ]
tidyr::pivot_longer(iris, matches(iris.pattern), values_to = "cm",
  names_to=c(".value", "dim"), names_pattern=iris.pattern)


###################################################
### code chunk number 9: who
###################################################
data(who, package = "tidyr")
set.seed(1);sample(names(who), 10)


###################################################
### code chunk number 10: whoreshape
###################################################
nc.who.sub.pattern <- list(
  "new_?", diagnosis = ".*", "_",
  gender = ".", function(mf)factor(mf, c("m", "f")))
nc.who.ages <- nc::capture_melt_single(who, nc.who.sub.pattern, ages = ".*")
print(nc.who.ages[1:2], class = TRUE)


###################################################
### code chunk number 11: pivot
###################################################
tidyr.who.sub.names <- c("diagnosis", "gender")               #L0
tidyr.who.sub.pattern <- "new_?(.*)_(.)"                      #L1
tidyr.who.pattern <- paste0(tidyr.who.sub.pattern, "(.*)")    #L2
tidyr::pivot_longer(                                          #L3
  who, cols = matches(tidyr.who.pattern),                     #L4
  names_to = c(tidyr.who.sub.names, "ages"),                  #L5
  names_ptypes = list(gender = factor(levels = c("m", "f"))), #L6
  names_pattern = tidyr.who.pattern)[1:2,]                    #L7


###################################################
### code chunk number 12: who2
###################################################
who.typed <- nc::capture_melt_single(who, nc.who.sub.pattern, ages = list(
  ymin = "0|[0-9]{2}", as.numeric,
  ymax = "[0-9]{0,2}", function(x)ifelse(x == "", Inf, as.numeric(x))))
who.typed[1:2]
who.typed[, .(rows = .N), by = .(ages, ymin, ymax)]


###################################################
### code chunk number 13: tidyRange
###################################################
tidyr.who.range.pattern <- paste0(tidyr.who.sub.pattern, "((0|[0-9]{2})([0-9]{0,2}))")
tidyr::pivot_longer(
  who, cols = matches(tidyr.who.range.pattern),
  names_to = c(tidyr.who.sub.names, "ages", "ymin", "ymax"),
  names_transform = list(
    gender = function(x)factor(x, levels = c("m", "f")),
    ymin = as.numeric,
    ymax = function(x)ifelse(x == "", Inf, as.numeric(x))),
  names_pattern = tidyr.who.range.pattern)[1:7,]


###################################################
### code chunk number 14: iris2
###################################################
(TC <- data.table::data.table(
  treatment.age = 13,
  control.gender = "M",
  treatment.gender = "F",
  control.age = 25))


###################################################
### code chunk number 15: ncgroups
###################################################
nc::capture_melt_multiple(TC, group = ".*", "[.]", column = ".*")


###################################################
### code chunk number 16: patterns
###################################################
data.table::melt(TC, measure.vars = patterns(age = "age", gender = "gender"))


###################################################
### code chunk number 17: oo
###################################################
data.table::melt(TC, measure.vars = list(age = c(1,4), gender = c(3,2)))


###################################################
### code chunk number 18: reshapenames
###################################################
TC.renamed <- structure(TC, names = sub("(.*)[.](.*)", "\\2.\\1", names(TC)))
stats::reshape(TC.renamed, 1:4, direction = "long", timevar = "group")


###################################################
### code chunk number 19: reshapesort
###################################################
TC.sorted <- data.frame(TC.renamed)[, sort(names(TC.renamed))]
stats::reshape(TC.sorted, 1:4, direction = "long", timevar = "group")


###################################################
### code chunk number 20: cdata
###################################################
cdata::rowrecs_to_blocks(TC, controlTable = data.frame(
  group = c("treatment", "control"),
  age = c("treatment.age", "control.age"),
  gender = c("treatment.gender", "control.gender"),
  stringsAsFactors = FALSE))


