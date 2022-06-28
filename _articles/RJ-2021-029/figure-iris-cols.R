source("packages.R")

pkg.color <- c(
  "data.table::melt"="#1B9E77",
  "reshape2::melt"="#D95F02",
  "tidyr::gather"="#7570B3",
  "tidyr::pivot_longer"="#7570B3",
  "cdata::unpivot_to_blocks"="#E7298A",
  "cdata::rowrecs_to_blocks"="#E7298A",
  "nc::capture_melt_multiple"="#66A61E",
  "stats::reshape"="#E6AB02",
  "utils::stack"="#E6AB02",
  "#A6761D", "#666666")

iris.timings <- rbind(
  data.table(
    "capture groups"=3, "type conversions"=0,
    readRDS("figure-iris-cols-data.rds")),
  data.table(
    "capture groups"=3, "type conversions"=1,
    readRDS("figure-iris-cols-convert-data.rds")))

fwrite(iris.timings, "figure-iris-cols.csv")

iris.timings <- fread("figure-iris-cols.csv")

iris.timings[, seconds := time/1e9]
stats.timings <- iris.timings[, .(
  median=median(seconds),
  q25=quantile(seconds, 0.25),
  q75=quantile(seconds, 0.75)
), by=.(`capture groups`, `type conversions`, N.col, expr)]
stats.timings[, minutes := median/60]
stats.timings[
, .SD[N.col==max(N.col), .(
  N.col, pkg=sub("::.*", "", expr), median, minutes)][order(-minutes)],
  by=`type conversions`]


stats.timings[N.col>4000, {
  fit <- lm(log10(median) ~ log10(N.col))
  w <- coef(fit)
  data.table(intercept=w[1], slope=w[2])
}, by=.(`type conversions`, expr)][order(slope)]

## Figure out where the two curves in the bottom panel intersect,
## approximately.
only.convert <- stats.timings[`type conversions` == 1]
wide.convert <- data.table::dcast(only.convert, N.col ~ expr, value.var="median")
wide.convert[, log10.cols := log10(N.col)]
wide.convert[, diff.log10.seconds := log10(`nc::capture_melt_multiple`) - log10(`tidyr::pivot_longer`)]
cross.log10.seconds <- wide.convert[, approx(
  log10.cols, diff.log10.seconds, seq(1, 5, l=100)
)][which.min(abs(y)), x]
cross.dt <- only.convert[, approx(
  log10(N.col), log10(median), cross.log10.seconds
), by=.(`capture groups`, `type conversions`, expr)]
cross.dt[, seconds := 10^y]
cross.dt[, N.col := 10^x]

cross.1 <- cross.dt[1]

lm.input <- only.convert[N.col > cross.1$N.col]
lm.input[, {
  fit <- lm(log10(median) ~ log10(N.col))
  w <- coef(fit)
  data.table(intercept=w[1], slope=w[2])
}, by=expr]
## linear model is intercept + slope * log10(median) = log10(seconds)
## 10^intercept * median^slope = seconds

## want: min_f \sum_i [ f(x_i) - y_i ]^2 such that f(x) = a x^j + b.
lm.dt <- lm.input[, {
  j <- if(expr=="tidyr::pivot_longer")2 else 1
  x.to.j <- N.col^j
  fit <- lm(median ~ x.to.j)
  w <- coef(fit)
  data.table(b=w[1], a=w[2], j)
}, by=expr]

join.dt <- lm.dt[lm.input, on=.(expr)]
join.dt[, x.to.j := N.col^j]
join.dt[, pred.seconds := a * x.to.j + b]

ref.dt <- data.table(
  seconds=c(60*60, 60, 1),
  unit=c("hour", "minute", "second"))

gg <- ggplot()+
  ggtitle("Multiple reshape output columns, variable number of input columns")+
  geom_hline(aes(
    yintercept=seconds),
    color="grey",
    data=ref.dt)+
  geom_text(aes(
    10, seconds, label=paste(1, unit)),
    data=ref.dt,
    color="grey",
    hjust=0,
    size=3,
    vjust=1.2)+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  scale_color_manual(values=pkg.color)+
  scale_fill_manual(values=pkg.color)+
  facet_grid(
    `capture groups` + `type conversions` ~ .,
    ##scales="free",
    labeller=label_both)+
  geom_line(aes(
    N.col, median, color=expr),
    data=stats.timings)+
  geom_ribbon(aes(
    N.col, ymin=q25, ymax=q75, fill=expr),
    alpha=0.2,
    data=stats.timings)+
  ## geom_line(aes(
  ##   N.col, pred.seconds, color=expr),
  ##   linetype="dashed",
  ##   data=join.dt)+
  ## geom_point(aes(
  ##   N.col, seconds, color=expr),
  ##   shape=1,
  ##   data=cross.dt)+
  scale_x_log10(
    "Number of cols in wide input data table",
    limits=c(NA, max(stats.timings$N.col)*20))+
  scale_y_log10(
    "Computation time (seconds)")
dl <- directlabels::direct.label(gg, list(cex=0.75, "last.polygons"))

pdf("figure-iris-cols.pdf", 7, 3)
print(dl)
dev.off()

png("figure-iris-cols.png", 7, 3, units="in", res=100)
print(dl)
dev.off()
