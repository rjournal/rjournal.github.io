source("packages.R")

pkg.color <- c(
  "data.table::melt"="#1B9E77",
  "reshape2::melt"="#D95F02",
  "tidyr::gather"="#7570B3",
  "tidyr::pivot_longer"="#7570B3",
  "cdata::unpivot_to_blocks"="#E7298A",
  "cdata::rowrecs_to_blocks"="#E7298A",
  "nc::capture_melt_single"="#66A61E",
  "stats::reshape"="#E6AB02",
  "utils::stack"="#E6AB02",
  "#A6761D", "#666666")
complex <- readRDS("figure-who-complex-cols-data.rds")
simple <- readRDS("figure-who-cols-data.rds")
who.timings <- rbind(
  data.table("capture groups"=3, "type conversions"=0, simple[, names(complex), with=FALSE]),
  data.table("capture groups"=5, "type conversions"=3, complex))

who.timings[, seconds := time/1e9]
stats.timings <- who.timings[, .(
  median=median(seconds),
  q25=quantile(seconds, 0.25),
  q75=quantile(seconds, 0.75),
  mean=mean(seconds),
  sd=sd(seconds)
), by=.(`capture groups`, `type conversions`, N.col, expr)]

stats.timings[`capture groups`==3][N.col==max(N.col), .(N.col, expr, median)][order(median)]
stats.timings[`capture groups`==5][N.col==max(N.col), .(N.col, expr, mean, sd)][order(-mean)]


gg <- ggplot()+
  ggtitle("Single reshape output column, variable number of input columns")+
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
  scale_x_log10(
    "Number of columns to reshape in wide input data table",
    breaks=10^seq(1, 4, by=1),
    limits=c(NA, max(stats.timings$N.col)*3))+
  scale_y_log10("Computation time (seconds)")
dl <- directlabels::direct.label(gg, list(cex=0.75, "last.polygons"))

pdf("figure-who-both-cols.pdf", 7, 3)
print(dl)
dev.off()

png("figure-who-both-cols.png", 7, 3, units="in", res=100)
print(dl)
dev.off()
