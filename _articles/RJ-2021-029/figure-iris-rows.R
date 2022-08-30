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
    readRDS("figure-iris-rows-data.rds")),
  data.table(
    "capture groups"=3, "type conversions"=1,
    readRDS("figure-iris-rows-convert-data.rds")))

iris.timings[, seconds := time/1e9]
stats.timings <- iris.timings[, .(
  median=median(seconds),
  q25=quantile(seconds, 0.25),
  q75=quantile(seconds, 0.75)
), by=.(`capture groups`, `type conversions`, N.row, expr)]

gg <- ggplot()+
  ggtitle("Multiple reshape output columns, variable number of input rows")+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  scale_color_manual(values=pkg.color)+
  scale_fill_manual(values=pkg.color)+
  facet_grid(
    `capture groups` + `type conversions` ~ .,
    ##scales="free",
    labeller=label_both)+
  geom_line(aes(
    N.row, median, color=expr),
    data=stats.timings)+
  geom_ribbon(aes(
    N.row, ymin=q25, ymax=q75, fill=expr),
    alpha=0.2,
    data=stats.timings)+
  scale_x_log10(
    "Number of rows in wide input data table",
    limits=c(NA, max(stats.timings$N.row)*30))+
  scale_y_log10(
    "Computation time (seconds)")
dl <- directlabels::direct.label(gg, list(cex=0.75, "last.polygons"))

pdf("figure-iris-rows.pdf", 7, 3)
print(dl)
dev.off()

png("figure-iris-rows.png", 7, 3, units="in", res=100)
print(dl)
dev.off()
