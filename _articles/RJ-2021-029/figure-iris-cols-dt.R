source("packages-dt.R")

RColorBrewer::display.brewer.all()
dput(RColorBrewer::brewer.pal(Inf, "Paired"))

#lite dark
pkg.color <- c("tidyr::pivot_longer"="black",
  "data.table::melt.new.sep"="#A6CEE3", "data.table::melt.new.pattern"="#1F78B4", #blue
  "#B2DF8A", "#33A02C", #green
  "data.table::melt.old.set"="#FB9A99", "data.table::melt.old.join"="#E31A1C", #red
  "#FDBF6F", "#FF7F00", #orange
  "#CAB2D6", "data.table::melt.new.var_tab"="#6A3D9A", #purple
  "#FFFF99", "#B15928") #yellow/brown

iris.timings[, seconds := time/1e9]
stats.timings <- iris.timings[, .(
  median=median(seconds),
  q25=quantile(seconds, 0.25),
  q75=quantile(seconds, 0.75)
), by=.(N.col, expr)]
stats.timings[, minutes := median/60]
stats.timings[
, .SD[N.col==max(N.col), .(
  N.col, pkg=sub("::.*", "", expr), median, minutes)][order(-minutes)],
]


stats.timings[N.col>4000, {
  fit <- lm(log10(median) ~ log10(N.col))
  w <- coef(fit)
  data.table(intercept=w[1], slope=w[2])
}, by=.(expr)][order(slope)]

ref.dt <- data.table(
  seconds=c(60*60, 60, 1),
  unit=c("hour", "minute", "second"))[unit=="second"]

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
  ## scale_color_manual(values=pkg.color)+
  ## scale_fill_manual(values=pkg.color)+
  geom_line(aes(
    N.col, median, color=expr),
    data=stats.timings)+
  geom_ribbon(aes(
    N.col, ymin=q25, ymax=q75, fill=expr),
    alpha=0.2,
    data=stats.timings)+
  scale_x_log10(
    "Number of cols in wide input data table",
    limits=c(NA, max(stats.timings$N.col)*20))+
  scale_y_log10(
    "Computation time (seconds)")
dl <- directlabels::direct.label(gg, list(cex=0.75, "last.polygons"))

pdf("figure-iris-cols-dt.pdf", 7, 3)
print(dl)
dev.off()

png("figure-iris-cols-dt.png", 7, 3, units="in", res=100)
print(dl)
dev.off()

