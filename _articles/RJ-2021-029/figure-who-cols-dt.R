source("packages-dt.R")

data.vec <- list(who="single (1)", iris="multiple (2)")
Nrow.vec <- c(who=11, iris=150)
timings.dt.list <- list()
for(data.name in names(data.vec)){
  output.value.columns <- data.vec[[data.name]]
  data.timings <- fread(paste0("figure-", data.name, "-cols-dt-data.csv"))
  timings.dt.list[[data.name]] <- data.table(
    data.name, input.rows=Nrow.vec[[data.name]],
    output.value.columns, data.timings)
}
timings.dt <- do.call(rbind, timings.dt.list)

timings.dt[, seconds := time/1e9]
stats.timings <- timings.dt[, .(
  median=median(seconds),
  q25=quantile(seconds, 0.25),
  q75=quantile(seconds, 0.75)
), by=.(data.name, input.rows, output.value.columns, N.col, expr)]

ref.dt <- data.table(
  seconds=c(60*60, 60, 1),
  unit=c("hour", "minute", "second"))[unit=="second"]
gg <- ggplot()+
  ggtitle("Variable number of input columns")+
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
    limits=c(NA, max(stats.timings$N.col)*300))+
  scale_y_log10(
    "Computation time (seconds)")+
  facet_grid(
    . ~ data.name + input.rows + output.value.columns,
    labeller=label_both)
dl <- directlabels::direct.label(gg, list(cex=0.75, "last.polygons"))
pdf("figure-who-cols-dt.pdf", 9, 4)
print(dl)
dev.off()
png("figure-who-cols-dt.png", 9, 4, units="in", res=100)
print(dl)
dev.off()

