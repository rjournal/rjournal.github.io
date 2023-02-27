# paper script
library(areal)
library(populR)
library(sf)
library(dplyr)
library(classInt)
library(RColorBrewer)
library(maptools)

# load data
data("trg", package = "populR")
data("src", package = "populR")

# plot study area
png(file = "figs/study_area.png", width = 1000, height = 700)
plot(src['geometry'], col = "#634B56", border = NA)
plot(trg['geometry'], col = "#FD8D3C", add = T)
dev.off()

nrow(trg)
nrow(src)
sum(src$pop)

# populR results
awi <- pp_estimate(target = trg, source = src, spop = pop, sid = sid, method = awi)
vwi <- pp_estimate(target = trg, source = src, spop = pop, sid = sid, volume = floors, method = vwi)
sum(awi$pp_est)
sum(vwi$pp_est)

# areal results
aws <- aw_interpolate(trg, tid = tid, source = src, sid = 'sid', weight = 'sum', output = 'sf', extensive = 'pop')
awt <- aw_interpolate(trg, tid = tid, source = src, sid = 'sid', weight = 'total', output = 'sf', extensive = 'pop')
colnames(aws)[colnames(aws) == 'pop'] <- 'pp_est'
colnames(awt)[colnames(awt) == 'pop'] <- 'pp_est'
sum(aws$pp_est)
sum(awt$pp_est)

# sf results
sf <- st_interpolate_aw(src['pop'], trg, extensive = TRUE)
colnames(sf)[colnames(sf) == 'pop'] <- 'pp_est'
sum(sf$pp_est)

# plot results
n <- 6
pal <- brewer.pal(n, "OrRd")
brks <- c(0, 1, 2, 4, 10, 40, 86)

myList <- list(awi = awi, vwi = vwi, aws = aws, awt = awt, sf = sf)

for (i in 1:length(myList)) {
  png(file = sprintf('figs/map_%s.png', names(myList)[i]), width = 1000, height = 700)
  plot(myList[[i]]['pp_est'], col = pal[findInterval(myList[[i]]$pp_est, brks, all.inside = T)], main = names(myList)[i])
  legend('bottomright', legend=rev(leglabs(round(brks))),  title= "Legend", fill=rev(pal))
  dev.off()
}

# reference data
png(file = 'figs/map_rf.png', width = 1000, height = 700)
plot(awi['rf'], col = pal[findInterval(awi$rf, brks, all.inside = T)], main = 'rf')
legend('bottomright', legend=rev(leglabs(round(brks))),  title= "Legend", fill=rev(pal))
dev.off()

# comparison using source
# src_awi <- awi %>%
#   group_by(sid) %>%
#   summarise(est = sum(pp_est), act = unique(pop))
#
# src_awi_compare <- pp_compare(src_awi, estimated = est, actual = act, title = "awi vs source")
# src_awi_compare
#
# src_vwi <- vwi %>%
#   group_by(sid) %>%
#   summarise(est = sum(pp_est), act = unique(pop))
#
# src_vwi_compare <- pp_compare(src_vwi, estimated = est, actual = act, title = "vwi vs source")
# src_vwi_compare

# evaluation using ijagr population
# first sort ascending populR results
awi <- awi[order(awi$tid),]
vwi <- vwi[order(vwi$tid),]
data <- data.frame(
  rf = awi$rf,
  awi = awi$pp_est,
  vwi = vwi$pp_est,
  aws = aws$pp_est,
  awt = awt$pp_est,
  sf = sf$pp_est)

data[1:10,]

awi_error <- pp_compare(data, estimated = awi, actual = rf, title = "awi vs rf")
awi_error

vwi_error <- pp_compare(data, estimated = vwi, actual = rf, title = "vwi vs rf")
vwi_error

sf_error <- pp_compare(data, estimated = sf, actual = rf, title = "sf vs rf")
sf_error

awt_error <- pp_compare(data, estimated = awt, actual = rf, title = "awt vs rf")
awt_error

aws_error <- pp_compare(data, estimated = aws, actual = rf, title = "aws vs rf")
aws_error

# point the importance of the rounding function
# awi <- pp_round(awi, tpop = pp_est, spop = pop, sid = sid)
# vwi <- pp_round(vwi, tpop = pp_est, spop = pop, sid = sid)

# simple round-off to the closest integer
# awi$simple_round <- round(awi$pp_est, 0)
# vwi$simple_round <- round(vwi$pp_est, 0)

# src_awi_rd <- awi %>%
#   group_by(sid) %>%
#   summarise(rd = sum(pp_est), srd = sum(simple_round), pop = unique(pop))
#
# src_vwi_rd <- vwi %>%
#   group_by(sid) %>%
#   summarise(rd = sum(pp_est), srd = sum(simple_round), pop = unique(pop))
#
# sum(src_awi_rd$rd)
# sum(src_awi_rd$srd)
# sum(src_vwi_rd$rd)
# sum(src_vwi_rd$srd)
#
# src_awi_rd_error <- pp_compare(src_awi_rd, estimated = rd, actual = pop, 'pp_round vs source - awi')
# src_awi_rd_error
#
# src_awi_srd_error <- pp_compare(src_awi_rd, estimated = srd, actual = pop, 'simple round vs source - awi')
# src_awi_srd_error
#
#
# src_vwi_rd_error <- pp_compare(src_vwi_rd, estimated = rd, actual = pop, 'pp_round vs source - vwi')
# src_vwi_rd_error
#
# src_vwi_srd_error <- pp_compare(src_vwi_rd, estimated = srd, actual = pop, 'simple round vs source - vwi')
# src_vwi_srd_error


# performance comparisons
# read external data
s <- st_read('./data/source/chios_pop_OT.shp')

t <- st_read('./data/target/chios_build.shp')

# convert pop into numeric
s$pop <- as.numeric(s$pop)

# create tid
t$tid <- 1:nrow(t)

# create a random floors col for vwi
t$floors <- sample(1:8, nrow(t), replace = T)

# keep only useful cols
t <- t[, c('tid', 'floors')]

library(microbenchmark)

# check on performance using built-in data
built_in <- microbenchmark(
  suppressWarnings(pp_estimate(target = trg, source = src, spop = pop, sid = sid,
                               method = awi)),
  suppressWarnings(pp_estimate(target = trg, source = src, spop = pop, sid = sid,
                               volume = floors, method = vwi)),
  aw_interpolate(trg, tid = tid, source = src, sid = 'sid',
                 weight = 'sum', output = 'sf', extensive = 'pop'),
  aw_interpolate(trg, tid = tid, source = src, sid = 'sid',
                 weight = 'total', output = 'sf', extensive = 'pop'),
  suppressWarnings(st_interpolate_aw(src['pop'], trg, extensive = TRUE))
)

# check on performance using external data
chios <- microbenchmark(
  suppressWarnings(pp_estimate(target = t, source = s, spop = pop, sid = ESYECODE,
                               method = awi)),
  suppressWarnings(pp_estimate(target = t, source = s, spop = pop, sid = ESYECODE,
                               volume = floors, method = vwi)),
  aw_interpolate(t, tid = tid, source = s, sid = 'ESYECODE',
                 weight = 'sum', output = 'sf', extensive = 'pop'),
  aw_interpolate(t, tid = tid, source = s, sid = 'ESYECODE',
                 weight = 'total', output = 'sf', extensive = 'pop'),
  suppressWarnings(st_interpolate_aw(s['pop'], t, extensive = TRUE))
)
