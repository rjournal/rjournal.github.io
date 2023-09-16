library(markovMSM)

data("colonMSM")
db_wide <- colonMSM
head(db_wide[c(1:2,16,21),1:11])


positions <- list(c(2, 3), c(3), c())
state.names <- c("Alive", "Rec", "Death")
tmat <- transMatMSM(positions, state.names)
tmat

timesNames <- c(NA, "time1", "Stime")
status <- c(NA, "event1", "event")
trans <- tmat
db_long <- prepMSM(data = db_wide, trans, timesNames, status)
db_long[1:10,]

eventsMSM(db_long)

res <- PHM.test(data = db_long, from = 2, to = 3)
res

set.seed(1234)
res2 <- AUC.test(data = db_long, times = 180, from = 1, to = 3, type = 'local',
                   replicas = 100, tmat = tmat)

res2$localTest


set.seed(1234)
res3 <- AUC.test(data = db_long, times = 180, from = 2, to = 3, type = 'local',
                   replicas = 100, tmat = tmat)
res3$localTest


plot(res2, to = 2, axis.scale = c(0,0.25), difP = FALSE) #Figure 2 (Left column)
plot(res3, to = 3, axis.scale = c(0,1), difP = FALSE) # Figure 2 (Right column)


plot(res2, to = 2, axis.scale = c(-0.03,0.03), difP = TRUE) #Figure 3 (Left column)
plot(res3, to = 3, axis.scale = c(-0.30,0.10), difP = TRUE) #Figure 3 (Right column)


set.seed(1234)
res4 <- AUC.test(data = db_long, from = 1, to = 3, type = 'global', replicas = 100,
                   tmat = tmat)

round(res4$globalTest,3)


set.seed(1234)
res5 <- update(res4, from = 2)
round(res5$globalTest,3)


round(res4$localTest,3)
round(res5$localTest,3)


plot(res4, quantileOrder = 3, to = 2, axis.scale = c(-0.04, 0.02))
#Figure 4 (Left column)

plot(res5, quantileOrder = 3, to = 3, axis.scale = c(-0.20, 0.10))
#Figure 4 (Right column)

db_wide_obs <- db_wide[db_wide$rx == 'Obs',]
db_long_obs <- prepMSM(data = db_wide_obs, trans, timesNames, status)

set.seed(12345)
res4.obs <- AUC.test(data = db_long_obs, times = 365, from = 1, to = 3,
                       type='local', replicas = 100, tmat = tmat)
res4.obs$localTest


set.seed(12345)
res5.obs <- AUC.test(data = db_long_obs, times = 365, from = 2, to = 3,
                       type = 'local', replicas=100, tmat = tmat)
res5.obs$localTest


set.seed(1234)
res6 <- LR.test(data = db_long, times = 180, from = 2, to = 3, replicas = 1000)
res6$globalTestLR


data("ebmt4")
db_wide <- ebmt4
positions <- list(c(2, 3, 5, 6), c(4, 5, 6), c(4, 5, 6),
                    c(5, 6), c(), c())
state.names <- c("Tx", "Rec", "AE", "Rec+AE", "Rel", "Death")
tmat <- transMatMSM(positions, state.names)
timesNames <- c(NA, "rec", "ae","recae", "rel", "srv")
status <- c(NA, "rec.s", "ae.s", "recae.s","rel.s", "srv.s")
trans <- tmat
db_long <- prepMSM(data = db_wide, trans, timesNames, status)
db_long[1:10,]


set.seed(1234)
res7 <- AUC.test(data = db_long, from = 1, to = 5, type = 'global',
                   quantiles = c(0.05, 0.10, 0.20, 0.30, 0.40),
                   tmat = tmat, replicas = 100,
                   positions = positions, namesStates = state.names,
                   timesNames = timesNames, status = status)

round(res7$globalTest, 4)

round(res7$localTests,4)