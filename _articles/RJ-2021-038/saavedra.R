library("IndexNumber")

index.number.serie(ActivePeople$TotalWomen[1:15],name="Woman",opt.plot=TRUE,opt.summary = TRUE)
index.number.serie(ActivePeople$TotalMen[1:15],name="Man",opt.plot=TRUE,opt.summary = TRUE)
index.number.chain(ActivePeople$TotalWomen[1:15],name="Woman",opt.plot=TRUE,opt.summary = TRUE)
index.number.chain(ActivePeople$TotalMen[1:15],name="Man",opt.plot=TRUE,opt.summary = TRUE)

ECResources[,2:6]

aggregated.index.number(ECResources[,2:6],base="serie",type="arithmetic",name="Prices",opt.plot=TRUE,opt.summary=TRUE)
aggregated.index.number(ECResources[,2:6],base="serie",type="geometric",name="Prices",opt.plot=FALSE,opt.summary=FALSE)
aggregated.index.number(ECResources[,2:6],base="serie",type="harmonic",name="Prices",opt.plot=FALSE,opt.summary=FALSE)
aggregated.index.number(ECResources[,2:6],base="serie",type="BDutot",name="Prices",opt.plot=FALSE,opt.summary=FALSE)
aggregated.index.number(ECResources[,2:6],base="chain",type="Carli",name="Prices",opt.plot=FALSE,opt.summary=FALSE)
aggregated.index.number(ECResources[,2:6],base="chain",type="Jevons",name="Prices",opt.plot=FALSE,opt.summary=FALSE)
aggregated.index.number(ECResources[,2:6],base="chain",type="Dutot",name="Prices",opt.plot=FALSE,opt.summary=FALSE)
laspeyres.index.number(ECResources[,2:6],ECResources[,7:11],name="Price",opt.plot=TRUE,opt.summary=TRUE)
paasche.index.number(ECResources[,2:6],ECResources[,7:11],name="Price",opt.plot=TRUE,opt.summary=TRUE)
edgeworth.index.number(ECResources[,2:6],ECResources[,7:11],name="Price",opt.plot=FALSE,opt.summary=FALSE)
fisher.index.number(ECResources[,2:6],ECResources[,7:11],name="Price",opt.plot=FALSE,opt.summary=FALSE)
aggregated.index.number(ECResources[,2:6]*ECResources[,7:11],base="serie",type="BDutot",name="Prices",opt.plot=FALSE,opt.summary=FALSE)
