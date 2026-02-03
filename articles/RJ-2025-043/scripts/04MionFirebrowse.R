#04MionFirebrowse.R
Comment="04MionFirebrowse.R"
library(ps)

# Get the PIDs of the workers in a PSOCK cluster
cluster_pids <- function(cl) {
  as.integer(unlist(clusterCall(cl, Sys.getpid)))
}

# Sum RSS (MB) for a vector of PIDs; optionally include the master process
total_rss_mb <- function(pids, include_master = TRUE) {
  rss_pid <- function(pid) {
    h <- ps_handle(pid)
    as.numeric(ps_memory_info(h)["rss"]) / (1024^2)
  }
  # sum worker RSS; skip PIDs that may have exited
  worker_sum <- sum(vapply(pids, function(pid) {
    tryCatch(rss_pid(pid), error = function(e) 0)
  }, numeric(1)))
  
  if (include_master) {
    master <- as.numeric(ps_memory_info()["rss"]) / (1024^2)
    worker_sum + master
  } else {
    worker_sum
  }
}
#set path to zenodo downloaded data 
path <- file.path(getwd(),"01Transformierte")
#user another csv reader
#for example
#read.table(file = file.path(dir,"data/12Genexpession_Firebrowse_d19637_N10446".lrn"),header = T,sep = "\t",skip = 5)

V <- ReadLRN("12Genexpession_Firebrowse_d19637_N10446",path)
Key=V$Key
Data=V$Data
Header=V$Header
#user another csv reader
V2 <- ReadCLS("14Genexpression_Firebrowse_Cls_N10446",path)
ClsKey <- V2$Key
Cls <- V2$Cls
TheSameKey(Key,ClsKey)
#FCPS::ClusterCount(Cls)
library(parallel)
library(memshare)
cl <- makeCluster(detectCores()-1)
namespace  <-  "mutual_info"

pids <- cluster_pids(cl)
mem_idle<- total_rss_mb(pids, include_master = TRUE)
start <- Sys.time()
mi_vals <- memshare::memApply(
  CLUSTER = cl,
  X = Data,
  MARGIN  = 2,
  FUN = function(x,y) {
    cc <- memshare::mutualinfo(x,y,isYDiscrete = T,na.rm = T,useMPMI = F)
    return(cc)
  },
  VARS = list(y=Cls),
  NAMESPACE = namespace)

mem_at_end <- total_rss_mb(pids, include_master = TRUE)
atend <- Sys.time()
mi_vals_vec <- unlist(mi_vals)
atend-start
Comments <- paste("TimeDiff in hours:",atend-start,"MemDiff in MB:",mem_at_end-mem_idle,"mem_idle:",mem_idle,"mem_at_end:",mem_at_end,"TimeStart:",start,"TimeEnd:",atend,Comment)

# not accessible externally
#WriteLRN("MI_values",Data =mi_vals_vec,Key = as.numeric(gsub("C","",Header)),Comments =  Comments,Header =c("MI"),OutDirectory = RelPath(1,"05OurPublication/data"))
