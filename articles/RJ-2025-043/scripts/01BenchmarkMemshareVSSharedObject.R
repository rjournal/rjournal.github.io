#01BenchmarkMemshareVSSharedObject.R
comment="01BenchmarkMemshareVSSharedObject.R"
if(R.Version()$os=="darwin20"){
  path <- file.path(getwd(),"01Transformierte/mac")
}else{
  path <- file.path(getwd(),"01Transformierte/win")
}    
library(ps)

# Get the PIDs of the workers in a PSOCK cluster
cluster_pids = function(cl) {
  as.integer(unlist(clusterCall(cl, Sys.getpid)))
}

# Sum RSS (MB) for a vector of PIDs; optionally include the master process
total_rss_mb = function(pids, include_master = TRUE) {
  rss_pid = function(pid) {
    h = ps_handle(pid)
    as.numeric(ps_memory_info(h)["rss"]) / (1024^2)
  }
  # sum worker RSS; skip PIDs that may have exited
  worker_sum = sum(vapply(pids, function(pid) {
    tryCatch(rss_pid(pid), error = function(e) 0)
  }, numeric(1)))
  
  if (include_master) {
    master = as.numeric(ps_memory_info()["rss"]) / (1024^2)
    worker_sum + master
  } else {
    worker_sum
  }
}

nearest_square <- function(i) {
  if(i%%1==0){
    A <- matrix(runif(round(10^(i+1))), nrow = 10^i, ncol = 10^i)
  }else{
    n <- as.double(round(10^i))
    A <- matrix(runif(round(n*n)), nrow = n, ncol = n)
  }
  return(A)
}

library(parallel)
# For comparison purpose, fix the number of clusters to 32
set.seed(1234)
Ivec <- c(1,2,3,4,4.2,4.5,4.7,5)
MaxIt <- 100
##SharedObject ----
cl <- makeCluster(32)
pids <- cluster_pids(cl)
library(SharedObject)
mem_idle_SharedObject <- total_rss_mb(pids, include_master = TRUE)

SharedObjectPerformance <- c()

for(i in Ivec){
  if(i>1){
    save(file=file.path(path,"SharedObjectPerformance_v2.rda"),
         SharedObjectPerformance,comment,mem_idle_SharedObject)
  }
  Start <- c()
  Ende <- c()
  print(i)
  mem_before_call <- c()
  mem_after_call <- c()
  p <- txtProgressBar(min = 0, max = MaxIt, style = 3)
  for(k in 1:MaxIt){
      setTxtProgressBar(p, k)
      mem_before_call[k]  <-  total_rss_mb(pids, include_master = TRUE)
      A1 <- nearest_square(i)
      x1 <- Sys.time()
      A2  <-  share(A1)
      clusterExport(cl, varlist = "A2", envir = environment())
      res <- clusterApply(cl, 1:ncol(A2), function(col_index) {
        sd(A2[, col_index])
      })
      Start[k] <- x1
      freeSharedMemory(listSharedObjects())
      x2 <- Sys.time()
      Ende[k] <- x2
      mem_after_call[k]  <-  total_rss_mb(pids, include_master = TRUE)
      # Cleanup
      clusterEvalQ(cl, {
       rm(A2)
       gc()
      })
      rm(A1)
      gc() 
      save(file=file.path(path,paste0("SharedObjectPerformance_v2_part",i,".rda")),
           mem_before_call,mem_after_call,Start,Ende,comment)
  }
  #temporary save ----
  mem_at_end_SharedObject  <-  total_rss_mb(pids, include_master = TRUE)
  
  SharedObjectPerformance[[as.character(i)]] <- cbind(DiffTime=Ende-Start,Start,Ende,mem_before_call,mem_after_call)
  
  save(file=file.path(path,"SharedObjectPerformance_v2.rda"),
       SharedObjectPerformance,comment,mem_idle_SharedObject,mem_at_end_SharedObject)
  
}
mem_at_end_SharedObject  <-  total_rss_mb(pids, include_master = TRUE)
stopCluster(cl)
save(file=file.path(path,"SharedObjectPerformance_v3.rda"),
     SharedObjectPerformance,comment,mem_idle_SharedObject,mem_at_end_SharedObject)

listSharedObjects()

##memshare ----
cl <- makeCluster(32)
pids  <-  cluster_pids(cl)
library(memshare)
mem_idle_memshare  <-  total_rss_mb(pids, include_master = TRUE)
memsharePerformance <- c()
for(i in  Ivec){
  Start <- c()
  Ende <- c()
  mem_before_call <- c()
  mem_after_call <- c()
  print(i)
  p <- txtProgressBar(min = 0, max = MaxIt, style = 3)
  for(k in 1:MaxIt){
    setTxtProgressBar(p, k)
    mem_before_call[k]  <-  total_rss_mb(pids, include_master = TRUE)
    A1  <-  nearest_square(i)
    x1 <- Sys.time()
    res  <-  memshare::memApply(X = A1, MARGIN = 2, FUN = function(x) {
      return(sd(x))
    }, CLUSTER=cl, NAMESPACE="namespace_id")
    #memshare::releaseViews("my_namespace",A1)
    x2 <- Sys.time()
    Start[k] <- x1
    Ende[k] <- x2
    mem_after_call[k]  <-  total_rss_mb(pids, include_master = TRUE)
    rm(A1)
    gc() 
  }
  memsharePerformance[[as.character(i)]]=cbind(DiffTime=Ende-Start,Start,Ende,mem_before_call,mem_after_call)
save(file=file.path(path,"memsharePerformance.rda"),
     memsharePerformance,comment,mem_idle_memshare)

}
mem_at_end_memshare  <-  total_rss_mb(pids, include_master = TRUE)

save(file=file.path(path,"memsharePerformance.rda"),
     memsharePerformance,comment,mem_idle_memshare,mem_at_end_memshare)
stopCluster(cl)
##baseline ----
 # 0.25-steps for better line interpolation for small matrices
Ivec  <-  c(seq(from = 1, to = 4, by = 0.25),4.2,4.5,4.7,5)  
mem_idle_base  <-  total_rss_mb(pids, include_master = TRUE)
BaselinePerformance  <-  list()
It <- 1
for(i in Ivec){
  print(paste("Exponent:", i))
  
  Start  <-  c()
  Ende   <-  c()
  mem_before_call  <-  c()
  mem_after_call   <-  c()
  
  for(k in 1:It){
 
    
    mem_before_call[k]  <-  total_rss_mb(pids, include_master = TRUE)
    
    x1  <-  Sys.time()
    A1  <-  nearest_square(i)
    res  <-  apply(A1, 2, sd)
    x2  <-  Sys.time()
    
    Start[k]  <-  x1
    Ende[k]   <-  x2
    mem_after_call[k]   <-  total_rss_mb(pids, include_master = TRUE)
    
    rm(A1)
    gc()
  }
  BaselinePerformance[[as.character(i)]]  <-  cbind(DiffTime = Ende - Start,
          Start, Ende,
          mem_before_call, mem_after_call)
}
mem_at_end_base  <-  total_rss_mb(pids, include_master = TRUE)

save(file = file.path(path, "BaselinePerformance.rda"),
     BaselinePerformance, comment, mem_idle_base, mem_at_end_base)

#parallel Baseline
cl <- makeCluster(32)
Ivec  <-  c(seq(from = 1, to = 4, by = 0.25),4.2,4.5,4.7)  
mem_idle_base  <-  total_rss_mb(pids, include_master = TRUE)
BaselinePerformanceParallel  <-  list()
It <- 1
for(i in Ivec){
  print(paste("Exponent:", i))
  
  Start  <-  c()
  Ende   <-  c()
  mem_before_call  <-  c()
  mem_after_call   <-  c()
  
  for(k in 1:It){
    
    
    mem_before_call[k]  <-  total_rss_mb(pids, include_master = TRUE)
    
    x1  <-  Sys.time()
    A1  <-  nearest_square(i)
    res  <-  parApply(cl,A1, 2, sd)
    x2  <-  Sys.time()
    
    Start[k]  <-  x1
    Ende[k]   <-  x2
    mem_after_call[k]   <-  total_rss_mb(pids, include_master = TRUE)
    
    rm(A1)
    gc()
  }
  BaselinePerformanceParallel[[as.character(i)]]  <-  cbind(DiffTime = Ende - Start,
                                                 Start, Ende,
                                                 mem_before_call, mem_after_call)
}
mem_at_end_base  <-  total_rss_mb(pids, include_master = TRUE)

save(file = file.path(path, "BaselinePerformanceParallel.rda"),
     BaselinePerformanceParallel, comment, mem_idle_base, mem_at_end_base)
stopCluster(cl)