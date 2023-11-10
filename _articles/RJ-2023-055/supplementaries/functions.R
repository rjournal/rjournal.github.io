

# Created 17.11.2021

# Functions to extract data

#---------------------------------------------------------------------------------


################################################################################
# Function for extracting data for 2020 (yearly total)
################################################################################

get2020data = function(post.samples = pois.samples.list, geo.res, link_table=NULL, stratify.by){
  
  # geo.res = c("province","region","country)
  # stratify.by = c("none","age","sex","agesex")   
  # link_table is a data frame with NAMNUTS2 (name NUTS2 region) and ID_space (ID of the NUTS3 region as given in the finaldb)
  # and NAMNUTS2 (the NUTS2 region ID)

  # this line is in case we want to exclude 40< that have the lowest predictive ability
  if(length(post.samples) == 8){
    groups4cv <- as.data.frame(expand.grid(age = c("40-59", "60-69", "70-79", "80+"),
                                           sex = c("F", "M")))
  }
  
  if(length(post.samples) == 10){
    groups4cv <- as.data.frame(expand.grid(age = c("40<", "40-59", "60-69", "70-79", "80+"),
                                           sex = c("F", "M")))
  }
  
  # Indexes for 2020 observations
  sub2020 = startsWith(post.samples[[1]]$EURO_LABEL, "2020")
  post.samples = lapply(post.samples, function(X) X[sub2020,])
  
  # For each i in 1:10 extract only the necessary data (1000 simulations + observed data)
  if(geo.res == "province"){
    lapply(post.samples, function(X){
      
      X <- select(X, starts_with("V"), "ID_space") %>%
        group_by(ID_space) %>%
        summarise_all(sum)
      return(X)
    }) -> list.sum.deaths
    
    lapply(post.samples, function(X){
      X <- data.frame(ID_space = X$ID_space, observed = X$deaths)
      Y <- X %>% 
        group_by(ID_space) %>% 
        summarise(observed = sum(observed))
      return(Y)
    }) -> list.observed
    
    lapply(post.samples, function(X){
      X <- data.frame(ID_space = X$ID_space, population = X$population, year = X$year)
      Y <- X %>% 
        group_by(ID_space) %>% 
        summarise(population = mean(population)) 
      return(Y)
    }) -> list.population
  }
  
  # For each i in 1:10 aggregate predicted/observed values by region 
  if(geo.res == "region"){
    lapply(post.samples, function(X){
      
      X <- select(X, starts_with("V"), "ID_space")
      X <- left_join(X, link_table, by = c("ID_space"))

      X %>% 
        select(starts_with("V"), "NAMNUTS2") %>% 
        group_by(NAMNUTS2) %>%
        summarise_all(sum) %>% 
        ungroup()-> X
      X <- as.data.frame(X)
      X[,-1] <- apply(X[,-1], 2, as.numeric)
      
      return(X)
      
    }) -> list.sum.deaths
    
    
    lapply(post.samples, function(X){
      X <- data.frame(ID_space = X$ID_space, observed = X$deaths)
      X <- left_join(X, link_table, by = c("ID_space"))
      
      X %>% group_by(NAMNUTS2) %>% summarise(observed = sum(observed)) -> Y
      Y$observed <- as.numeric(Y$observed)
      return(Y)
    }) -> list.observed
    
    lapply(post.samples, function(X){
      X <- data.frame(ID_space = X$ID_space, population = X$population)
      X <- left_join(X, link_table, by = c("ID_space"))
      Y <- X %>% 
        group_by(NAMNUTS2) %>% 
        summarise(population = mean(population))
      return(Y)
    }) -> list.population
    
  }
  
  # For each i in 1:10 aggregate predicted/observed values to get the total for the country
  if(geo.res == "country"){
    lapply(post.samples, function(X){
      X %>% 
        dplyr::select(starts_with("V")) %>% 
        summarise_all(sum) -> X
      X <- as.data.frame(X)
      X$COUNTRY = "Italy"
      return(X)
    }) -> list.sum.deaths
    
    lapply(post.samples, function(X){
      X %>% summarise(observed = sum(deaths)) -> Y
      Y$COUNTRY = "Italy"
      return(Y)
    }) -> list.observed
    
    lapply(post.samples, function(X){
      X %>% group_by(ID_space) %>% summarise(population = mean(population)) -> Y
      Y %>% summarise(population = sum(population)) -> Y
      Y$COUNTRY = "Italy"
      return(Y)
    }) -> list.population
  }
  
  # Aggregate by age and sex  
  if(stratify.by == "none"){
    
    sum.deaths <- Reduce("+", lapply(list.sum.deaths,
                                     function(X) select(X, starts_with("V"))))
    if(geo.res=="province") sum.deaths$ID_space <- list.sum.deaths[[1]]$ID_space
    if(geo.res=="region") sum.deaths$NAMNUTS2 <- list.sum.deaths[[1]]$NAMNUTS2
    if(geo.res=="country") sum.deaths$COUNTRY <- "Italy"
    
    
    sum.observed <- Reduce("+", lapply(list.observed,
                                       function(X) select(X, "observed")))
    if(geo.res=="province") sum.observed$ID_space <- list.observed[[1]]$ID_space
    if(geo.res=="region") sum.observed$NAMNUTS2 <- list.observed[[1]]$NAMNUTS2
    if(geo.res=="country") sum.observed$COUNTRY <- "Italy"
    
    sum.population <- Reduce("+", lapply(list.population,
                                         function(X) select(X, "population")))
    if(geo.res=="province") sum.population$ID_space <- list.population[[1]]$ID_space
    if(geo.res=="region") sum.population$NAMNUTS2 <- list.population[[1]]$NAMNUTS2
    if(geo.res=="country") sum.population$COUNTRY <- "Italy"
    
    #sum.deaths.obs = left_join(sum.deaths, sum.observed) #memory exhausted
    sum.observed <- left_join(sum.observed, sum.population)
    sum.deaths.obs = data.frame(sum.deaths,sum.observed)
    if(sum(endsWith(colnames(sum.deaths.obs),".1")) != 0){
      sum.deaths.obs = sum.deaths.obs %>% select(-ends_with(".1"))
    }
    out = sum.deaths.obs
    
    return(out)
  }
  
  
  # Aggregate by age 
  if(stratify.by == "sex"){
    
    out = list()
    for(i in 1:n_distinct(groups4cv$sex)){
      sum.deaths <- Reduce("+", lapply(list.sum.deaths[groups4cv$sex == levels(groups4cv$sex)[i]],
                                       function(X) select(X, starts_with("V"))))
      if(geo.res=="province") sum.deaths$ID_space <- list.sum.deaths[[1]]$ID_space
      if(geo.res=="region") sum.deaths$NAMNUTS2 <- list.sum.deaths[[1]]$NAMNUTS2
      if(geo.res=="country") sum.deaths$COUNTRY <- "Italy"
      
      
      sum.observed <- Reduce("+", lapply(list.observed[groups4cv$sex == levels(groups4cv$sex)[i]],
                                         function(X) select(X, "observed")))
      if(geo.res=="province") sum.observed$ID_space <- list.observed[[1]]$ID_space
      if(geo.res=="region") sum.observed$NAMNUTS2 <- list.observed[[1]]$NAMNUTS2
      if(geo.res=="country") sum.observed$COUNTRY <- "Italy"
      
      sum.population <- Reduce("+", lapply(list.population[groups4cv$sex == levels(groups4cv$sex)[i]],
                                           function(X) select(X, "population")))
      if(geo.res=="province") sum.population$ID_space <- list.population[[1]]$ID_space
      if(geo.res=="region") sum.population$NAMNUTS2 <- list.population[[1]]$NAMNUTS2
      if(geo.res=="country") sum.population$COUNTRY <- "Italy"
      
      sum.observed <- left_join(sum.observed, sum.population)
      #sum.deaths.obs = left_join(sum.deaths, sum.observed) #memory exhausted
      sum.deaths.obs = data.frame(sum.deaths,sum.observed)
      
      if(sum(endsWith(colnames(sum.deaths.obs),".1")) != 0){
        sum.deaths.obs = sum.deaths.obs %>% select(-ends_with(".1"))
      }
      out[[i]] = sum.deaths.obs
      
    }
    names(out) = levels(groups4cv$sex)
    
    return(out)
  }
  
  # Aggregate by sex 
  if(stratify.by == "age"){
    
    out = list()
    for(i in 1:n_distinct(groups4cv$age)){
      sum.deaths <- Reduce("+", lapply(list.sum.deaths[groups4cv$age == levels(groups4cv$age)[i]],
                                       function(X) select(X, starts_with("V"))))
      if(geo.res=="province") sum.deaths$ID_space <- list.sum.deaths[[1]]$ID_space
      if(geo.res=="region") sum.deaths$NAMNUTS2 <- list.sum.deaths[[1]]$NAMNUTS2
      if(geo.res=="country") sum.deaths$COUNTRY <- "Italy"
      
      
      sum.observed <- Reduce("+", lapply(list.observed[groups4cv$age == levels(groups4cv$age)[i]],
                                         function(X) select(X, "observed")))
      if(geo.res=="province") sum.observed$ID_space <- list.observed[[1]]$ID_space
      if(geo.res=="region") sum.observed$NAMNUTS2 <- list.observed[[1]]$NAMNUTS2
      if(geo.res=="country") sum.observed$COUNTRY <- "Italy"
      
      
      sum.population <- Reduce("+", lapply(list.population[groups4cv$age == levels(groups4cv$age)[i]],
                                           function(X) select(X, "population")))
      if(geo.res=="province") sum.population$ID_space <- list.population[[1]]$ID_space
      if(geo.res=="region") sum.population$NAMNUTS2 <- list.population[[1]]$NAMNUTS2
      if(geo.res=="country") sum.population$COUNTRY <- "Italy"
      
      #sum.deaths.obs = left_join(sum.deaths, sum.observed)
      sum.observed <- left_join(sum.observed, sum.population)
      sum.deaths.obs = data.frame(sum.deaths, sum.observed)
      if(sum(endsWith(colnames(sum.deaths.obs),".1")) != 0){
        sum.deaths.obs = sum.deaths.obs %>% select(-ends_with(".1"))
      }
      out[[i]] = sum.deaths.obs
    }
    names(out) = levels(groups4cv$age)
    
    return(out)
  }
  
  #  don't aggregate
  if(stratify.by == "agesex"){
    
    #out = Map(dplyr::left_join, list.sum.deaths, list.observed)
    list.obs <- list()
    
    for(i in 1:length(post.samples)){
      list.obs[[i]] <- left_join(list.observed[[i]], list.population[[i]])
    }
    
    out = Map(data.frame, list.sum.deaths, list.obs)
    out = lapply(out,function(X) {X=X %>% select(-ends_with(".1"))})
    
    names(out) = paste0(groups4cv$sex,groups4cv$age)
    return(out)
  }
  
}



################################################################################
# Function for extracting data for 2020 (weekly total)
################################################################################

get2020weeklydata = function(post.samples = pois.samples.list, geo.res, link_table=NULL, stratify.by){
  
  # geo.res = c("province","region","country)
  # stratify.by = c("none","age","sex","agesex")   
  # link_table is a data frame with ID_space (province code) and NAMNUTS2 (region name). required only if geo.res="region"
  # groups4cv is the data frame with the 10 combinations of age x sex groups
  
  # this line is in case we want to exclude 40< that have the lowest predictive ability
  if(length(post.samples) == 8){
    groups4cv <- as.data.frame(expand.grid(age = c("40-59", "60-69", "70-79", "80+"),
                                           sex = c("F", "M")))
  }
  
  if(length(post.samples) == 10){
    groups4cv <- as.data.frame(expand.grid(age = c("40<", "40-59", "60-69", "70-79", "80+"),
                                           sex = c("F", "M")))
  }
  
  
  # Indexes for 2020 observations
  sub2020 = startsWith(post.samples[[1]]$EURO_LABEL, "2020")
  post.samples = lapply(post.samples, function(X) X[sub2020,])
  
  
  # For each i in 1:10 extract only the necessary data (1000 simulations + observed data)
  if(geo.res == "province"){
    lapply(post.samples, function(X){
      X <- select(X, starts_with("V"), "ID_space", "EURO_LABEL")
      return(X)
    }) -> list.sum.deaths
    
    lapply(post.samples, function(X){
      X <- data.frame(ID_space = X$ID_space, observed = X$deaths, EURO_LABEL=X$EURO_LABEL)
      return(X)
    }) -> list.observed
  
    lapply(post.samples, function(X){
      X <- data.frame(ID_space = X$ID_space, population = X$population, EURO_LABEL=X$EURO_LABEL)
      return(X)
    }) -> list.population
 }
  

  # For each i in 1:10 aggregate predicted/observed values by region 
  if(geo.res == "region"){
    lapply(post.samples, function(X){
      
      X <- select(X, starts_with("V"), "ID_space","EURO_LABEL")
      X <- left_join(X, link_table, by = c("ID_space"))
      
      X %>% 
        select(starts_with("V"), "NAMNUTS2","EURO_LABEL") %>% 
        group_by(NAMNUTS2,EURO_LABEL) %>%
        summarise_all(sum) %>% 
        ungroup() -> X
      X <- as.data.frame(X)
      return(X)
      
    }) -> list.sum.deaths
    
    lapply(post.samples, function(X){
      X <- data.frame(ID_space = X$ID_space, observed = X$deaths,EURO_LABEL=X$EURO_LABEL)
      X <- left_join(X, link_table, by = c("ID_space"))
      
      X %>% 
        group_by(NAMNUTS2,EURO_LABEL) %>%
        summarise(observed = sum(observed)) %>% 
        ungroup() -> Y
      Y$observed <- as.numeric(Y$observed)
      return(Y)
    }) -> list.observed
    
    lapply(post.samples, function(X){
      X <- data.frame(ID_space = X$ID_space, population = X$population,EURO_LABEL=X$EURO_LABEL)
      X <- left_join(X, link_table, by = c("ID_space"))
      
      X %>% 
        group_by(NAMNUTS2,EURO_LABEL) %>%
        summarise(population = sum(population)) %>% 
        ungroup() -> Y
      Y$population <- as.numeric(Y$population)
      return(Y)
    }) -> list.population
  }
  
  # For each i in 1:10 aggregate predicted/observed values to get the total for the country
  if(geo.res == "country"){
    lapply(post.samples, function(X){
      
      X %>% 
        select(starts_with("V"),"EURO_LABEL") %>% 
        group_by(EURO_LABEL) %>% 
        summarise_all(sum) %>%  
        ungroup() -> X
      X <- as.data.frame(X)
      X$COUNTRY = "Italy"
      return(X)
    }) -> list.sum.deaths
    
    lapply(post.samples, function(X){
      X %>% 
        group_by(EURO_LABEL) %>% 
        summarise(observed = sum(deaths)) %>% 
        ungroup() -> Y
      Y$COUNTRY = "Italy"
      return(Y)
    }) -> list.observed
    
    lapply(post.samples, function(X){
      X %>% 
        group_by(EURO_LABEL) %>% 
        summarise(population = sum(population)) %>% 
        ungroup() -> Y
      Y$COUNTRY = "Italy"
      return(Y)
    }) -> list.population
  }
  
  # Aggregate by age and sex  
  if(stratify.by == "none"){
    
    sum.deaths <- Reduce("+", lapply(list.sum.deaths,
                                     function(X) select(X, starts_with("V"))))
    sum.deaths$EURO_LABEL = list.sum.deaths[[1]]$EURO_LABEL
    if(geo.res=="province") sum.deaths$ID_space <- list.sum.deaths[[1]]$ID_space
    if(geo.res=="region") sum.deaths$NAMNUTS2 <- list.sum.deaths[[1]]$NAMNUTS2
    if(geo.res=="country") sum.deaths$COUNTRY <- "Italy"
    
    
    sum.observed <- Reduce("+", lapply(list.observed,
                                       function(X) select(X,observed)))
    sum.observed$EURO_LABEL = list.observed[[1]]$EURO_LABEL
    if(geo.res=="province") sum.observed$ID_space <- list.observed[[1]]$ID_space
    if(geo.res=="region") sum.observed$NAMNUTS2 <- list.observed[[1]]$NAMNUTS2
    if(geo.res=="country") sum.observed$COUNTRY <- "Italy"
    
    sum.population <- Reduce("+", lapply(list.population,
                                       function(X) select(X,population)))
    sum.population$EURO_LABEL = list.population[[1]]$EURO_LABEL
    if(geo.res=="province") sum.population$ID_space <- list.population[[1]]$ID_space
    if(geo.res=="region") sum.population$NAMNUTS2 <- list.population[[1]]$NAMNUTS2
    if(geo.res=="country") sum.population$COUNTRY <- "Italy"
    
    #sum.deaths.obs = left_join(sum.deaths, sum.observed) #memory exhausted
    sum.observed <- left_join(sum.observed, sum.population)
    sum.deaths.obs = data.frame(sum.deaths,sum.observed)
    sum.deaths.obs = sum.deaths.obs %>% select(-ends_with(".1"))
    out = sum.deaths.obs
    
    return(out)
  }
  
  
  
  # Aggregate by age 
  if(stratify.by == "sex"){
    
    out = list()
    for(i in 1:n_distinct(groups4cv$sex)){
      sum.deaths <- Reduce("+", lapply(list.sum.deaths[groups4cv$sex == levels(groups4cv$sex)[i]],
                                       function(X) select(X, starts_with("V"))))
      sum.deaths$EURO_LABEL = list.sum.deaths[[1]]$EURO_LABEL
      if(geo.res=="province") sum.deaths$ID_space <- list.sum.deaths[[1]]$ID_space
      if(geo.res=="region") sum.deaths$NAMNUTS2 <- list.sum.deaths[[1]]$NAMNUTS2
      if(geo.res=="country") sum.deaths$COUNTRY <- "Italy"
      
      
      sum.observed <- Reduce("+", lapply(list.observed[groups4cv$sex == levels(groups4cv$sex)[i]],
                                         function(X) select(X, observed)))
      sum.observed$EURO_LABEL = list.observed[[1]]$EURO_LABEL
      if(geo.res=="province") sum.observed$ID_space <- list.observed[[1]]$ID_space
      if(geo.res=="region") sum.observed$NAMNUTS2 <- list.observed[[1]]$NAMNUTS2
      if(geo.res=="country") sum.observed$COUNTRY <- "Italy"
      
      sum.population <- Reduce("+", lapply(list.population,
                                           function(X) select(X,population)))
      sum.population$EURO_LABEL = list.population[[1]]$EURO_LABEL
      if(geo.res=="province") sum.population$ID_space <- list.population[[1]]$ID_space
      if(geo.res=="region") sum.population$NAMNUTS2 <- list.population[[1]]$NAMNUTS2
      if(geo.res=="country") sum.population$COUNTRY <- "Italy"
      
      #sum.deaths.obs = left_join(sum.deaths, sum.observed) #memory exhausted
      sum.observed <- left_join(sum.observed, sum.population)
      sum.deaths.obs = data.frame(sum.deaths,sum.observed)
      sum.deaths.obs = sum.deaths.obs %>% select(-ends_with(".1"))
      out[[i]] = sum.deaths.obs
      
    }
    names(out) = levels(groups4cv$sex)
    
    return(out)
  }
  
  # Aggregate by sex 
  if(stratify.by == "age"){
    
    out = list()
    for(i in 1:n_distinct(groups4cv$age)){
      sum.deaths <- Reduce("+", lapply(list.sum.deaths[groups4cv$age == levels(groups4cv$age)[i]],
                                       function(X) select(X, starts_with("V"))))
      sum.deaths$EURO_LABEL = list.sum.deaths[[1]]$EURO_LABEL
      if(geo.res=="province") sum.deaths$ID_space <- list.sum.deaths[[1]]$ID_space
      if(geo.res=="region") sum.deaths$NAMNUTS2 <- list.sum.deaths[[1]]$NAMNUTS2
      if(geo.res=="country") sum.deaths$COUNTRY <- "Italy"
      
      
      sum.observed <- Reduce("+", lapply(list.observed[groups4cv$age == levels(groups4cv$age)[i]],
                                         function(X) select(X, observed)))
      sum.observed$EURO_LABEL = list.observed[[1]]$EURO_LABEL
      if(geo.res=="province") sum.observed$ID_space <- list.observed[[1]]$ID_space
      if(geo.res=="region") sum.observed$NAMNUTS2 <- list.observed[[1]]$NAMNUTS2
      if(geo.res=="country") sum.observed$COUNTRY <- "Italy"
      
      sum.population <- Reduce("+", lapply(list.population,
                                           function(X) select(X,population)))
      sum.population$EURO_LABEL = list.population[[1]]$EURO_LABEL
      if(geo.res=="province") sum.population$ID_space <- list.population[[1]]$ID_space
      if(geo.res=="region") sum.population$NAMNUTS2 <- list.population[[1]]$NAMNUTS2
      if(geo.res=="country") sum.population$COUNTRY <- "Italy"
      
      #sum.deaths.obs = left_join(sum.deaths, sum.observed) #memory exhausted
      sum.observed <- left_join(sum.observed, sum.population)
      sum.deaths.obs = data.frame(sum.deaths, sum.observed)
      sum.deaths.obs = sum.deaths.obs %>% select(-ends_with(".1"))
      out[[i]] = sum.deaths.obs
    }
    names(out) = levels(groups4cv$age)
    
    return(out)
  }
  
  #  don't aggregate
  if(stratify.by == "agesex"){
    
    #out = Map(dplyr::left_join, list.sum.deaths, list.observed)
    out = Map(data.frame, list.sum.deaths, list.observed, list.population)
    out = lapply(out,function(X) {X=X %>% select(-ends_with(".1"))})
    
    names(out) = paste0(groups4cv$sex,groups4cv$age)
    return(out)
  }
  
}

################################################################################
# Compute relative excess mortality
################################################################################


compute.excess = function(data, mortality="REM", geo.name){
  
  #data is a dataframe which contains:
  # - the 1000 predicted values with colnames starting with V 
  # - observed deaths (observed)
  # - the name of the column containing the reference to the spatial unit (e.g. "NAMNUTS2")
  
  xs=-1*sweep(as.matrix(data %>% select(starts_with("V"))),
              1,data$observed,FUN ="-")
  
  sum.stats <- data.frame(
    median.pred = apply(as.matrix(data %>% select(contains("V"))),1,median,na.rm=T),
    LL.pred = apply(as.matrix(data %>% select(contains("V"))),1,quantile,.025,na.rm=T),
    UL.pred = apply(as.matrix(data %>% select(contains("V"))),1,quantile,.975,na.rm=T)
  )

  
  if(mortality == "REM"){
    xs=as.matrix(xs)/as.matrix(data %>% select(starts_with("V")))
  }
  
  if(mortality == "NED"){
    xs=xs
  }
  
  colnames(xs)=stringr::str_replace(colnames(xs),"V","xs")
  xs = as_tibble(xs)
  
  xs[,geo.name] = data[,geo.name]
  
  # Compute posterior summary statistics
  xs = xs %>% mutate(
    !!paste0("mean.", mortality) := apply(as.matrix(xs %>% select(contains("xs"))),1,mean,na.rm=T),
    !!paste0("median.", mortality) := apply(as.matrix(xs %>% select(contains("xs"))),1,median,na.rm=T),
    !!paste0("sd.", mortality) := apply(as.matrix(xs %>% select(contains("xs"))),1,sd,na.rm=T),
    !!paste0("LL.", mortality) := apply(as.matrix(xs %>% select(contains("xs"))),1,quantile,.025,na.rm=T),
    !!paste0("UL.", mortality) := apply(as.matrix(xs %>% select(contains("xs"))),1,quantile,.975,na.rm=T), 
    !!paste0("exceedance.", mortality) := apply(select(xs, starts_with("xs")) > 0, 1, mean))
    
  
  if(mortality == "REM"){
  xs$median.REM.cat = cut(xs$median.REM,
                      breaks = c(-100, 0, 0.05, 0.10, 0.15, 0.20, 100),
                      labels = c("0%<", "[0%, 5%)", "[5%, 10%)",
                                 "[10%, 15%)", "[15%, 20%)", "20%>"),
                      include.lowest = TRUE, right = FALSE)
  
  xs$exceedance.REM.cat = cut(xs$exceedance.REM, 
                  breaks = c(-0.01, 0.05, 0.20, 0.80, 0.95, 1.01),
                  labels = c("[0, 0.05]", "(0.05, 0.2]", "(0.2, 0.8]", "(0.8, 0.95]", "(0.95, 1]"),
                  include.lowest = FALSE, right = FALSE)
  }
  
  if(mortality == "NED"){
  xs$median.NED.cat = cut(xs$median.NED,
                      breaks = c(-1000000, -1000, -500, -100, 0, 100, 500, 1000, 1000000), 
                      labels = c("-1000<", "[-1000, -500)", "[-500, -100)", "[-100, 0)",
                                 "[0, 100)", "[100, 500)", "[500, 1000)", "1000>"),
                      include.lowest = TRUE, right = FALSE)
  
  xs$exceedance.NED.cat = cut(xs$exceedance.NED, 
                  breaks = c(-0.01, 0.05, 0.20, 0.80, 0.95, 1.01),
                  labels = c("[0, 0.05]", "(0.05, 0.2]", "(0.2, 0.8]", "(0.8, 0.95]", "(0.95, 1]"),
                  include.lowest = FALSE, right = FALSE)
  }
  

  xs <- cbind(xs, sum.stats)
  return(xs)
}




################################################################################
################################################################################
################################################################################
################################################################################
################################################################################



