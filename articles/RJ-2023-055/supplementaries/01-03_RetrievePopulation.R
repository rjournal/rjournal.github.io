


# Created 08.10.2021

# Clean and download population


#---------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(sf)

options(encoding = "ISO-8859-1")

# Population for January 1st 2020 can be downloaded from: https://demo.istat.it/popres/download.php?anno=2020&lingua=ita
# selecting Province on the bottom right of the page. Save this object as POP2020 on the data folder.  
# (POP2020 is already provided on the data folder).

# Skip 1st line as it is a table header with a description of the data
pop20 <- read_csv("data/POP2020.csv", skip = 1)
#colnames(pop20) <- pop20[1,]
#pop20 <- pop20[-1,]

colnames(pop20)[3] <- 'Eta'

# select the relevant columns
pop20 = pop20 %>% select(`Codice provincia`, `Provincia`, `Totale Maschi`, `Totale Femmine`, `Eta`)

# bring together
pop20 %>% select(`Codice provincia`, `Provincia`, `Totale Maschi`, `Eta`) %>% 
  mutate(sex = "M") %>% 
  rename(pop := `Totale Maschi`) %>% 
  rbind(., 
        pop20 %>% select(`Codice provincia`, `Provincia`, `Totale Femmine`, `Eta`) %>% 
          mutate(sex = "F") %>% 
          rename(pop := `Totale Femmine`)) %>% 
  rename(Code := `Codice provincia`, 
         Province = Provincia, 
         Age := `Eta`) -> pop20

pop20 <- pop20[complete.cases(pop20),]
# also remove the total age
pop20 %>% filter(!Age %in% "Totale") -> pop20


# Population for the years 2015-2019 is available here: https://demo.istat.it/ricostruzione/download.php?lingua=ita
# Select the second Province link as you read the page from the top and name it POP2002_2019. Store it on the data
# folder. # (POP2002_2019 is already provided on the data folder).

# pop15_19 <- read_delim("data/POP2002_2019.csv", 
#                        ";", escape_double = FALSE, trim_ws = TRUE, 
#                        skip = 4)


pop15_19 <- read.csv("data/POP2002_2019.csv", 
                       sep = ";", header = TRUE, 
                       skip = 4)

colnames(pop15_19)[1] <- "Territorio/Eta"
# rename the columns:
colnames(pop15_19)[colnames(pop15_19) %in% paste0("X", 0:100)] <- 0:100

# We are interested in all nationalities and the years 2015:2019
pop15_19 <- pop15_19[(which(pop15_19$`Territorio/Eta` == "Tutte le cittadinanze - Anno: 2015")):
                       (which(pop15_19$`Territorio/Eta` == "Cittadinanza italiana - Anno: 2002")), ]


which.keep <- substr(pop15_19$`Territorio/Eta`, 1, stop = 1) == '0' | substr(pop15_19$`Territorio/Eta`, 1, stop = 1) == '1'
pop15_19$`Territorio/Eta`[which.keep] %>% as.numeric() %>% unique() %>% length() -> n.dat


# Seperate by sex and bring together
lapply(c("Maschi", "Femmine"), function(Y){
  
  lapply(which(pop15_19$`0` %in% Y), function(X) seq(from = X+1, to  = X + n.dat, by = 1)) -> list.sex
  
  pop15_19_sex <- NULL
  
  for(i in 1:length(list.sex)){
    pop15_19_sex_loop <- pop15_19[list.sex[[i]],]
    pop15_19_sex_loop$year <- 2014+i
    pop15_19_sex <- rbind(pop15_19_sex, pop15_19_sex_loop)
  }
  
  return(pop15_19_sex)
}
) -> pop.sex
  
pop.sex[[1]]$sex <- "M"
pop.sex[[2]]$sex <- "F"

pop15_19 <- rbind(pop.sex[[1]], pop.sex[[2]])

# and make long format
pop15_19 <- gather(pop15_19, Age, pop, `0`:`100`)
colnames(pop15_19)[c(1:2)] <- c("Code", "Province")
pop20$year <- 2020
pop15_19 <- pop15_19[,colnames(pop20)]
pop15_20 <- rbind(pop15_19, pop20)


# aggregate by age group
pop15_20 %>% 
  mutate(Age = as.numeric(Age)) %>% 
  mutate(
    Age = cut(Age, breaks = c(-1, 39, 59, 69, 79, 101), 
              labels = c("less40", "40-59", "60-69", "70-79", "80plus"))
  ) %>% 
  group_by(Code, Province, Age, sex, year) %>% 
  summarise(pop = sum(as.numeric(pop))) -> pop15_20







# Fix problems with province names (with respect to the shapefile names)
pop15_20$Province[startsWith(pop15_20$Province, "Valle d")] = "Aosta"
pop15_20$Province[pop15_20$Province=="Bolzano/Bozen"] = "Bolzano"
pop15_20$Province[startsWith(pop15_20$Province, "Forl")] = "Forli'-Cesena"
pop15_20$Province[pop15_20$Province=="Massa-Carrara"] = "Massa Carrara"




# Province information from the shapefile
prov.shp = read_sf("data/ProvCM01012020_g_WGS84.shp")
geodata = prov.shp %>% dplyr::select(COD_RIP,COD_REG,COD_PROV,DEN_UTS,SIGLA)


# Merge pop and geodata to have it in compatible format
pop15_20 = left_join(pop15_20, geodata, by=c("Province"="DEN_UTS"))
pop15_20 <- pop15_20 %>% ungroup() %>% select(SIGLA, Age, sex, year, pop)


# the population file should have the following format, so the population interpolation file
# runs smoothly

#   NUTS318CD ageg   sex     year population
# 1 TO        less40 female  2015     435758
# 2 TO        less40 female  2016     427702
# 3 TO        less40 female  2017     420498
# 4 TO        less40 female  2018     413141
# 5 TO        less40 female  2019     406937
# 6 TO        less40 male    2015     449605
# 7 TO        less40 male    2016     443941
# 8 TO        less40 male    2017     439522
# 9 TO        less40 male    2018     433365
# 10 TO        less40 male    2019     427936

colnames(pop15_20)
pop15_20 <- pop15_20[, c("SIGLA", "Age", "sex", "year", "pop")]
colnames(pop15_20) <- c("NUTS318CD", "ageg", "sex", "year", "population")
pop15_20$sex[pop15_20$sex %in% "M"] <- "male"
pop15_20$sex[pop15_20$sex %in% "F"] <- "female"

# Save data
save(pop15_20, file="Output/pop15_20_final.RData")

##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
