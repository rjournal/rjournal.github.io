

# Created 08.10.2021


# Calculate weekly population


#---------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(lubridate)

load("Output/pop15_20_final.RData")
pop <- pop15_20; rm(pop15_20)



# ISO weeks file
EUROSTAT_ISO <- readRDS("Output/EUROSTAT_ISO")


# the population file should have the following format:
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
head(pop)


# retrieve all possible combinations of age groups, sex, region and EURO_LABEL weeks
expand.grid(age = c("less40", "40-59", "60-69", "70-79", "80plus"), 
            sex = c("male", "female"), 
            region = unique(pop$NUTS318CD), 
            EURO_LABEL = unique(EUROSTAT_ISO$EURO_LABEL)) -> pop_weekly

# As the population is only available for the 1st of January of every year, we need to create a weekly version
# to feed in the model. The above gives as all the possible combinations required for the linear interpolation.


pop_weekly$EURO_LABEL <- as.character(pop_weekly$EURO_LABEL)
EUROSTAT_ISO$EURO_TIME <- as.Date(EUROSTAT_ISO$EURO_TIME) # make data format

# here we select row_number()==4 to represent the day that is in the middle of the week
EUROSTAT_ISO %>% filter(YEAR <= 2020) %>% group_by(EURO_LABEL) %>% filter(row_number()==4) -> EUROSTAT_ISO
# the reference date refers to the population availability of the first of each year
EUROSTAT_ISO %>% group_by(YEAR) %>% mutate(refdate = as.Date(paste0(YEAR, "-01-01"))) -> EUROSTAT_ISO
# the day2pred is needed for the linear interpolation within the year
EUROSTAT_ISO$day2pred <- as.numeric(EUROSTAT_ISO$EURO_TIME - as.Date("2015-01-01") + 1)


# add the reference date and days two predict in the matrix for predictions, i.e. pop_weekly
pop_weekly <- left_join(pop_weekly, EUROSTAT_ISO[,c("EURO_LABEL", "YEAR", "day2pred", "refdate")], 
                        by = c("EURO_LABEL" = "EURO_LABEL"))


pop_weekly %>% as_tibble() %>%  rename(year = YEAR) -> pop_weekly

# and now merge with the population to get the values for the 1st of each year
pop_weekly <- left_join(pop_weekly, pop, by = c("year" = "year", "age" = "ageg", "sex" = "sex", "region" = "NUTS318CD"))



##--## First two panels of the plot
pop %>% group_by(NUTS318CD, sex, ageg) %>% 
  reframe(coef = as.vector(coef(lm(population ~ year)))) %>% 
  filter(NUTS318CD %in% "VE", sex %in% "female", ageg %in% "40-59") -> coef.VE

pop %>% filter(NUTS318CD %in% "VE", sex %in% "female", ageg %in% "40-59") -> pop.VE

ggplot() + geom_point(data = pop.VE, aes(x=year, y=population), size = 1) + 
  geom_point(aes(x=2021, y = coef.VE$coef %*% c(1, 2021)), col = "red", pch = 17, size = 1) + 
  geom_line(aes(x=2015:2021, y = c(coef.VE$coef[1] + coef.VE$coef[2]*2015:2021)), 
            linetype = 2, col = "red", linewidth = 0.3) + 
  scale_x_continuous(breaks = 2015:2021) + ylim(c(136400, 139200)) +
  theme_light() +
  ggtitle("A.") + 
  theme(text = element_text(size = 6), 
        plot.margin=unit(c(0,0,0,0), "cm")) -> p1


ggplot() + geom_point(data = pop.VE, aes(x=year, y=population), size = 1) + 
  geom_point(aes(x=2021, y = coef.VE$coef %*% c(1, 2021)), col = "red", pch = 17, size = 1) + 
  geom_line(aes(x=2015:2021, y = c(pop.VE$population, coef.VE$coef %*% c(1, 2021))), linetype = 2, col = "black", linewidth = 0.3) + 
  scale_x_continuous(breaks = 2015:2021) + ylim(c(136400, 139200)) + 
  theme_light() + 
  # geom_rect(aes(xmin = 2014.9, xmax = 2016.1, ymin = 138300, ymax = 139000),
  #                          fill = "transparent", color = "blue", size = 0.5) + 
  ggtitle("B.") + 
  theme(text = element_text(size = 6), 
        plot.margin=unit(c(0,0,0,0), "cm"))-> p2
##--##



# we do not have the population for 2021, we can calculate with linear interpolation
pop %>% group_by(NUTS318CD, sex, ageg) %>% 
  summarise(population = as.vector(coef(lm(population ~ year)) %*% c(1, 2021))) %>% 
  mutate(year = 2021) -> pop2021

pop$X <- NULL
pop2021 <- pop2021[,colnames(pop)]
pop <- rbind(pop, pop2021)
length(unique(pop$ageg))*length(unique(pop$year))*length(unique(pop$sex))*length(unique(pop$NUTS318CD))
# looks correct

# need to add the population of next year
pop$year <- pop$year - 1
colnames(pop)[5] <- "pop.next.year"

pop_weekly <- left_join(pop_weekly, pop, by = c("year" = "year", "age" = "ageg", "sex" = "sex", "region" = "NUTS318CD"))

# get the next years ref date
pop_weekly$refdate2 <- as.Date(paste0(pop_weekly$year + 1, "-01-01"))
pop_weekly$X <- NULL

# and here we compute the slope, intercept for the within year interpolation and use them to predict on day2pred
pop_weekly %>% mutate(lambda = (pop.next.year - population)/as.numeric((refdate2 - refdate))) %>% 
  mutate(beta0 = population - lambda*as.numeric(refdate - as.Date("2015-01-01") + 1)) %>% 
  mutate(popfin = beta0 + lambda*day2pred) -> pop_weekly


# remove what is not needed any longer
pop_weekly$day2pred <- pop_weekly$refdate <- pop_weekly$population <- pop_weekly$pop.next.year <- pop_weekly$refdate2 <-
  pop_weekly$lambda <- pop_weekly$beta0 <-  pop_weekly$days2plot <- NULL

colnames(pop_weekly)[3] <- "NUTS318CD"
colnames(pop_weekly)[6] <- "population"

# store
saveRDS(pop_weekly, file = "Output/pop_weekly")


# For the plot we are focusing on Veneto and to female population less than 40

dat_weekly_VE <- pop_weekly %>% filter(NUTS318CD  %in% "VE", sex %in% "female", age %in% "40-59", year == 2015) %>% 
  mutate(x = as.numeric(as.factor(EURO_LABEL)))

# Figure 1
png("Output/PopulationPlot.png", width = 14, height = 5, units = "cm", res = 300)
(p1|p2)
dev.off()

######################################################################################
######################################################################################
######################################################################################
######################################################################################
