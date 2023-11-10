
# install genpathmox package
install.packages("genpathmox")

# load genpathmox package
library(genpathmox)

# load data
data(climate)




## genpathmox analysis 

# define del model  
climate_model <- "
# structural model
SAT ~ EMP + REP + PAY + WC + LEAD
LOY ~   SAT
# measurement model
EMP =~ Empo1 + Empo2 + Empo3 + Empo4 + Empo5 
REP  =~ Imag1 + Imag2 + Imag3 
PAY  =~ Pay1 + Pay2 + Pay3 + Pay4
WC =~ Work1 + Work2 + Work3
LEAD =~ Lead1 + Lead2 + Lead3 + Lead4 + Lead5
SAT =~Sat1 + Sat2 + Sat3 + Sat4 + Sat5 + Sat6
LOY =~ Loy1 + Loy2 + Loy3
"
# define the set of categorical variables
climate_catvar = climate[,1:3]

# run the pls.pathmox() function
climate.pathmox = pls.pathmox(
  .model = climate_model,
  .data = climate,
  .catvar = climate_catvar
)

# summary 
summary(climate.pathmox)


## visualization

# tree plot
plot(climate.pathmox)

# terminal nodes plot
bar_terminal(climate.pathmox, .LV = "SAT")

# ranking of importance plot
bar_impvar(climate.pathmox)

## full nodes analysis and hybrid multigroup 
## analysis (combining genpathmox and cSEM)

#install.packages("cSEM")
# load cSEM package
library(cSEM)

## terminal nodes analysis 

# terminanl nodes definition
terminal_nodes_data = climate.pathmox$hybrid

# terminal nodes full results
terminal_nodes_results = csem(.data = terminal_nodes_data, .model= climate_model)
                              

## hybrid multigroup analysis

# MICOM test for invariance of terminal nodes
climateMICOM = testMICOM(terminal_nodes_results)

climateMICOM

climate_innermodel <- "
# Structural model
SAT ~ EMP + REP + PAY + WC + LEAD
LOY ~   SAT
"

# multigroup test for  terminal nodes
climateMGA = testMGD(terminal_nodes_results, .parameters_to_compare = climate_innermodel,
                     .approach_mgd = "Chin")
climateMGA

