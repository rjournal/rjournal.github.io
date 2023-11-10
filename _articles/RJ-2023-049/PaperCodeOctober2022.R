library(CA3variants)
options(max.overlaps = Inf) 
#---------------------------------------- Assessing the model dimensionality for classic three-way CA  using original data for all triplet of dimensions
tune.ca3.out <- tunelocal(ratrank, ca3type = "CA3")
print(tune.ca3.out)
#----------to get Figure 1
plot(tune.ca3.out)
#-----------------------------------------
# Indicated dimensionality: 2,2,1 (two components for rows and columns and 1 component for tubes) ----section 6.2 of the paper
#----------Perform classical three-way CA
ca3.out<-CA3variants(ratrank, ca3type = "CA3", dims=c(2,2,1))
print(ca3.out)
#----------to get Figure 2
plot(ca3.out, biptype = "row-tube", addlines = F)
#----------to get Figure 5
plot(ca3.out, biptype = "row",  addlines = F,  scaleplot = 15)
#---------------------------------------- Assessing the model dimensionality for ordered three-way CA using original data for all triplet of dimensions
tune.oca3.out <- tunelocal(ratrank, ca3type = "OCA3", norder = 2)
print(tune.oca3.out)
plot(tune.oca3.out)
#-----------------------------------------
# Indicated dimensionality: 2,2,1 (two components for rows and columns and 1 component for tubes) ----section 6.4 of the paper
#-----------Perform ordered three-way CA using ratrank data
oca3.out <- CA3variants(ratrank, ca3type = "OCA3", dims = c(2, 2, 1),norder = 2)
summary(oca3.out)
print(oca3.out)
#----------to get Figure 6
plot(oca3.out, biptype = "row",  scaleplot = 15)

#--------------second example happiness data-----section 6.3 of the paper
#------------------------------------Assessing the model dimensionality for non-symmetric three-way  CA using bootstrap samples
tune.nsca3.out <- tunelocal(happyNL, ca3type = "NSCA3", resp = "row", boots=T)
print(tune.nsca3.out)
#----------to get Figure 3
plot(tune.nsca3.out)
#---------------------------------------------
# Indicated dimensionality: 2,2,2 (two components for rows, columns and tubes)
#---------Perform three-way non-symmetrical CA
nsca3.out<-CA3variants(happyNL,ca3type="NSCA3", resp="row", dims=c(2,2,2))
print(nsca3.out)
summary(nsca3.out)
#----------to get Figure 4
plot(nsca3.out, biptype = "pred")
