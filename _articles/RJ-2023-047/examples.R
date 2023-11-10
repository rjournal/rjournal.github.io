
if (!isTRUE(require(mutualinf, quiet=TRUE))) {
    install.packages("mutualinf")
    library(mutualinf)
}

# where is the dataset?
# https://github.com/RafaelFuentealbaC/mutualinf/blob/master/data-raw/DF_Seg_Chile.csv

DF_Seg_Chile <- read.csv("data-raw/DF_Seg_Chile.csv", header = TRUE, sep = ",")

DT_Seg_Chile <- prepare_data(data = DF_Seg_Chile, vars = "all_vars", fw = "nobs", col.order = "region")

head(DT_Seg_Chile, 5)

mutual(data = DT_Seg_Chile,
       group = "csep",
       unit = "school")

mutual(data = DT_Seg_Chile,
       group = "ethnicity",
       unit = "school")

mutual(data = DT_Seg_Chile,
       group = c("csep", "ethnicity"),
       unit = "school")

mutual(data = DT_Seg_Chile,
       group = c("csep", "ethnicity"),
       unit = c("school", "district"))

mutual(data = DT_Seg_Chile,
       group = c("csep", "ethnicity"),
       unit = c("school", "sch_type"))

mutual(data = DT_Seg_Chile,
       group = c("csep", "ethnicity"),
       unit = c("school", "sch_type"),
       by = "region")

mutual(data = DT_Seg_Chile,
       group = c("csep", "ethnicity"),
       unit = c("school", "sch_type"),
       by = c("region", "year"))

mutual(data = DT_Seg_Chile,
       group = c("csep", "ethnicity"),
       unit = c("school", "sch_type"),
       by = "region",
       within = "ethnicity")

mutual(data = DT_Seg_Chile,
       group = c("csep", "ethnicity"),
       unit = c("school", "sch_type"),
       by = "region",
       within = "csep")

mutual(data = DT_Seg_Chile,
       group = c("csep", "ethnicity", "gender"),
       unit = c("school", "grade"),
       by = "region",
       within = c("csep", "ethnicity"))

mutual(data = DT_Seg_Chile,
       group = c("csep", "ethnicity"),
       unit = c("school", "sch_type"),
       by = "region",
       contribution.from = "group_vars")

mutual(data = DT_Seg_Chile,
       group = c("csep", "ethnicity"),
       unit = c("school", "sch_type"),
       by = "region",
       within = "ethnicity",
       contribution.from = "unit_vars")

mutual(data = DT_Seg_Chile,
       group = c("csep", "ethnicity"),
       unit = c("school", "sch_type"),
       by = "region",
       contribution.from = "csep")

mutual(data = DT_Seg_Chile,
       group = c("csep", "ethnicity"),
       unit = c("school", "sch_type"),
       by = "region",
       contribution.from = "unit_vars")

mutual(data = DT_Seg_Chile,
       group = c("csep", "ethnicity"),
       unit = c("school", "district"),
       by = "region",
       contribution.from = "unit_vars")

mutual(data = DT_Seg_Chile,
       group = c("csep", "ethnicity", "gender"),
       unit = c("school", "sch_type"),
       by = "region",
       contribution.from = "group_vars")

mutual(data = DT_Seg_Chile,
       group = c("csep", "ethnicity"),
       unit = c("school", "sch_type"),
       within = "csep",
       components = TRUE)

