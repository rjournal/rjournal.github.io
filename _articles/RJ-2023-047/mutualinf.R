library(mutualinf)

DT_Seg_Chile

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
