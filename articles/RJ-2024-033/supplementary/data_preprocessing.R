###############################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%% SUPPLEMENTARY MATERIAL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#------------------------------------------------------------------------------
# Data preprocessing 
# for the data (.csv file) to be instantly used in the article.
# The "data/rawdata_GCBS.csv" can be downloaded from 
# <http://openpsychometrics.org/_rawdata/GCBS.zip>.



# Read the raw data
#------------------------------------------------------------------------------
# The first 15 relevant columns are selected.
rawdata <- read.csv("data/rawdata_GCBS.csv")[, 1:15]

# Remove unacceptable respondents
#------------------------------------------------------------------------------
# There are some respondents who provided the same answer to all questions.
index <- apply(X = rawdata, MARGIN = 1, FUN = sd, na.rm = TRUE) == 0

# Mark omitted responses as `NA`
#------------------------------------------------------------------------------
# Omission in the raw data is denoted as 0.
rawdata[rawdata == 0] <- NA

# Modify item names from Q1-15 to Item1-15
#------------------------------------------------------------------------------
colnames(rawdata) <- paste0("Item", 1:15)

 
data_preprocessed <- rawdata[!index,]

# Export
#------------------------------------------------------------------------------
write.csv(data_preprocessed, "data/data_GCBS.csv", row.names = FALSE)
