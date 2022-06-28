#Execute bdpar framework
bdpar::runPipeline(path = system.file("example", package = "bdpar"),
                   cache = FALSE)
#Load CSV generated after executing bdpar
dataset <- read.csv(file = bdpar.Options$get("teeCSVPipe.output.path"),
                    sep = ";", stringsAsFactors = FALSE )

#Separate instances by type
sms <- dataset[dataset$extension == "tsms", ]
eml <- dataset[dataset$extension == "eml", ]
# Function to clean text and compute frequencies
word.frec <- function(data) {
  corpus <- tm::VCorpus(VectorSource(data))
  corpus <- tm::tm_map(corpus, removePunctuation)
  corpus <- tm::tm_map(corpus, removeNumbers)
  corpus <- tm::tm_map(corpus, stemDocument)
  sorted <- sort(rowSums(as.matrix(tm::TermDocumentMatrix(corpus))), decreasing = TRUE)
  return(data.frame(word = names(sorted), freq = sorted))
}
sms.words <- word.frec(sms$data)
eml.words <- word.frec(eml$data)
all.words <- word.frec(dataset$data)
# Wordcloud for sms and e-mail
par(mfrow=c(1,2))
wordcloud::wordcloud( words = sms.words$word, freq = sms.words$freq, min.freq = 1,
                      max.words = 100, random.order = FALSE, rot.per = .5,
                      colors = RColorBrewer::brewer.pal(8, "Dark2") )
wordcloud::wordcloud( words = eml.words$word, freq = eml.words$freq,
                      min.freq = 1, max.words = 100, random.order = FALSE,
                      rot.per = .5, colors = RColorBrewer::brewer.pal(8, "Dark2") )
par(mfrow=c(1,1))
#Wordcloud for all instances (sms and e-mail)
wordcloud::wordcloud( words = dataset.words$word, freq = all.words$freq, min.freq = 1,
                      max.words = 100, random.order = FALSE,
                      rot.per = .5, colors = RColorBrewer::brewer.pal(8, "Dark2") )
