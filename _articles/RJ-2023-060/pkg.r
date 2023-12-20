

# Here we provided the code and data for the paper titled "Variety and Mainstays of the R Developer Community".
# Code, data, and results are also avaliable through https://github.com/zhanglj37/R_Developer_Community
# Content 
# Text Mining
## 1 Data Cleaning
### 1.1 Length of the descriptions
### 1.2 Convert upper cases, delete web links and doi
### 1.3 Lemmatization
### 1.4 Delete numbers and symbols
### 1.5 remove common stopwords
## 2 Frequency Analysis
### 2.1 Word Frequency
### 2.2 Phrase Frequency
## 3 Topic Modeling
### 3.1 Determine the number of topics
### 3.2 Topic 20
# Network Analysis
## 4 Package dependency network and author collaboration network
### 4.1 Build network and set edges
### 4.2 Measures of Influence
### 4.3 Sensitivity Analysis
### 4.4 Select Important Packages and Authors
### 4.5 Visualization
### 4.6 Correlation among importance indexes and downloads
## 5 Bipartite Network
### 5.1 Build bipartite network and set edges (weights)
### 5.2 Visulization
### 5.3 Centrality


## package and function perparation
library(dplyr)
library(tidytext)
library(janeaustenr)
library(ggplot2)
library(scales)
library(textdata)
library(tidyr)
library(igraph)
library(ggraph)
library(tm)
library(stringr)
library(wordcloud)
library(topicmodels)
library(wordcloud2)
library(spacyr) # for lemmatizing verbs / aux by importing python packages
library(SemNetCleaner) # for lemmatizing nouns



# function for save data
write.csv.utf8.BOM <- function(df, filename){
    con <- file(filename, "w")
    tryCatch({
        for (i in 1:ncol(df))
            df[,i] = iconv(df[,i], to = "UTF-8") 
        writeChar(iconv("\ufeff", to = "UTF-8"), con, eos = NULL)
        write.csv(df, file = con, row.names=FALSE)
    },finally = {close(con)})
}
# function for spliting the name and email of maintainer
split_mt <- function(maintainer){
  mt2name_email = strsplit(maintainer,'<')[[1]]
  name = mt2name_email[1]
  email = strsplit(mt2name_email[2],'>')[[1]]

  return(mt_name_email = list(name = name, email = email))
}



# extract the pkg information from cran on 2022-11-17
cran <- tools::CRAN_package_db()
cran <- cran[!duplicated(cran$Package), ] # Remove duplicated packages
cat(paste0('There are ', dim(cran)[1], ' packages on CRAN at ', Sys.time()))
# There are 18898 packages on CRAN at 2022-11-27 17:01:43
save.image("pkg_221127.RData")
write.csv(cran, 'pkg_221127.csv', row.names=F)



# Load the extracted data which can be obtained from [https://github.com/zhanglj37/R_Developer_Community](https://github.com/zhanglj37/R_Developer_Community).

load("pkg_221127.RData") 



# Text Mining

## 1 Data Cleaning


### 1.1 Length of the descriptions
desc = cran[,c("Package","Description")]
desc_len = str_count(desc$Description, '\\w+')
ggplot(as.data.frame(desc_len)) + geom_histogram(aes(x=desc_len), bins=20) + theme_set(theme_bw()) 
summary(desc_len)




### 1.2 Convert upper cases, delete web links and doi
desc[,"Description"] = tolower(desc[,"Description"]) 
# delete links
desc[,"Description"] = str_remove_all(str_remove_all(desc[,"Description"], "(?<=(http)).+?(?=\\s|$)"), fixed("http")) 
# delete links
desc[,"Description"] = str_remove_all(str_remove_all(desc[,"Description"], "(?<=(www\\.)).+?(?=\\s|$)"), fixed("www.")) 
# delete doi
desc[,"Description"] = str_remove_all(str_remove_all(desc[,"Description"], "(?<=(doi)).+?(?=\\s|$)"), fixed("doi")) 
# replace "\\n  " with " "
for(i in 1:nrow(desc)){
    desc[i,"Description"] = str_replace_all(desc[i,"Description"], "\\s+", " ")
}



### 1.3 Lemmatization    
desc_sentence = desc[,"Description"]
names(desc_sentence) = desc[,"Package"]


# lemmatize verbs (applied, applies --> apply) and aux (was --> be) using spacyr spacy_parse()
# lemmatize NOUNs using SemNetCleaner singularize()
# https://spacy.io/usage
desc_token = spacy_parse(desc_sentence)
desc_token2 = desc_token
for (i in 1:nrow(desc_token2)){
    if(desc_token2[i,"pos"]=="VERB" || desc_token2[i,"pos"]=="AUX" ){
        desc_token2[i,"lemma"] = desc_token[i,"lemma"]
    }else if(desc_token2[i,"pos"]=="NOUN"){
        desc_token2[i,"lemma"] = singularize(as.character(desc_token2[i,"token"]))
    }else if(desc_token2[i,"pos"]=="PROPN"){
        desc_token2[i,"lemma"] = singularize(as.character(desc_token2[i,"token"]))
    }else if(desc_token2[i,"pos"]=="PRON"){
        desc_token2[i,"lemma"] = singularize(as.character(desc_token2[i,"token"]))
    }else{
        desc_token2[i,"lemma"] = desc_token[i,"token"]
    }
}

colnames(desc_token2) = c("doc_id", "sentence_id", "token_id", "token", "word", "pos", "entity")


# transferred desc_token2 back into sentence for phrase frequency analysis
desc_lemma = desc
for (i in 1:nrow(desc)){
    loc_sentence = which(desc_token2[,"doc_id"] == desc[i,"Package"])
    # combine words into sentense
    desc_lemma[i,"Description"] = paste(desc_token2[loc_sentence, "word"], collapse = " ")
}



### 1.4 Delete numbers and symbols 
delete_pos = c("PUNCT", "NUM", "SYM") 
pos_loc1 = which(desc_token2[,"pos"]==delete_pos[1])
pos_loc2 = which(desc_token2[,"pos"]==delete_pos[2])
pos_loc3 = which(desc_token2[,"pos"]==delete_pos[3])

loc_token = c(1:nrow(desc_token2))
# detect numbers # numbers that are not detected by spacy_sparse
numbers_only <- function(x) !grepl("\\D", x)
loc_number = loc_token[numbers_only(desc_token2[,"word"])]
# detect space
space_only <- function(x) !grepl("\\S", x)
loc_space = loc_token[space_only(desc_token2[,"word"])]
# detect symbols # symbols that are not detected by spacy_sparse
no_str_num <- function(x) !grepl("\\w", x) 
loc_no_str_num = loc_token[no_str_num(desc_token2[,"word"])]
one_char1 <- function(x) grepl("^\\w\\W$", x) 
one_char2 <- function(x) grepl("^\\W\\w$", x) 
loc_onechar1 = loc_token[one_char1(desc_token2[,"word"])]
loc_onechar2 = loc_token[one_char2(desc_token2[,"word"])]

pos_delete = c(pos_loc1, pos_loc2, pos_loc3, loc_number, loc_space, loc_no_str_num, loc_onechar1, loc_onechar2)

token_clean = desc_token2[-pos_delete,]

token_clean = tibble(token_clean)



### 1.5 remove common stopwords    
# our_stop_words
our_stop <- c("et", "etc", "al", "i.e.", "e.g.", 
    "package", "provide", "method", "function", "approach", "reference", "implement", "contain", "include")

token_nostop <- token_clean %>% 
    anti_join(stop_words) %>%
    filter(!word %in% our_stop)  %>%
    filter(!word %in% stop_words$word)  


#  TF-IDF value 
#token_count = token_nostop %>% 
#  count(doc_id, word)

#total_words = token_count %>% 
#  group_by(doc_id) %>% 
#  summarize(total = sum(n))

#token_count2 <- left_join(token_count, total_words)

#token_tf_idf <- token_count2 %>%
#  bind_tf_idf(word, doc_id, n)

#temp = token_tf_idf %>%
#  select(-total) %>%
#  arrange(desc(tf_idf))



## 2 Frequency Analysis

###  2.1 Word Frequency    
word_totals <- token_nostop %>%
    count(word, sort = TRUE)


word_totals_sub = word_totals[c(1:200),]

wordcloud(word_totals_sub$word, word_totals_sub$n, random.color=FALSE, random.order=FALSE, color=colorRampPalette(brewer.pal(8, "Blues"))(50)[25:50], scale=c(4,1),family="serif")




### 2.2 Phrase Frequency  

#### Get Phrases and Frequency


#### 2-gram 
desc_lemma.2gram <- unnest_tokens(desc, word, Description, token = "ngrams", n = 2)  
desc_lemma.2gram %>% filter(word=="p value" | word == "p values") %>% count(word, sort = T)

desc_lemma.2count <- desc_lemma.2gram %>% separate(word, c("word1", "word2"), sep = " ")  %>% 
    filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>% # remove phrase consist with all stop words
    unite(word, word1, word2, sep = " ") %>%
    count(word, sort = TRUE)  
head(desc_lemma.2count)
#write.csv(desc_lemma.2count, 'freq/2word_freq.csv', row.names=F)

#### 3-gram  
desc_lemma.3gram <- unnest_tokens(desc, word, Description, token = "ngrams", n = 3) 

desc_lemma.3count <- desc_lemma.3gram %>% separate(word, c("word1", "word2", "word3"), sep = " ")  %>% 
    filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word & !word3 %in% stop_words$word) %>% # remove phrase consist with all stop words
    unite(word, word1, word2, word3, sep = " ") %>%
    count(word, sort = TRUE)  
head(desc_lemma.3count)
#write.csv(desc_lemma.3count, 'freq/3word_freq.csv', row.names=F)

# select meaningful phrases and combine phrases with same meaning manully
word2 = read.csv("freq/2WordTermTop30.csv")
word3 = read.csv("freq/3WordTermTop30.csv")


### 456-gram
desc_lemma.4gram <- unnest_tokens(desc_lemma, word, Description, token = "ngrams", n = 4)
desc_lemma.4count <- desc_lemma.4gram %>%  count(word, sort = TRUE) 
#write.csv(desc_lemma.4count, 'freq/4word_freq.csv', row.names=F)

desc_lemma.5gram <- unnest_tokens(desc_lemma, word, Description, token = "ngrams", n = 5)
desc_lemma.5count <- desc_lemma.5gram %>%  count(word, sort = TRUE) 
#write.csv(desc_lemma.5count, 'freq/5word_freq.csv', row.names=F)

desc_lemma.6gram <- unnest_tokens(desc_lemma, word, Description, token = "ngrams", n = 6)
desc_lemma.6count <- desc_lemma.6gram %>%  count(word, sort = TRUE) 
#write.csv(desc_lemma.6count, 'freq/6word_freq.csv', row.names=F)



#### Frequency  Figures      
word1_plot = word_totals %>%
    top_n(30) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    theme(axis.title.x = element_text(face = 'bold'), axis.title.y = element_text(face = 'bold'), axis.title = element_text(face = 'bold'), text=element_text(size=20)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_y_continuous(expand = c(0,0)) +
    labs(x = NULL, y = NULL) + 
    theme(panel.grid=element_blank()) 
word1_plot

word2_plot = word2 %>%
    top_n(30) %>%
    mutate(term = reorder(term, frequency)) %>%
    ggplot(aes(term, frequency)) +
    theme(axis.title.x = element_text(face = 'bold'), axis.title.y = element_text(face = 'bold'), axis.title = element_text(face = 'bold'), text=element_text(size=20)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_y_continuous(expand = c(0,0)) +
    labs(x = NULL, y = NULL) +
    theme(panel.grid=element_blank()) 
word2_plot

word3_plot = word3 %>%
    top_n(30) %>%
    mutate(term = reorder(term, frequency)) %>%
    ggplot(aes(term, frequency)) +
    theme(axis.title.x = element_text(face = 'bold'), axis.title.y = element_text(face = 'bold'), axis.title = element_text(face = 'bold'), text=element_text(size=20)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_y_continuous(expand = c(0,0),breaks = c(0, 50,100)) +
    labs(x = NULL, y = NULL) +
    theme(panel.grid=element_blank()) 
word3_plot

ggsave("freq/word1.pdf", word1_plot, dpi = 400,width=5, height = 7)		
ggsave("freq/word2.pdf", word2_plot, dpi = 400,width=5, height = 7)				
ggsave("freq/word3.pdf", word3_plot, dpi = 400,width=5, height = 7)	


## 3 Topic Modeling

### 3.1 Determine the number of topics
# data perparation
library(topicmodels)
desc_dtm_select = token_nostop[,c("doc_id","word")] 
desc_dtm_select2 = desc_dtm_select %>% 
    group_by(doc_id) %>%
    count(word) %>% ungroup()

desc_dtm = desc_dtm_select2 %>% cast_dtm(doc_id, word, n)
tm::inspect(desc_dtm)



k_max = 30
k.topics <- 2:k_max
doc_num = nrow(cran)

folding4 <- rep(1:4, each = round(doc_num/5,0))
folding = c(folding4, rep(5, (doc_num-length(folding4))))


## parallel computation
runonce_paral <- function(sed) {
    res <- NULL
    for (k in k.topics) {
        for (fold in 1:5) {
            testing.dtm <- which(folding == fold)
            training.dtm <- which(folding != fold)
            
            training.model <- LDA(desc_dtm[training.dtm, ], k = k, control = list(seed = sed*100))
            test.model <- LDA(desc_dtm[testing.dtm, ], model = training.model, control = list(estimate.beta = FALSE))
            
            prep = perplexity(test.model)
            cat(paste(sed, k, fold, prep, "\n", sep="\t"),append=T)
            res <- rbind(res, c(sed, k, fold, prep))
        }   
    }
    return(res)
}


library(doParallel)

ncores <- 32

#switch between %do% (serial) and %dopar% (parallel)
if (ncores == 1){  #serial
    `%is_par%` <- `%do%`
}else{  #parallel
    `%is_par%` <- `%dopar%`
    cl <- makeCluster(ncores)#, outfile="res.txt")
    registerDoParallel(cores = ncores)
}

foreach(sed = 1:100, .packages = c("topicmodels") ) %is_par%{
	sink("res.txt", append=TRUE) # divert the output to the log file
    res = runonce_paral(sed)
	
}

if(ncores > 1) stopCluster(cl)


res = read.table("lda/res.txt", sep="\t")

colnames(res) = c('sed', 'topics', 'fold', 'perplexity')

total.perp <- NULL 
for(sedi in 1:100){ 
    for(ki in 2:k_max){
        loc = which(res[,'sed']==sedi & res[,'topics']==ki)
        total.perp = rbind(total.perp, c(sedi, ki, mean(res[loc,'perplexity'])))
    }
}


total.perp.all = tapply(total.perp[, 3], total.perp[, 2], mean)
sort(total.perp.all)

plot(2:k_max, total.perp.all, type = "b", xlab = "Number of topics", ylab = "Perplexity")



num_top = rep(0,k_max-1)
names(num_top) = c(2:k_max)
for(sedi in 71:100){
    loc = which(total.perp[,1]==sedi)
    temp = total.perp[loc,3]
    names(temp) = total.perp[loc,2]
    num_top[as.numeric(names(sort(temp)[1]))-1] = num_top[as.numeric(names(sort(temp)[1]))-1] + 1
}
# times that the number was selected as the best
num_top




desc_lda20 <- LDA(desc_dtm, k = 20, control = list(seed = 1000))

desc_topics <- tidy(desc_lda20, matrix = "beta")
desc_topics

desc_terms <- desc_topics %>% group_by(topic) %>% top_n(100, beta) %>% ungroup() %>% 
    arrange(topic, -beta)


library(ggwordcloud)

set.seed(1)
lda_wcloud = ggplot(desc_terms, aes(label = term, size = sqrt(beta)*100)) +
  geom_text_wordcloud_area(shape='circle') +
  scale_size_area(max_size = 5) +
  theme_minimal() +
  facet_wrap(~topic, ncol=4) + 
  theme_set(theme_bw()) + theme(panel.grid=element_blank()) +
  theme(panel.grid.major=element_line(colour=NA), panel.spacing=unit(0.5, "lines")) 

head(desc_terms)



# Relationships between topic and packages

desc_topics_doc <- tidy(desc_lda20, matrix = "gamma") %>% group_by(topic) %>% top_n(100, gamma) %>% ungroup() %>% arrange(topic, -gamma)
head(desc_topics_doc)

desc_alpha <- tidy(desc_lda20, matrix = "gamma")

desc_alpha[which(desc_alpha$document == 'Rcpp'),]


# Network Analysis

## 4 Package dependency network and author collaboration network

### 4.1 Build network and set edges
library(cranly)
library(influential)
library(igraph)
load("pkg_221127.RData")  # load cran
cran_ly <- clean_CRAN_db(cran)

pkg_net <- build_network(cran_ly, perspective = "package")  
aut_net <- build_network(cran_ly, perspective = "author") 

pkg_graph = as.igraph(pkg_net,reverse=TRUE) # reverse the direction
aut_graph = as.igraph(aut_net)


######### flag weights 
######### pkg_graph: weight = relationships
pkg_net_type = E(pkg_graph)$type
pkg_net_type_num = pkg_net_type
pkg_type = c("depends", "imports", "suggests", "linking_to", "enhances")
pkg_type_num = c(5:1)
for (typei in 1:length(pkg_type)){
    loc = which(pkg_net_type==pkg_type[typei])
    pkg_net_type_num[loc] = pkg_type_num[typei]
}
pkg_graph_w = pkg_graph %>% set_edge_attr("weight", value = pkg_net_type_num)


######### special cases: same anthor with different names (Gábor Csárdi and Kirill Müller)
aut_adj_matrix = as_adjacency_matrix(aut_graph)
loc1 = which(colnames(aut_adj_matrix)=="Gabor Csardi") 
loc2 = which(colnames(aut_adj_matrix)=="Gábor Csárdi") 
loc11 = which(aut_adj_matrix[loc1,]>0)
loc21 = which(aut_adj_matrix[loc2,]>0)
aut_adj_matrix[loc2,loc11]=aut_adj_matrix[loc2,loc11]+aut_adj_matrix[loc1,loc11]
aut_adj_matrix[loc11,loc2]=aut_adj_matrix[loc11,loc2]+aut_adj_matrix[loc11,loc1]
aut_adj_matrix = aut_adj_matrix[-loc1, -loc1]

loc1 = which(colnames(aut_adj_matrix)=="Kirill Muller") 
loc2 = which(colnames(aut_adj_matrix)=="Kirill Müller") 
loc11 = which(aut_adj_matrix[loc1,]>0)
loc21 = which(aut_adj_matrix[loc2,]>0)
aut_adj_matrix[loc2,loc11]=aut_adj_matrix[loc2,loc11]+aut_adj_matrix[loc1,loc11]
aut_adj_matrix[loc11,loc2]=aut_adj_matrix[loc11,loc2]+aut_adj_matrix[loc11,loc1]
aut_adj_matrix = aut_adj_matrix[-loc1, -loc1]

######### aut_graph: weight = number of coauthored pkgs (collaboration intensity)
aut_graph_w = graph_from_adjacency_matrix(aut_adj_matrix,mode="undirected",weighted=TRUE,diag=FALSE)
aut_graph_nw = graph_from_adjacency_matrix(aut_adj_matrix,mode="undirected",weighted=NULL,diag=FALSE)



######### frequency figure
pkg_net_type = factor(pkg_net_type, levels = pkg_type) 
table(pkg_net_type)
ggplot(as.data.frame(pkg_net_type), aes(pkg_net_type)) +
    geom_bar() +
    labs(x = NULL) + 
    theme(panel.grid=element_blank())  


### 4.2 Measures of Influence 
pkgw_betw = betweenness(pkg_graph)
# Note that weights were not considered in calculating the betweenness centrality. This is because, in the \CRANpkg{igraph} package, weights are viewed as distances for calculating the betweenness, which does not align with the methodology of this study. 
pkgw_dg = degree(pkg_graph_w, mode = c("in"))  
pkgw_pgRk = page_rank(pkg_graph_w, weights = E(pkg_graph_w)$weight)
pkgw_eigen = eigen_centrality(pkg_graph_w, weights = E(pkg_graph_w)$weight) #directed = TRUE, 


autw_betw = betweenness(aut_graph_nw) # weight = distance, so no weight was include
autw_dg = degree(aut_graph_w)
autw_pgRk = page_rank(aut_graph_w, weights = E(aut_graph_w)$weight)
autw_eigen = eigen_centrality(aut_graph_w, weights = E(aut_graph_w)$weight)

## sort
pkg_rank_dg = sort(pkgw_dg, decreasing = TRUE)
aut_rank_dg = sort(autw_dg, decreasing = TRUE)
pkg_rank_betw = sort(pkgw_betw, decreasing = TRUE)
aut_rank_betw = sort(autw_betw, decreasing = TRUE)
pkg_rank_eigen = sort(pkgw_eigen[["vector"]], decreasing = TRUE) 
aut_rank_eigen = sort(autw_eigen[["vector"]], decreasing = TRUE) 
pkg_rank_pgRk = sort(pkgw_pgRk[["vector"]], decreasing = TRUE) 
aut_rank_pgRk = sort(autw_pgRk[["vector"]], decreasing = TRUE) 

## organize
table_pkg = array(NA, dim=c(50,4))
colnames(table_pkg) = c("In-degree", "Betweenness",  
 "Eigenvector", "PageRank")
table_pkg[,"In-degree"] = names(pkg_rank_dg[1:50])
table_pkg[,"Betweenness"] = names(pkg_rank_betw[1:50])
table_pkg[,"PageRank"] = names(pkg_rank_pgRk[1:50])
table_pkg[,"Eigenvector"] = names(pkg_rank_eigen[1:50])
write.csv(table_pkg, "network/50pkg_w_54321.csv")

table_aut = array(NA, dim=c(50,4))
colnames(table_aut) = c("In-degree", "Betweenness",
 "Eigenvector", "PageRank")
table_aut[,"In-degree"] = names(aut_rank_dg[1:50])
table_aut[,"Betweenness"] = names(aut_rank_betw[1:50])
table_aut[,"PageRank"] = names(aut_rank_pgRk[1:50])
table_aut[,"Eigenvector"] = names(aut_rank_eigen[1:50])
write.csv.utf8.BOM(table_aut, "network/50aut_w.csv") 


### 4.3 Sensitivity Analysis
# weight: 321
pkg_net_type = E(pkg_graph)$type
pkg_net_type_num321 = pkg_net_type
pkg_type = c("depends", "imports", "suggests", "linking_to", "enhances")
pkg_type_num321 = c(3,2,1,1,1)
for (typei in 1:length(pkg_type)){
    loc = which(pkg_net_type==pkg_type[typei])
    pkg_net_type_num321[loc] = pkg_type_num321[typei]
}
pkg_graph_w321 = pkg_graph %>% set_edge_attr("weight", value = pkg_net_type_num321)

pkgw_321_betw = betweenness(pkg_graph)
pkgw_321_dg = degree(pkg_graph_w321, mode = c("in"))  
pkgw_321_pgRk = page_rank(pkg_graph_w321, weights = E(pkg_graph_w321)$weight)
pkgw_321_eigen = eigen_centrality(pkg_graph_w321, weights = E(pkg_graph_w321)$weight)



# no weight



pkgw_1_betw = betweenness(pkg_graph)
pkgw_1_dg = degree(pkg_graph, mode = c("in"))  
pkgw_1_pgRk = page_rank(pkg_graph)
pkgw_1_eigen = eigen_centrality(pkg_graph)



score_54321 = cbind(pkgw_dg, pkgw_betw, pkgw_eigen[["vector"]], pkgw_pgRk[["vector"]])
score_321 = cbind(pkgw_321_dg, pkgw_321_betw, pkgw_321_eigen[["vector"]], pkgw_321_pgRk[["vector"]])
score_1 = cbind(pkgw_1_dg, pkgw_1_betw, pkgw_1_eigen[["vector"]], pkgw_1_pgRk[["vector"]])
for (i in 1:ncol(score_1)){
    cat(cor(score_54321[,i], score_321[,i]))
    cat('\t')
    cat(cor(score_54321[,i], score_1[,i]))
    cat('\n')
}



### 4.4 Select Important Packages and Authors
# Packages
imp_pkg = NULL
for(i in 1:nrow(table_pkg)){
  for(j in 1:ncol(table_pkg)){
    pkg_temp = as.character(table_pkg[i,j])
    loc = which(table_pkg==pkg_temp)
    if(length(loc)>1){
      if(length(which(imp_pkg==pkg_temp))==0){
        imp_pkg = c(imp_pkg, pkg_temp)
      }
    }
  }
}


imp_pkg2 = array(NA, dim=c(length(imp_pkg),3))
imp_pkg2[,1] = imp_pkg
colnames(imp_pkg2) = c('pkg','title','mt')
for(i in 1:length(imp_pkg)){
  imp_pkg2[i,'pkg'] = imp_pkg[i]
  loc = which(cran[,'Package']==imp_pkg[i])
  temp = try(cran[loc,'Title'])
  if(length(temp)>0){
    imp_pkg2[i,'title'] = temp
  }
  temp = try(cran[loc,'Maintainer'])
  if(length(temp)>0){
    imp_pkg2[i,'mt'] = split_mt(temp)$name 
  }
}
write.csv.utf8.BOM(imp_pkg2,'network/imp_pkg2_54321.csv')
imp_pkg2


# Authors

## important authors (identified by at least two indexes) 
imp_aut = NULL
for(i in 1:nrow(table_aut)){
  for(j in 1:ncol(table_aut)){
    aut_temp = as.character(table_aut[i,j])
    loc = which(table_aut==aut_temp)
    if(length(loc)>1){
      if(length(which(imp_aut==aut_temp))==0){
        imp_aut = c(imp_aut, aut_temp)
      }
    }
  }
}

imp_aut


# package sub-graph
library(wordcloud)
colors <- colorspace::diverge_hcl(10, c = 100, l = c(50, 100), power = 1)
pkg_graph_w_v = pkg_graph_w %>% set_vertex_attr("indegree", value = pkgw_dg)
pkg_sub_graph = subgraph(pkg_graph_w_v,names(pkg_rank_dg)[1:15]) 

pkg_net_type = E(pkg_sub_graph)$type
pkg_net_type_color = pkg_net_type
pkg_type = c("depends", "imports", "suggests", "linking_to", "enhances")
pkg_type_color = c(brewer.pal(8,"Blues")[7],brewer.pal(8,"Blues")[4],brewer.pal(8,"Blues")[2])


for (typei in 1:length(pkg_type)){
    loc = which(pkg_net_type==pkg_type[typei])
    pkg_net_type_color[loc] = pkg_type_color[typei]
}
pkg_net_type_label = rep(NA,length(pkg_net_type))
for(typei in 1:length(pkg_type)){ # select five labels as example
    loc = which(pkg_net_type==pkg_type[typei])[1]
    pkg_net_type_label[loc] = pkg_type[typei]
}


pkg_sub_graph = pkg_sub_graph %>% 
    set_edge_attr("color", value = pkg_net_type_color) %>% 
    set_edge_attr("label", value = pkg_net_type_label) 


plot.igraph(pkg_sub_graph, 
    edge.color = E(pkg_sub_graph)$color,
    edge.width = as.numeric(E(pkg_sub_graph)$weight),
    edge.label = E(pkg_sub_graph)$label,
    edge.arrow.size = 1,
    edge.curved = .1,
    vertex.color = colors[5],
    vertex.frame.color=colors[1],
    vertex.label.color=colors[1],
    layout=layout.circle,
    vertex.size = (V(pkg_sub_graph)$indegree)/120,
    vertex.label.cex = 1.6)




# author sub-graph
colors <- colorspace::diverge_hcl(10, c = 100, l = c(50, 100), power = 1)
aut_graph_w_v = aut_graph_w %>% set_vertex_attr("degree", value = autw_dg)
aut_sub_graph = subgraph(aut_graph_w_v,names(aut_rank_dg)[1:15]) #imp_aut2[,1]
aut_type_color = c(brewer.pal(8,"Blues")[4])


plot.igraph(aut_sub_graph, 
    edge.color = aut_type_color,
    edge.width = E(aut_sub_graph)$weight,
    edge.arrow.size = 1,
    edge.curved = .1,
    vertex.color = colors[5],
    vertex.frame.color=colors[1],
    vertex.label.color=colors[1],
    layout=layout.circle,
    vertex.size = (V(aut_sub_graph)$degree)/20,
    vertex.label.cex = 1.6)



# heatmap of author collaboration
 
library(corrplot)
library(pheatmap)
loc=NULL
for(i in 1:length(imp_aut)){
  loc = c(loc, which(colnames(aut_adj_matrix)==imp_aut[i]))
}
aut_corsub_matrix = as.matrix(aut_adj_matrix[loc,loc])


pheatmap(aut_corsub_matrix,col=c("white",colorRampPalette(brewer.pal(8, "Blues"))(170)[80:170]),
    angle_col="45",
    show_colnames = F,
    fontsize=12,
    treeheight_row = 0, treeheight_col = 0)



### 4.6 Correlation among importance indexes and downloads
library(cranlogs)
pkg_down_year = array(NA, dim = c(nrow(cran),2))
colnames(pkg_down_year) = c("pkg", "downloads")
pkg_down_year[,"pkg"] = cran[,"Package"]

for(pkgi in 1:nrow(pkg_down_year)){ #6402
    cat(paste0(pkgi,"-",pkg_down_year[pkgi, "pkg"],"\n"))
    pkg_down_year[pkgi, "downloads"] = sum(cran_downloads(pkg_down_year[pkgi, "pkg"], from = "2021-11-01", to = "2022-10-31")$count)
}

pkg_down_year = sort(pkg_down_year, decreasing = TRUE)
write.csv(pkg_down_year,"pkg_down_year.csv",row.names=F)




pkg_down_year = read.csv('pkg_down_year.csv')
pkg_down_year_rank = as.numeric(pkg_down_year[,2])
names(pkg_down_year_rank) = pkg_down_year[,1] 

pkg_all_index = cbind(pkgw_dg, pkgw_betw, pkgw_eigen[["vector"]], pkgw_pgRk[["vector"]])
colnames(pkg_all_index) = c('dg','bet','eigen','pg')
pkg_down_rank_year_n = pkgw_dg
no_down = NULL
for(i in 1:length(pkg_down_rank_year_n)){
  loc = which(names(pkg_down_year_rank) == names(pkgw_dg)[i])
  if(length(loc)!=0){
    pkg_down_rank_year_n[i] = pkg_down_year_rank[loc]
  }else{
    no_down = c(no_down, names(pkgw_dg)[i])
  }
}

for (i in 1:4){
    cat(cor(pkg_down_rank_year_n, pkg_all_index[,i]))
    cat('\n')
}





## 5 Bipartite Network


### 5.1 Build bipartite network and set edges (weights)
basepkg = c("base", "boot", "class", "cluster", "codetools", "compiler", "datasets",
    "foreign", "graphics", "grDevices", "grid", "KernSmooth", "lattice", "MASS",
    "Matrix", "methods", "mgcv", "nlme", "nnet", "parallel", "rpart", "spatial", 
    "splines", "stats", "stats4", "survival", "tcltk", "tools", "utils")

bi_matrix_w = array(0, dim = c(length(V(aut_graph)$name), length(V(pkg_graph_w)$name)))
colnames(bi_matrix_w) = V(pkg_graph_w)$name
rownames(bi_matrix_w) = V(aut_graph)$name

for (auti in 1:nrow(bi_matrix_w)){
    aut_pkg = V(aut_graph)$package[[auti]]
    aut_pkg_num = length(aut_pkg)
    for (aut_pkgi in 1:aut_pkg_num){
        pkgi = which(colnames(bi_matrix_w)==aut_pkg[aut_pkgi])
        #if(aut_pkg[aut_pkgi] == colnames(bi_matrix_w)[pkgi]){
        bi_matrix_w[auti, pkgi] = 1
        if(V(aut_graph)$name[auti] == V(pkg_graph_w)$maintainer[pkgi]){
            bi_matrix_w[auti, pkgi] = 3
        }
    }
}

# base r packages
locRcore = which(rownames(bi_matrix_w) == "R Core")
for (pkgi in 1:length(basepkg)){
    locpkg = which(colnames(bi_matrix_w) == basepkg[pkgi])
    if(is.na(V(pkg_graph_w)$maintainer[locpkg])){
            bi_matrix_w[locRcore, locpkg] = 3
    }
}
bi_matrix_w0 = bi_matrix_w


loc1 = which(rownames(bi_matrix_w)=="Kirill Muller") 
loc2 = which(rownames(bi_matrix_w)=="Kirill Müller") 
loc11 = which(bi_matrix_w[loc1,]>0)
loc21 = which(bi_matrix_w[loc2,]>0)
bi_matrix_w[loc2,loc11]=bi_matrix_w[loc2,loc11]+bi_matrix_w[loc1,loc11]
bi_matrix_w = bi_matrix_w[-loc1, ]

loc1 = which(rownames(bi_matrix_w)=="Gabor Csardi") 
loc2 = which(rownames(bi_matrix_w)=="Gábor Csárdi") 
loc11 = which(bi_matrix_w[loc1,]>0)
loc21 = which(bi_matrix_w[loc2,]>0)
bi_matrix_w[loc2,loc11]=bi_matrix_w[loc2,loc11]+bi_matrix_w[loc1,loc11]
bi_matrix_w = bi_matrix_w[-loc1, ]

bi_graph_w = graph.incidence(bi_matrix_w, weighted = T) #graph_from_incidence_matrix






### 5.2 Visulization

loc_sub1 = loc_sub2 = NULL
for (i in 1:15){
    loc_sub1 = c(loc_sub1, which(V(aut_graph)$name==names(aut_rank_dg)[i]))
    loc_sub2 = c(loc_sub2, which(V(pkg_graph_w)$name==names(pkg_rank_dg)[i]))
    
}
bi_matrix_sub = bi_matrix_w[loc_sub1, loc_sub2]

bi_graph_w_sub = graph.incidence(bi_matrix_sub, weighted = T)  


V(bi_graph_w_sub)$color <- V(bi_graph_w_sub)$type
V(bi_graph_w_sub)$color=gsub("FALSE","#FAEBD7",V(bi_graph_w_sub)$color)
V(bi_graph_w_sub)$color=gsub("TRUE",colors[5],V(bi_graph_w_sub)$color)
plot(igraph::simplify(bi_graph_w_sub), edge.color="#E9967A", edge.width=E(bi_graph_w_sub)$weight, layout=LO, vertex.size = 6, vertex.label.cex = 1.4,vertex.label.color = "black", vertex.label.family="Arial")



### 5.3 Centrality
library(bipartite)  
library(birankr) 

#bi_matrix_w
bi_adj = as_adjacency_matrix(bi_graph_w) #as_edgelist
bi_edge = as_edgelist(bi_graph_w)
bi_e_weight = E(bi_graph_w)$weight


bi_edge_dt_w = data.table(cbind(bi_edge,bi_e_weight))



bi_cohits_w_0 = br_cohits(bi_edge_dt_w, weight_name = "bi_e_weight")
bi_cohits_w = bi_cohits_w_0[,'rank']
names(bi_cohits_w) = bi_cohits_w_0[,1]
head(sort(bi_cohits_w,T))

bi_birank_w_0 = br_birank(bi_edge_dt_w, weight_name = "bi_e_weight")
bi_birank_w = bi_birank_w_0[,'rank']
names(bi_birank_w) = bi_birank_w_0[,1]
head(sort(bi_birank_w,T))


# Relationship between cohits and birank
cor(bi_cohits_w, bi_birank_w)

bi_cohits_w_50 = sort(bi_cohits_w,T)[1:50]
bi_birank_w_50 = sort(bi_birank_w,T)[1:50]


# Relationships between birank and other indexes

aut_birank = autw_dg
for(i in 1:length(aut_birank)){
    loc = which(names(bi_birank_w) == names(aut_birank)[i])
    aut_birank[i] = bi_birank_w[loc]
}
aut_allindex = cbind(autw_dg, autw_betw, autw_eigen[["vector"]], autw_pgRk[["vector"]], aut_birank)
colnames(aut_allindex) = c('dgree', 'betw', 'eigen', 'pgRk',  'BiRank')

for (i in 1:4){
    cat(colnames(aut_allindex)[i])
    cat('\t')
    cat(cor(aut_allindex[,i], aut_allindex[,5]))
    cat('\n')
}




# Visualization
pkg_download = sort(pkg_down_year_rank, decreasing = TRUE)
downloads_sorted = cbind(names(pkg_download), as.numeric(pkg_download) )

library(latex2exp)

downloads_50 = tibble(package = downloads_sorted[1:50,1],downloads = as.numeric(downloads_sorted[1:50,2])/1000000)
downloads_50_plot = downloads_50 %>%
    mutate(package = reorder(package, downloads)) %>%
    ggplot(aes(downloads,package)) +
    labs(y = "Package", x = TeX("Downloads(*$10^6$)")) +
    theme(panel.grid=element_blank())  +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5))+
    theme(axis.title.x = element_text(face = 'bold'),  axis.title = element_text(face = 'bold'), text=element_text(size=12))+
    geom_col() 
downloads_50_plot





