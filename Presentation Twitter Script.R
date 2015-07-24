#_Install Rgraphviz_# 
##source("http://bioconductor.org/biocLite.R")
##biocLite("Rgraphviz")
#__Load Libraries__#
library("twitteR")
library("tm")
library("wordcloud")
#__Access Twitter__#
Consumer_Key=''                  #Enter information from dev.twitter.com
Consumer_Secret=''
Access_Token=''
Access_Token_Secret=''
setup_twitter_oauth(Consumer_Key, Consumer_Secret, Access_Token, Access_Token_Secret)#Connect to Twitter

#__Creates Corpus from Tweets__# 
roch=strip_retweets(searchTwitter("rochester", lang="en",n=1000))                    #Searches for last 1000 tweets
roch_frame <- do.call("rbind", lapply(roch, as.data.frame))                          #Converts to data frame by row
roch_text_corpus <- Corpus(VectorSource(roch_frame$text))                            #Coverts to VCorpus

#__Data Normalization__#
myStopwords <- c(stopwords('english'), "available", "via","r","rt",'new',"amp",'rochester','go','us','man','im','Rochester','will','rochest','ny','roc','get','come')
removeURLs <- function(x) gsub("http[[:alnum:]]*","",x)
removenalnum <- function(x) gsub("[^[:alnum:][:blank:]]", "", x, ignore.case = TRUE)
roch_text_corpus <- tm_map(roch_text_corpus, content_transformer(removePunctuation)) #Removes Punctuation
roch_text_corpus <- tm_map(roch_text_corpus, removeNumbers)                          #Removes Numbers
roch_text_corpus <- tm_map (roch_text_corpus, content_transformer(removeURLs))       #Removes URLS
roch_text_corpus <- tm_map (roch_text_corpus, content_transformer(removenalnum))     #Removes non-alphanumeric
roch_text_corpus <- tm_map (roch_text_corpus, content_transformer(tolower))          #Converts to lower case
roch_text_corpus <- tm_map(roch_text_corpus, function(x)removeWords(x,myStopwords))  #Removes Stop words

#__Stemming the words__#
roch_text_corpus <-tm_map(roch_text_corpus, stemDocument)                            #Stems Corpus

#__Create Term Document Matrix__# 
roch_text_corpus.tdm <-TermDocumentMatrix(roch_text_corpus,                          #Creates TDM with TF-IDF weight
                                          control=list(wordLengths=c(1,Inf),
                                                       weighting = function(x) weightTfIdf(x, normalize = FALSE)))


#__Create a Word Cloud__#
roch_text_corpus_matrix <- as.matrix(roch_text_corpus.tdm)                            #Makes Matrix from TDM
wordFreq.sort <- sort(rowSums(roch_text_corpus_matrix),decreasing=T)                  #Sums weights for each and sorts
set.seed(1234)                                                                        #Sets Seed to 1234
word.cloud <- wordcloud(words=names(wordFreq.sort), freq=wordFreq.sort, min.freq=62,  #Creates WordCloud with the first 60 words with frequency more than 62
                        max.words=60, random.order=F)   


#__Creates a H.Cluster__#
roch_text_corpus.tdm2 <-removeSparseTerms(roch_text_corpus.tdm, sparse=.97)           #Creates TDM removing 5% sparcest terms
roch_text_corpus.matrix2 <- as.matrix(roch_text_corpus.tdm2)                          #Converts TDM to matrix
distMatrix <- dist(scale(roch_text_corpus.matrix2))                                   #Creates a distance matrix
roch_text_corpus.fit<-hclust(distMatrix,method="ward.D")                              #Using Ward method create a hcluster
plot(roch_text_corpus.fit, cex=.9, hang=-1, main="Cluster")                           #Plot the Hcluster
rect.hclust(roch_text_corpus.fit, k=3)                                                #Create 4 Clusters
#__Creates Correlation Network__#
plot(roch_text_corpus.tdm2,                                                           #Creates and Plots a network
     terms=findFreqTerms(roch_text_corpus.tdm2, lowfreq=100)[1:13],
     corThreshold=0.5)

#__Comparison and Commonality Cloud__#

Geneseo_tweets = userTimeline("SUNYGeneseo", n=1000)                                  # SUNY Geneseo tweets
UofR_tweets = userTimeline("UofR", n=1000)                                            # University of Rochester tweest
Geneseo_txt = sapply(Geneseo_tweets, function(x) x$getText())
UofR_txt = sapply(UofR_tweets, function(x) x$getText())
Geneseo = paste(Geneseo_txt, collapse=" ")
UofR= paste(UofR_txt, collapse=" ")
Total = c(Geneseo, UofR)                                                               #Put everything in a single vector
Total_text_corpus <- Corpus(VectorSource(Total))                                       #Coverts to VCorpus
myStopwords <- c(stopwords('english'),'amp')                                           #Stopwords
Total_text_corpus <- tm_map(Total_text_corpus, content_transformer(removePunctuation)) #Removes Punctuation
Total_text_corpus <- tm_map(Total_text_corpus, removeNumbers)                          #Removes Numbers
Total_text_corpus <- tm_map (Total_text_corpus, content_transformer(removeURLs))       #Removes URLS
Total_text_corpus <- tm_map (Total_text_corpus, content_transformer(removenalnum))     #Removes non-alphanumeric
Total_text_corpus <- tm_map (Total_text_corpus, content_transformer(tolower))          #Converts to lower case
Total_text_corpus <- tm_map(Total_text_corpus, function(x)removeWords(x,myStopwords))  #Removes Stop words
tdm = TermDocumentMatrix(Total_text_corpus)                                            #Create term-document matrix
tdm = as.matrix(tdm)                                                                   #Convert as matrix

# add column names
colnames(tdm) = c("Geneseo", "UofR")                                                   #Give Column names to matrix
comparison.cloud(tdm, random.order=FALSE,                                              #Comparison Cloud
                 colors = c("#00B2FF", "red"),
                 title.size=1.5, max.words=200)
commonality.cloud(tdm, random.order=FALSE,                                             #Commonality Cloud
                  colors = brewer.pal(8, "Dark2"),
                  title.size=1.5)