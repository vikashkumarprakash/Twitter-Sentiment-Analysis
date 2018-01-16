install.packages(c("devtools", "rjson", "bit64", "httr"))
library(devtools)
library(rjson)
library(bit64)
install_github("twitteR", username="geoffjentry")
library(twitteR)
library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(httr)
library(wordcloud)
library(RCurl)
library(syuzhet)
##istalling sentiment package as this package is removed from Cran so follow below steps
install.packages("tm",dependencies = TRUE)
install.packages("ftp//cran.r.project.org/pub/R/src/contrib/Archive/Rstem_0.4-tar.gz",repos = NULL,type="source",dependencies = TRUE)
install.packages("https://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz",repos = NULL,type="source",dependencies = TRUE)
library(SentimentAnalysis)

##
oauth_endpoint(authorize="https://api.twitter.com/oauth",
              access = "https://api.twitter.com/oauth/access_token")

#connect to API

download.file(url='http://curl.haxx.se/ca/cacert.pem', destfile='cacert.pem')
reqURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'

###Twitter Application
consumerKey="THWQ4dArRsSmX8B93QbgL6nA9"
consumerSecret ="jOfgZkHd99pMMsHNTepduD2leB0n6FFzjzDkKcODuXLCynVyKU"
accesstoken="1687355989-lTzl0j2rWAfjnyYBfMvhwd2kQc8buoOscXwrufa"
accesssecret="xmPwgSSCSDuEL7vcJYJsW6UZTRzAU3Gql9H9nKN1eiytQ"

Cred<-OAuthFactory$new(consumerKey=consumerKey,
                       consumerSecret=consumerSecret,
                       requestURL=reqURL,
                       accessURL=accessURL,
                       authURL=authURL)


#not working
Cred$handshake(cainfo = system.file('CurlSSL','cacert.pem',package ='RCurl'))

#There is URL in the Consol.You need to go to it,get code and enter it on Console


#### Authorization PIN -DYNAMIC

save(Cred,file='twitter authentication.Rdata')
load('twitter authentication.Rdata')

setup_twitter_oauth
#Once you launch the code First time you can start from this line in
#the future libraries should be connected
library(base64enc)
setup_twitter_oauth(consumer_key = consumerKey,consumer_secret = consumerSecret,access_token = accesstoken,access_secret = accesssecret)
setup_twitter_oauth(consumerKey,consumerSecret,accesstoken,accesssecret)
sessionInfo()
##********************Step3
some_tweets<- searchTwitter("Modi",n=10000,since = "2016-09-01",lang = "en")

##if you want to see the someone particular twitter account tweets
account.timeline<-userTimeline(account,n=1000,includeRts = TRUE)
#Expore tweets
length.some_tweets<-length(some_tweets)
some_tweets.df<-ldply(some_tweets,function(t)t$toDataFrame())
write.csv(some_tweets.df,"Modi_tweets.csv")
class(some_tweets.df)
#get the text
some_txt<-sapply(some_tweets,function(x) x$getText())
#cleaning -remove people name,RT text etc.
some_txt1<-gsub("(RT|via)((?:\\b\\W*@\\w+)+)*)","",some_txt)
                
#Cleaning the html links
some_txt2<-gsub("http[^[:blank:]]+","",some_txt1)
#Cleaning the people name
some_txt3<-gsub("@\\w+","",some_txt2)
#remove the Punctuations
some_txt4<-gsub("[[:punct:]]"," ",some_txt3)
some_txt5<-gsub("[^[:alnum:]]"," ",some_txt4)
#Export the clean tweet in the excell
write.csv(some_txt5,"Tweet ON Modi.csv")
#Create wordcorpus and cleaning
library(tm)

some_txt6<-Corpus(VectorSource(some_txt5))
some_txt6<-tm_map(some_txt6,removePunctuation)                  
some_txt6<-tm_map(some_txt6,content_transformer(tolower)) 
some_txt6<-tm_map(some_txt6,removeWords,stopwords("english")) 
some_txt6<-tm_map(some_txt6,stripWhitespace) 
#Building worldcloud
pal<-brewer.pal(8,"Dark2")

wordcloud(some_txt6,min.freq = 5,max.words = Inf,width=1000,height=1000,random.order = FALSE,colors = pal)
#Sentiment Analysis
#how the function works
class(some_txt5)
class(some_txt6)
mysentiment<-get_nrc_sentiment(some_txt5)
SentimentScore<-data.frame(colSums(mysentiment[,]))
names(SentimentScore)<-"Score"
SentimentScore<-cbind("sentiment" = rownames(SentimentScore),SentimentScore)
rownames(SentimentScore)<-NULL

#plot
ggplot(data=SentimentScore,aes(x=sentiment,y = Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position = "none")+
  xlab("Sentiment") + ylab("Score") + ggtitle("People Sentiment Score based on Tweets where Modi")

ggplot(data=SentimentScore,aes(x=sentiment,y = Score))+
  geom_col(aes(fill=sentiment),stat = "identity")+
  theme(legend.position = "none")+
  xlab("Sentiment") + ylab("Score") + ggtitle("People Sentiment Score based on Tweets where Modi")

#############################Arvind Kejriwal
Kejriwal_tweets<- searchTwitter("Kejriwal",n=10000,since = "2016-09-01",lang = "en")
#Expore tweets
length.Kejriwal_tweets<-length(Kejriwal_tweets)
Kejriwal_tweets.df<-ldply(Kejriwal_tweets,function(t)t$toDataFrame())
write.csv(Kejriwal_tweets.df,"Kejriwal_tweets.csv")
class(Kejriwal_tweets.df)
#get the text
Kejriwal_tweets_txt<-sapply(Kejriwal_tweets,function(x) x$getText())
#cleaning -remove people name,RT text etc.
Kejriwal_tweets_txt1<-gsub("(RT|via)((?:\\b\\W*@\\w+)+)*)","",Kejriwal_tweets_txt)

#Cleaning the html links
Kejriwal_tweets_txt2<-gsub("http[^[:blank:]]+","",Kejriwal_tweets_txt1)
#Cleaning the people name
Kejriwal_tweets_txt3<-gsub("@\\w+","",Kejriwal_tweets_txt2)
#remove the Punctuations
Kejriwal_tweets_txt4<-gsub("[[:punct:]]"," ",Kejriwal_tweets_txt3)
Kejriwal_tweets_txt5<-gsub("[^[:alnum:]]"," ",Kejriwal_tweets_txt4)
#Export the clean tweet in the excell
write.csv(Kejriwal_tweets_txt5,"Tweet ON Kejriwal.csv")
#Create wordcorpus and cleaning
library(tm)

Kejriwal_tweets_txt6<-Corpus(VectorSource(Kejriwal_tweets_txt5))
Kejriwal_tweets_txt6<-tm_map(Kejriwal_tweets_txt6,removePunctuation)                  
Kejriwal_tweets_txt6<-tm_map(Kejriwal_tweets_txt6,content_transformer(tolower)) 
Kejriwal_tweets_txt6<-tm_map(Kejriwal_tweets_txt6,removeWords,stopwords("english")) 
Kejriwal_tweets_txt6<-tm_map(Kejriwal_tweets_txt6,stripWhitespace) 
#Building worldcloud
pal<-brewer.pal(8,"Dark2")

wordcloud(Kejriwal_tweets_txt6,min.freq = 5,max.words = Inf,width=1000,height=1000,random.order = FALSE,colors = pal)
#Sentiment Analysis
#how the function works
class(some_txt5)
class(some_txt6)
Kejriwalsentiment<-get_nrc_sentiment(Kejriwal_tweets_txt5)
SentimentScore<-data.frame(colSums(Kejriwalsentiment[,]))
names(SentimentScore)<-"Score"
SentimentScore<-cbind("sentiment" = rownames(SentimentScore),SentimentScore)
rownames(SentimentScore)<-NULL

#plot
ggplot(data=SentimentScore,aes(x=sentiment,y = Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position = "none")+
  xlab("Sentiment") + ylab("Score") + ggtitle("People Sentiment Score based on Tweets where Kejriwal")


#####################26Sep2017###which topics represent negtivity and positivity 

library(SnowballC)
library(RColorBrewer)
Kejriwal_tweets_txt6<-Corpus(VectorSource(Kejriwal_tweets_txt5))
##create a plain text
Kejriwal_tweets_corpus<-Kejriwal_tweets_txt6
Kejriwal_tweets_corpus<-tm_map(Kejriwal_tweets_corpus,PlainTextDocument)
Kejriwal_tweets_corpus <- tm_map(Kejriwal_tweets_corpus, stemDocument) # stem words
writeLines(strwrap(Kejriwal_tweets_corpus[[19]]$content, 100))



stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

Kejriwal_tweets_corpus <- lapply(Kejriwal_tweets_corpus, stemCompletion2, dictionary=Kejriwal_tweets_txt6)
Kejriwal_tweets_corpus <- Corpus(VectorSource(Kejriwal_tweets_corpus))
writeLines(strwrap(Kejriwal_tweets_corpus[[190]]$content, 60))

wordFreq <- function(corpus, word) {
  results <- lapply(corpus,
                    function(x) { grep(as.character(x), pattern=paste0("\\<",word)) }
  )
  sum(unlist(results))
}
#n.miner <- wordFreq(Kejriwal_tweets_corpus, "miner")
#n.mining <- wordFreq(Kejriwal_tweets_corpus, "mining")
#cat(n.miner, n.mining)
# replace oldword with newword
replaceWord <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub),
         pattern=oldword, replacement=newword)
}

tdm <- TermDocumentMatrix(Kejriwal_tweets_corpus,
                          control = list(wordLengths = c(1, Inf)))
tdm
# inspect frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq = 550))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 550)
df <- data.frame(term = names(term.freq), freq = term.freq)

ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
# colors
pal <- brewer.pal(9, "BuGn")[-(1:4)]

wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F, colors = pal)
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
library(graph)
library(Rgraphviz)
library(RGraphics)
plot(tdm, term = freq.terms, corThreshold = 0.1, weighting = T)


#####27Sep2017******************######################
##if you want to see the someone particular twitter account tweets
account<-"MODI"
account.timeline<-userTimeline(account,n=1000,includeRts = TRUE)
TrailDF<-twListToDF(account.timeline)
file.timeline<-paste(account,"MODI.csv",sep="")
write.csv(TrailDF,file.timeline)