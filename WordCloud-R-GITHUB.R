library(tm)
library(wordcloud)
require(spacyr)
spacy_initialize()

#################################################################################
#####  WordCloud function for text variables ####################################
#################################################################################

#performs POS tagging, one doc at a time
tagPOS <-  function(x) {
  res <- spacy_parse(x,tag=TRUE)
  #select words with types NNP and FW
  #(change here if you want other types, for example, JJ or NN)
  #check NLP dictionary for POS tagging with spacyr Package
  res <- res[which(res[,7] %in% c("NNP","FW") ),]
  paste(res[,"token"],collapse = " ")
}

wordcloudfunc <- function(data,textvar){
  
  res.cloud <- list()
  
  docs <- Corpus(VectorSource(enc2utf8(as.character(na.omit(data[,paste(textvar)])))))
  
  #clean special characters
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x)) #function to replace a pattern to white space using regex
  docs <- tm_map(docs, toSpace, "(RT|via)((?:\\b\\W*@\\w+)+)") #match rt or via
  docs <- tm_map(docs, toSpace, "@\\w+") #match @
  docs <- tm_map(docs, toSpace, "[ \t]{2,}") #match tabs
  docs <- tm_map(docs, toSpace, "[ |\n]{1,}") #match new lines
  docs <- tm_map(docs, toSpace, "^ ") #match white space at begenning
  docs <- tm_map(docs, toSpace, " $") #match white space at the end
  docs <- tm_map(docs, PlainTextDocument)
  
  #tagPOS feature
  i <- 0
  docs.new <- c()
  sapply(as.character(docs[[1]]), function(x){
    i <<- i + 1
    docs.new <<- c(docs.new,tagPOS(x))
    cat("Processing POS tagging of document: ",i,"\n")
  })
  
  #redo Corpus for tm package tfidf
  docs.new <- Corpus(VectorSource(enc2utf8(as.character(na.omit(docs.new)))))
  docs.new <- tm_map(docs.new, content_transformer(tolower))
  docs.new <- tm_map(docs.new, removePunctuation)
  docs.new <- tm_map(docs.new, toSpace, "http[[:alnum:]]*") #remove url from tweets
  docs.new <- tm_map(docs.new,removeWords,stopwords("english"))
  
  #redo Corpus for tm package tfidf
  docs <- docs.new
  
  #table with tfidf values
  tdm.tfidf <- TermDocumentMatrix(docs, control = list(weighting =
                                                   function(x)
                                                     weightTfIdf(x, normalize = TRUE)))
  m.tfidf <- as.matrix(tdm.tfidf)
  v.tfidf <- sort(rowSums(m.tfidf),decreasing=TRUE)
  
  #table with word frequencies
  tdm <- TermDocumentMatrix(docs)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
                                  
  #dataframe with both previous tables of results
  d <- data.frame(Variable=paste(textvar),word.freq = names(v),freq=v,word.tfidf=names(v.tfidf),weight.tf.idf=v.tfidf)
  
  #list with results
  res.cloud$df <- d
  
  #no random fenomena
  set.seed(1234)
 
  #return the list
  return(res.cloud)
}

#read csv with text cells
dat.df <- read.csv("<csv_file_name>.csv", sep=",")
d <<- wordcloudfunc(dat.df,"<text_var_name>")$df

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word.freq,
          col ="lightblue", main ="Most frequent words",
          ylab = "Word frequencies")
  
wordcloud(words = d$word.freq, freq = d$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))
  
wordcloud(words = d$word.tfidf, freq = d$weight.tf.idf, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))
  
