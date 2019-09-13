#whatsapp data 

# email any whatsapp chat without media to self 
# download on local machine folder

getwd()
setwd("F:/VLL")

library(ggplot2)
library(tm)
library(wordcloud)
library(syuzhet)

texts=readLines("Div_D_data.txt")
print(texts)


##used to remove emoji gsub("[^\x01-\x7F]", "", texts)
#convert text into documents 

docs=Corpus(VectorSource(texts))#tokenzing data
docs

trans=content_transformer(function(x,pattern)
  gsub(pattern ," ",x))
docs=tm_map(docs,trans,"/")
docs=tm_map(docs,trans,"@")
docs=tm_map(docs,trans,"\\|")
docs=tm_map(docs,content_transformer(tolower))
docs=tm_map(docs,removeNumbers)
docs=tm_map(docs,removeWords,stopwords("english"))
docs=tm_map(docs,removePunctuation)
docs=tm_map(docs,stripWhitespace)
docs=tm_map(docs,stemDocument)
# install pACKAGE SnowballC for execution of last statement
library(SnowballC)

#find frequency of word 
# first convert documents to matrix
dtm=TermDocumentMatrix(docs)
mat=as.matrix(dtm)
mat

#sort data alphabeticaly and count
v=sort(rowSums(mat),decreasing = TRUE)
print(v)

#take words and their count input is v
d=data.frame(word=names(v),freq=v)
head(d)

set.seed(1056)
?wordcloud
wordcloud(words = d$word,freq=d$freq,scale=c(4,.5),min.freq = 1,
          max.words = 200,random.order = FALSE,rot.per = 0.35,
          colors = brewer.pal(8,"Dark2"))
#rot.per radius of display 

#sentiment analysis library=>syuzhet
?get_nrc_sentiment  # to get sentiment values
texts
sentiment=get_nrc_sentiment(texts)
text=cbind(texts,sentiment)

print(text)
# get only sentiment cols c(2,11)=>gives only two sentiment cols so use c(2:11)
TotalSentiment=data.frame(colSums(text[,c(2:11)]))

names(TotalSentiment)="count"

TotalSentiment=cbind("sentiment" = rownames(TotalSentiment),
                     TotalSentiment)

print(TotalSentiment)

names(TotalSentiment)


library(ggplot2)
ggplot(data=TotalSentiment,aes(x=sentiment,y=count))+
  geom_bar(aes(fill=sentiment),stat = "identity") + 
  theme(legend.position="none") + xlab("sentiment") + 
  ylab("total count") + 
  ggtitle("Total sentiment score")
