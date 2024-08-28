twitter=read.csv("C:\\users\\hp\\OneDrive\\Documents\\Books\\twitter_training.csv")
dim(twitter)
colnames(twitter)
str(twitter)
summary(twitter)
sum(is.na(twitter))
library("ggplot2")
ggplot(twitter,aes(x=Topic,fill=Topic))+geom_bar()+theme(legend.position = "none")+coord_flip()+labs(title =" Topic")
data1=data.frame("cat1"=c("Positive","Neutral","Negative","Irrelevant"),"val1"=c(sum(twitter$Sentiment=="Positive"),sum(twitter$Sentiment=="Nutral"),sum(twitter$Sentiment=="Negative"),sum(twitter$Sentiment=="Irrelevant")))
slices1=c(sum(twitter$Sentiment=="Positive"),sum(twitter$Sentiment=="Neutral"),sum(twitter$Sentiment=="Negative"),sum(twitter$Sentiment=="Irrelevant"))
frac1=(slices1/sum(slices1))
ymax1=cumsum(frac1)
ymin1=c(0,head(ymax1,n=-1))
labposi1=(ymax1+ymin1)/2
labls1=paste0(c("Positive","Neutral","Negative","Irrelevant"),"\n value:",paste(round(frac1*100)),"%",sep="")
ggplot(data1,aes(ymax=ymax1,ymin=ymin1,xmax=4,xmin=3,fill=cat1))+geom_rect()+geom_label(x=3.5,aes(y=labposi1,label=labls1),size=3)+coord_polar(theta="y")+xlim(c(2,4))+theme_void()+theme(legend.position = "none")+labs(title=" Sentiment")+scale_fill_manual(values=c("yellow","purple","red","lightblue"))
ggplot(twitter,aes(Topic,Sentiment))
ggplot(twitter,aes(x=Topic,y=Sentiment,fill=Sentiment))+geom_bar(aes(fill=Sentiment),stat="identity",positive="dodge")+theme(axis.text.x=element_text(angle=90))
sort(table(twitter$Topic),decreasing = TRUE)
df=twitter[twitter$Topic=="Google",]
ggplot(df,aes(x=Sentiment,fill=Sentiment))+geom_bar()+scale_fill_manual(values=c("#999900","#3399FF","#CC6699","#CC0000"))+labs(title = "GOOGLE")
df=twitter[twitter$Topic=="Facebook",]
ggplot(df,aes(x=Sentiment,fill=Sentiment))+geom_bar()+scale_fill_manual(values=c("#33FF99","#336666","#CC6666","#663333"))+labs(title = "FACEBOOK")

df=twitter[twitter$Topic=="Microsoft",]
ggplot(df,aes(x=Sentiment,fill=Sentiment))+geom_bar()+scale_fill_manual(values=c("#999999","#FF3300","#003399","#990099"))+labs(title = "MICROSOFT")

hist(nchar(twitter$Text),col = "yellow",main="Histogram for the length of Text",xlab="Text Length",ylab="Frequency")

library("ggplot2")
library("reshape2")
da = data.frame("Topic" =twitter$Topic,"Sentiment"=twitter$Sentiment)
crosstab=table(da$Topic,da$Sentiment)
melt_crosstab = melt(crosstab)
ggplot(melt_crosstab, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color="white")+scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = median(melt_crosstab$value))+labs(title = "Heatmap of Topic vs Sentiment", x = "Sentiment",y = "Topic") +theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))

library("wordcloud2")
library("wordcloud")
library("tm")
corpus = paste(twitter$Text,collapse = "")
corpus = tolower(corpus)
corpus = removePunctuation(corpus)
corpus = removeNumbers(corpus)
corpus = removeWords(corpus, stopwords("en"))

corpus = Corpus(VectorSource(corpus))
dtm = TermDocumentMatrix(corpus)
m = as.matrix(dtm)
word_freqs = sort(rowSums(m), decreasing = TRUE)
word_freqs = data.frame(word = names(word_freqs), freq = word_freqs)

# Generate the word cloud
wordcloud2(data = word_freqs, size = 0.5, color = 'random-light', backgroundColor = 'white')


