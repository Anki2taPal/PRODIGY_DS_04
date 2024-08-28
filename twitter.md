---
date: 2024-08-21
output: html_document
title: Twitter
---

`{r setup, include=FALSE} knitr::opts_chunk$set(echo = TRUE)`

# Twitter Sentiment Analysis Data-set

This is an entity-level sentiment analysis data-set of twitter. Given a
message and an entity, the task is to judge the sentiment of the message
about the entity. There are three classes in this data-set: Positive,
Negative and Neutral. We regard messages that are not relevant to the
entity (i.e. Irrelevant) as Neutral.

``` {r,comment=na,echo=false}
library("rmarkdown")
twitter=read.csv("C:\\users\\hp\\OneDrive\\Documents\\Books\\twitter_training.csv")
paged_table(twitter)
```

# Dimension of the data-set

``` {r,comment=na,echo=false}
dim(twitter)
```

# Variables in the data-set

``` {r,comment=na,echo=false}
colnames(twitter)
```

# Description of the variables in the dataset

``` {r,comment=na,echo=false}
str(twitter)
```

# Summary of the data-set

``` {r,comment=na,echo=false}
summary(twitter)
```

# Checking for the missing values

``` {r,comment=na,echo=false}
sum(is.na(twitter))
```

### **Column diagram for differnt topics vs count**

``` {r,comment=na,echo=false}
library("ggplot2")
ggplot(twitter,aes(x=Topic,fill=Topic))+geom_bar()+theme(legend.position = "none")+coord_flip()+labs(title =" Topic")
```

### **Donut chart for Sentiment (Positive,Neutral,Negative,Irrelevant)**

``` {r,comment=na,echo=false}
data1=data.frame("cat1"=c("Positive","Neutral","Negative","Irrelevant"),"val1"=c(sum(twitter$Sentiment=="Positive"),sum(twitter$Sentiment=="Nutral"),sum(twitter$Sentiment=="Negative"),sum(twitter$Sentiment=="Irrelevant")))
slices1=c(sum(twitter$Sentiment=="Positive"),sum(twitter$Sentiment=="Neutral"),sum(twitter$Sentiment=="Negative"),sum(twitter$Sentiment=="Irrelevant"))
frac1=(slices1/sum(slices1))
ymax1=cumsum(frac1)
ymin1=c(0,head(ymax1,n=-1))
labposi1=(ymax1+ymin1)/2
labls1=paste0(c("Positive","Neutral","Negative","Irrelevant"),"\n value:",paste(round(frac1*100)),"%",sep="")
ggplot(data1,aes(ymax=ymax1,ymin=ymin1,xmax=4,xmin=3,fill=cat1))+geom_rect()+geom_label(x=3.5,aes(y=labposi1,label=labls1),size=3)+coord_polar(theta="y")+xlim(c(2,4))+theme_void()+theme(legend.position = "none")+labs(title=" Sentiment")+scale_fill_manual(values=c("yellow","purple","red","lightblue"))
```

## **Bar chart for different levels of sentiment (Positive, Neutral, Negative, Irrelevant) vs. different topics**

``` {r,comment=na,echo=false}
ggplot(twitter,aes(x=Topic,y=Sentiment,fill=Sentiment))+geom_bar(aes(fill=Sentiment),stat="identity",positive="dodge")+theme(axis.text.x=element_text(angle=90))
```

### **We list the Twitter topics in decreasing order of their trending popularity.**

``` {r,comment=na,echo=false}
sort(table(twitter$Topic),decreasing = TRUE)
```

## **Sentiment Distribution for 'Google','Facebook','Microsoft' in Twitter **

``` {r,comment=na,echo=false}
df=twitter[twitter$Topic=="Google",]
ggplot(df,aes(x=Sentiment,fill=Sentiment))+geom_bar()+scale_fill_manual(values=c("#999900","#3399FF","#CC6699","#CC0000"))+labs(title = "GOOGLE")
df=twitter[twitter$Topic=="Facebook",]
ggplot(df,aes(x=Sentiment,fill=Sentiment))+geom_bar()+scale_fill_manual(values=c("#33FF99","#336666","#CC6666","#663333"))+labs(title = "FACEBOOK")

df=twitter[twitter$Topic=="Microsoft",]
ggplot(df,aes(x=Sentiment,fill=Sentiment))+geom_bar()+scale_fill_manual(values=c("#999999","#FF3300","#003399","#990099"))+labs(title = "MICROSOFT")
```

# Histogram of Text Length in Tweets

``` {r,comment=na,echo=false}
hist(nchar(twitter$Text),col = "yellow",main="Histogram for the length of Text",xlab="Text Length",ylab="Frequency")
```

This histogram visualizes the distribution of tweet lengths based on the
number of characters.

The distribution is right-skewed, with the majority of tweets having
shorter text lengths.

The highest frequency of tweets falls within the 0 to 100 character
range, with over 20,000 tweets in this interval. This indicates that
most tweets are concise.

As text length increases, the frequency of tweets decreases sharply.
Very few tweets exceed 300 characters, and tweets with lengths
approaching the maximum of 1,000 characters are extremely rare.

### **This heatmap visualizes the relationship between sentiment (Positive, Neutral, Negative, Irrelevant) and various topics discussed on Twitter. Each cell in the heatmap represents the intensity or frequency of a particular sentiment associated with a specific topic, allowing for a quick identification of patterns and trends across different topics.**

``` {r,comment=na,echo=false}
library("ggplot2")
library("reshape2")
da = data.frame("Topic" =twitter$Topic,"Sentiment"=twitter$Sentiment)
crosstab=table(da$Topic,da$Sentiment)
melt_crosstab = melt(crosstab)
ggplot(melt_crosstab, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile(color="white")+scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = median(melt_crosstab$value))+labs(title = "Heatmap of Topic vs Sentiment", x = "Sentiment",y = "Topic") +theme_minimal()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

# Word Cloud

*In a word cloud, the size of each word indicates its frequency or
importance---the larger the word, the more frequently it appears in the
text.*

``` {r,comment=na,echo=false}
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
```

Dominant Words: The largest words like "game," "just," "like," "will,"
and "good" are the most frequently mentioned in the dataset. This
suggests that the tweets may be heavily focused on gaming-related
discussions.

Sentiment and Topics: Words like "good," "love," and "great" suggest
positive sentiment, while words like "fix," "shit," and "fucking" might
indicate negative sentiment or frustration. The word "game" is central,
which could imply that the primary topic of discussion is gaming.

Trends: The variety of words related to gaming, companies (e.g.,
"Verizon," "Google," "Amazon"), and social media engagement (e.g.,
"Facebook," "Twitter") indicate the topics that are trending or commonly
discussed in the dataset.
