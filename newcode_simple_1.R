#MCD dataset
txt <- read.csv("C://Users//Dell//Downloads//Excel R//R codes//NLP//Mcd_Small.csv", header = TRUE)
class(txt)
str(txt)
View(txt)
length(txt)#gives how many columns are there in txt 
txt <- as.data.frame(txt)#converting to dataframe
x <- as.character(txt)
x <- as.character(txt$text)#convert review factor to character
length(x)# gives the num of documents

# Corpus
library(tm)
x<-iconv(x,"UTF-8","latin1")
x <- Corpus(VectorSource(x))#converting docx's to corpus 
inspect(x[1])#to view 1st review
inspect(x[300])
#Data Cleansing
x1 <- tm_map(x, tolower)#convert to lowercase
inspect(x1[1])

x1 <- tm_map(x1, removePunctuation)#removes punctuations
inspect(x1[1])

inspect(x1[300])
x1 <- tm_map(x1, removeNumbers)#removes numbers
inspect(x1[300])

x1 <- tm_map(x1, removeWords, stopwords('english'))
inspect(x1[1])


# Remove URL's from corpus

# removeURL <- function(z) gsub('http[[:alnum:]]*', '', z)
# x1 <- tm_map(x1, content_transformer(removeURL))
# inspect(x1[1])

#striping white spaces 
x1 <- tm_map(x1, stripWhitespace)#removes whitespace 
inspect(x1[1])

#Term document matrix 
# converting unstructured data to structured format using TDM
tdm <- TermDocumentMatrix(x1)
dtm <- t(tdm)#t - transpose which gives DocumentTermMatrix

tdm <- as.matrix(tdm)#read it as matrix
tdm[100:109, 1:10]
tdm[90:100, 1:20]
View(tdm)
dim(tdm)
inspect(x[3])

# Bar plot
w <- rowSums(tdm)#sums of rows
w

w_sub <- subset(w, w >= 25)#Return subsets of vectors, matrices or data frames which meet conditions.
w_sub

barplot(w_sub, las=2, col = rainbow(30))

# Term phone repeats in all most all documents
x1 <- tm_map(x1, removeWords, 'google')
x1 <- tm_map(x1, stripWhitespace)

tdm <- TermDocumentMatrix(x1)
tdm

tdm <- as.matrix(tdm)
tdm[100:109, 1:20]

# Word cloud
library(wordcloud)
w_sub1 <- sort(rowSums(tdm), decreasing = TRUE)
#set.seed(222)
wordcloud(words = names(w_sub), freq = w_sub)
wordcloud(words = names(w_sub1), freq = w_sub1)

wordcloud(words = names(w_sub), freq = w_sub, random.order = F, colors = rainbow(50), scale=c(5,2), rot.per = 0.4)
?wordcloud

# wordcloud2 - shapes for word cloud
w

w_small <- subset(w, w >= 10)
w_small

barplot(w_small, las=2, col = rainbow(30))

library(wordcloud2)

w1 <- data.frame(names(w_small), w_small)
colnames(w1) <- c('word', 'freq')
#windows()
wordcloud2(w1, size=0.5, shape='circle')
?wordcloud2

wordcloud2(w1, size=0.5, shape = 'triangle')

wordcloud2(w1, size=0.5, figPath = "C://Users//Dell//Downloads//wick wall//637352.jpg")

#### Letter cloud
letterCloud(w1, word = "ABC", wordSize = 0.1)
?letterCloud

#windows()
#letterCloud(w1, word = "w")


#### emotion mining
library("syuzhet")
library(lubridate)
library(ggplot2)
library(scales)
library(dbplyr)
library(reshape2)

txt = readLines(file.choose())
x <- iconv(txt, "UTF-8")

s <- get_nrc_sentiment(x)
head(s)

x[4]
get_nrc_sentiment('unfriendly')

# Bar plot for emotion mining

barplot(colSums(s), las = 2, col = rainbow(10), ylab = 'Count', main = 'Emotion scores')

