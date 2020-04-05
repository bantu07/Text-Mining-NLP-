#install.packages("tm")
library(tm)

#install.packages("slam")
library(slam)

#install.packages("topicmodels")
library(topicmodels)

x <- readLines("C:\\1.Srinivas\\Material\\Excelr\\1.Course Material\\Week 4\\Text+Mining,+NLP+codes\\modi.txt")
x
length(x)

mydata.corpus <- Corpus(VectorSource(x))

mydata.corpus <- tm_map(mydata.corpus, removePunctuation)

my_stopwords <- c(stopwords('english'),"brothers", "sisters", "the", "due", "are", "not", "for", "this", "and",  "that", "there", "new", "near", "beyond", "time", "from", "been", "both", "than",  "has","now", "until", "all", "use", "two", "ave", "blvd", "east", "between", "end", "have", "avenue", "before",    "just", "mac", "being",  "when","levels","remaining","based", "still", "off", "over", "only", "north", "past", "twin", "while","then")

mydata.corpus <- tm_map(mydata.corpus, removeWords, my_stopwords)

mydata.corpus <- tm_map(mydata.corpus, removeNumbers)

mydata.corpus <- tm_map(mydata.corpus, stripWhitespace)

## build a term-document matrix
mydata.dtm3 <- TermDocumentMatrix(mydata.corpus)
mydata.dtm3

dim(mydata.dtm3)

# dtm <- as.DocumentTermMatrix(mydata.dtm3)
# dtm <- DocumentTermMatrix(mydata.corpus)
dtm <- t(mydata.dtm3)

rowTotals <- apply(dtm, 1, sum)
?apply

dtm.new   <- dtm[rowTotals > 0, ]
dim(dtm.new)

lda <- LDA(dtm.new, 10) # find 10 topics
?LDA

term <- terms(lda, 10) # first 10 terms of every topic
term

tops <- terms(lda)
#?terms
tb <- table(names(tops), unlist(tops))
tb <- as.data.frame.matrix(tb)
#?unlist

#cls <- hclust(dist(tb), method = 'ward.D2') #ward is absolute distance
#?hclust
##par(family = "HiraKakuProN-W3")
#windows()
#plot(cls)
#?par

