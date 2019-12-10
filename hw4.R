# install.packages("tm")
library(tm) # load text mining library
library(tidyverse)


workDir <- "C:/temp/cpsc375hw4/"
setwd(workDir)

filename <- "seattle_rainfall.txt"
sc <- scan(filename)

aplt <- acf(sc)
aplt

xts <- ts(sc, frequency = 12)

de <- decompose(xts)

deplt <- plot(de)


# cost <- function(xi, yj) {
  
#   return ((xi - yj)^2)
# }

dtw <- function (v1, v2) {
  M <- length(v1)
  N <- length(v2)
  Cost <- matrix(0,M,N) # Initialize with zeros
  for (i in 1:M) {
    for (j in 1:N) {
      # Cost[i,j] <- (v1[i] - v2[j])*(v1[i] - v2[j]) # distance function
      Cost[i,j] <- (v1[i] - v2[j])^2 # distance function
    }
  }
  C <- matrix(0,M,N) # Initialize with zeros
  C[1,1] <- Cost[1,1] # Initialize top left cell
  for (i in 2:M) { # Initialize first column
    C[i,1] <- C[i-1,1] + Cost[i,1]
  }
  for (j in 2:N) { # Initialize first row
    C[1,j] <- C[1,j-1] + Cost[1,j]
  }
  # Complete the main loop
  for (i in 2:M) {
    for (j in 2:N) {
      C[i,j] <- min(C[i-1,j], C[i, j-1], C[i-1,j-1]) + Cost[i,j]
    }
  } 
  return (C[M,N])
}


X = c(1, 3, 4, 4)
Y = c(1, 2, 5)

c <- dtw(X, Y)

docs <- c("see spot", "see spot run", "run spot run") # dataset: 3 documents
mycorpus <- VCorpus(VectorSource(docs)) # convert to VCorpus data type
print(mycorpus) # a summary
inspect(mycorpus) # a longer summary
content(mycorpus[[1]]) # underlying data for one document

# calculating DocumentTermMatrix
dtm <- DocumentTermMatrix(mycorpus) 
inspect(dtm) # default: only counts
dtm <- DocumentTermMatrix(mycorpus, control=list(weighting=weightTfIdf))
inspect(dtm) # DocumentTermMatrix with Tf-Idf weighting


# workDir <- "C:/temp/cpsc375hw4/"
# setwd(workDir)

filename <- "airline_tweets_sentiment2.csv"
# filename <- "short.csv"
docs <- read.csv(filename)
docArray <- unlist(docs[,4])

twitterData <- VCorpus(VectorSource(docArray))
twitterData[[1]][["content"]][1]


# twitterData <- tm_map(twitterData, content_transformer(tolower)) # to lower case
twitterData <- tm_map(twitterData, removeNumbers)
twitterData <- tm_map(twitterData, removePunctuation)
twitterData <- tm_map(twitterData, removeWords, stopwords("english")) # remove stop words
# inspect(twitterData) # a longer summary
# content(twitterData[[1]]) # underlying data for one document

dtm <- DocumentTermMatrix(twitterData, control=list(weighting=weightTfIdf))
inspect(dtm) # DocumentTermMatrix with Tf-Idf weighting
vocabs <- dtm$dimnames$Terms
length(vocabs)

sparsity <- .99
dtm <- removeSparseTerms(dtm, sparsity)
vocabs <- dtm$dimnames$Terms
length(vocabs)

cosine <- function (A, B) {
  sumAB = 0
  for (i in 1:length(A)){
    sumAB = sumAB + A[i]*B[i]
  }
  
  sumA2 = 0
  for (i in 1:length(A)){
    sumA2 = sumA2 + A[i]^2
  }

  sumB2 = 0
  for (i in 1:length(B)){
    sumB2 = sumB2 + B[i]^2
  }
  
  denom = sqrt(sumA2*sumB2)
  if (denom > 0) {
    return (sumAB/sqrt(sumA2*sumB2))
  }
  else {
    return (0)
  }
}

mymatrix <- as.matrix(dtm)

dimMatrix <- dim(mymatrix)

maxValue = 0
maxIndex = 0
current = 0
for (i in 2:dimMatrix[1]) {
  current = cosine(as.vector(mymatrix[1,]), as.vector(mymatrix[i,]))
  if (current > maxValue) {
    maxValue = current
    maxIndex = i
  }
}

print(twitterData[[1]][["content"]][1])
print(twitterData[[maxIndex]][["content"]][1])
print(maxValue)


print(docs[['airline_sentiment']][maxIndex])
print(docs[['airline_sentiment']][1])


# tf: 115/(115+10+2)
# nomalization: 115/sqrt(115^2 + 10^2 + 2^2)









