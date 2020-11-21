#setwd("C:/Users/Dell/Desktop/WISC/20-21 fall/STAT 628/module 3")

library("Matrix")
data_text <- readLines("lemmatized_words.txt",encoding = "UTF-8")
vocab <- read.csv("counter_words.txt",header = F)

num_thre <- 3
words <- as.character(vocab$V1[vocab$V2>=num_thre])

n_obs <- length(data_text)
n_var <- length(words)

data_text.test <- data_text[1:65313]
names(data_text.test) <- 1:65313

freq <- function(x, corp){
  len <- length(corp)
  freq0 <- numeric(len)
  names(freq0) <- corp
  for(i in x){
    if(!(i %in% corp)) next
    freq0[i] <- freq0[i]+1
  }
  names(freq0) <- NULL
  return(freq0)
}
X0 <- sapply(X = data_text.test, FUN = function(x){
  temp <- unlist(strsplit(x, split = ","))
  as(as.matrix(freq(temp,words)),"dgCMatrix")
})

## clean
sthunknown <- NULL
for(i in seq_len(length(X0))){
  if(dim(X0[[i]])[1]!=21848)
    sthunknown <- c(sthunknown,i)
}


X <- NULL
for(i in seq_len(length(X0))){
  X <- cbind(X,X0[[i]])
}
X <- t(X)

colnames(X) <- words

save(X, file = "covariates.RData")
