## data transformation

library(Matrix)
library(glmnet)
library(parallel)
data_text <- readLines("lemmatized_words.txt",encoding = "UTF-8")
vocab <- read.csv("counter_words.txt",header = F, encoding = "UTF-8")

num_thre <- 3
words <- as.character(vocab$V1[vocab$V2>=num_thre])

n_obs <- length(data_text)
n_var <- length(words)

data_text.test <- data_text
names(data_text.test) <- seq_len(n_obs)

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
})#create sparse matrix



X <- NULL
for(i in seq_len(length(X0))){
  X <- cbind(X,X0[[i]])
}
X <- t(X)

colnames(X) <- words

save(X, file = "covariates.RData")
Y=read.csv("burger_noreview.csv",colClasses=c(NA, "NULL", "NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL","NULL"))
save(list=c("Y","X"), file = "covariates_withY.RData")



## stability selection
rm(list=ls())
load("covariates_withY.RData")

y <- Y$stars_x
num.split <- 100
n.lam <- 100
px <- ncol(X)
n <- nrow(X)
cutoff <- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)
PFER <- 0.05
namesX <- colnames(X)

selection <- function(ii, X, y, n, lam){
  set.seed(ii)
  ind <- sample.int(n, floor(n/2), replace = F)
  est <- glmnet(X[ind,],y[ind], lambda = lam, standardize = F)
  beta.sel <- est$bet!=0
  return(list(beta.sel=beta.sel))
}


q <- ceiling(sqrt(px^2*(2*cutoff-1)*PFER))

model.lasso <- glmnet(X, y, nlambda = n.lam, standardize = F)
model.lasso_cv <- cv.glmnet(X, y, standardize = F)

lam <- exp(seq(from = log(max(model.lasso$lambda)*2), to = log(min(model.lasso$lambda)), 
               length.out = n.lam))

res.list <- mcmapply(selection, ii = 1:num.split, SIMPLIFY = FALSE,mc.cores = detectCores()-2,
                     MoreArgs = list(y = y, X = X, n = n, lam = lam))
freq <- Matrix(0, nrow = px, ncol = n.lam, sparse = T)
lam.min <- Matrix(0, nrow = num.split, ncol = length(q))

for(i in seq_len(num.split)){
  freq <- freq + res.list[[i]]$beta.sel
  tmp <- rowSums(apply(res.list[[i]]$beta.sel,1,cumsum)>0)
  for(j in 1:length(q)) lam.min[i,j] <- min(lam[tmp<=q[j]])
}

lam.min <- apply(lam.min, 2, max)
freq <- freq/num.split

maxfreq <- Matrix(0, px, length(q), sparse = T)
selected <- Matrix(FALSE, px, length(q), sparse = T)


for (i in 1:length(q)){
  maxfreq[,i] <- apply(freq[,lam>=lam.min[i]], 1, max)
  selected[,i] <- maxfreq[,i] >=cutoff[i]
}
rownames(maxfreq) <- namesX
rownames(freq) <- namesX
rownames(selected) <- namesX

save.image(file = "selected_new.RData")


#writelines & t.test
load(file = "selected_new.RData")
num_sel <- colSums(selected)

for(i in seq_len(dim(selected)[2])){
  filecon <- file(paste("selected_",num_sel[i],".txt",sep = ""))
  writeLines(rownames(selected)[selected[,i]],filecon)
  close(filecon)
}

selected_final <- apply(selected,1, prod)
selected_final <- as.logical(selected_final)

test <- lm(y~as.matrix(X[,selected_final]))
s <- summary(test)

tscore <- data.frame(words=namesX[selected_final], occurence= colSums(X[,selected_final]), pval=s$coefficients[,4][-1],tscore=s$coefficients[,3][-1])
tscore <- tscore[order(tscore$pval),]
write.table(tscore, file = "summary.txt", row.names = F)
