load("covariates_withY.RData")

library(Matrix)
library(glmnet)
library(parallel)

y <- Y$stars_x
num.split <- 100
n.lam <- 100
px <- ncol(X)
n <- nrow(X)
cutoff <- c(0.6,0.65,0.7,0.75,0.8,0.85,0.9)
PFER <- 0.05
namesX <- colnames(X)

#X_orig <- X
#y_orig <- y

#mean.X<-colMeans(X)
#var.X<-apply(X, 2, var)
#mean.y<-mean(y)
#var.y<-var(y)
#X <- scale(X, center = mean.X, scale = F)
#y <- scale(y, center = mean.y, scale = F)

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

save.image(file = "selected.RData")
