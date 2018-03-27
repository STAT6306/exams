load(file="./2014flights.Rdata")
format(object.size(df),units='MiB')#get the size in memory

require(dplyr)

# I'm including the mid-exam email here for an explanation:
# All,
#  I've been getting a lot of questions about grappling with 
#  data size and the nature of the features.  Be wary of some of 
#  the features.
str(df)
#   Many of these are qualitative but are coded as numeric.  You 
#   need to treat these as qualitative which means coding them as 
#   a factor.  If you attempt to convert this to a feature matrix, 
#   the "p" for the problem can get extremely large.  Below I write 
#   out a way of dealing with this.  This is really just the most 
#   basic way.  A better way would be, for example, to look at the 
#   origin and dest and combine together the less common airports 
#   to reduce the number of levels of those features. 

dfSub = df %>% filter(CANCELLED == 0) %>% na.omit %>%
  select(-contains('AIRPORT'))  %>% select(-contains('MARKET'))  %>%
  select(-TAIL_NUM) %>% select(-CANCELLED) %>%
  select(-ORIGIN) %>% select(-DEST) %>%  
  mutate(MONTH = as.factor(MONTH), DAY_OF_WEEK = as.factor(DAY_OF_WEEK))

Xsub    = dfSub %>% select(-contains('DELAY'))
save(Xsub,file = './Xsub.Rdata')#save to harddrive anything that would suck to have to recreate
Xmm     = model.matrix(~.,data=Xsub)
format(object.size(Xmm),units='MiB')
save(Xmm,file = './Xmm.Rdata')

Ytotal       = (dfSub %>% select(ARR_DELAY))$ARR_DELAY
save('Ytotal',file = './Ytotal.Rdata')

nTotal = length(Ytotal)
trainTestSplit = 0.8
set.seed(1)
train = sample(c(TRUE,FALSE),nTotal,replace=TRUE,prob=c(trainTestSplit,1-trainTestSplit))
save('train',file = './train.Rdata')
Ytrain = Ytotal[train]
Ytest  = Ytotal[!train]

n = length(Ytrain)
#From here, we can take at least two approaches (there are numerous). 
#   1) We could fit a least squares solution using biglm
#        (sometimes with a large data set, this is all that can be done)
#   2) We could convert to a sparse matrix and use glmnet
#   3) We could do ridge on the dense data via stochastic gradient descent
#        (not implemented but the code from the homework will work with minor adjustment)

################
####   1) We could fit a least squares solution using biglm
################
Xtrain = Xmm[train,]
Xtest  = Xmm[!train,]
rm(Xmm)

nChunks   = 10
chunkSize = n/nChunks

for(chunk in 1:nChunks){
  print(chunk)
  fName = paste('Xchunk',chunk,'.txt')
  chunkGrid = ((chunk-1)*chunkSize + 1):(chunk*chunkSize)
  write.table(Xtrain[chunkGrid,],file=fName,sep=',',row.names=F,col.names=colnames(Xtrain))
}
getMemorySizeStringF = function(a){
  return(object.size(get(a)))
}
sum(sapply(ls(),getMemorySizeStringF)/(2**20))#this gets total memory used in MB in base 2 (that is, 1024 instead of 1000)
rm(list=c('df','Xtrain','dfSub'))
sum(sapply(ls(),getMemorySizeStringF)/(2**20))#free up some memory

if(!require(biglm,quietly=TRUE)){
  install.packages('biglm',repos='http://cran.us.r-project.org');require(biglm)
}

#read in and update:
for(chunk in 1:nChunks){
  print(chunk)
  fName  = paste('Xchunk',chunk,'.txt')
  Xchunk = read.table(file=fName,sep=',',header=T)
  dim(Xchunk)
  names(Xchunk)
  chunkGrid = ((chunk-1)*chunkSize + 1):(chunk*chunkSize)
  Ychunk = Ytrain[chunkGrid]
  form = as.formula(paste('Ychunk ~ -1 + ',paste(names(Xchunk),collapse=' + '),collapse=''))
  if(chunk == 1){
    out.biglm = biglm(formula = form,data=Xchunk)  
  }else{
    out.biglm = update(out.biglm,moredata=Xchunk)
  }
  coef(out.biglm)[1:5]
}


summary(out.biglm)
betaHat.lm = coef(out.biglm)

#Get training predictions:
#   Note: without Xtrain ever being loaded into memory at the same time,
#         we don't have the training predictions and need to compute them
YhatTrain.lm = rep(0,n)
for(chunk in 1:nChunks){
  print(chunk)
  fName  = paste('Xchunk',chunk,'.txt')
  Xchunk = as.matrix(read.table(file=fName,sep=',',header=T))
  chunkGrid = ((chunk-1)*chunkSize + 1):(chunk*chunkSize)
  #next line: get predictions at each chunck.  converting Xchunk to a matrix object is crucial
  YhatTrain.lm[chunkGrid] = Xchunk %*% betaHat.lm
}
#We can directly multiply due to Xtest being smaller and in memory (or we can do the chunking method)
YhatTest.lm  = Xtest %*% betaHat.lm

(trainingError.lm = mean( (YhatTrain.lm - Ytrain)**2 ))
(testError.lm     = mean( (YhatTest.lm - Ytest)**2 ))
# Note: in large samples with small/fixed p (here, p = 137),
#       the training error will converge on the risk (remember Exam 1?).  
#       Another way to see this is that the sample mean converges to the 
#       population mean.  The sample mean is the least squares estimator
#       for the intercept only model.  So, as long as p is very small
#       relative to n, the training error isn't too bad.  n must be huge, however.

################
####   2) We could convert to a sparse matrix and use glmnet
################
require(Matrix)
load('Xmm.Rdata')

XmmSpar = Matrix(Xmm,sparse=TRUE)
rm(Xmm)
format(object.size(XmmSpar),units='MiB')
writeMM(XmmSpar,file = './XmmSpar.mtx')
tmp = summary(XmmSpar)#Note, for reference, this creates the "3 vector" sparse coding from lecture

Xtrain = XmmSpar[train,]
Xtest  = XmmSpar[!train,]

require(glmnet)
#I chose alpha = .5 just in case some of the features are highly correlated
#  I realize that the cv curve wants to choose lambda = 0, but I went with 
#  the default grid because I wanted to provide a different solution than 
#  the least squares solution from above.
out.lasso = cv.glmnet(x=Xtrain,y=Ytrain,intercept=FALSE,alpha=.5)


betaHat.lasso   = coef(out.lasso,s='lambda.min')[-1]
YhatTrain.lasso = Xtrain %*% betaHat.lasso
YhatTest.lasso  = Xtest %*% betaHat.lasso 
(trainingError.lasso = mean( (YhatTrain.lasso - Ytrain)**2 ))
(testError.lasso     = mean( (YhatTest.lasso - Ytest)**2 ))

activeSet   = which(abs(coef(out.lasso,s='lambda.1se')[-1]) > 1e-16)

require(irlba)
Xtrain_S = Xtrain[,activeSet]
Xtest_S  = Xtest[,activeSet]
svd.out  = svd(Xtrain_S)

#Note: this is a conceptually simple way to multiply these objects, but it is 
#      inefficient to make this matrix and multiply with it: diag ((svd.out$d)^(-1))
betaHat.refit = svd.out$v %*% diag ((svd.out$d)^(-1)) %*%  t( svd.out$u) %*% Ytrain
YhatTrain.refit = Xtrain_S%*%betaHat.refit + coef(out.lasso,s='lambda.1se')[1]
YhatTest.refit  = Xtest_S%*%betaHat.refit + coef(out.lasso,s='lambda.1se')[1]

##########
# Compare all training/test errors
##########
(trainingError.lm = mean( (YhatTrain.lm - Ytrain)**2 ))
(testError.lm     = mean( (YhatTest.lm - Ytest)**2 ))
(trainingError.lasso = mean( (YhatTrain.lasso - Ytrain)**2 ))
(testError.lasso     = mean( (YhatTest.lasso - Ytest)**2 ))
(trainingError.refit = mean( (YhatTrain.lasso - Ytrain)**2 ))
(testError.lasso     = mean( (YhatTest.lasso - Ytest)**2 ))

#All procedures considered have about the same risk