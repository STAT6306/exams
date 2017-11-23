load(file="./2014flights.Rdata")
format(object.size(df),units='MiB')#get the size in memory

require(dplyr)




dfSub = df %>% filter(CANCELLED == 0) %>% na.omit %>%
  select(MONTH,DAY_OF_WEEK,DEST_STATE_ABR,ARR_DELAY) %>%
  mutate(MONTH = as.factor(MONTH), DAY_OF_WEEK = as.factor(DAY_OF_WEEK))

Ytotal = (dfSub %>% select(ARR_DELAY))$ARR_DELAY
Xtotal = dfSub %>% select(-c(ARR_DELAY))

nrow(Xtotal)

#This step is necessary.  With qualitative features, if you iteratively
# fit least squares, sometimes all the levels of a qualitative feature won't
# appear in the first chunk.  Hence, R will produce an error when a new factor level
# it wasn't anticipating appears in a later chunk

Xquant = model.matrix(~.,data=Xtotal)
dim(Xquant)

rm(Xtotal)

nChunks   = 10
chunkSize = n/nChunks

for(chunk in 1:nChunks){
  print(chunk)
  fName = paste('Xchunk',chunk,'.txt')
  chunkGrid = ((chunk-1)*chunkSize + 1):(chunk*chunkSize)
  write.table(Xquant[chunkGrid,],file=fName,sep=',',row.names=F,col.names=colnames(Xquant))
}
getMemorySizeStringF = function(a){
  return(object.size(get(a)))
}
sum(sapply(ls(),getMemorySizeStringF)/(2**20))
rm(list=c('df','Xquant','dfSub'))
sum(sapply(ls(),getMemorySizeStringF)/(2**20))

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
  Ychunk = Ytotal[chunkGrid]
  form = as.formula(paste('Ychunk ~ -1 + ',paste(names(Xchunk),collapse=' + '),collapse=''))
  if(chunk == 1){
    out.biglm = biglm(formula = form,data=Xchunk)  
  }else{
    out.biglm = update(out.biglm,moredata=Xchunk)
  }
  coef(out.biglm)[1:5]
}


summary(out.biglm)