set.seed(5)
require(bit64)
require(data.table)
require(Rtsne)

removeCols = c('VAR_0207', 'VAR_0213', 'VAR_0840', 'VAR_0847', 'VAR_1428', 'VAR_0027', 'VAR_0228')

#t VAR_0227 & VAR_0228 are these ID fields?

testX = fread('/home/mikeskim/Desktop/kaggle/springleaf/test.csv',data.table=F)
trainX = fread('/home/mikeskim/Desktop/kaggle/springleaf/train.csv',data.table=F)

testX$ID = NULL
trainX$ID = NULL; trainX$target=NULL

tmpI = apply(trainX,2,function(x) length(unique(x)))
tmpI2 = which(tmpI==2)

trainX = trainX[,tmpI2]
testX = testX[,tmpI2]


gc()


both = rbind(trainX,testX)
rm(trainX); rm(testX)
gc()

for (j in 1:ncol(both)) {
  both[,j] = as.numeric(as.factor(both[,j]))
}

both[is.na(both)]=2

gc()

both = as.matrix(both)
gc()
both = both-1

tsne <- Rtsne(both, check_duplicates = FALSE, pca = FALSE, verbose=TRUE,
              perplexity=30, theta=0.5, dims=2)


write.csv(tsne$Y, file='/home/mikeskim/Desktop/kaggle/springleaf/afeat1_train_test.csv', quote=FALSE,row.names=FALSE)

