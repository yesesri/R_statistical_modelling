library("glmnet")
setwd("/home/ycheruku/nanopore_qs/feature_selection_round2/error_noerror_data/glmnet/ecoli")
data<- read.delim("ecoli_10mer_binary.txt")
t<- split(data, data[,7])
error<- data.frame(t$`0`)
noerror<- data.frame(t$`1`)
n<- nrow(error)
m<- n/2
error_set1<- error[1:m,]
error_set2<- error[(m-1):n,]
ne<- nrow(noerror)
me<- ne/2
noerror_set1<- noerror[1:me,]
noerror_set2<- noerror[(me-1):ne,]
train <- rbind(error_set1, noerror_set2)
x<- as.matrix(train[,8:95])
y<- as.factor(train[,7])
cvfit <-  cv.glmnet(x, y, family = "binomial", type.measure = "class", alpha =0.5)
result<- coef( cvfit, s =  cvfit$lambda.min, exact = T)
result<- capture.output(print(result))
test<- rbind(error_set2,noerror_set1)
x<- as.matrix(test[,8:95])
prediction <- predict(cvfit, newx = x, s =  cvfit$lambda.min, type = "class")
write.table(prediction, file= "ecoli_10mer_predictions_bionomial.txt",quote = F,row.names = F)
data_split<- test[,7] #particular columns containing predictor base altbase info
prediction<- read.delim("ecoli_10mer_predictions_bionomial.txt") #file having predicted base info
matrix<- cbind(data_split,prediction)
x<- split(matrix,matrix[,1])
datasize<- nrow(matrix)
error <- data.frame(x$`0`)
noerror<- data.frame(x$`1`)

test<- capture.output(for(i in 1:nrow(error))
{
  if (error$data_split[i] == error$ X1[i])
  {print("TP")}
  else {
    {print("FN")}  }
  
  
} )

error$match<- data.frame(test)

test<- capture.output(for(i in 1:nrow(noerror))
{
  if (noerror$data_split[i] == noerror$X1[i])
  {print("TN")}
  else {
    {print("FP")}  }
  
  
} )

noerror$match<- data.frame(test)
ncol(error)
y<- split(error,error[,3])
TP<- as.numeric(nrow(y$`[1] "TP"`))
FN<- as.numeric(nrow(y$`[1] "FN"`))

if(length(TP) == 0) {
  TP <-  0.01
}
if(length(FN) == 0) {
  FN <-  0.01
}

Z<- split(noerror,noerror[,3])
FP<- as.numeric(nrow(Z$`[1] "FP"`))
TN<- as.numeric(nrow(Z$`[1] "TN"`))
if(length(TN) == 0) {
  TN <-  0.01
}
if(length(FP) == 0) {
  FP <-  0.01
}
senstivity = TP/(TP+FP)
specificity = TN/(TN+FP)
Accuracy = (TP+TN)/(TP+FP+TN+FN)
senstivity<- capture.output(senstivity)
specificity<- capture.output(specificity)
Accuracy<- capture.output(Accuracy)
datasize<- capture.output(datasize)
TN<- capture.output(TN)
TP<- capture.output(TP)
FN<- capture.output(FN)
FP<- capture.output(FP)
out<- rbind(datasize,TP,FP,senstivity,TN,FN,specificity,Accuracy)
write.table(out,file= "ecoli_10mer_roc_calc.txt" ,row.names = F,quote = F)
write.table(result, file= "ecoli_10mer_features_bionomial.txt",,quote = F,row.names = F)


