library("e1071")
setwd("/home/ycheruku/nanopore_qs/feature_selection_round2/error_noerror_data/svm")
data<-read.delim("yeast_10mer_binary.txt")
data$predictor<- gsub("1","error",data$predictor)
data$predictor<- gsub("0","noerror",data$predictor)
t<- split(data, data[,7])
error<- data.frame(t$error)
noerror<- data.frame(t$noerror)
n<- nrow(error)
n<- n/2
m<- n/2
error_set1<- error[1:m,]
error_set2<- error[(m-1):n,]
ne<- nrow(noerror)
ne<- ne/2
me<- ne/2
noerror_set1<- noerror[1:me,]
noerror_set2<- noerror[(me-1):ne,]
train <- rbind(error_set1, noerror_set2)
test<- rbind(error_set2,noerror_set1)
for (i in 1:88)
{
levels(test[,i])<- levels(train[,i])
}
train<- data.frame(as.matrix(train[,7:95]))
test<- data.frame(as.matrix(test[,8:95]))
svm.model <- svm(predictor ~ ., data = train, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, new.data = test)
write.table(svm.pred,file="yeast_5mer_svm.txt",row.names = F,quote = F)
test<- as.matrix(rbind(error_set2,noerror_set1))
data_split<- test[,7] #particular columns containing predictor base altbase info
prediction<- read.delim("yeast_5mer_svm.txt") #file having predicted base info
matrix<- cbind(data_split,prediction)
x<- split(matrix,matrix[,1])
datasize<- nrow(matrix)
error <- data.frame(x$error)
noerror<- data.frame(x$noerror)
test<- capture.output(for(i in 1:nrow(error))
{
  if (error$data_split[i] == error$ x[i])
  {print("TP")}
  else {
    {print("FN")}  }
  
  
} )

error$match<- data.frame(test)

test<- capture.output(for(i in 1:nrow(noerror))
{
  if (noerror$data_split[i] == noerror$x[i])
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
write.table(out,file= "yeast_10mer_roc_calc.txt" ,row.names = F,quote = F)
