library("gbm")
setwd("/home/ycheruku/nanopore_qs/feature_selection_round2/error_noerror_data/gbm_ranknet/")
data<- read.delim("yeast_5mer_binary.txt")
data$predictor<- gsub("1","error",data$predictor)
data$predictor<- gsub("0","noerror",data$predictor)
t<- split(data, data[,7])
error<- data.frame(t$error)
noerror<- data.frame(t$noerror)
n<- nrow(error)
m<- n/2
error_set1<- error[1:m,]
error_set2<- error[(m-1):n,]
ne<- nrow(noerror)
me<- ne/2
noerror_set1<- noerror[1:me,]
noerror_set2<- noerror[(me-1):ne,]
train <- rbind(error_set1, noerror_set2)
test<- rbind(error_set2,noerror_set1)
test<- test[,8:95]
x<- train[,8:95]
y<- train[,7]
model<- gbm.fit(x,y, offset = NULL, distribution = "multinomial" , n.trees = 2, interaction.depth = 2,response.name = "y" ,var.names = colnames(x))
prediction<- predict.gbm(model,test,n.trees = 2, type = "response")
prediction <- data.frame(prediction)
out<- capture.output(for (i in 1:nrow(prediction))
{
  if (prediction[,1][i] > prediction[,2][i])
  {
      print("error")
  }
  else 
  {
       print("noerror")
    }  
})
prediction$out <- out
write.table(prediction, file= "yeast_5mer_model.txt")
y<- data.frame(y)
y_replace<- capture.output(for (i in 1:nrow(y))
{
  if (y[,1][i]=="error")
  {
    print("error")
  }
  else {
    print("noerror")
  }
})
t<- cbind(y,y_replace)
matrix<- cbind(t,prediction$out)
x<- split(matrix,matrix[,1])
datasize<- nrow(matrix)
error <- data.frame(x$error)
noerror<- data.frame(x$noerror)
t<- capture.output(for(i in 1:nrow(error))
{
  if (error$y[i] == error$prediction.out[i])
  {print("TP")}
  else {
    {print("FN")}  }
  
  
} )

error$match<- data.frame(t)

tn<- capture.output(for(i in 1:nrow(noerror))
{
  if (noerror$y[i] == noerror$prediction.out[i])
  {print("TN")}
  else {
    {print("FP")}  }
  
  
} )

noerror$match<- data.frame(tn)
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
write.table(out,file= "yeast_5mer_roc_calc.txt" ,row.names = F,quote = F)
