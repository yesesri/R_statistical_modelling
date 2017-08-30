setwd("/home/ycheruku/nanopore_qs/prediction_half_half_data/modelling/glm_old_data/relief_features")
train_data<-read.delim("yeast_5mer_train_matrix.txt")
train_data_model<-train_data[,8:97]
predict_data<-read.delim("yeast_5mer_predict_matrix.txt")
predict_data_model<-predict_data[,9:97]
library(ordinal)
model<- clm(base~CA + TG +  T + AG + TAG + GTA + GTG + TCG + TC + GAA + AAG + CC + AAA + ATC + TTC + AA + TAA + TAC + CTA,data= train_data_model)
summary<- capture.output(summary(model))
predict<- predict(model,predict_data_model)
write.table(summary,file="yeast_5mer_summary_commmon_features.txt")
predict<- data.frame(predict)
predicted_base<- colnames(predict)[apply(predict,1,which.min)]
data<- transform(predict,min = pmin(fit.A,fit.G,fit.C,fit.T))
final_data<- cbind(predicted_base,data)
write.table(final_data,file="yeast_5mer_predictions_commmon_features.txt")
#### 10mer #####
setwd("/home/ycheruku/nanopore_qs/prediction_half_half_data/modelling/glm_old_data/relief_features")
train_data<-read.delim("yeast_10mer_train_matrix.txt")
train_data_model<-train_data[,8:97]
predict_data<-read.delim("yeast_10mer_predict_matrix.txt")
predict_data_model<-predict_data[,9:97]
library(ordinal)
model<- clm(base~AG + CCC + CCA + A + AC + CA + GAG + CCG + TC + TCC + AGT + T + TT + TG + ACT + CTG + CGG,data= train_data_model)
summary<- capture.output(summary(model))
predict<- predict(model,predict_data_model)
write.table(summary,file="yeast_10mer_summary_commmon_features.txt")
predict<- data.frame(predict)
predicted_base<- colnames(predict)[apply(predict,1,which.min)]
data<- transform(predict,min = pmin(fit.A,fit.G,fit.C,fit.T))
final_data<- cbind(predicted_base,data)
write.table(final_data,file="yeast_10mer_predictions_commmon_features.txt")
