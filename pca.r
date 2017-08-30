setwd("/home/ycheruku/nanopore_qs/new_analysis_10mer/modelling")
Ecoli_2D_data<-read.delim("ecoli_pca.txt")
Ecoli_2D_data_model<-Ecoli_2D_data[,8:93]
Ecoli_2D_data_model_predictor<-Ecoli_2D_data_model[,7]
pca <- princomp(Ecoli_2D_data_model, cor=T)
plot(pca,type="l")
pca1<-pca$loadings
write.table(pca1,file="ecoli_pca_loadings.txt")
###yeast#####
Yeast_2D_data<-read.delim("yeast_pca.txt")
Yeast_2D_data_model<-Yeast_2D_data[,8:93]
Yeast_2D_data_model_predictor<- Yeast_2D_data[,7]
pca <- princomp(Yeast_2D_data_model, cor=T)
pca1<-pca$loadings
write.table(pca1,file="yeast_pca_loadings.txt")