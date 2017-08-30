setwd("/home/ycheruku/nanopore_qs/new_analysis_10mer/modelling")
data<- read.delim("Ecoli_prob_matrix.txt")
write.table(t(colnames(data)),file="column_names.txt", quote = F,row.names = F,col.names = F)
model_data<-data[,7:93]
model<-glm(predictor~distace._.5end+distance_3end+A+T+G+C+AA+TT+GG+CC+AT+AG+AC+TG+TC+TA+GA+GT+GC+CA+CT+CG+ATT+CTT+GTT+TTT+ATG+TGT+GCT+GGT+CCT+ACT+TCT+TAT+TGG+CAA+AAT+CAT+GAA+GAT+AAA+CGT+TAA+ATC+CTC+GTC+TTC+TGC+GCC+GGC+CCC+ACC+TCC+TAC+CAG+AAC+CAC+GAG+GAC+AAG+CGC+TAG+ATA+CTA+GTA+GCA+GGA+CCA+ACA+TCA+CGA+TGA+CTG+GTG+GCG+GGG+CCG+ACG+TCG+CGG+TTA+AGT+AGA+TTG+AGC+AGG,data =model_data,family = "binomial")
write.table(capture.output(summary(model)),file="ecoli_out.txt",quote=F)
###### YEAST#####
data<- read.delim("yeast_prob_matrix.txt")
write.table(t(colnames(data)),file="column_names.txt", quote = F,row.names = F,col.names = F)
model_data<-data[,7:93]
model<-glm(predictor~distace._.5end+distance_3end+A+T+G+C+AA+TT+GG+CC+AT+AG+AC+TG+TC+TA+GA+GT+GC+CA+CT+CG+ATT+CTT+GTT+TTT+ATG+TGT+GCT+GGT+CCT+ACT+TCT+TAT+TGG+CAA+AAT+CAT+GAA+GAT+AAA+CGT+TAA+ATC+CTC+GTC+TTC+TGC+GCC+GGC+CCC+ACC+TCC+TAC+CAG+AAC+CAC+GAG+GAC+AAG+CGC+TAG+ATA+CTA+GTA+GCA+GGA+CCA+ACA+TCA+CGA+TGA+CTG+GTG+GCG+GGG+CCG+ACG+TCG+CGG+TTA+AGT+AGA+TTG+AGC+AGG,data =model_data,family = "binomial")
write.table(capture.output(summary(model)),file="yeast_out.txt",quote=F)