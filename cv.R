install.packages("pheatmap")
library(pheatmap)
pheatmap(p, display_numbers = TRUE, number_format = "%.3f",cluster_row = FALSE, cluster_col = FALSE,main="FCN-8s"
         ,fontface="italic",fontfamily= "Times New Roman")
install.packages("reshape2")
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)
library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +geom_tile()
p<-ggplot(data = melted_cormat,aes(x=Var1,y=Var2,fill=value))+xlab("行业")+ylab("星期")+geom_tile()

p_cor <-ggplot(melted_cormat, aes(Var1, Var2)) + 
  geom_tile(aes(fill = value),colour = "white") 

p_cor + scale_fill_gradient(name="Value", low = "white",high = "red") +
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5,family = "serif"))+
  coord_fixed(ratio=1)+
  theme(axis.text= element_text(size = 10,family="serif"))+
  theme(plot.margin = unit(c(0.1,0,0,0), unit="mm"))+
  labs(x = "True label", y = "Predict label", title = "Segformer confusion matrix")+
  theme(plot.title = element_text(size = 12,hjust = 0.5,vjust = 1.0,family = "serif" ))+
  theme(axis.title.x = element_text(size = 12,family="serif"))+
  theme(axis.title.y = element_text(size = 12,family="serif"))+
  theme(legend.key.width=unit(5,'mm'),legend.key.height=unit(2,'cm'))+
  theme(legend.title = element_text(size = 12,family="serif"))+
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 3,family="serif")
table<-read.csv('F:/Work/cv/dataset/segformer_swin_unet/segformer_swin_unet/segformer/valid_confusion_matrix.csv',fileEncoding = "gbk")
mydata <-table[, c(2,3,4,5,6)]
t <- round(apply(mydata[,],2,function(y){y/sum(y)}),3)
rownames(t) <- c("Other","Vegetation","Architecture","Path","Water body")
melted_cormat <- melt(t)
c(0.866,0.032,0.020,0.078,0.004,0.017,0.967,0.003,0.008,0.005,0.146,0.022,0.823,0.008,0.000,0.113,0.024,0.002,0.860,0.001
   ,0.015,0.058,0.000,0.002,0.924)
matrix(c(0.866,0.032,0.020,0.078,0.004,0.017,0.967,0.003,0.008,0.005,0.146,0.022,0.823,0.008,0.000,0.113,0.024,0.002,0.860,0.001,0.015,0.058,0.000,0.002,0.924),nrow=5,dimnames=list(c("Other","Vegetation","Architecture","Path","Water body"),c("Other","Vegetation","Architecture","Path","Water body")))
S1_cor + scale_fill_gradient(name="Value", low = "white",high = "red") +
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5,family = "serif"))+
  coord_fixed(ratio=1)+
  theme(axis.text= element_text(size = 10,family="serif"))+
  theme(plot.margin = unit(c(0.1,0,0,0), unit="mm"))+
  labs(x = "True label", y = "Predict label", title = "SegNet confusion matrix")+
  theme(plot.title = element_text(size = 12,hjust = 0.5,vjust = 1.0,family = "serif" ))+
  theme(axis.title.x = element_text(size = 12,family="serif"))+
  theme(axis.title.y = element_text(size = 12,family="serif"))+
  theme(legend.key.width=unit(5,'mm'),legend.key.height=unit(2,'cm'))+
  theme(legend.title = element_text(size = 12,family="serif"))+
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 3,family="serif")
U1_cor + scale_fill_gradient(name="Value", low = "white",high = "red") +
  theme(axis.text.x = element_text(vjust = 0.5, hjust = 0.5,family = "serif"))+
  coord_fixed(ratio=1)+
  theme(axis.text= element_text(size = 15,family="serif"))+
  theme(plot.margin = unit(c(0.1,0,0,0), unit="mm"))+
  labs(x = "True label", y = "Predict label")+
  theme(plot.title = element_text(size = 12,hjust = 0.5,vjust = 1.0,family = "serif" ))+
  theme(axis.title.x = element_text(size = 15,family="serif"))+
  theme(axis.title.y = element_text(size = 15,family="serif"))+
  theme(legend.key.width=unit(5,'mm'),legend.key.height=unit(2,'cm'))+
  theme(legend.title = element_text(size = 12,family="serif"))+
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 3,family="serif")

matrix(c(0.870,0.021,0.023,0.083,0.003,0.013,0.970,0.003,0.009,0.005,0.093,0.014,0.882,0.010,0.000,0.071,0.013,0.002,0.914,0.000,0.011,0.040,0.000,0.003,0.946),nrow=5,
       dimnames=list(c("Other","Vegetation","Architecture","Path","Water body"),c("Other","Vegetation","Architecture","Path","Water body")))
