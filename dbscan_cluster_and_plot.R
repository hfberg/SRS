library(dbscan)


leg = 6 # column number for the legend you want to plot.
PC_x = 1 # the number of the PC you want to plot on the x-axis
PC_y = 2 # the number of the PC you want to plot on the y-axis
k = 3

x =as.matrix(cbind(PCs[,PC_x],PCs[,PC_y]))



## find suitable eps parameter using a k-NN plot for k = dim + 1 ## Look for the knee! 
kNNdistplot(x, k = 5) 
abline(h=1.9, col = "red", lty=2)

res <- dbscan(x, eps = 1.9, minPts = 5) 
res

## plot

viz_leg_dbscan <- data.frame(x,as.factor(res$cluster),customer_labels[,leg])
colnames(viz_leg_dbscan)<-c(colnames(PCs[PC_x]),colnames(PCs[PC_y]),"Clusters",colnames(customer_labels[leg]))


counts_dbscan<-as.data.frame(table(viz_leg_dbscan[,4], viz_leg_dbscan[,3]))

counts_perc_dbscan<-(table(viz_leg_dbscan[,4], viz_leg_dbscan[,3]))

for (i in 1:nrow(counts_perc_dbscan)){
  counts_perc_dbscan[i,]<-round(counts_perc_dbscan[i,]/sum(counts_perc_dbscan[i,]),3)
}
counts_perc_dbscan<-as.data.frame(counts_perc_dbscan)

#two plots beside each other

library(cowplot)

p1 <- ggplot(data=counts_perc_dbscan[which(counts_perc_dbscan$Freq>0),], aes(x=counts_perc_dbscan[which(counts_perc_dbscan$Freq>0),][,2], y=counts_dbscan[which(counts_dbscan$Freq>0),][,3], fill=counts_perc_dbscan[which(counts_perc_dbscan$Freq>0),][,1])) +
  geom_bar(stat="identity", position="fill") +
  geom_text(aes(label=percent(x=counts_perc_dbscan[which(counts_perc_dbscan$Freq>0),][,3])), vjust=1.6, color="black",
            position = position_fill(0.9), size=3) +
  labs(title = "Percentages of total label for each cluster",x = colnames(viz_leg_dbscan[3]), y = "", fill = colnames(viz_leg_dbscan[4]))


p2<- ggplot() + geom_point(alpha = 1,size=1.5, aes(x=viz_leg_dbscan[,1], y=viz_leg_dbscan[,2],color = viz_leg_dbscan[,3]), show.legend = T) +
  colScale +  
  labs(title = "Clustering based on PC of choice",x = colnames(viz_leg_dbscan[1]), y=colnames(viz_leg_dbscan[2]), caption = "cluster 0 = outliers")


# set labels

p <- plot_grid(p1, p2)
p

save_results <- "y"


if (save_results == "y"){
  
  result_path<-paste0("C:/Users/habe/Documents/NKI_clustering/Resultat NKI skala 1-10")
  sub_dir<-paste0("/dbscan")
  
  #if a folder for the PCs doesn't exist, create one.
  dir.create(file.path(result_path, sub_dir), showWarnings = FALSE)
  
  
  
  filen2<-paste0(result_path,sub_dir,"/PC",PC_x, "-PC", PC_y," ", colnames(viz_leg_AGNES[4])," ", k, " kluster, barplot.pdf")
  
  
  if (file.exists(filen2)){
    file.remove(filen2)}  
  
  
  save_plot(filen2,p,ncol=2)
}
