# Hierarcial agglomerative clustering

leg = 6 # column number for the legend you want to plot.
PC_x = 1 # the number of the PC you want to plot on the x-axis
PC_y = 2 # the number of the PC you want to plot on the y-axis
k = 3


x =as.matrix(cbind(PCs[,PC_x],PCs[,PC_y]))


# Dissimilarity matrix
d <- dist(x, method = "euclidean")
# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )
# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

clust <- cutree(hc1, k)




## plot

viz_leg_AGNES <- data.frame(x,as.factor(clust),customer_labels[,leg])
colnames(viz_leg_AGNES)<-c(colnames(PCs[PC_x]),colnames(PCs[PC_y]),"Clusters",colnames(customer_labels[leg]))


counts_AGNES<-as.data.frame(table(viz_leg_AGNES[,4], viz_leg_AGNES[,3]))

counts_perc_AGNES<-(table(viz_leg_AGNES[,4], viz_leg_AGNES[,3]))

for (i in 1:nrow(counts_perc_AGNES)){
  counts_perc_AGNES[i,]<-round(counts_perc_AGNES[i,]/sum(counts_perc_AGNES[i,]),3)
}
counts_perc_AGNES<-as.data.frame(counts_perc_AGNES)

### create and map colors to clusters
library(RColorBrewer)
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#pie(rep(1,n), col=sample(col_vector, n))

names(col_vector)<-levels(viz_leg_AGNES$Clusters)
colScale <- scale_colour_manual(name = "Clusters",values = col_vector)


#two plots beside each other

library(cowplot)

p1 <- ggplot(data=counts_perc_AGNES[which(counts_perc_AGNES$Freq>0),], aes(x=counts_perc_AGNES[which(counts_perc_AGNES$Freq>0),][,2], y=counts_AGNES[which(counts_AGNES$Freq>0),][,3], fill=counts_perc_AGNES[which(counts_perc_AGNES$Freq>0),][,1])) +
  geom_bar(stat="identity", position="fill") +
  geom_text(aes(label=percent(x=counts_perc_AGNES[which(counts_perc_AGNES$Freq>0),][,3])), vjust=1.6, color="black",
            position = position_fill(0.9), size=3) +
  labs(title = "Percentages of total label for each cluster",x = colnames(viz_leg_AGNES[3]), y = "", fill = colnames(viz_leg_AGNES[4]))


p2<- ggplot() + geom_point(alpha = 1,size=1.5, aes(x=viz_leg_AGNES[,1], y=viz_leg_AGNES[,2],color = viz_leg_AGNES[,3]), show.legend = T) +
  colScale +  
  labs(title = "Clustering based on PC of choice",x = colnames(viz_leg_AGNES[1]), y=colnames(viz_leg_AGNES[2]))


# set labels

p <- plot_grid(p1, p2)
p

save_results <- "y"


if (save_results == "y"){
  
  result_path<-paste0("C:/Users/habe/Documents/NKI_clustering/Resultat NKI skala 1-10")
  sub_dir<-paste0("/AGNES")
  
  #if a folder for the PCs doesn't exist, create one.
  dir.create(file.path(result_path, sub_dir), showWarnings = FALSE)
  
  

  filen2<-paste0(result_path,sub_dir,"/PC",PC_x, "-PC", PC_y," ", colnames(viz_leg_AGNES[4])," ", k, " kluster, barplot.pdf")
  
  
  if (file.exists(filen2)){
    file.remove(filen2)}  
  
  
  save_plot(filen2,p,ncol=2)
}
