# Testing mean shift clustering
# 7 clusters should be optimal.
# PC 1-2, label 6
# 3 n-start 25, max iter = 1000

library(meanShiftR)

# set a seed to make this reproducible 
set.seed(100)


leg = 6 # column number for the legend you want to plot.
PC_x = 1 # the number of the PC you want to plot on the x-axis
PC_y = 2 # the number of the PC you want to plot on the y-axis
#k = 3


x =as.matrix(cbind(PCs[,PC_x],PCs[,PC_y]))

########### meanShiftR ###################
result <- meanShift(x,iterations = 1000)

# assignment
meanShiftR_assignment <- result$assignment


########## plot barpolot ############
#prepare for barplot

viz_leg_meanShift <- data.frame(x,as.factor(result$assignment),customer_labels[,leg])
colnames(viz_leg_meanShift)<-c(colnames(PCs[PC_x]),colnames(PCs[PC_y]),"Clusters",colnames(customer_labels[leg]))


counts_meanShift<-as.data.frame(table(viz_leg_meanShift[,4], viz_leg_meanShift[,3]))

counts_perc_meanShift<-(table(viz_leg_meanShift[,4], viz_leg_meanShift[,3]))

for (i in 1:nrow(counts_perc_meanShift)){
  counts_perc_meanShift[i,]<-round(counts_perc_meanShift[i,]/sum(counts_perc_meanShift[i,]),3)
}
counts_perc_meanShift<-as.data.frame(counts_perc_meanShift)

#two plots beside each other

library(cowplot)

p1 <- ggplot(data=counts_perc_meanShift[which(counts_perc_meanShift$Freq>0),], aes(x=counts_perc_meanShift[which(counts_perc_meanShift$Freq>0),][,2], y=counts_meanShift[which(counts_meanShift$Freq>0),][,3], fill=counts_perc_meanShift[which(counts_perc_meanShift$Freq>0),][,1])) +
  geom_bar(stat="identity", position="fill") +
  geom_text(aes(label=percent(x=counts_perc_meanShift[which(counts_perc_meanShift$Freq>0),][,3])), vjust=1.6, color="black",
            position = position_fill(0.9), size=3) +
  labs(title = "Percentages of total label for each cluster",x = colnames(viz_leg_meanShift[3]), y = "", fill = colnames(viz_leg_meanShift[4]))


p2<- ggplot() + geom_point(alpha = 1,size=1.5, aes(x=viz_leg_meanShift[,1], y=viz_leg_meanShift[,2],color = viz_leg_meanShift[,3]), show.legend = T) +
   colScale +
  labs(title = "Clustering based on PC of choice",x = colnames(viz_leg_meanShift[1]), y=colnames(viz_leg_meanShift[2]))


# set labels

p <- plot_grid(p1, p2)
p

save_results <- "y"


if (save_results == "y"){
  
  result_path<-paste0("C:/Users/habe/Documents/NKI_clustering/Resultat NKI skala 1-10")
  sub_dir<-paste0("/Mean shift")
  
  #if a folder for the PCs doesn't exist, create one.
  dir.create(file.path(result_path, sub_dir), showWarnings = FALSE)
  
  
  
  filen2<-paste0(result_path,sub_dir,"/PC",PC_x, "-PC", PC_y," ", colnames(viz_leg_AGNES[4])," ", k, " kluster, barplot.pdf")
  
  
  if (file.exists(filen2)){
    file.remove(filen2)}  
  
  
  save_plot(filen2,p,ncol=2)
}
