# load necessary libraries
library(ggplot2)
library(RColorBrewer)
library(ggnewscale)

customer_labels<-customer_labels_backup

#for (i in c(1:ncol(customer_labels))){
# load excel sheet with customaer labels placed in theworking directory.
#customer_labels = read.xlsx(file = "NKI_legends.xlsx",1)

#leg = i # column number for the legend you want to plot.
leg = 6
PC_x = 1 # the number of the PC you want to plot on the x-axis
PC_y = 5 # the number of the PC you want to plot on the y-axis


# Prepare a data frame with PCs to plot, clusters and labels
viz_leg<-as.data.frame(cbind((PCs[,PC_x]),(PCs[,PC_y])))

# number of clusters
k=3

# predict_no_clust <- (nrow(PCs[,PC_x:PC_y])-1)*sum(apply(PCs[,PC_x:PC_y],2,var))
#  for (i in 2:15) predict_no_clust[i] <- sum(kmeans(PCs[,PC_x:PC_y],
#                                                     centers=i)$withinss)
#   plot(1:15, predict_no_clust, type="b", xlab="Number of Clusters",
#        ylab="Within groups sum of squares")


# re-cluster based on new PCs?
#k_for_plot <- kmeans(viz_leg, k, nstart=25, iter.max=1000)

# Plot without re-clustering
viz_leg<-as.data.frame(cbind(viz_leg,k_for_plot[["cluster"]]))


viz_leg<-cbind(viz_leg,customer_labels[,leg])
viz_leg[,3]<-as.factor(viz_leg[,3])
viz_leg[,4]<-as.factor(viz_leg[,4])
colnames(viz_leg)<-c(colnames(PCs[PC_x]),colnames(PCs[PC_y]),"Clusters",colnames(customer_labels[leg]))


### create and map colors to clusters
library(RColorBrewer)
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#pie(rep(1,n), col=sample(col_vector, n))

names(col_vector)<-levels(viz_leg$Clusters)
colScale <- scale_colour_manual(name = "Clusters",values = col_vector)



# Plot
plot_leg_clust = ggplot() +
  
  # scatter plot
  geom_point(alpha = 1,size=1.5, aes(x=viz_leg[,1], y=viz_leg[,2],color = viz_leg[,4])) +
  labs(color=colnames(viz_leg[4])) + theme(legend.key.size =  unit(0.1, "in"))+
  
  # reset color scale
  new_scale_color() +
  
  #plot ellipses
  stat_ellipse(aes(x = viz_leg[,1], y= viz_leg[,2], color = as.factor(viz_leg[,3])), data = viz_leg) +
  colScale +
  
  
  # set labels
  labs(title = "Clusters and label of choice plotted",x = colnames(viz_leg[1]), y=colnames(viz_leg[2]))
plot_leg_clust
#dev.off()
#save_plot("plot1.pdf",plot_leg_clust)

#prepare for barplot
counts<-as.data.frame(table(viz_leg[,4], viz_leg[,3]))

counts_perc<-(table(viz_leg[,4], viz_leg[,3]))

for (i in 1:nrow(counts_perc)){
  counts_perc[i,]<-round(counts_perc[i,]/sum(counts_perc[i,]),3)
}
counts_perc<-as.data.frame(counts_perc)
counts_perc[,3] <- as.numeric(counts_perc[,3])

#two plots beside each other

  library(cowplot)

p1 <- ggplot(data=counts_perc[which(counts_perc$Freq>0),], aes(x=counts_perc[which(counts_perc$Freq>0),][,2], y=counts[which(counts$Freq>0),][,3], fill=counts_perc[which(counts_perc$Freq>0),][,1])) +
  geom_bar(stat="identity", position="fill") +
  geom_text(aes(label=percent(x=counts_perc[which(counts_perc$Freq>0),][,3], accuracy = 0.1)), vjust=1.6, color="black",
            position = position_fill(0.9), size=2) +
  labs(title = "Percentages of total label for each cluster",x = colnames(viz_leg[3]), y = "", fill = colnames(viz_leg[4]))


p2<- ggplot() + geom_point(alpha = 1,size=1.5, aes(x=viz_leg[,1], y=viz_leg[,2],color = viz_leg[,3]), show.legend = T) +
  colScale + guides(col = guide_legend(nrow = floor(k/2))) +
  labs(title = "Clustering based on PC of choice",x = colnames(viz_leg[1]), y=colnames(viz_leg[2]))


# set labels

p <- plot_grid(p1, p2)
p


save_plot("plot2.pdf",p, ncol = 2)



save_results <- "n"


 if (save_results == "y"){

   result_path<-paste0("C:/Users/habe/Documents/NKI_clustering/Resultat NKI skala 1-10")
   sub_dir<-paste0("/Klustring PC", PC_x, " vs PC", PC_y)

   #if a folder for the PCs doesn't exist, create one.
   dir.create(file.path(result_path, sub_dir), showWarnings = FALSE)

   
   filen1<-paste0(result_path,sub_dir,"/PC",PC_x, "-PC", PC_y," ", colnames(viz_leg[4])," ",k, " kluster.pdf")
   
   
   if (file.exists(filen1)){
     file.remove(filen1)}
  
   if (PC_y == 2){
     plot_dim <- 2.5 
   } else if (PC_y == 3){
     plot_dim <- 3
   } else if (PC_y == 4){
     plot_dim <- 3.5
   } else if (PC_y == 5){
     plot_dim <- 4
   }
   
   
   save_plot(filen1,plot_leg_clust, base_asp = plot_dim)
   
   
  filen2<-paste0(result_path,sub_dir,"/PC",PC_x, "-PC", PC_y," ", colnames(viz_leg[4])," ", k, " kluster, barplot.pdf")
  
  
  if (file.exists(filen2)){
    file.remove(filen2)}  
  
  
  save_plot(filen2,p,ncol=2)
   }

#}