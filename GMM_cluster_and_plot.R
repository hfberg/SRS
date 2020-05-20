# Gaussian mixture models for klustering.
# load necessary libraries
library(ggplot2)
library(RColorBrewer)
library(ggnewscale)
library(scales)

# Load the package
library(mclust)


leg = 6 # column number for the legend you want to plot.
PC_x = 1 # the number of the PC you want to plot on the x-axis
PC_y = 2 # the number of the PC you want to plot on the y-axis


x =as.matrix(cbind(PCAs[,PC_x],PCAs[,PC_y]))

# Select 4 continuous variables and look for three distinct groups.
#mcl.model <- Mclust(x,4, modelNames = c("VEI"))

BIC <- mclustBIC(x)
mcl.model <- Mclust(x, x = BIC)


# Plot our results.
plot(mcl.model, what = "uncertainty", main = "Mclust Classification")




## plot

viz_leg_GMM <- data.frame(x,as.factor(mcl.model$classification),customer_labels[,leg])
colnames(viz_leg_GMM)<-c(colnames(PCAs[PC_x]),colnames(PCAs[PC_y]),"Clusters",colnames(customer_labels[leg]))


counts_GMM<-as.data.frame(table(viz_leg_GMM[,4], viz_leg_GMM[,3]))

counts_perc_GMM<-(table(viz_leg_GMM[,4], viz_leg_GMM[,3]))

for (i in 1:nrow(counts_perc_GMM)){
  counts_perc_GMM[i,]<-round(counts_perc_GMM[i,]/sum(counts_perc_GMM[i,]),3)
}
counts_perc_GMM<-as.data.frame(counts_perc_GMM)

### create and map colors to clusters
library(RColorBrewer)
n <- 60
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
#pie(rep(1,n), col=sample(col_vector, n))

names(col_vector)<-levels(viz_leg_GMM$Clusters)
colScale <- scale_colour_manual(name = "Clusters",values = col_vector)


#two plots beside each other

library(cowplot)

p1 <- ggplot(data=counts_perc_GMM[which(counts_perc_GMM$Freq>0),], aes(x=counts_perc_GMM[which(counts_perc_GMM$Freq>0),][,2], y=counts_GMM[which(counts_GMM$Freq>0),][,3], fill=counts_perc_GMM[which(counts_perc_GMM$Freq>0),][,1])) +
  geom_bar(stat="identity", position="fill") +
  geom_text(aes(label=percent(x=counts_perc_GMM[which(counts_perc_GMM$Freq>0),][,3])), vjust=1.6, color="black",
            position = position_fill(0.9), size=3) +
  labs(title = "Percentages of total label for each cluster",x = colnames(viz_leg_GMM[3]), y = "", fill = colnames(viz_leg_GMM[4]))


p2<- ggplot() + geom_point(alpha = 1,size=1.5, aes(x=viz_leg_GMM[,1], y=viz_leg_GMM[,2],color = viz_leg_GMM[,3]), show.legend = T) +
  colScale +  
  labs(title = "Clustering based on PC of choice",x = colnames(viz_leg_GMM[1]), y=colnames(viz_leg_GMM[2]))


# set labels

p <- plot_grid(p1, p2)
p

