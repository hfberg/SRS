# load ggplot2
library(ggplot2)
library(RColorBrewer)
library(ggnewscale)

# load excel sheet with customaer lables placed in theworking directory.
#customer_lables = read.xlsx(file = "NKI_legends.xlsx",1)

leg = 6 # column number for the legend you want to plot.
PC_x = 1 # the number of the PC you want to plot on the x-axis
PC_y = 4 # the number of the PC you want to plot on the y-axis


# Prepare a data frame with PCs to plot, clusters and lables
viz_leg<-as.data.frame(cbind(PCAs[,PC_x],PCAs[,PC_y]))

# re-cluster based on new PCs?
k_for_plot <- kmeans(viz_leg, 18, nstart=25, iter.max=1000)
viz_leg<-as.data.frame(cbind(viz_leg,k_for_plot[["cluster"]]))

viz_leg<-cbind(viz_leg,customer_lables[,leg])
viz_leg[,3]<-as.factor(viz_leg[,3])
viz_leg[,4]<-as.factor(viz_leg[,4])
colnames(viz_leg)<-c(colnames(PCAs[PC_x]),colnames(PCAs[PC_y]),"Clusters",colnames(customer_lables[leg]))


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
  geom_point(alpha = 1,size=1.5, aes(x=viz_leg[,1], y=viz_leg[,2],color = viz_leg[,4]), show.legend = T,inherit.aes = FALSE) +
  scale_color_brewer(name = colnames(viz_leg[4]),palette="Set3") +
  
  # reset color scale
  new_scale_color() +
  
  #plot ellipses
  stat_ellipse(show.legend = T, aes(x = viz_leg[,1], y= viz_leg[,2], color = as.factor(viz_leg[,3])), data = viz_leg,inherit.aes = FALSE) +
  colScale +
  #scale_color_brewer(name = colnames(viz_leg[3]), palette="Paired") +
  
  # set lables
  labs(title = "Clusters and lable of choice plotted",x = colnames(viz_leg[1]), y=colnames(viz_leg[2]))
plot_leg_clust
#dev.off()

#two plots beside each other
par(mfrow=c(1,2),mai=c(0.7,0.7,0.5,0.5))
counts <- table(viz_leg[,4], viz_leg[,3])

#barplot
barplot(counts,  cex.names = 1,main=colnames(viz_leg[4]), col= as.factor(rownames(counts)),
        beside=TRUE,xlab="")
legend("topleft", legend = rownames(counts), fill =as.factor(rownames(counts)), x.intersp=0.3, horiz=F,cex=0.5)
title(xlab="Cluster index", line=2, cex.lab=0.8)

#scatter of clusters
plot(x=viz_leg[,1], y=viz_leg[,2],xlab="",ylab="",col=as.factor(viz_leg[,3]),main= "Clusters", pch=16)
legend("topright",legend = colnames(counts),x.intersp=0.3, fill =as.factor(colnames(counts)),  horiz=F,cex=0.5)
title(ylab=colnames(viz_leg[2]), line=2, cex.lab=0.8)
title(xlab=colnames(viz_leg[1]), line=2, cex.lab=0.8)
