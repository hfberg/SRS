# load ggplot2
library(ggplot2)
library(RColorBrewer)
library(ggnewscale)

# load excel sheet with customaer lables placed in theworking directory.
#customer_lables = read.xlsx(file = "NKI_legends.xlsx",1)

leg = 3 # column number for the legend you want to plot.
PC_x = 1 # the number of the PC you want to plot on the x-axis
PC_y = 5 # the number of the PC you want to plot on the y-axis

# Prepare a data frame with PCs to plot, clusters and lables
viz_leg<-as.data.frame(cbind(PCAs[,PC_x],PCAs[,PC_y],k[["cluster"]]))
viz_leg<-cbind(viz_leg,customer_lables[,leg])
viz_leg[,3]<-as.factor(viz_leg[,3])
viz_leg[,4]<-as.factor(viz_leg[,4])
colnames(viz_leg)<-c(colnames(PCAs[PC_x]),colnames(PCAs[PC_y]),"Clusters",colnames(customer_lables[leg]))

# Plot
plot_leg_clust = ggplot() +

  # scatter plot
  geom_point(alpha = 1,size=1.5, aes(x=viz_leg[,1], y=viz_leg[,2],color = viz_leg[,4]), show.legend = T,inherit.aes = FALSE) +
  scale_color_brewer(name = colnames(viz_leg[4]),palette="Set3") +

  # reset color scale
  new_scale_color() +

  #plot ellipses
  stat_ellipse(show.legend = T, aes(x = viz_leg[,1], y= viz_leg[,2], color = as.factor(k[["cluster"]])), data = viz_leg,inherit.aes = FALSE) +
  scale_color_brewer(name = colnames(viz_leg[3]), palette="Paired") +

  # set lables
  labs(title = "Clusters and lable of choice plotted",x = colnames(viz_leg[1]), y=colnames(viz_leg[2]))
plot_leg_clust


par(mfrow=c(1,2),mai=c(0.7,0.7,0.5,0.5))
counts <- table(viz_leg[,4], viz_leg$Clusters)

barplot(counts,  cex.names = 1,main=colnames(viz_leg[4]), col= as.factor(rownames(counts)),
        beside=TRUE,xlab="")
legend("topleft", legend = rownames(counts), fill =as.factor(rownames(counts)), x.intersp=0.3, horiz=F,cex=0.5)
title(xlab="Cluster index", line=2, cex.lab=0.8)

plot(x=viz_leg[,1], y=viz_leg[,2],xlab="",ylab="",col=k$clust,main= "Clusters", pch=16)
legend("bottomleft",legend = colnames(counts),x.intersp=0.3, fill =as.factor(colnames(counts)),  horiz=F,cex=0.5)
title(ylab=colnames(viz_leg[2]), line=2, cex.lab=0.8)
title(xlab=colnames(viz_leg[1]), line=2, cex.lab=0.8)
