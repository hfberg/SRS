# load ggplot2
library(ggplot2)
library(RColorBrewer)
library(ggnewscale)

customer_lables<-customer_lables_backup

# load excel sheet with customaer lables placed in theworking directory.
#customer_lables = read.xlsx(file = "NKI_legends.xlsx",1)

leg =43 # column number for the legend you want to plot.
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
  geom_point(alpha = 1,size=1.5, aes(x=viz_leg[,1], y=viz_leg[,2],color = viz_leg[,4])) +
  
  # reset color scale
  new_scale_color() +
  
  #plot ellipses
  stat_ellipse(aes(x = viz_leg[,1], y= viz_leg[,2], color = as.factor(viz_leg[,3])), data = viz_leg) +
  colScale +

  
  # set lables
  labs(title = "Clusters and lable of choice plotted",x = colnames(viz_leg[1]), y=colnames(viz_leg[2]))
plot_leg_clust
#dev.off()
#save_plot("plot1.pdf",plot_leg_clust)

#prepare for barplot
counts_leg<-(table(viz_leg[,4], viz_leg[,3]))
for (i in 1:nrow(counts_leg))
  {counts_leg[i,]<-round(counts_leg[i,]/sum(counts_leg[i,]),3)
  }
counts_leg<-as.data.frame(counts_leg)

#two plots beside each other

library(cowplot)

p1 <- ggplot(data=counts_leg, aes(x=counts_leg[,2], y=counts_leg[,3], fill=counts_leg[,1])) +
  geom_bar(stat="identity", position="fill") +
  geom_text(aes(label=percent(x=counts_leg[,3])), vjust=1.6, color="black",
            position = position_fill(0.9), size=3) +
  labs(title = "Percentages of total lable for each cluster", x = colnames(viz_leg[3]), y = colnames(viz_leg[4]), fill = colnames(viz_leg[4]))


p2<- ggplot() + geom_point(alpha = 1,size=1.5, aes(x=viz_leg[,1], y=viz_leg[,2],color = viz_leg[,3]), show.legend = T) +
  colScale +
  labs(title = "Clustering based on PC of choice",x = colnames(viz_leg[1]), y=colnames(viz_leg[2]))

  
# set lables

p <- plot_grid(p1, p2, labels = c(colnames(viz_leg[4]), colnames(viz_leg[3])))
p
save_plot("plot2.pdf",p, ncol = 2)
