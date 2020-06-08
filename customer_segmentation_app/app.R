# This is a Shiny web application. You can run the application by clicking

# the 'Run App' button above.

#

# Find out more about building applications with Shiny here:

#

#    http://shiny.rstudio.com/

#



library(shiny)
library(xlsx)
library(scales)
library(RColorBrewer)
library(ggplot2)
library(ggnewscale)


# Define UI for application that draws a histogram

# k-means only works with numerical variables,

# so don't give the user the option to select

# a categorical variable


PCs <- xlsx::read.xlsx("PCs.xlsx",1)
#PCs[,1]<- NULL
customer_labels <- xlsx::read.xlsx("customer_labels.xlsx", 1)
customer_labels[,1] <- NULL
vars <- names(PCs)



ui <-pageWithSidebar(
  
  headerPanel('Customer segmentation NKI data'),
  
  sidebarPanel(
    
    selectInput('xcol', 'X Variable', names(PCs)),
    
    selectInput('ycol', 'Y Variable', names(PCs), selected = names(PCs)[[2]]),
    
    sliderInput("clusters", "Number of clusters:", min = 1, max = 30, value = 3),
    
    selectInput('legend', 'Choose Legend', names(customer_labels), selected = names(customer_labels)[[6]])
    
    
  ),


  mainPanel(
    
    textOutput("text"),
    plotOutput('plot1'),
    plotOutput("barplot1"),
    tableOutput("table"),
    plotOutput("barplot2")


    
  )
  
)

###################################################################################################



server <- function(input, output, session) {
  
  PC_x <-reactive(PCs[,input$xcol])
  PC_x_label <- reactive(input$xcol)
  
  
  PC_y <- reactive(PCs[,input$ycol])
  PC_y_label <- reactive(input$ycol)
  
  clusters <- reactive({
    
    x = data.frame(PC_x(), PC_y())
    kmeans(x, input$clusters)
    
  })

  clus <- reactive(clusters()$cluster)
  labels <- reactive(customer_labels[,input$legend])
  n_labels <- reactive(names(customer_labels[,input$legend]))

  n <- 60
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  #pie(rep(1,n), col=sample(col_vector, n))
  
  output$text <- renderText({
  # prit stuff here for easier visualization and error search.
  })

  
   output$plot1 <- renderPlot({
    
     names(col_vector)<-levels(clus())
     colScale <- scale_colour_manual(name = "Clusters",values = col_vector)
     
      plot_leg_clust = ggplot() +
       
       # scatter plot
       geom_point(alpha = 1,size=1.5, aes(x=PC_x(), y=PC_y(),color = labels())) +
       labs(color=colnames(labels())) + theme(legend.key.size =  unit(0.1, "in"))+
       
       # reset color scale
       new_scale_color() +
       
       #plot ellipses
       stat_ellipse(aes(x = PC_x(), y= PC_y(), color = as.factor(clus()))) +
       colScale +
       
       
       # set labels
       labs(title = "Clusters and label of choice plotted",x = PC_x_label(), y=PC_y_label())+
       theme(panel.background = element_rect(fill = "white", color = "black"))
     plot_leg_clust
   })
  
  
##################### barplot

  output$barplot1 <- renderPlot({
  
    ggplot() + geom_point(alpha = 1,size=1.5, aes(x=PC_x(), y=PC_y(),color = as.factor(clus())), show.legend = T) +
     # colScale + 
      labs(title = "Clustering based on PC of choice",x = PC_x_label(), y=PC_y_label(), color = "Clusters") +
      theme(panel.background = element_rect(fill = "white", color = "black"))
    
    })  
  
  output$barplot2 <- renderPlot({
    counts_perc<-table(labels(), clus())
    counts<-as.data.frame(table(labels(), clus()))
    
      for (i in 1:nrow(counts_perc)){
         counts_perc[i,]<-round(counts_perc[i,]/sum(counts_perc[i,]),3)
      }
    counts_perc <- as.data.frame(counts_perc)
    ggplot(data=counts_perc[which(counts_perc$Freq>0),], aes(x=counts_perc[which(counts_perc$Freq>0),][,2], y=counts[which(counts$Freq>0),][,3], fill=counts_perc[which(counts_perc$Freq>0),][,1])) +
      geom_bar(stat="identity", position="fill") +
      geom_text(aes(label=percent(x=counts_perc[which(counts_perc$Freq>0),][,3], accuracy = 0.1)), vjust=1.6, color="black",
                position = position_fill(0.9), size=2)+ 
      theme(panel.background = element_rect(fill = "white")) +
      labs(title = "Percentages of total label for each cluster",x = unique("Clusters"), y = "", fill = n_labels())
    
    #working
      #barplot(counts_perc, legend.text = unique(labels()))
    
  })
  
} 

shinyApp(ui = ui, server = server)