# A shiny web application for clustering of NKI data. 
# HAnna F. Berg for SRS 2020-06-09

# Load libraries
library(shiny)
library(xlsx)
library(scales)
library(RColorBrewer)
library(ggplot2)
library(ggnewscale)
library(stringr)
#library(mclust)

# Load and shape PC data
PCs <- xlsx::read.xlsx("PCs.xlsx",1)
names(PCs) <- str_replace_all(names(PCs), pattern = "[.]", replacement = " ")
vars <- names(PCs)

# Load and shape legends data
customer_labels <- xlsx::read.xlsx("customer_labels.xlsx", 1)
names(customer_labels) <- str_replace_all(names(customer_labels), pattern = "[.]", replacement = " ")
#customer_labels <- mapply()
for (i in 1:ncol(customer_labels)){
  customer_labels[,i] <- as.factor(customer_labels[,i])
}


################################################# GUI part

ui <-pageWithSidebar(
  
  headerPanel('Customer segmentation NKI data'),
  sidebarPanel(
    selectInput('xcol', 'X Variable', names(PCs)),
    selectInput('ycol', 'Y Variable', names(PCs), selected = names(PCs)[[2]]),
    sliderInput("clusters", "Number of clusters:", min = 1, max = 30, value = 3),
    selectInput('legend', 'Choose Legend', names(customer_labels), selected = names(customer_labels)[[6]]),
    #selectInput("clus_algorithm", "Choose clustering algorith", c("k-means", "GMM"), selected = "k-means"),
    # Button
    downloadButton("downloadData", "Download current dataset")
  ),

  mainPanel(
    
    textOutput("text"),
    plotOutput('plot1'),
    plotOutput("plot2"),
    plotOutput("barplot")
    
  )
)

################################################# Code part

server <- function(input, output, session) {

################# Preparing for output
  
# Create variables for x-axis   
  PC_x <-reactive(PCs[,input$xcol])
  PC_x_label <- reactive(input$xcol)
  
# Create variables for y-axis
  PC_y <- reactive(PCs[,input$ycol])
  PC_y_label <- reactive(input$ycol)

  # Clustering on selected axes and save to variable
  
  clus_algoritm <- reactive(input$clus_algorithm)
  
  ########################################## choose clustering algoritm
  # clus <- reactive({
  #   if (input$clus_algorithm == "k-means"){
  #     clusters <- reactive({
  #       
  #       kmeans(c(PCs[,input$xcol], PCs[,input$ycol]), input$clusters)
  #       
  #     })
  #     reactive(clusters()$cluster)
  #   } else if (input$clus_algoritm == "GMM"){
  #     
  #     mcl.model <- reactive(Mclust(c(PCs[,input$xcol], PCs[,input$ycol])))
  #     reactive(mcl.model()$classification)
  #   }
  #   
  #   
  # })
  
  #############################################################
  
  
# Clustering on selected axes and save to variable
  clusters <- reactive({

    x = data.frame(PC_x(), PC_y())
    kmeans(x, input$clusters)

  })

  clus <- reactive(clusters()$cluster)

 
# Create legend variables
  labels <- reactive(customer_labels[,input$legend])
  n_labels <- reactive(names(customer_labels[input$legend]))

# create our own color chart  
  n <- 30
  qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  
################# Generate output
  
  output$text <- renderText({
# Print stuff here for easier visualization and error search.
    
   })


# Plot legends with clusters circled
  output$plot1 <- renderPlot({
    
  # Preparation
    names(col_vector)<-levels(clus())
    colScale <- scale_colour_manual(name = "Clusters",values = col_vector)
     
  # Plot
    ggplot() +
       
     # scatter plot
       geom_point(alpha = 1,size=1.5, aes(x=PC_x(), y=PC_y(),color = labels())) +
       labs(color=colnames(labels())) + theme(legend.key.size =  unit(0.1, "in"))+
       
     # reset color scale
       new_scale_color() +
       
     #plot ellipses
       stat_ellipse(aes(x = PC_x(), y= PC_y(), color = as.factor(clus()))) + colScale +
       
     # set labels and themes
       labs(title = "Clusters and label of choice plotted",x = PC_x_label(), y=PC_y_label())+
       theme(panel.background = element_rect(fill = "white", color = "black"))
   })
  
  
# Plot only clusters
  output$plot2 <- renderPlot({
  
  #Preparation
    names(col_vector)<-levels(clus())
    colScale <- scale_colour_manual(name = "Clusters",values = col_vector)
  
  #Plot  
    ggplot() +
      
    # scatter plot
      geom_point(aes(x=PC_x(), y=PC_y(),color = as.factor(clus())), alpha = 1, size=1.5, show.legend = T) +
     
    # set color
      colScale + 
      
    # set labels and themes
      labs(title = "Clustering based on PC of choice",x = PC_x_label(), y=PC_y_label(), color = "Clusters") +
      theme(panel.background = element_rect(fill = "white", color = "black"))
    })  
  
  
# Plot barplot
  output$barplot <- renderPlot({
    
  # Preparation
    counts_perc<-table(labels(), clus())
    counts<-as.data.frame(table(labels(), clus()))
    
    for (i in 1:nrow(counts_perc)){
       counts_perc[i,]<-round(counts_perc[i,]/sum(counts_perc[i,]),3)
    }
    
    counts_perc <- as.data.frame(counts_perc)
    
  # Plot
    ggplot(data=counts_perc[which(counts_perc$Freq>0),], aes(x=counts_perc[which(counts_perc$Freq>0),][,2], y=counts[which(counts$Freq>0),][,3], fill=counts_perc[which(counts_perc$Freq>0),][,1])) +
      geom_bar(stat="identity", position="fill") +
      
    # Plot percentages on bars
      geom_text(aes(label=percent(x=counts_perc[which(counts_perc$Freq>0),][,3], accuracy = 0.1)), vjust=1.6, color="black",
      position = position_fill(0.9), size=2)+
      
    # Plot labels and themes
      labs(title = "Percentages of total label for each cluster",x = "Clusters", y = "", fill = n_labels())+
      theme(panel.background = element_rect(fill = "white"))
  })
  
  
#Generate dataset to be downloaded
  
  dataset <- reactive({
    n_clus <- paste0("Kluster 1:", length(unique(clus())))
    n_dataset <- c(PC_x_label(), PC_y_label(), n_clus, n_labels())
    x = data.frame(PC_x = PC_x(), PC_y = PC_y(), clusters = clus(), label = labels())
    setNames(x,n_dataset)
  })


  output$downloadData <- downloadHandler(
    filename = function() {
      paste("current dataset.xlsx", sep = "")
    },
    content = function(file) {
      xlsx::write.xlsx(dataset(), file, row.names = FALSE)
    }
  )
  
} 

################################################# Run app
shinyApp(ui = ui, server = server)