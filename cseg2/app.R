# This is a Shiny web application. You can run the application by clicking

# the 'Run App' button above.

#

# Find out more about building applications with Shiny here:

#

#    http://shiny.rstudio.com/

#



library(shiny)
library(xlsx)



# Define UI for application that draws a histogram

# k-means only works with numerical variables,

# so don't give the user the option to select

# a categorical variable


PCs <-  read.xlsx("PCs.xlsx",1)
vars <- names(PCs)
#PCs[,1]<- NULL
customer_labels <- read.xlsx("customer_labels.xlsx", 1)


ui <-pageWithSidebar(
  
  headerPanel('Customer segmentation NKI data'),
  
  sidebarPanel(
    
    selectInput('xcol', 'X Variable', names(PCs)),
    
    selectInput('ycol', 'Y Variable', names(PCs), selected = names(PCs)[[2]]),
    
    sliderInput("clusters", "Number of clusters:", min = 1, max = 30, value = 3),
    
    selectInput('legend', 'Choose Legend', names(customer_labels), selected = names(customer_labels)[[6]])
    
    
  ),


  mainPanel(
    
    plotOutput('plot1'),
    plotOutput("barplot1"),
      tableOutput("table")

    
  )
  
)



# Define server logic required to draw a histogram

server <- function(input, output, session) {
  
  
  
  # Combine the selected variables into a new data frame
  
  selectedData <- reactive({
    
    PCs[, c(input$xcol, input$ycol)]
    
  })
  
  
  
  clusters <- reactive({
    
    kmeans(selectedData(), input$clusters)
    
  })
  
  
  output$plot1 <- renderPlot({
    
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    
    
    par(mar = c(5.1, 4.1, 0, 1))
    
    plot(selectedData(),
         
         col = clusters()$cluster,
         
         pch = 16, cex = 1)
    
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    
  })
  
  
##################### barplot
  PC_x <-reactive(PCs[,input$xcol])
  PC_y <- reactive(PCs[,input$ycol])
  clus <- reactive(clusters()$cluster)
  labels <- reactive(customer_labels[,input$legend])
  
  #counts_perc<-reactive((table(customer_labels[,input$legend], clusters()$cluster)))
  
  #for (i in 1:nrow(counts_perc())){
  #  counts_perc()[i,]<-round(counts_perc()[i,]/sum(counts_perc()[i,]),3)
  #}
  
  #counts_perc<-as.data.frame(counts_perc())
  #counts_perc[,3] <- as.numeric(counts_perc()[,3])
  
  output$barplot1 <- renderPlot({
  
    # 
    # #prepare for barplot
    # counts<-as.data.frame(table(customer_labels[,input$legend], clusters()$cluster))
    # 
    # counts_perc<-(table(customer_labels[,input$legend], clusters()$cluster))
    # 
    # for (i in 1:nrow(counts_perc())){
    #   counts_perc()[i,]<-round(counts_perc()[i,]/sum(counts_perc()[i,]),3)
    # }
    # counts_perc()<-as.data.frame(counts_perc())
    # counts_perc()[,3] <- as.numeric(counts_perc()[,3])
    
    
      

    
   plot(PC_x(), PC_y(), col = labels(), pch = 16)
    legend(3, 5,legend = unique(labels()), fill = unique(labels()))
    
    # below works:
     #plot(selectedData(), col = customer_labels[,input$legend])
    
    })    
  

  output$table <- renderTable({
    
   
   # table(counts_perc())
  })
} 

shinyApp(ui = ui, server = server)