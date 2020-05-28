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

vars <- names(PCs)
PCs <-  read.xlsx("PCs.xlsx",1)
PCs[,1]<- NULL
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
    plotOutput("barplot1")
    
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
  
  
  
  ################## barplot
  
  viz_leg <<- reactive({
    
    df$PC_x <-as.data.frame(PCs[,input$xcol]) 
    df$PC_y <- as.data.frame(PCs[,input$ycol])
    df$clusters <- as.data.frame(clusters()$cluster)
    df$labels <- as.data.frame(customer_labels[,input$legend])
    
    return(df)
    
    })
  
 
  
  output$barplot1 <- renderPlot({
    
   plot(selectedData(), col = df$clusters)
    
    # below works:
   #  plot(selectedData(), col = customer_labels[,input$legend])
    
  #viz_leg()$PC_x, viz_leg()$PC_y
    })    
    
} 


# 
# ggplot() + geom_point(alpha = 1,size=1.5, aes(x=viz_leg()PC_x, y=viz_leg()PC_y,color = viz_leg()clusters), show.legend = T) +
#   colScale + guides(col = guide_legend(nrow = floor(k/2))) +
#   labs(title = "Clustering based on PC of choice",x = colnames(viz_leg[1]), y=colnames(viz_leg[2]))
# Run the application 

shinyApp(ui = ui, server = server)