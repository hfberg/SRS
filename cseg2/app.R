#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
 # k-means only works with numerical variables,
    # so don't give the user the option to select
    # a categorical variable
    vars <- names(PCs)

ui <-pageWithSidebar(
    headerPanel('Customer segmentation NKI data'),
    sidebarPanel(
        selectInput('xcol', 'X Variable', vars),
        selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
        sliderInput("clusters", "Number of clusters:", min = 1, max = 30, value = 3)
    ),
    mainPanel(
        plotOutput('plot1')
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
    
} 

# Run the application 
shinyApp(ui = ui, server = server)
