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
ui <- navbarPage("Inferential Stats",
                 tabPanel("Random Sampling",
                          
                          
                          # sidebarLayout(
                          #   sidebarPanel(
                          #     sliderInput("b",
                          #                 "Select sample size:",
                          #                 min = 1,
                          #                 max = 100,
                          #                 value = 20)
                          #   ),
                          # mainPanel(
                          #   plotOutput("distPlot",hover = "plot_hover")
                          # ))
                          column(4,  fluidRow(
                            column(12,
                                   h4("Choose a sample size:"),
                                   sliderInput('samplesize1', 'Sample Size', 
                                               min=1, max=100, value=20),
                                   
                                   radioButtons("replace1", label = h5("Choose:"),
                                                choices = list("With Replacement" = 1, "Without Replacement" = 2))),
                            actionButton("plot1", label = "Plot"))                                     
                          ),
                          column(8,
                                 plotOutput("distPlot1"))),
                 
                 
                 
                 
                 tabPanel("Normal Dist.",
                          column(3, fluidRow(
                            
                            column(12,
                                   sliderInput('samplesize2', 'Sample Size', 
                                               min=1, max=100, value=20),
                                   
                                   
                                   
                                   sliderInput('mean2', 'mean', 
                                               min=-100, max=100, value=50),
                                   
                                   
                                   
                                   sliderInput('sd2', 'SD', 
                                               min=1, max=100, value=50),
                                   
                                   
                                   
                                   actionButton("plot2", label = "Plot")    
                                   
                            ))),
                          column(8, 
                                 plotOutput("distPlot2"))
                          
                          
                          
                 ),
                 
                 tabPanel("Uniform Dist.",
                          
                          
                          
                          column(3, fluidRow(
                            
                            column(12,
                                   sliderInput('samplesize3', 'Sample Size', 
                                               min=1, max=100, value=20),
                                   
                                   
                                   
                                   sliderInput('min3', 'Minimum', 
                                               min=-100, max=100, value=1),
                                   
                                   
                                   
                                   sliderInput('max3', 'Maximum', 
                                               min=-100, max=100, value=5),
                                   
                                   
                                   
                                   actionButton("plot3", label = "Plot")    
                                   
                            ))),
                          column(8, 
                                 plotOutput("distPlot3"))
                          
                          
                          
                          
                          
                          
                          
                 ),
                 tabPanel("Exponential Dist.",
                          
                          
                          
                          
                          
                          column(3, fluidRow(
                            
                            column(12,
                                   sliderInput('samplesize4', 'Sample Size', 
                                               min=1, max=100, value=20),
                                   
                                   
                                   
                                   sliderInput('rate4', 'rate (or lambda)', 
                                               min=1, max=10, value=2),
                                   
                                   
                                   
                                   
                                   
                                   
                                   
                                   actionButton("plot4", label = "Plot")    
                                   
                            ))),
                          column(8, 
                                 plotOutput("distPlot4"))
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                 )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot1 <- renderPlot({ 
    if (input$plot1>=1) { 
      if (input$replace1 == 1)
      {x1 <- sample(c(1:100), input$samplesize1, replace=TRUE)
      hist(x1, col = 'blue', border = 'white', xlim=c(0,100), breaks=10)}
      else {x1 <- sample(c(1:100), input$samplesize1, replace=FALSE)
      hist(x1, col = 'blue', border = 'white', xlim=c(0,100), breaks=10)}
    }
    
  })
  
  
  output$distPlot2 <- renderPlot({ 
    if (input$plot2>=1) { 
      
      x2 <- rnorm(input$samplesize2, mean=input$mean2, sd = input$sd2)
      hist(x2, col = 'blue', border = 'white', xlim=c(-300,300), breaks = 10)
      
    }
    
  })
  
  output$distPlot4 <- renderPlot({ 
    if (input$plot4>=1) { 
      
      x4 <- rexp(n=input$samplesize4, rate = input$rate4)
      hist(x4, col = 'blue', border = 'white', xlim=c(-3,5))
      
    }
    
  })
  
  output$distPlot3 <- renderPlot({ 
    if (input$plot3>=1) { 
      
      x3 <- runif(input$samplesize3, input$min3, input$max3)
      hist(x3, col = 'blue', border = 'white', xlim=c(-10,10))
      
    }
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

