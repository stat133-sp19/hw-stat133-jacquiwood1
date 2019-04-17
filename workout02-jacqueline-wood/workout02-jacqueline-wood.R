#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(reshape2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("$avings"),
   
   fluidRow(
     
     column(4,
            # Sidebar with a slider input for initial amount
            sliderInput("initial",
                        "Initial Amount",
                        min = 1,
                        max = 100000,
                        value = 1000,
                        step = 20000,
                        pre = "$",
                        sep = ",")),
     
     column(4,
            # Sidebar with a slider input for return rate
            sliderInput("return",
                        "Return Rate (in %)",
                        min = 0,
                        max = 20,
                        value = 5,
                        step = 2)),
     
     column(4,
            # Sidebar with a slider input for years
            sliderInput("years",
                        "Years",
                        min = 0,
                        max = 50,
                        value = 10,
                        step = 5))
     ),
     
     fluidRow(
       
       column(4,
              # Sidebar with a slider input for annual contribution
              sliderInput("contrib",
                          "Annual Contribution",
                          min = 0,
                          max = 50000,
                          value = 2000,
                          step = 10000,
                          pre = "$",
                          sep = ",")),
       
       column(4,
              # Sidebar with a slider input for growth rate
              sliderInput("growth",
                          "Growth Rate (in %)",
                          min = 0,
                          max = 20,
                          value = 2,
                          step = 2)),
       
       column(4,
              selectInput("facet",
                          "Facet", 
                          choices = list("No" = 1, "Yes" = 2),
                          selected = 1))
       ),
   
   h4("Timelines"),
      # Show timelines
      plotOutput("timelines"),
   
   h4("Balances"),
   # Table of values
   verbatimTextOutput("balances")
   )

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  modalities <- reactive({ 
     future_value <- function(amount, rate, years) {
       return(amount * ((1 + rate)^years))
     }
     
     annuity <- function(contrib, rate, years) {
       return(contrib * ((((1+ rate)^years) - 1) / rate))
     }
     
     growing_annuity <- function(contrib, rate, growth, years) {
       return(contrib * ((((1 + rate)^years) - ((1 + growth)^years)) / (rate - growth)))
     }
     
     no_contrib <- c()
     fixed_contrib <- c()
     growing_contrib <- c()
     
     for(year in 0:input$years) {
       no_contrib <- c(no_contrib, future_value(input$initial,input$return/100,year))
       fixed_contrib <- c(fixed_contrib, future_value(input$initial,input$return/100,year) + annuity(input$contrib,input$return/100,year))
       growing_contrib <- c(growing_contrib, future_value(input$initial,input$return/100,year) + growing_annuity(input$contrib,input$return/100,input$growth/100,year))
     }
     
     modalities <- data.frame("year" = 0:input$years, "no_contrib" = no_contrib, "fixed_contrib" = fixed_contrib, "growing_contrib" = growing_contrib)
     
     modalities
     })
  
   output$timelines <- renderPlot({
     
     
     melted <- melt(modalities(), id.vars='year')
     names(melted)[2] <- "variable"
     names(melted)[3] <- "value"
     
     if(input$facet == 1){
       ggplot(melted, aes(year,value, col=variable)) + geom_point() + geom_line() + ggtitle("Three modes of investing")
     } else {
       ggplot(melted, aes(year,value, col=variable)) + facet_grid(. ~ variable) + geom_area(aes(fill=variable),alpha=0.4) + geom_point() + geom_line() + ggtitle("Three modes of investing")
     }

     })
   
   output$balances <- renderPrint(modalities())
   
}

# Run the application 
shinyApp(ui = ui, server = server)

