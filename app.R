#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)

# We read the CSV
df <- read_csv("data/mxmh_survey_results.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Music Taste X Mental Problemitas"),
    # Input for selecting the two variables to compare
    selectInput(inputId = "var1", label = "Variable 1", choices = names(df)[sapply(df, is.numeric)]),
    selectInput(inputId = "var2", label = "Variable 2", choices = names(df)[sapply(df, is.numeric)]),
    
    # Output for displaying the correlation between the selected variables
    plotOutput(outputId = "corplot")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  col1 <- reactive({ df[, input$var1] })
  col2 <- reactive({ df[, input$var2] })
  
  # Remove rows with missing values
  col1 <- reactive({ col1[!is.na(col1)] })
  col2 <- reactive({ col2[!is.na(col2)] })
  
  print("pollon")
  print(class(col1))
  print(col1)
  
  if(is.numeric(col1) && is.numeric(col2)){
    # Calculate the correlation between the selected variables
    cor_val <- reactive({
      cor(col1, col2)
    })
    
    # Display the correlation in a plot
    output$corplot <- renderPlot({
      plot(col1, col2, main = paste("Correlation:", cor_val()))
    })
  }
}

# Run the application 
shinyApp(ui = ui, server = server)
