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
    tags$h3("Correlation between numeric variables"),
    tags$h4("Choose the variables you wish to study"),
    # Input for selecting the two variables to compare
    selectInput(inputId = "var1", label = "Variable 1", choices = names(df)[sapply(df, is.numeric)]),
    selectInput(inputId = "var2", label = "Variable 2", choices = names(df)[sapply(df, is.numeric)]),
    actionButton("button1", "Submit"),
    
    # Output for displaying the correlation between the selected variables
    # plotOutput(outputId = "corplot")
    tags$div(style = "margin-top: 20px", textOutput("correlation"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$button1, {
  
    # Remove these rows from the data frame
    print("Removing empty rows")
    df <- df[rowSums(is.na(df)) == 0,]
    
    # Calculate the correlation between the selected variables
    print("Calculating correlation")
    cor_val <- cor(df[, input$var1], df[, input$var2])
    print(cor_val)

    # # Display the correlation in a plot
    # print("Plotting correlations")
    # output$corplot <- renderPlot({
    #   plot(df[, input$var1], df[, input$var2])
    # })
    
    output$correlation <- renderText({
      # Return the value as a character string
      return(paste("Correlation between", input$var1, "and", input$var2, ":", as.character(cor_val)))
    })
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
