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
library(caret)

# We read the CSV
df <- read_csv("data/mxmh_survey_results.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Music Taste X Mental Problemitas"),
    tags$h3("Correlation between numeric variables"),
    tags$h4("Choose the variables you wish to study"),
    # Input for selecting the two variables to compare
    selectInput(inputId = "var1", label = "Variable 1", choices = names(df)),
    selectInput(inputId = "var2", label = "Variable 2", choices = names(df)),
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
    col1 <- df[, input$var1]
    col2 <- df[, input$var2]
    
    
    # if(!is.numeric(df[, input$var1])){
    #   print("OneHotEncoding col1...")
    #   # Now, create the one-hot encoder object
    #   encoder <- OneHotEncoder(cols = input$var1)
    #   # Fit the encoder to the data
    #   encoder <- fit(encoder, data)
    #   # Transform the data using the encoder
    #   col1 <- transform(encoder, data)
    # }
    # 
    # if(!is.numeric(df[, input$var2])){
    #   print("OneHotEncoding col2...")
    #   # Now, create the one-hot encoder object
    #   encoder <- OneHotEncoder(cols = input$var2)
    #   # Fit the encoder to the data
    #   encoder <- fit(encoder, data)
    #   # Transform the data using the encoder
    #   col2 <- transform(encoder, data)
    # }
    
    
    # Calculate the correlation between the selected variables
    print("Calculating correlation")
    cor_val <- cor(col1, col2)
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
