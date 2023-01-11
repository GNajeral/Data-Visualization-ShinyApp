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
library(mltools)
library(data.table)
library(ggplot2)
library(ggstream)


# We read the CSV
df <- read_csv("data/mxmh_survey_results.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Music Taste X Mental Health"),
  tags$h3("Correlation between numeric variables"),
  tags$h4("Choose the variables you wish to study"),
  
  # Input for selecting the two variables to compare
  selectInput(inputId = "var1", label = "Variable 1", choices = names(df)),
  selectInput(inputId = "var2", label = "Variable 2", choices = names(df)),
  #selectInput(inputId = "var1", label = "Variable 1 (time)", choices = names(df)),
  #selectInput(inputId = "var2", label = "Variable 2 (flow)", choices = names(df)),
  selectInput(inputId = "var3", label = "Variable 3", choices = names(df)),
  actionButton("button1", "Submit"),
  
  # Output for displaying the correlation between the selected variables
  tags$div(style = "margin-top: 20px", textOutput("correlation")),
  tags$div(style = "margin-top: 20px", plotOutput("corplot")),
  
  # Output for displaying the streamgraph
  actionButton("create_streamgraph", "Create Streamgraph"),
  plotOutput("streamgraph")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$button1, {
    
    # Remove empty rows from the data frame
    print("Removing empty rows")
    df <- df[rowSums(is.na(df)) == 0,]
    col1 <- df[, input$var1]
    col2 <- df[, input$var2]

    print(is.numeric(as.data.frame(col1)[[1]]))
    print(is.numeric(as.data.frame(col2)[[1]]))
    print(class(as.data.frame(col1)[[1]]))
    print(class(as.data.frame(col2)[[1]]))
    
    
    if(!is.numeric(as.data.frame(col1)[[1]]) && is.numeric(as.data.frame(col2)[[1]])){
      print("Boxplot of col2 grouped by col1")
      output$corplot <- renderPlot({
        boxplot(unlist(col2) ~ unlist(col1), xlab = input$var1, ylab = input$var2, main = paste("Boxplot of", input$var2, "grouped by", input$var1))
      })
    }
    
    if(!is.numeric(as.data.frame(col2)[[1]]) && is.numeric(as.data.frame(col1)[[1]])){
      print("Boxplot of col1 grouped by col2")
      output$corplot <- renderPlot({
        boxplot(unlist(col1) ~ unlist(col2), xlab = input$var2, ylab = input$var1, main = paste("Boxplot of", input$var1, "grouped by", input$var2))
      })
    }
    
    if(is.numeric(as.data.frame(col1)[[1]]) && is.numeric(as.data.frame(col2)[[1]])){
      # Calculate the correlation between the selected variables
      print("Calculating correlation")
      cor_val <- cor(col1, col2)
      print(cor_val)
      print("Correlation between col1 and col2")
      output$correlation <- renderText({
        # Return the value as a character string
        return(paste("Correlation between", input$var1, "and", input$var2, ":", as.character(cor_val)))
      })
      output$corplot <- renderPlot({
        print("Scatter plot of col1 vs col2")
        ggplot(data = df, aes(x = unlist(col1), y = unlist(col2))) +
          geom_point() +
          labs(title = "Scatter plot of col1 vs col2", x = input$var1,  y = input$var2)
      })
    }
    
  })
  
  observeEvent(input$create_streamgraph, {
    
    # Remove empty rows from the data frame
    print("Removing empty rows")
    df <- df[rowSums(is.na(df)) == 0,]
    col1 <- df[, input$var1]
    col2 <- df[, input$var2]
    col3 <- df[, input$var3]
    
    print(is.numeric(as.data.frame(col1)[[1]]))
    print(is.numeric(as.data.frame(col2)[[1]]))
    print(is.numeric(as.data.frame(col3)[[1]]))
    print(class(as.data.frame(col1)[[1]]))
    print(class(as.data.frame(col2)[[1]]))
    print(class(as.data.frame(col3)[[1]]))
    
    if(is.numeric(as.data.frame(col1)[[1]]) && is.numeric(as.data.frame(col2)[[1]]) && !is.numeric(as.data.frame(col3)[[1]])){
      # Create a streamgraph
      output$streamgraph <- renderPlot({
        print("Creating a streamgraph")
        ggplot(data = df, aes(x = unlist(col1), y = unlist(col2), fill = unlist(col3))) +
          geom_stream() +
          labs(title = "Streamgraph of col2 over col1", x = input$var1,  y = input$var2)
      })
      print("Done")
      # output$streamgraph <- renderPlot({
      #   streamgraph(data, x = input$var1, y = input$var2, group = input$var3, type = "normalized", main = paste("Streamgraph of", input$var2, "over", input$var1))
      # })
    }
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)