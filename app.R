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
library(reshape2)
library(plyr)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(plotly)
library(heatmaply)
library(dplyr)
library(tidyr)


# We read the CSV
df <- read_csv("data/mxmh_survey_results.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("Music X Mental Health",
             id="tabs",

    tabPanel("Correlations",
             pageWithSidebar(
               headerPanel("Relationships between two variables"),
               sidebarPanel(
                 tags$h3("Correlation between numeric variables"),
                 tags$h4("Choose the variables you wish to study"),
                 # Input for selecting the two variables to compare
                 selectInput(inputId = "var1", label = "Variable 1", choices = names(df)),
                 selectInput(inputId = "var2", label = "Variable 2", choices = names(df)),
                 actionButton("button1", "Submit")
               ),
               mainPanel(
                 # Output for displaying the correlation between the selected variables
                 tags$div(style = "margin-top: 20px", textOutput("correlation")),
                 tags$div(style = "margin-top: 20px", plotlyOutput("corplot")),
               )
             )
    ),

    tabPanel("HeatMap",
             pageWithSidebar(
               headerPanel("Relationships between two variables"),
               sidebarPanel(
                 checkboxInput("check", "Create Heatmap with favorite genre and different mental illnesses"),
                 conditionalPanel(
                   condition = "input.check == false",
                   # Input for selecting the two variables to compare
                   selectInput(inputId = "var3", label = "Categorical Variable", choices = names(df)[!sapply(df, is.numeric)]),
                   selectInput(inputId = "var4", label = "Variable to aggregate 1", choices = names(df)[sapply(df, is.numeric)]),
                   selectInput(inputId = "var5", label = "Variable to aggregate 2", choices = names(df)[sapply(df, is.numeric)]),
                   selectInput(inputId = "var6", label = "Variable to aggregate 3", choices = names(df)[sapply(df, is.numeric)]),
                   selectInput(inputId = "var7", label = "Variable to aggregate 4", choices = names(df)[sapply(df, is.numeric)]),
                 ),
                 actionButton("button2", "Submit")
               ),
               mainPanel(
                 # Output for displaying the heatmap of the selected aggregated variables
                 tags$div(style = "width:100%; height:100%;", plotlyOutput("heatmap"))
               )
             )
    ),

    tabPanel("LineChart",
             pageWithSidebar(
               headerPanel("Evolution of mental illnesses based on subject\'s age"),
               sidebarPanel(
                 checkboxInput("plotAnxiety", "Plot Anxiety illness"),
                 checkboxInput("plotDepression", "Plot Depression illness"),
                 checkboxInput("plotInsomnia", "Plot Insomnia illness"),
                 checkboxInput("plotOCD", "Plot OCD illness"),
                 actionButton("button3", "Submit")
               ),
              mainPanel(
                # Output for displaying the line chart of the selected variables
                tags$div(style = "width:100%; height:100%;", plotlyOutput("plotIllness"))
              )
             )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  # Remove empty rows from the data frame
  print("Removing empty rows")
  df <- df[rowSums(is.na(df)) == 0,]

  observeEvent(input$button1, {
    col1 <- df[, input$var1]
    col2 <- df[, input$var2]

    print(is.numeric(as.data.frame(col1)[[1]]))
    print(is.numeric(as.data.frame(col2)[[1]]))
    print(class(as.data.frame(col1)[[1]]))
    print(class(as.data.frame(col2)[[1]]))


    if(!is.numeric(as.data.frame(col1)[[1]]) && is.numeric(as.data.frame(col2)[[1]])){
      print("Boxplot of col2 grouped by col1")
      output$corplot <- renderPlotly({
        boxplot(unlist(col2) ~ unlist(col1), xlab = input$var1, ylab = input$var2, main = paste("Boxplot of", input$var2, "grouped by", input$var1))
      })
    }

    if(!is.numeric(as.data.frame(col2)[[1]]) && is.numeric(as.data.frame(col1)[[1]])){
      print("Boxplot of col1 grouped by col2")
      output$corplot <- renderPlotly({
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
      output$corplot <- renderPlotly({
        print("Scatter plot of col1 vs col2")
        ggplot(data = df, aes(x = unlist(col1), y = unlist(col2))) +
          geom_point() +
          labs(title = "Scatter plot of col1 vs col2", x = input$var1,  y = input$var2)
      })
    }
  })

  observeEvent(input$button2, {
    if(isTRUE(input$check)){
      df2 <- df %>% select("Fav genre", "Anxiety", "Depression", "Insomnia", "OCD")
      print("Aggregating variables")
      df_agg <- aggregate(cbind(Anxiety, Depression, Insomnia, OCD) ~ `Fav genre`, df2, sum)
      print("Heatmap")
      output$heatmap <- renderPlotly({
        heatmaply(df_agg)
      })
    }
    else{
      df2 <- df %>% select(input$var3, input$var4, input$var5, input$var6, input$var7)
      col_labels <- c(input$var4, input$var5, input$var6, input$var7)
      print(input$var3)
      print(input$var4)
      print("Aggregating selected variables")
      df_agg <- aggregate(cbind(unlist(df2[,input$var4]), unlist(df2[,input$var5]), unlist(df2[,input$var6]), unlist(df2[,input$var7])) ~ unlist(df2[,input$var3]), df2, sum)
      colnames(df_agg)[1] <- input$var3
      print("Heatmap")
      output$heatmap <- renderPlotly({
        heatmaply(df_agg, labCol = col_labels)
      })
    }
  })

  observeEvent(input$button3, {
    df3 <- df %>%
      group_by(Age) %>%
      summarize(Anxiety = mean(Anxiety),
                Depression = mean(Depression),
                Insomnia = mean(Insomnia),
                OCD = mean(OCD))
    output$plotIllness <- renderPlotly({
      print("Line Chart mental illnesses vs age")
      plot_ly(df3, x = ~Age, y = ~Anxiety, name = "Anxiety",
              type = "scatter", mode = "lines+markers",
              line = list(width = 2, dash = "solid", color = "blue")) %>%
        layout(title = "Line Chart",
               xaxis = list(title = "Age"),
               yaxis = list(title = "Illness Mean Degree")
        )
    })
    
    if(isTRUE(input$plotAnxiety)){
      
    }

    # df3 <- df %>%
    #   group_by(Age) %>%
    #   summarize(Anxiety = mean(Anxiety),
    #             Depression = mean(Depression),
    #             Insomnia = mean(Insomnia),
    #             OCD = mean(OCD))
    # 
    # output$plotIllness <- renderPlotly({
    #   print("Line Chart mental illnesses vs age")
    #   plot_ly(df3, x = ~Age, y = ~Anxiety, name = "Anxiety",
    #           type = "scatter", mode = "lines+markers",
    #           line = list(width = 2, dash = "solid", color = "blue")) %>%
    #     add_trace(x = ~Age, y = ~Depression, name = "Depression",
    #               type = "scatter", mode = "lines+markers",
    #               line = list(width = 2, dash = "solid", color = "red")) %>%
    #     add_trace(x = ~Age, y = ~Insomnia, name = "Insomnia",
    #               type = "scatter", mode = "lines+markers",
    #               line = list(width = 2, dash = "solid", color = "yellow")) %>%
    #     add_trace(x = ~Age, y = ~OCD, name = "OCD",
    #               type = "scatter", mode = "lines+markers",
    #               line = list(width = 2, dash = "solid", color = "green")) %>%
    #     layout(title = "Line Chart",
    #            xaxis = list(title = "Age"),
    #            yaxis = list(title = "Illness Mean Degree")
    #     )
    # })
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)