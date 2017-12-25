# Title: app
# Date: December 2017
# Goal: Build shiny app to illustrate our hockey players clustering results


# Load packages -----------------------------------------------------------

library(shiny)
library(ggplot2)
library(ggthemes)
library(broom)

# Load the data -----------------------------------------------------------

source("codes/import_clean.R")


# Define user interface ---------------------------------------------------

ui <- fluidPage(
   
   # Application title
   titlePanel("Hockey players kmeans clustering"),
   
   fluidRow(
     column(5,
            sliderInput("clusters",
                        "Number of clusters:",
                        min = 1,
                        max = 10,
                        value = 5),
            selectizeInput(inputId = "variables", 
                           label = "Variables to include in the clustering:", 
                           choices = colnames(data_final)[c(8:ncol(data_final))], 
                           multiple = TRUE,
                           selected = c("G", "A", "PTS", "TOI_GP", "Hits")),
            selectizeInput(inputId = "team", 
                           "Teams:", 
                           choices = sort(unique(data_final$Team)), 
                           selected = "MTL", 
                           multiple = TRUE),
            selectizeInput(inputId = "position", 
                           "Positions:", 
                           choices = unique(data_final$Position), 
                           selected = c("C", "LW", "RW", "D"), 
                           multiple = TRUE)
     ),
     column(7,
       plotOutput("clusterElbow")
     )
   ),
   hr(),
   fluidRow(
     plotOutput("clusterPlot") 
     ),
   hr(),
   fluidRow(
     column(2,
            selectizeInput(inputId = "x_axis", 
                           label = "x-asis variable:", 
                           choices = c("G", "A", "PTS", "TOI_GP", "Hits"),
                           multiple = FALSE)
            ),
     column(2,
            selectizeInput(inputId = "y_axis", 
                           label = "y-asis variable:",
                           choices = c("G", "A", "PTS", "TOI_GP", "Hits"),
                           multiple = FALSE))
   )
)


# Define server -----------------------------------------------------------

server <- function(input, output, session) {
  
  observe({
    input$variables
    updateSelectizeInput(session, "x_axis",
                      choices = input$variables,
                      selected = input$variables[1])
    updateSelectizeInput(session, "y_axis",
                      choices = input$variables,
                      selected = input$variables[2])
    })
  
  number_clusters <- reactive({
    input$clusters
  })
   
  data_clustering <- reactive({
    data_clustering <- data_final[Team %in% input$team & Position %in% input$position, c(input$variables), with = FALSE]
  })
  
  data_clusters <- reactive({
    kmeans(x = data_clustering(), number_clusters(), nstart = 100)
  })
  
  data_output <- reactive({
    data_output <- cbind(data_final[Team %in% input$team & Position %in% input$position,], data_clusters()$cluster) %>% 
      rename(
        cluster = V2
      )
    data_output[, cluster := as.factor(cluster)]
  })
  
  clusters_analysis <- reactive({
    clusters_analysis <- data.frame(k = 1:9) %>% 
                          group_by(k) %>% 
                          do(kclust = kmeans(data_clustering(), .$k)) %>% 
                          group_by(k) %>%
                          do(glance(.$kclust[[1]]))
  })
  
  output$clusterElbow <- renderPlot({
    ggplot(clusters_analysis(), aes(k, tot.withinss)) + 
      geom_line(size = 2) +
      scale_x_continuous(name = "Number of clusters", breaks = c(1:9)) +
      scale_y_continuous("Total within Sum of Squares") +
      theme_classic()
      
  })
  
  axis <- reactive({
    list(x = input$x_axis,
         y = input$y_axis)
  })
  
  output$clusterPlot <- renderPlot({
    ggplot(data_output(), aes_string(x = axis()$x, y = axis()$y, label = "Last_Name", color = "cluster")) +
      geom_label() +
      theme_classic() +
      theme(legend.position = "none")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

