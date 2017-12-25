# Title: app
# Date: December 2017
# Goal: Build shiny app to illustrate our hockey players clustering results


# Load packages -----------------------------------------------------------

library(shiny)
library(ggplot2)
library(ggthemes)

# Load the data -----------------------------------------------------------

source("codes/import_clean.R")


# Define user interface ---------------------------------------------------

ui <- fluidPage(
   
   # Application title
   titlePanel("Hockey players kmeans clustering"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
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
                        choices = unique(data_final$Team), 
                        selected = "MTL", 
                        multiple = TRUE),
         selectizeInput(inputId = "position", 
                        "Positions:", 
                        choices = unique(data_final$Position), 
                        selected = c("C", "LW", "RW", "D"), 
                        multiple = TRUE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("clusterPlot")
      )
   )
)


# Define server -----------------------------------------------------------

server <- function(input, output) {
  
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
  })
  
  output$clusterPlot <- renderPlot({
      ggplot(data_output(), aes(x = Shot.SlapShot, y = TOI_GP, label = Last_Name, color = as.factor(cluster))) +
        geom_label() +
        theme_classic() +
        theme(legend.position = "none")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

