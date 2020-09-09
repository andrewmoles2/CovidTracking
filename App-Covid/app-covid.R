library(shiny)
library(tidyverse)
library(data.table)
library(tibbletime)

# load data
data_url <- 'https://c19downloads.azureedge.net/downloads/csv/coronavirus-cases_latest.csv'
rawdata <- data.table::fread(data_url, check.names = TRUE)
# rolling mean
rolling_mean <- tibbletime::rollify(mean, window = 7)

# Define UI ----
ui <- fluidPage(
  titlePanel("Local COVID-19 lab case tracker"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create visualisation of COVID-19 lab cases in your area"),
                 
      selectInput("location",
                  label = "Choose a location to display",
                  choices = unique(rawdata$Area.name),
                  selected = "London")),
    mainPanel(
      h4("Visualisation of local COVID-19 cases data with rolling mean",
         align = "center",
         plotOutput("covidPlot"))
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$covidPlot <- renderPlot({
    
    appData <- reactiveVal(filter(rawdata, Area.name == input$location))
    
    theme_set(theme_bw())
    
    ggplot(appData(), aes(Specimen.date , Daily.lab.confirmed.cases)) +
      geom_bar(stat = 'identity', fill = '#34273C') + 
      geom_line(aes(Specimen.date, rolling_mean(Daily.lab.confirmed.cases)), colour = '#E6CF44', size = 1.1) +
      scale_x_date(date_breaks = "2 weeks", date_labels = "%Y-%m-%d") +
      theme(axis.text.x = element_text(size = 12, angle = -15, hjust = 0.3),
            axis.title=element_text(size = 14),
            axis.text.y = element_text(size = 12))
    
  })
  
    
}

# Run app ----
shinyApp(ui = ui, server = server)

