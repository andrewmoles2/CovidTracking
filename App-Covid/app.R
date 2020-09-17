library(shiny)
library(tidyverse)
library(data.table)
library(tibbletime)

# load data
data_url <- 'https://c19downloads.azureedge.net/downloads/csv/coronavirus-cases_latest.csv'
rawdata <- data.table::fread(data_url, check.names = TRUE)
url2 <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
worldRawData <- fread(url2, check.names = T)
# rolling mean
rolling_mean <- tibbletime::rollify(mean, window = 7)

theme_set(theme_bw())

# Define UI ----
ui <- fluidPage(
  titlePanel("COVID-19 lab case tracker"),
  
  sidebarLayout(
    sidebarPanel(
      #local cases
      helpText("Create visualisation of COVID-19 lab cases in your chosen area"),
      selectInput("location",
                  label = "Choose a location to display",
                  choices = unique(rawdata$Area.name),
                  selected = "London"),
      #world cases
      helpText("Create visualisation of COVID-19 lab cases in your chosen country"),
      selectInput("country",
                  label = "Choose a country to display",
                  choices = unique(worldRawData$location),
                  selected = "United Kingdom")),
    mainPanel(
      #local cases figure
      h4("Local visualisation COVID-19 cases data with rolling mean",
         align = "center",
         plotOutput("covidPlot")),
      #world cases figure
      h4("Country visualisation COVID-19 cases and deaths",
         align = "center",
         plotOutput("worldCovid"))
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$covidPlot <- renderPlot({
    
    appData <- reactiveVal(dplyr::filter(rawdata, Area.name == input$location))
    
    ggplot(appData(), aes(Specimen.date , Daily.lab.confirmed.cases)) +
      geom_bar(stat = 'identity', fill = '#34273C') + 
      geom_line(aes(Specimen.date, rolling_mean(Daily.lab.confirmed.cases)), colour = '#E6CF44', size = 1.1) +
      scale_x_date(date_breaks = "2 weeks", date_labels = "%Y-%m-%d") +
      theme(axis.text.x = element_text(size = 12, angle = -15, hjust = 0.3),
            axis.title=element_text(size = 14),
            axis.text.y = element_text(size = 12))
    
  })
  
  output$worldCovid <- renderPlot({
    
    appWorld <- reactiveVal(dplyr::filter(worldRawData, location == input$country))
    
    ggplot(appWorld(), aes(date, new_cases)) + geom_bar(stat = 'identity', aes(fill = "Daily_new_cases"), alpha = 0.9) +
      geom_line(aes(date, new_deaths, colour = "Daily_new_deaths"), size = 1.25) +
      scale_x_date(date_breaks = '2 weeks') +
      theme(axis.text.x = element_text(size = 12, angle = -15, hjust = 0.3),
            axis.title=element_text(size = 14),
            axis.text.y = element_text(size = 12),
            legend.justification=c(.1,.9), legend.position=c(.1,.9)) +
      scale_colour_manual(name = "Deaths",
                          values = c(Daily_new_deaths="#033A22")) +
      scale_fill_manual(name = "Cases", values = c(Daily_new_cases="#6B0308")) 
  })
    
}

# Run app ----
shinyApp(ui = ui, server = server)

