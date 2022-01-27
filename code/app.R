library(sqldf)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(shiny)

party_antiestablishment <- read.csv("/Users/guadalupeandreagonzalez/Downloads/CPD_V-Party_CSV_v1/V-Dem-CPD-Party-V1.csv")

# Keep only 3 names
party_antiestablishment <- party_antiestablishment %>% select(nombre_partido=v2paenname, v2pashname, 
                                                              country_name, year, 
                                                              ep_antielite_salience )

ui <- fluidPage(
  fluidPage(    
    
    # Give the page a title
    titlePanel("Antielite by party and year"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        selectInput("pais", "Country:", 
                    choices=unique(country_name)),
        hr(),
        sliderInput("slider", label = h3("Year Range"), min = min(year), 
                    max = max(year), value = c(min(year), max(year) ))    
        hr(),
        helpText("Data from V-Party dataset (Oct, 2020).")
      ),
      
      # Create a spot for the barplot
      mainPanel(
        plotOutput("antiestablishmentPlot")  
      )
      
    )
  )
)

server <- function(input, output, session) {
  output$antiestablishmentPlot <- renderPlot({
    party_antiestablishment %>% 
      filter(country_name == input$pais) %>%
      filter(year >= input$slider[1] & year <= input$slider[2]) %>%
      ggplot( aes(x=year, y=ep_antielite_salience, group=nombre_partido, color=nombre_partido)) +
      geom_line() +
      theme_bw() 
  })
  
}

shinyApp(ui, server)
