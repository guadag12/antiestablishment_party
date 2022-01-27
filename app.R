library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(shiny)
library(viridis)
library(plotly)
party_antiestablishment <- read_csv("V-Dem-CPD-Party-V1.csv")

# Keep only 3 names
party_antiestablishment <- party_antiestablishment %>% select(nombre_partido=v2paenname, v2pashname, 
                                                              country_name, year, 
                                                              ep_antielite_salience ) %>%
  filter(!is.na(ep_antielite_salience)) %>%
  mutate(ep_antielite_salience = round(ep_antielite_salience,0))

ui <-fluidPage(    
    
    # Give the page a title
    titlePanel("Antielite measure by party and year"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        selectInput(inputId = "pais", label = "Country:", selected = "Belgium", 
                    choices=unique(party_antiestablishment$country_name)),
        hr(),
        sliderInput("slider", label = h3("Year Range"), min = min(party_antiestablishment$year), 
                    max = max(party_antiestablishment$year), value = c(min(party_antiestablishment$year),
                                                                      max(party_antiestablishment$year) )),    
        hr(),
        helpText("Antielite measure: Intervalar (0-10) 0: 'Not important at all', 10:'Extremely important', 
                 Data from V-Party dataset (Oct, 2020).")
        ),
      
      # Create a spot for the barplot
      mainPanel(
        plotlyOutput("antiestablishmentPlot")  
      )
      
    )
)


  server <- function(input, output, session) {
  output$antiestablishmentPlot <- renderPlotly({
    p <- party_antiestablishment %>% 
      filter(country_name == input$pais) %>%
      filter(!is.na(ep_antielite_salience)) %>%
      filter(year >= input$slider[1] & year <= input$slider[2]) %>%
      mutate(year = as.factor(year)) %>%
      ggplot( aes(x=year, y=ep_antielite_salience, group=nombre_partido, fill =nombre_partido, color=nombre_partido)) +
      geom_line(alpha = 0.5, size = 1.5) +
      geom_point(alpha = 0.5, size = 2.5, position=position_jitter(h=0.1, w=0.1)) +
      theme_bw() +
      labs(x = "Year", y = "Antielite measure") +
      scale_colour_viridis_d(alpha = 0.5) +
      scale_fill_viridis_d(alpha = 0.5) +
      theme(text=element_text(size=11))
    ggplotly(p)
  })
  
}

shinyApp(ui, server)