# load packages ---- 
#(dashes help create outline on right)
library(shiny)

#user interface ----
ui <- fluidPage(
  
  # app title ----
  tags$h1("My App Title"),
  
  # app subtitle ----
  p(strong("Exploring Antarctic Penguin Data")), #strong bolds text
  
  sliderInput(inputId = "body_mass_input",
              label = "Select a range of body masses (g)",
              value = c(3000, 4000), #sets defaults of slider, min and max set min/max of slider
              min = 2700,
              max = 6300
              ), #EO of slider
  
  selectInput(inputId = "island_input",
              label = "Choose an island",
              choices = c("Torgersen", "Biscoe", "Dream")),
  

  
  plotOutput(outputId = "bodyMass_scatterPlot"), #don't see a change in app bc haven't made plot yet, but have placeholder for plot
  
  
  checkboxGroupInput(inputId = "year_input",
                     label = "Choose a Year",
                     choices = c("2007", "2008", "2009"),
                     selected = c("2007", "2008")),
  
  DT::dataTableOutput(outputId = "penguin_data") #we are naming the IDs so we can call them in render section
  #DT::dataTableOutput(outputId = "year_input")
  
) #end of (EO) fluidPage

#server instruction----

server <- function(input, output) {
 
  #filter body massess ----
  body_mass_df <- reactive({
    
    penguins %>% 
      filter(body_mass_g %in% input$body_mass_input[1]:input$body_mass_input[2])
    
  }) #EO of reactive body mass
  
  #render the scatter plot ----
  output$bodyMass_scatterPlot <- renderPlot({
    
    #code to generate plot
    # load packages
    library(palmerpenguins)
    library(tidyverse)
    
    # create plot
    ggplot(na.omit(body_mass_df()), #all reactive elements need () after them
           aes(x = flipper_length_mm, y = bill_length_mm, 
               color = species, shape = species)) +
      geom_point() +
      scale_color_manual(values = c("Adelie" = "#FEA346", "Chinstrap" = "#B251F1", "Gentoo" = "#4BA4A4")) +
      scale_shape_manual(values = c("Adelie" = 19, "Chinstrap" = 17, "Gentoo" = 15)) +
      labs(x = "Flipper length (mm)", y = "Bill length (mm)", 
           color = "Penguin species", shape = "Penguin species") +
      theme_minimal() +
      theme(legend.position = c(0.85, 0.2),
            legend.background = element_rect(color = "white"))
    
  }) # EO renderPlot
  
  # select year ----
  year_df <- reactive({
    
    penguins %>%  
      filter(year %in% input$year_input)
  }) #EO reactive year
  
  # make table ----
  
  output$year_table <- DT::renderDataTable({
    
    DT::datatable(year_df(),
                  options = list(pagelength = 10),
                  rownames = FALSE)
    
  }) # EO make interactive table
  
  
  
} # EO sever


#combine UI & server into an app ----
#don't need this is have two-file system
shinyApp(ui = ui, server = server)