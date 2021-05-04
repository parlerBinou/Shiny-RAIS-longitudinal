library(shiny)
library(plotly)
library(tidyverse)
library(shinydashboard)
library(circlize)
library(Cairo)
library(shinyWidgets)
options(shiny.usecairo=T)

source("R/module_pathway.R")
source("R/module_mobility_matrix.R")
source("R/module_mobility_measure.R")

ui <- bootstrapPage(
  
  tags$head(
    tags$script(
      '$(document).on("shiny:connected", function(e) {
              Shiny.onInputChange("innerSize", Math.min(window.innerWidth * 0.65, window.innerHeight * 0.85));
            });
           $(window).resize(function(e) {
              Shiny.onInputChange("innerSize", Math.min(window.innerWidth * 0.65, window.innerHeight * 0.85));
            });
        ')),
  
  navbarPage(
    
    title=NULL,
    
    tabPanel(
      textOutput("title_pathway"),
      pathway_ui("pathway")
    ), 
    
    tabPanel(
      textOutput("title_mob_measures"),
      mob_measure_ui("mob_measure")
    ),
    tabPanel(
      textOutput("title_mob_matrix"),
      mob_matrix_ui("mob_matrix")
    )
    
  ) # navbar page
) # bootstrap page


server <- function(input, output, session) {
  
  language <- reactiveVal("fr")
  # translator <- SimpleTranslator$new('dictionary/dict_main.csv', language)
  # tr <- translator$tr
  dictionary <- read.csv('dictionary/dict_main.csv') %>%
    split(.$key)
  
  # uses a reactiveVal language.
  tr <- function(key) {
    dictionary[[key]][[language()]]
  }
  
  output$title_pathway <- renderText(tr("title_pathway"))
  output$title_mob_measures <- renderText(tr("title_mob_measures"))
  output$title_mob_matrix <- renderText(tr("title_mob_matrix"))
  
  pathway_server("pathway", language)
  
  mob_measure_server("mob_measure", language)
  
  mob_matrix_server("mob_matrix", language, reactive(input$innerSize))
  
}

shinyApp(ui, server)
