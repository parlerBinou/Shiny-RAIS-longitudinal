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
source("R/translator.R")

language <- "en"
translator <- SimpleTranslator$new('dictionary/dict_main.csv', language)
tr <- translator$tr
# dictionary <- read.csv('dictionary/dict_main.csv',encoding = "utf-8")
# translation <- dlply(dictionary ,.(key), function(s) key = as.list(s))


# 
# tr <- function(text){ 
#   ls <-lapply(text,function(s) translation[[s]][[language]])
#   return(ls[[1]])
# }

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
      tr("title_pathway"),
      pathway_ui("pathway")
    ), 
    
    tabPanel(
      tr("title_mob_measures"),
      mob_measure_ui("mob_measure")
    ),
    tabPanel(
      tr("title_mob_matrix"),
      mob_matrix_ui("mob_matrix")
    )
    
  ) # navbar page
) # bootstrap page


server <- function(input, output, session) {
  
  pathway_server("pathway", language)
  
  mob_measure_server("mob_measure", language)
  
  mob_matrix_server("mob_matrix", language, reactive(input$innerSize))
  
}

shinyApp(ui, server)
