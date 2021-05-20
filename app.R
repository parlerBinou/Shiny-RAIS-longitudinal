library(shiny)
library(plotly)
library(tidyverse)
library(shinydashboard)
library(circlize)
#library(Cairo)
library(shinyWidgets)
options(shiny.usecairo=T)

source("R/module_pathway.R")
source("R/module_mobility_matrix.R")
source("R/module_mobility_measure.R")

navbarPageWithButton <- function(..., button) {
  navbar <- navbarPage(...)
  div <- tags$div(class = "navbar-form", style = 'float: right; margin-top: 15px;', button)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], div)
  navbar
}

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
  
  navbarPageWithButton(
    
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
    ),
    button = actionLink("change_lang", textOutput("other_lang"))
    
  ) # navbar page
) # bootstrap page


server <- function(input, output, session) {
  
  language <- reactiveVal("fr")
  
  output$other_lang <- renderText({
    if (language() == "en") {"Français"} else {"English"}
  })
  
  observeEvent(input$change_lang, {
    if (language() == "en") {language("fr")} else {language("en")}
  })
  
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
  
  # because of downloading and processing the data,
  # initial loading takes quite some time.
  pathway_server("pathway", language)
  mob_measure_server("mob_measure", language)    
  mob_matrix_server("mob_matrix", language, reactive(input$innerSize))

}

shinyApp(ui, server)
