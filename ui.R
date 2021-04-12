library(shiny)
library(plotly)
library(tidyverse)
library(reshape2)
library(shinyjs)
library(shinythemes)
library(shinydashboard)

navbarPageWithButton <- function(..., button) {
  navbar <- navbarPage(...)
  div <- tags$div(class = "navbar-form", style = 'float: right', button)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], div)
  navbar
}

ui <- bootstrapPage(
  # To set the main plot size in response to the browser windows size,
  # read in the current windows size.
  tags$head(
    tags$script(
      '$(document).on("shiny:connected", function(e) {
              Shiny.onInputChange("innerSize", Math.min(window.innerWidth * 0.65, window.innerHeight * 0.85));
            });
           $(window).resize(function(e) {
              Shiny.onInputChange("innerSize", Math.min(window.innerWidth * 0.65, window.innerHeight * 0.85));
            });
        ')),
  
  shinyjs::useShinyjs(),
  
  navbarPageWithButton(
    
    theme = shinytheme('flatly'), title=NULL, id='nav',
    
    tabPanel(textOutput('pathwayTab'),
             sidebarLayout(
               
               sidebarPanel(
                 uiOutput('year_control'),
                 uiOutput('gender_control'),
                 uiOutput('times_control'),
                 br(),
                 uiOutput('direc_control'),
                 uiOutput("trade_control"),
                 uiOutput("geo_control"),
                 width = 2
               ), # sidebarPanel
               
               mainPanel(
                 
                 fluidRow(
                   column(offset = 1, width = 11,valueBoxOutput("ibox_regTrade",width = 8))
                  ),
                 
                 fluidRow(
                   column(offset = 1, width = 5, valueBoxOutput("ibox_Size",width = 8)),
                   column(width  = 5,valueBoxOutput("ibox_durpgm",width = 8))
                 ),
                 
                 fluidRow(
                   column(offset = 1, width = 5,valueBoxOutput("ibox_ageReg",width = 8)),
                   column(width  = 5,valueBoxOutput("ibox_ageCert",width = 8))
                 ),
                 
                 fluidRow(
                   column(offset = 1, width = 5,valueBoxOutput("ibox_timeCert",width = 8)),
                   column(width  = 5,valueBoxOutput("ibox_timeDisc",width = 8))
                 ),
                 
                 br(),
                 br(),
                 br(),
                 
                 fluidRow(
                   column(width = 12, plotlyOutput("outBarChart"))
                 )
               ) # mainPanel
             ) # sidebarLayout
            ), # tabPanel
    
    button = actionButton('btn_language', label = textOutput("label_language"))
    
    ) # navbar page
) # bootstrap page

























