library(shiny)
library(plotly)
library(tidyverse)
library(reshape2)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
library(plyr)

navbarPageWithButton <- function(..., button) {
  navbar <- navbarPage(...)
  div <- tags$div(class = "navbar-form", style = 'float: right', button)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], div)
  navbar
}

ui <- bootstrapPage(
  
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
                   column(offset = 1, width = 4, valueBoxOutput("ibox_Size",width = 12)),
                   column(width  = 4,valueBoxOutput("ibox_ageReg",width = 12)),
                   column( width = 3,valueBoxOutput("ibox_timeCert",width = 12))
                   
                 ),
                 
                 
                 fluidRow(
                   column(offset = 1,width  = 4,valueBoxOutput("ibox_durpgm",width = 12)),
                   column(width = 4,valueBoxOutput("ibox_ageCert",width = 12)),
                   column(width  = 3,valueBoxOutput("ibox_timeDisc",width = 12))
                 ),
                 
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


