mob_matrix_ui <- function(id) {
  sidebarLayout(
    sidebarPanel(
      uiOutput(NS(id, "year_control")),
      
      uiOutput(NS(id, "time_control")),
      
      uiOutput(NS(id, "trade_control")),
      
      uiOutput(NS(id, "mode_control")),
      
      uiOutput(NS(id, "type_control")),
      
      uiOutput(NS(id, "all_regions")),
      
      uiOutput(NS(id, "region_selection"))
    ), # sidebarPanel
    
    mainPanel(
      fillRow(
        plotOutput(NS(id, "outPlot"), width = "100%", height = "100%"),
        width = "100%"
      )
    )
  )
}

mob_matrix_server <- function(id, language, innerSize) {

  moduleServer(id, function(input, output, session) {
    # load in the dictionary.
    source("R/download_data.R")
    source("R/translator.R")
    translator <- SimpleTranslator$new('dictionary/dict_mobility_matrix.csv', language)
    tr <- translator$tr
    
    # load in the data file
    full <- reactive(
      # make it reactive, so it only downloads the data when the tab is selected
      # download_data("37100204", c("trad", "mode", "years", "type", "to")) %>%
      # before release, use downloaded csv file 
      read_csv("data/mig_mat.csv", 
        col_types = cols_only(
          REF_DATE = col_integer(),
          dim_geo = col_integer(),
          dim_trad = col_integer(),
          dim_mode = col_integer(),
          dim_years = col_integer(),
          dim_type = col_integer(),
          dim_to = col_integer(),
          VALUE = col_number())) %>% 
        rename(from = dim_geo, to = dim_to) %>%
        as.data.frame() %>%
        filter(!is.na(VALUE) & VALUE > 0 & to > 4) %>%
        mutate(to = to - 4) %>%
        filter(from != to)
    )
    # load in the meta data
    meta <- read_csv("data/mobility_matrix_metadata.csv")
    
    # define region and colour of each region as named lists.
    region = setNames(meta$region, meta$code)
    colour = setNames(meta$colour, meta$code)
    #print(head(full))
    
    # time (year after certification)
    # translation is done manually using the selected_dict().
    output$time_control <- renderUI({
      radioButtons(
        inputId = NS(id, "time"),
        label = NULL,
        choices = setNames(1:2, tr("mem_year")),
        selected = 1
      )
    })
    
    
    # the most recent year (cohort) is used to define the range of
    # cohorts in the drop down menu.
    # but it depends on the time (year after certification)
    # so define "last_yr" as reactive
    last_yr <- reactive({
      req(input$time)
      
      last_year <- full() %>%
        filter(dim_years == input$time) %>%
        pull(REF_DATE) %>%
        max()
    })
    
    #  menu for year (cohort)
    # note last_yr() is used as it's reactive.
    # last_yr() appears first in the list, and it goes back to 2008.
    output$year_control <- renderUI({
      selectizeInput(
        inputId = NS(id, "year"),
        label = tr("lab_cert_year"),
        choices = c(last_yr():2008),
        selected = last_yr()
      )
    })
    
    
    # trade
    output$trade_control <- renderUI({
      selectInput(
        inputId = NS(id, "trade"),
        label = tr("lab_trade"),
        choices = setNames(1:5, tr("mem_trade")),
        selected = 1
      )
    })
    
    # mode of certification
    output$mode_control <- renderUI({
      selectInput(
        inputId = NS(id, "mode"),
        label = tr("lab_mode"),
        choices = setNames(1:3, tr("mem_mode")),
        selected = 1
      )
    })
    
    # type of mobility
    output$type_control <- renderUI({
      selectInput(
        inputId = NS(id, "type"),
        label = tr("lab_type"),
        choices = setNames(1:3, tr("mem_type")),
        selected = 1
      )
    })
    
    output$all_regions <- renderUI({
      checkboxInput(
        inputId = NS(id, "all_regions"),
        label = "All regions",
        value = TRUE)
    })

    # region, only appears on the screen if 'all region' is ticked off
    # no default value is provided
    output$region_selection <- renderUI({
      req(input$all_regions)
      if (!input$all_regions) {
        selectizeInput(
          inputId = NS(id, "sel_region"),
          label = NULL,
          choices = setNames(c("", 1:11), tr("mem_geo")),
          selected = NULL
        )
      }
    })

    output$outPlot <- renderPlot({
      df <- full() %>%
        subset(
          REF_DATE == input$year &
            dim_trad == input$trade &
            dim_mode == input$mode &
            dim_years == input$time &
            dim_type == input$type,
          select = c(from, to, VALUE)
        )
      
      # the province names also need to change depending on the selected language.
      pr_abbr <- if (language == "en") {
        abbr <- meta$pr_en
      } else {
        abbr <- meta$pr_fr
      }

      req(input$all_regions) 
      if (input$all_regions == FALSE) {
        req(input$sel_region, cancelOutput = TRUE)
        selected_region <- as.numeric(input$sel_region)
        df <- df %>%
          subset(from == selected_region | to == selected_region)
      }
      
      # check if there are data points left after the filtering.
      validate(need(nrow(df) > 0, message = tr("text_no_data")))
      
      # main code for the chord diagram.
      # largely based on https://github.com/guyabel/migest/blob/master/demo/cfplot_reg2.
      circos.clear()
      circos.par(
        cell.padding = c(0.01, 0.01, 0.01, 0.01),
        track.margin = c(0.01,-0.01),
        start.degree = 90,
        gap.degree = 2,
        canvas.xlim = c(-0.95, 0.95),
        canvas.ylim = c(-0.95, 0.95)
      )
      
      # region, code, and colour are defined in metadata.
      chordDiagram(
        x = df,
        group = region,
        order = meta$code,
        grid.col = colour,
        transparency = 0.25,
        directional = 1,
        direction.type = "diffHeight+arrows",
        link.arr.type = "big.arrow",
        diffHeight = -0.03,
        link.sort = TRUE,
        link.largest.ontop = TRUE,
        annotationTrack = c("grid"),
        preAllocateTracks = list(track.height = 0.25)
      )
      
      circos.track(
        track.index = 1,
        bg.border = NA,
        panel.fun = function(x, y) {
          #s = get.cell.meta.data("sector.index")
          l = as.numeric(get.cell.meta.data("sector.index"))
          # replace the label by abbreviations in the selected language
          s = pr_abbr[l]
          xx = get.cell.meta.data("xlim")
          circos.text(
            x = mean(xx),
            y = 0.3,
            labels = s,
            cex = 1.2,
            adj = c(0, 0.5),
            facing = "clockwise",
            niceFacing = TRUE
          )
          circos.axis(
            h = "bottom",
            labels.cex = 0.6,
            labels.pos.adjust = TRUE,
            labels.facing = "outside",
            labels.niceFacing = TRUE
          )
        }
      )
    },
    # the plot height is adjusted by the browser window size.
    height = innerSize
      
    )
    
  })

}


matrix_demo <- function() {
  ui <- fluidPage(
    tags$head(
      tags$script(
        '$(document).on("shiny:connected", function(e) {
              Shiny.onInputChange("innerSize", Math.min(window.innerWidth * 0.65, window.innerHeight * 0.85));
            });
           $(window).resize(function(e) {
              Shiny.onInputChange("innerSize", Math.min(window.innerWidth * 0.65, window.innerHeight * 0.85));
            });
        ')),
    
    mob_matrix_ui("x"))
  server <- function(input, output, session) {
    mob_matrix_server("x", "en", reactive(input$innerSize))
  }
  shinyApp(ui, server)
  
}

#matrix_demo()