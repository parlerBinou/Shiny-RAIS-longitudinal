mob_measure_ui <- function(id) {
  
  sidebarLayout(
    sidebarPanel(
      uiOutput(NS(id, "comp_control")),
      
      uiOutput(NS(id, "year_control")),
      
      uiOutput(NS(id, "time_control")),
      
      uiOutput(NS(id, "region_selection")),
      
      uiOutput(NS(id, "trade_control")),
      
      uiOutput(NS(id, "mode_control")),
      
      uiOutput(NS(id, "type_control")),
      
      uiOutput(NS(id, "unit_control"))

    ), 
    
    mainPanel(
      fluidRow(
        valueBoxOutput(NS(id, "vbox_prov"), width = 8),
        valueBoxOutput(NS(id, "vbox_year"), width = 4)),
      fluidRow(
        valueBoxOutput(NS(id, "vbox_trade"), width = 12)),
      fluidRow(
        valueBoxOutput(NS(id, "vbox_cohort")),
        valueBoxOutput(NS(id, "vbox_medage")),
        valueBoxOutput(NS(id, "vbox_absence"))),
      fluidRow(
        valueBoxOutput(NS(id, "vbox_net_measure")),
        valueBoxOutput(NS(id, "vbox_in_measure")),
        valueBoxOutput(NS(id, "vbox_out_measure"))),

      
      #tableOutput(NS(id, "outtable")),
      fillRow(
        plotlyOutput(NS(id, "outPlot")),
        width = "100%"
      )
    )
  )
}

mob_measure_server <- function(id, language) {

  moduleServer(id, function(input, output, session) {
    source("R/format_number.R")
    source("R/valuebox.R")
    
    dictionary <- read.csv('dictionary/dict_mobility_measures.csv') %>%
      split(.$key)
    
    # uses a reactiveVal language.
    tr <- function(key) {
      dictionary[[key]][[language()]]
    }
    
    geo_names <- reactive(tr("mem_geo"))
    grp = reactive(setNames(c(1:3,19,20), tr("mem_trade_grp")))
    rs = reactive(setNames(c(4:18), tr("mem_trade_rs")))
    trade_names <- reactive({
      c(grp(), rs()) %>% sort() %>% names()
    })
    
    # load in the data file
    full <- download_data(
      "37100205", c("trade", "mode", "years", "type", "ind")) %>%
      # before release, use downloaded csv file 
      # read_csv("data/mobility_measures.csv",
      #          col_types = cols_only(
      #            REF_DATE = col_integer(),
      #            dim_geo = col_integer(),
      #            dim_trade = col_integer(),
      #            dim_mode = col_integer(),
      #            dim_years = col_integer(),
      #            dim_type = col_integer(),
      #            dim_ind = col_integer(),
      #            VALUE = col_double())) %>% 
      pivot_wider(id_cols=c(REF_DATE, dim_geo, dim_trade, dim_mode, dim_years, dim_type),
                  names_from=dim_ind, values_from=VALUE, names_prefix = "ind") %>%
      subset(!is.na(ind3)) %>% # remove if taxfilers is missing
      as.data.frame()
    
    # time (year after certification)
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
      
      last_year <- full %>%
        filter(dim_years == input$time) %>%
        pull(REF_DATE) %>%
        max()
    })
    
    # to make the cohort selection persistent even if input$time changed,
    # define it as a reactiveVal.
    # initialize it with the most recent available cohort.
    selected_cohort <- reactiveVal(max(full$REF_DATE))
    
    # get the selected cohort value and update selected_cohort.
    get_cohort <- function() {
      selected_cohort(max(input$year))
    }
    
    # reset the stored value when the selected_chort is invalid.
    reset_cohort <- function() {
      selected_cohort(last_yr())
    }
    
    # observe changes in input$year and update the stored value in selected_cohort.
    observeEvent(input$year, get_cohort())
    # observe changes in input$time
    # if the stored value in selected_cohort is invalid, reset it.
    observeEvent(input$time, {
      if (selected_cohort() > last_yr()) {reset_cohort()}
    })
    
    
    #  menu for year (cohort)
    # note last_yr() is used as it's reactive.
    # last_yr() appears first in the list, and it goes back to 2008.
    output$year_control <- renderUI({
      req(input$comp)
      if (input$comp == 3) {
        sliderTextInput(
          inputId = NS(id,"year"),
          label = tr("lab_cert_year"), 
          choices = c(2008:last_yr()),
          selected = c(2008, selected_cohort())
        )
      } else {
        pickerInput(
          inputId = NS(id, "year"),
          label = tr("lab_cert_year"),
          choices = c(last_yr():2008),
          selected = selected_cohort(),
          multiple = FALSE
        )
      }
    })
    
    # comparison dimension
    output$comp_control <- renderUI({
      radioButtons(
        inputId = NS(id, "comp"),
        label = tr("lab_comp"),
        choices = setNames(c(1:3), tr("mem_comp")),
        selected = 1
      )
    })
    
    # region
    output$region_selection <- renderUI({
      req(input$comp)
      if (input$comp == 1) {
        multi_selection <- TRUE
        default_selection <- c(1:11)
      } else {
        multi_selection <- FALSE
        default_selection <- 1
      }
      pickerInput(
        inputId = NS(id, "geo"),
        label = tr("lab_geo"),
        choices = setNames(c(1:12), tr("mem_geo")),
        multiple = multi_selection,
        selected = default_selection
      )  
    })
    
    # trade
    output$trade_control <- renderUI({
      req(input$comp)
      choice_set = list(
        grp = grp(),
        rs = rs()
      )
      names(choice_set) <- c(tr("lab_trade_grp"), tr("lab_rs"))
      
      if (input$comp == 2) {
        multi_selection <- TRUE
        default_selection <- c(1:3,19,20)
        options_set = pickerOptions(actionsBox = TRUE, selectAllText = tr("sAll_lbl"), deselectAllText = tr("dsAll_lbl"))
      } else {
        multi_selection <- FALSE
        default_selection <- 1
        options_set = NULL
      }
      
      pickerInput(
        inputId = NS(id, "trade"),
        label = tr("lab_trade"),
        choices = choice_set,
        multiple = multi_selection,
        selected = default_selection,
        options = options_set
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
    
    output$unit_control <- renderUI({
      req(input$comp)
      if (input$comp == 3) {
      radioButtons(
        inputId = NS(id, "unit"),
        label = NULL,
        choices = setNames(1:2, tr("mem_unit")), 
        selected = 2)
      }
    })
    
    # creating outputs
    
    df <- reactive({
      req(input$geo, input$year, input$trade)
      df <- full %>%
        subset(
          REF_DATE %in% c(min(input$year):max(input$year)) &
            dim_geo %in% input$geo &
            dim_trade %in% input$trade &
            dim_mode == input$mode &
            dim_years == input$time &
            dim_type == input$type) %>%
        arrange(dim_geo, dim_trade, desc(REF_DATE)) %>%
        mutate(
          supp = c(1:max(nrow(.), 1)),
          label1 = geo_names()[dim_geo],
          label2 = trade_names()[dim_trade],
          label3 = as.character(REF_DATE)
        )
    })
    
    
    # render the table for testing / debugging
    output$outtable <- renderTable(
      df()
    )

    output$outPlot <- renderPlotly({
      
      # check if there are data points left after the filtering.
      validate(need(nrow(df()) > 0, message = tr("text_no_data")))

      # if comparing across geography or trade, make horizontal bar charts
      if (input$comp != 3) {
        tick_label <- if (input$comp == 1) {df()$label1} else {df()$label2}
        
        net_text <- format_number(df()$ind11, locale = language())
        in_text <- format_number(df()$ind9, locale = language())
        out_text <- format_number(df()$ind10, locale = language())
        
        fig <- plot_ly(
          x = df()$ind11, y = df()$supp, name = tr("net"), type = "bar",
          text = net_text, orientation = "h", marker = list(color = '66c2a5'),
          hovertemplate = "%{y}: %{text}%",
          source = "mm",
          # when comparing across geography and Canada is selected, show
          # In and Out and hide Net - always zero.
          # otherwise, show Net only by default.
          visible = ifelse(
            (input$comp == 2 & input$geo == 1), "legendonly", TRUE)) %>%
          add_trace(x = df()$ind9, name = tr("in"), type = "bar",
                    text = in_text, marker = list(color = 'fc8d62'),
                    visible = ifelse(
                      (input$comp == 2 & input$geo == 1), TRUE, "legendonly")
          ) %>%
          add_trace(x = df()$ind10, name = tr("out"), type = "bar",
                    text = out_text, marker = list(color = '8da0cb'),
                    visible = ifelse(
                      (input$comp == 2 & input$geo == 1), TRUE, "legendonly")
          ) %>% 
          layout(
            yaxis = list(
              ticktext = tick_label,
              tickvals = df()$supp,
              autorange = "reversed"
            ),
            barmode = "group",
            legend=list(
              traceorder = "normal", orientation="h", yanchor="bottom",
              y=1, xanchor="left", x=0)
          ) 
      } else {
        # compare across cohorts (year of certification)
        # two possibilities - by number or by percent.
        req(input$unit)

        if (input$unit == 1) {
          net_measure <- df()$ind8
          in_measure <- df()$ind6
          out_measure <- df()$ind7
          
          hover_template <- "%{x}: %{text}"
        } else {
          net_measure <- df()$ind11
          in_measure <- df()$ind9
          out_measure <- df()$ind10
          
          hover_template <- "%{x}: %{text} %"
        }
        net_text <- format_number(net_measure, locale=language())
        in_text <- format_number(in_measure, locale=language())
        out_text <- format_number(out_measure, locale=language())

        fig <- plot_ly(
          x = df()$supp, y = net_measure, name = tr("net"), type = "bar",
          text = net_text, hovertemplate = hover_template, source = "mm",
          marker = list(color = '66c2a5'),
          visible = ifelse(
            (input$geo == 1), "legendonly", TRUE)
          ) %>%
          add_trace(y = in_measure, name = tr("in"), type = "bar",
                    text = in_text, marker = list(color = 'fc8d62'),
                    visible = ifelse(
                      (input$geo == 1), TRUE, "legendonly")) %>%
          add_trace(y = out_measure, name = tr("out"), type = "bar",
                    text = out_text, marker = list(color = '8da0cb'),
                    visible = ifelse(
                      (input$geo == 1), TRUE, "legendonly")) %>%
          layout(
            barmode = "group",
            legend=list(
              traceorder = "normal", orientation="h", yanchor="bottom",
              y=1, xanchor="left", x=0),
            xaxis = list(
              ticktext = df()$label3,
              tickvals = df()$supp,
              autorange = "reversed"
            ))

      }
    })
    
    
    # define the index of values to be shown in text boxes
    selected_supp <- reactiveVal(1)

    
    get_clicked <- function() {
      clk <- event_data("plotly_click", source = "mm")
      if (is.null(clk)) {selected_supp(1)} else {
        if (input$comp == 3) { selected_supp(clk$x) } else { selected_supp(clk$y) }
      }
    }
    
    
    reset_selection <- function() {
      selected_supp(1)
    }
    
    # if click on plotly, read the selected index
    observeEvent(event_data("plotly_click", source = "mm"), get_clicked())
    
    # if anything changes in df(), reset the selection
    observeEvent(df(), reset_selection())
    
    # render value boxes
    output$vbox_year <- renderValueBox({
      my_valueBox(
        df()$REF_DATE[df()$supp == selected_supp()], tr("lab_cert_year"),
        icon = "calendar")
    })
    
    output$vbox_prov <- renderValueBox({
      my_valueBox(
        df()$label1[df()$supp == selected_supp()], tr("lab_geo"),
        icon = "map-marker")
    })
    
    output$vbox_trade <- renderValueBox({
      my_valueBox(
        df()$label2[df()$supp == selected_supp()], tr("lab_trade"),
        icon = "toolbox")
    })
    
    output$vbox_cohort <- renderValueBox({
      my_valueBox(
        format_number(
          df()$ind1[df()$supp == selected_supp()],
          in_bracket = df()$ind3[df()$supp == selected_supp()],
          in_bracket_percent = FALSE,
          locale = language()),
        paste0(tr("cohort"), " (", tr("taxfilers"), ")"),
        icon = "users", size = "small")
    })
    
    
    output$vbox_medage <- renderValueBox({
      my_valueBox(
        format_number(
          df()$ind2[df()$supp == selected_supp()],
          locale = language()), tr("medage"),
          icon = "award", size = "small")
    })
    
    output$vbox_absence <- renderValueBox({
      my_valueBox(
        format_number(
          df()$ind4[df()$supp == selected_supp()],
          in_bracket = df()$ind5[df()$supp == selected_supp()],
          locale = language()), tr("absence"),
        icon = "house-user", size = "small")
    })
    
    output$vbox_net_measure <- renderValueBox({
      my_valueBox(
        format_number(
          df()$ind8[df()$supp == selected_supp()],
          in_bracket = df()$ind11[df()$supp == selected_supp()],
          locale = language()), tr("net"),
          icon = "exchange-alt", size = "small")
    })
    
    output$vbox_in_measure <- renderValueBox({
      my_valueBox(
        format_number(
          df()$ind6[df()$supp == selected_supp()],
          in_bracket = df()$ind9[df()$supp == selected_supp()],
          locale = language()), tr("in"),
          icon = "sign-in-alt", size = "small")
    })
    
    output$vbox_out_measure <- renderValueBox({
      my_valueBox(
        format_number(
          df()$ind7[df()$supp == selected_supp()],
          in_bracket = df()$ind10[df()$supp == selected_supp()],
          locale = language()), tr("out"),
          icon = "sign-out-alt", size = "small")
    })
    
  })

}

# for testing

measure_demo <- function() {
  ui <- fluidPage(
    mob_measure_ui("x"))
  server <- function(input, output, session) {
    mob_measure_server("x", "en")
  }
  shinyApp(ui, server)
}


# measure_demo()
