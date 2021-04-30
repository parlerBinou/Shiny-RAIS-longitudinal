pathway_ui <- function(id) {
  
  sidebarLayout(
    sidebarPanel(
      uiOutput(NS(id, 'year_control')),
      uiOutput(NS(id, 'gender_control')),
      uiOutput(NS(id, 'times_control')),
      br(),
      uiOutput(NS(id, 'direc_control')),
      uiOutput(NS(id, "trade_control")),
      uiOutput(NS(id, "geo_control")),
      width = 4
    ), 
    
    mainPanel(
      fluidRow(
        valueBoxOutput(NS(id, "vbox_prov"), width = 8),
        valueBoxOutput(NS(id, "vbox_year"), width = 4)),
      fluidRow(
        valueBoxOutput(NS(id, "vbox_trade"), width = 12)),
      fluidRow(
        valueBoxOutput(NS(id, "vbox_cohort"), width = 4),
        valueBoxOutput(NS(id, "vbox_durpgm"), width = 4),
        valueBoxOutput(NS(id, "vbox_age_reg"), width = 4)),
      fluidRow(
        valueBoxOutput(NS(id, "vbox_time_cert"), width = 4),
        valueBoxOutput(NS(id, "vbox_age_cert"), width = 4),
        valueBoxOutput(NS(id, "vbox_time_disc"), width = 4)),
      
      
      # tableOutput(NS(id, "outtable")),
      fillRow(
        plotlyOutput(NS(id, "outBarChart")),
        width = "100%"
      )
    )
  )
}

pathway_server <- function(id, language) {
  
  moduleServer(id, function(input, output, session) {
    
    # Preparation --------------------------------------------------------------
    source("R/format_number.R")
    source("R/translator.R")
    translator <- SimpleTranslator$new('dictionary/dict_pathway.csv', language)
    tr <- translator$tr
    # dictionary <- read.csv('dictionary/dict_pathway.csv',encoding = "latin1")
    # translation <- dlply(dictionary ,.(key), function(s) key = as.list(s))
     
    # define not in function
    #'%!in%' <- function(x,y)!('%in%'(x,y))
    
    # define trasnlation  function
    # tr <- function(text){ 
    #   ls <-lapply(text,function(s) translation[[s]][[language]])
    #   return(ls[[1]])
    # }
    # 
    # extract list of trades and geos
    refGeo <- tr("mem_geo")
    
    grp <- setNames(c(1:3,29,30,36,37), tr("mem_trade_grp"))
    rs <- setNames(c(4:28), tr("mem_trade_rs"))
    nrs <- setNames(c(31:35), tr("mem_trade_nrs"))
    refTrade <- c(grp, rs, nrs) %>% sort() %>% names()
    
    #  Data processing----------------------------------------------------------
    dims <- c("sex", "trade", "ind")
    # full <- download_data("37100205", dims) %>%
    # before release, use downloaded csv file 
    full <- read_csv("data/pathway.csv",
             col_types = cols_only(
               REF_DATE = col_integer(),
               dim_geo = col_integer(),
               dim_sex = col_integer(),
               dim_trade = col_integer(),
               dim_ind = col_integer(),
               VALUE = col_double(),
               STATUS = col_character(),
               SYMBOL = col_character())) %>% 
      mutate(flag = paste(
        ifelse(is.na(STATUS) | STATUS == "..", " ", STATUS),
        ifelse(is.na(SYMBOL), " ", SYMBOL))) %>%
      pivot_wider(id_cols=c(REF_DATE, dim_geo, dim_sex, dim_trade),
                  names_from=dim_ind, values_from=c(VALUE, flag)) %>%
      subset(REF_DATE <= (max(.$REF_DATE) - 4)) %>% # keep only if they are at least 4 years old
      as.data.frame() %>%
      mutate(
        cohort = as.integer(VALUE_1),
        durpgm = as.integer(VALUE_2),
        age_reg = as.integer(VALUE_3),
        time_disc10 = as.integer(VALUE_8), # Median time to discontinuation (within program duration)
        age_cert10 = as.integer(VALUE_9), # Median age at certification (within program duration)
        time_disc15 = as.integer(VALUE_14), # Median time to discontinuation (within 1.5 times program duration)
        age_cert15 = as.integer(VALUE_15), # Median age at certification (within 1.5 times program duration)
        time_disc20 = as.integer(VALUE_20), # Median time to discontinuation (within 2 times program duration)
        age_cert20 = as.integer(VALUE_21) # Median age at certification (within 2 times program duration)
        )
    
    #  sidebar widgets----------------------------------------------------------
    # the most recent year (cohort) is used to define the range of
    # cohorts in the dropdown menu.
    last_year <- reactive({
      req(input$times)
      
      # the data is already restricted to at least 4 years old cohorts
      # if 1.5 dur, subtract 2 more: at least 6 years old cohorts
      # if 2 dur, subtract 4 more: at least 8 years old cohorts
      max(full$REF_DATE) - 2 * (as.integer(input$times) - 1)
       
    })
    
    # slider for "reference period"
    output$year_control <- renderUI({
      req(input$direc)
      if (input$direc == 3){
        # slide time range
        sliderTextInput(
          inputId = NS(id,"year"),
          label = tr("lab_year"), 
          choices = c(2008:last_year()),
          selected = c(2008, last_year())
        )
      } else {
        # slide time point
        selectizeInput(
          inputId = NS(id,"year"),
          label = tr("lab_year"),
          choices = c(last_year():2008),
          selected = last_year()
        )
      }
    })
    
    # dropdown menu for "Sex"
    output$gender_control <- renderUI({
      selectInput(
        inputId = NS(id,"sex"),
        label = tr("lab_sex"),
        choices = setNames(1:3, tr("sex_mem")),
        selected = 1
      )
    })
    
    # dropdown to select time point
    output$times_control <- renderUI({
      selectInput(
        inputId = NS(id,"times"),
        label = tr("lab_time"),
        choices = setNames(1:3, tr("mem_time")),
        selected = 2
      )
    })
    
    # dropdown to select trade/trade list
    output$trade_control <- renderUI({
      req(input$direc)
      
      choice_set = list(
        grp = grp, # these are defined at the beginning of the server logic.
        rs = rs,
        nrs = nrs 
      )
      names(choice_set) <- c(tr("lab_trade_grp"), tr("lab_rs"), tr("lab_nrs"))
      
      if (input$direc == 1) {
        multi_selection <- TRUE
        default_selection <- c(1:3,29,30,36,37)
      } else {
        multi_selection <- FALSE
        default_selection <- 1
      }
      
      pickerInput(
        inputId = NS(id, "trade"),
        label = tr("lab_trade"),
        choices = choice_set,
        multiple = multi_selection,
        selected = default_selection
      ) 
    })
    
    # dropdown menu for "Geography"
    output$geo_control <- renderUI({
      req(input$direc)
    
      if (input$direc == 2) {
        multi_selection <- TRUE
        default_selection <- c(1:2,7:12)
      } else {
        multi_selection <- FALSE
        default_selection <- 1
      }
      pickerInput(
        inputId = NS(id, "geo"),
        label = tr("lab_geo"),
        choices = setNames(c(1:13), tr("mem_geo")),
        multiple = multi_selection,
        selected = default_selection
      )  
    
    })
    
    # radio Buttons for "Compare by"
    output$direc_control <- renderUI({
      radioButtons(
        inputId = NS(id,"direc"),
        label = tr("lab_comp"),
        choices = setNames(1:3, tr("mem_comp") ),
        selected = 1
      )
    })
    
    #  create plotly chat-------------------------------------------------------
    
    df <- reactive({
      req(input$geo, input$year, input$trade, input$times, input$direc)
      
      selected_year <- if (input$direc == 3) {
        c(min(input$year):max(input$year))
      } else {
        input$year
      }
      df <- full %>%
        subset(
          REF_DATE %in% selected_year &
            dim_geo %in% input$geo &
            dim_sex == input$sex &
            dim_trade %in% input$trade) %>%
        arrange(dim_geo, dim_trade, desc(REF_DATE)) %>%
        mutate(
          supp = c(1:max(nrow(.), 1)),
          label1 = refTrade[dim_trade],
          label2 = refGeo[dim_geo],
          label3 = as.character(REF_DATE))
      
      if (input$times == 1) {
        df <- df %>%
          rename(
            cert = VALUE_4,
            cert_flag = flag_4,
            cont = VALUE_5,
            cont_flag = flag_5,
            disc = VALUE_6,
            disc_flag = flag_6,
            time_cert = VALUE_7,
            time_cert_flag = flag_7,
            time_disc = time_disc10,
            time_disc_flag = flag_8,
            age_cert = age_cert10,
            age_cert_flag = flag_9
          )
      } else if (input$times == 2) {
        df <- df %>%
          rename(
            cert = VALUE_10,
            cert_flag = flag_10,
            cont = VALUE_11,
            cont_flag = flag_11,
            disc = VALUE_12,
            disc_flag = flag_12,
            time_cert = VALUE_13,
            time_cert_flag = flag_13,
            time_disc = time_disc15,
            time_disc_flag = flag_14,
            age_cert = age_cert15,
            age_cert_flag = flag_15
          )
      } else {
        df <- df %>%
          rename(
            cert = VALUE_16,
            cert_flag = flag_16,
            cont = VALUE_17,
            cont_flag = flag_17,
            disc = VALUE_18,
            disc_flag = flag_18,
            time_cert = VALUE_19,
            time_cert_flag = flag_19,
            time_disc = time_disc20,
            time_disc_flag = flag_20,
            age_cert = age_cert20,
            age_cert_flag = flag_21
          )
      }
    })
    
    output$outtable <- renderTable(
      df()
    )
    
    output$outBarChart <- renderPlotly({
      
      # check if there are data points left after the filtering.
      validate(need(nrow(df()) > 0, message = tr("text_no_data")))
      
      if (input$direc != 3) {
        tick_label <- if (input$direc == 1) {df()$label1} else {df()$label2}
        
        fig <- plot_ly(
          x = df()$cert, y = df()$supp, name = tr("rate_cert"), type = "bar",
          orientation = "h", marker = list(color = '66c2a5'),
          text = df()$cert_flag, source = "p",
          hovertemplate = "%{y}: %{x}%<sup>%{text}</sup>") %>%
        add_trace(
          x = df()$cont, name = tr("rate_cont"), marker = list(color = 'fc8d62'),
          text = df()$cont_flag) %>%
        add_trace(
          x = df()$disc, name = tr("rate_disc"), marker = list(color = '8da0cb'),
          text = df()$disc_flag) %>%
        layout(
          barmode = 'stack',
          yaxis = list(
                    ticktext = tick_label,
                    tickvals = df()$supp,
                    autorange = "reversed"
                  ),
          xaxis = list(range = c(-2, 100)), # a way to add gap between axis label and the axis
          legend=list(
                    traceorder = "normal", orientation="h", yanchor="bottom",
                    y=1.05, xanchor="left", x=0))
      } else { #comparing over time
        fig <- plot_ly(
          x = df()$supp, y = df()$cert, name = tr("rate_cert"), type = "bar",
          marker = list(color = '66c2a5'),
          text = df()$cert_flag, source = "p",
          hovertemplate = "%{x}: %{y}%<sup>%{text}</sup>") %>%
          add_trace(
            y = df()$cont, name = tr("rate_cont"), marker = list(color = 'fc8d62'),
            text = df()$cont_flag) %>%
          add_trace(
            y = df()$disc, name = tr("rate_disc"), marker = list(color = '8da0cb'),
            text = df()$disc_flag) %>%
          layout(
            barmode = 'stack',
            xaxis = list(
              ticktext = df()$label3,
              tickvals = df()$supp,
              autorange = "reversed"
            ),
            legend=list(
              traceorder = "normal", orientation="h", yanchor="bottom",
              y=1.05, xanchor="left", x=0))
      }
    })

    # # creating value boxes to all indicators  -------------------------------
    selected_supp <- reactiveVal(1)
    
    get_clicked <- function() {
      clk <- event_data("plotly_click", source = "p")
      if (is.null(clk)) {selected_supp(1)} else {
        if (input$direc == 3) { selected_supp(clk$x) } else { selected_supp(clk$y) }
      }
    }
    
    reset_selection <- function() {
      selected_supp(1)
    }
    
    # if click on plotly, read the selected index
    observeEvent(event_data("plotly_click", source = "p"), get_clicked())
    
    # if anything changes in df(), reset the selection
    observeEvent(df(), reset_selection())
    
    # render value boxes
    output$vbox_year <- renderValueBox({
      my_valueBox(
        df()$REF_DATE[df()$supp == selected_supp()], tr("lab_year"),
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
          df()$cohort[df()$supp == selected_supp()], locale = language),
          tr("cohort"), icon = "users", size = "small")
    })
    
    output$vbox_age_reg <- renderValueBox({
      my_valueBox(
        df()$age_reg[df()$supp == selected_supp()],
        tr("age_reg"), size = "small")
    })
    
    output$vbox_time_cert <- renderValueBox({
      my_valueBox(
        HTML(
          paste0(
            format_number(df()$time_cert[df()$supp == selected_supp()],
                          locale = language),
            "<sup>", df()$time_cert_flag[df()$supp == selected_supp()],
            "</sup>", collapse = NULL)),
        tr("time_cert"), size = "small")
    })
    
    output$vbox_durpgm <- renderValueBox({
      my_valueBox(
        df()$durpgm[df()$supp == selected_supp()],
        tr("dur_pgm"), size = "small")
    })
    
    output$vbox_age_cert <- renderValueBox({
      my_valueBox(
        HTML(
          paste0(
            df()$age_cert[df()$supp == selected_supp()],
            "<sup>", df()$age_cert_flag[df()$supp == selected_supp()],
            "</sup>", collapse = NULL)),
        tr("age_cert"), size = "small")
    })
    
    output$vbox_time_disc <- renderValueBox({
      my_valueBox(
        HTML(
          paste0(
            df()$time_disc[df()$supp == selected_supp()],
            "<sup>", df()$time_disc_flag[df()$supp == selected_supp()],
            "</sup>", collapse = NULL)),
        tr("time_disc"), size = "small")
    })
    
  }) # module func
}

# for testing

pathway_demo <- function() {
  ui <- fluidPage(
    pathway_ui("pathway"))
  server <- function(input, output, session) {
    pathway_server("pathway", "en")
  }
  shinyApp(ui, server)
}


# pathway_demo()
