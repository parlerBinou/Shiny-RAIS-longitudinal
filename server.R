dictionary_content <- read.csv('translation_fr.csv',encoding = "latin1")
translation <- dlply(dictionary_content ,.(key), function(s) key = as.list(s))

server <- function(input, output, session) {
  
# Translation -------------------------------------------------------------
#  dictionary <- read.csv("translation_fr.csv",encoding = "latin1")
  
  reactive_vars <- reactiveValues()
  reactive_vars$language <- "en"
  
  # Translate text given current language
  tr <- function(text){ 
    ls <-lapply(text,function(s) translation[[s]][[reactive_vars$language]])
    return(ls[[1]])
  }
  
  # Change button label when click
  output$label_language <- renderText({
    reactive_vars$language
  })
  
  observeEvent(input$btn_language, {
    if (reactive_vars$language == "en") {
      reactive_vars$language <- "fr"
    } else {
      reactive_vars$language <- "en"
    }
  })
  

# Pathway tab -------------------------------------------------------------
  #  Data processing-------------------------------------------------------
  url = "3710019301_databaseLoadingData.csv"
  
  dims <- c("dim_geo", "dim_sex", "dim_trad", "dim_inds")
  full <- readr::read_csv(url) %>%
    separate(col=COORDINATE, into=dims) %>%
    as.data.frame() %>%
    rename_at(c("Pathway indicators","Selected trades"),
              ~ c("Pathway_inds","trades"))
  
  full[, dims] <- sapply(full[, dims], as.numeric)
  
  #  Update language of variables in dataset
  
  reactive_full_data <- reactive({
    req(full, dictionary_content, reactive_vars$language)
    
    geo_tr <- dictionary_content %>% filter(key == 'geo_mem')
    trade_tr <- dictionary_content %>% filter(key == 'trade_mem')
    
    if (reactive_vars$language == 'en') {
      full <- full %>%
         mutate(
           GEO = mapvalues(GEO, geo_tr$fr, geo_tr$en, warn_missing = FALSE),
           trades = mapvalues(trades, trade_tr$fr, trade_tr$en, warn_missing = FALSE)
         )
     }
     else if (reactive_vars$language == 'fr') {
       full <- full %>%
         mutate(
           GEO = mapvalues(GEO, geo_tr$en, geo_tr$fr, warn_missing = FALSE),
           trades = mapvalues(trades, trade_tr$en, trade_tr$fr, warn_missing = FALSE)
         )
     }
     return (full)
   })
  
 
  # extract geo and trade from dict to maintain order
  referenceGeo <- reactive({ 
    req(reactive_vars$language)
    referenceGeo <- dictionary_content %>%
      slice(6:18) %>% # Geo_mem rows
      pull(reactive_vars$language) %>%
      rev.default() 
  })
  
  referenceTrade <-reactive({ 
    req(reactive_vars$language)
    referenceTrade <-dictionary_content %>%
      slice(22:58) %>% # trades_mem rows
      pull(reactive_vars$language) %>%
      rev.default()
  })  
  
  # bulding sider bar widgets --------------------------------------------- 
  
  # pathway tab title
  output$pathwayTab <- renderText({
    paste(tr("pathway_tab"))
  })
  
  # the most recent year (cohort) is used to define the range of
  # cohorts in the dropdown menu.
  last_year <- full %>%
    pull(REF_DATE) %>%
    max()

  
  # dropdown menu for "reference period"
  output$year_control <- renderUI({
    selectizeInput(
      inputId = "year",
      label = tr("year"),
      choices = c(last_year:2008),
      selected = last_year-6
    )
  })
  
  # dropdown menu for "Sex"
  output$gender_control <- renderUI({
    selectInput(
      inputId = "Sex",
      label = tr("sex_dim"),
      choices = setNames(1:3, tr("sex_mem")),
      selected = 1
    )
  })
  
  # dropdown to select time range
  output$times_control <- renderUI({
    selectInput(
      inputId = "times",
      label = tr("times_dim"),
      choices = setNames(c(4,10,16), tr("times_mem")),
      selected = 4
    )
  })
  
  # dropdown to select trade/trade list
  output$trade_control <- renderUI({
    req(input$direc)
    if(input$direc == "1" ){
      selectInput(
        inputId = "trade",
        label = tr("trade_dim"),
        choices = setNames(1:37, tr("trade_mem")),
        selected = 1
      ) 
    } else {
      selectInput(
        inputId = "tradeList",
        label = tr("trade_dim"),
        choices = setNames(1:37, tr("trade_mem")),
        multiple = TRUE,
        selected = c(1,2,3,29,30,36,37)
      )
    }
  })
  
  # dropdown menu for "Geography"
  output$geo_control <- renderUI({
    req(input$direc == "2" )
    selectInput(
      inputId = "geo",
      label = tr("geo_dim"),
      choices = setNames(1:13, tr("geo_mem")),
      selected = 1
    )
  })
  
  # radio Buttons for "Compare by"
  output$direc_control <- renderUI({
    radioButtons(
      inputId = "direc",
      label = tr("comp_dim"),
      choices = setNames(1:2, tr("comp_mem") ),
      selected = 1
    )
  })
  
  # creating bar 100% stack barchat  --------------------------------------
  
  output$outBarChart<- renderPlotly({
    
    full <- reactive_full_data()
    
    req(input$direc)
    if (input$direc == "1"){
      req(input$year,input$Sex,input$trade,input$times)
      df <- full %>%
        subset(
        REF_DATE  ==  input$year &
        dim_sex   ==  input$Sex &
        dim_trad  ==  input$trade &
        dim_inds %in% c(input$times,as.numeric(input$times) +1,as.numeric(input$times) +2),
        select   =  c(GEO,Pathway_inds,VALUE)) %>%
        recast(GEO ~ Pathway_inds, id.var = c("GEO", "Pathway_inds"))
      
      # to keep the order of geography
      df <- df[order(factor(df$GEO, levels = referenceGeo())),]
      
      # check if there are data points left after the filtering.
      validate(need(all(is.na(select(df,-1))) == FALSE, message = tr("mesg_val") ))
      
      df[is.na(df)] <- 0
      
      fig <- plot_ly(df, x = df[[2]], y = df[[1]], type = 'bar', orientation = 'h', name = tr("rate_cert"),
                     marker = list(color = '66c2a5'))
      
      fig <- fig %>% add_trace(x = df[[3]], name = tr("rate_cont"),
                               marker = list(color = 'fc8d62'))
      
      fig <- fig %>% add_trace(x = df[[4]], name = tr("rate_disc"),
                               marker = list(color = '8da0cb'))
      
      fig <- fig %>% layout(barmode = 'stack',
                            paper_bgcolor='rgba(0,0,0,0)',
                            plot_bgcolor='rgba(0,0,0,0)',
                            xaxis = list(title = ""),
                            yaxis = list(categoryorder = "array",
                                         categoryarray = df$GEO,
                                         range = c(0:12)),
                            margin = list(pad = 15),
                            legend=list(
                              traceorder = "normal",
                              orientation="h",
                              yanchor="button",
                              y=1.1,
                              xanchor="left",
                              x=0
                             )
                            )#layout

    } else {
      req(input$year,input$Sex,input$geo,input$tradeList,input$times)
      df <- full %>%
        subset(
          REF_DATE  ==    input$year &
          dim_sex   ==    input$Sex &
          dim_geo   ==    input$geo &
          dim_trad  %in%  input$tradeList &
          dim_inds  %in%  c(input$times,as.numeric(input$times) +1,as.numeric(input$times) +2),
          select   =  c(trades,Pathway_inds,VALUE)) %>%
        recast(trades ~ Pathway_inds, id.var = c("trades", "Pathway_inds"))
      
      # to keep the order of trade
      df <- df[order(factor(df$trades, levels = referenceTrade() )),] 
      
      # check if there are data points left after the filtering.
      validate(need(all(is.na(select(df,-1))) == FALSE, message = tr("mesg_val") ))
      
      fig <- plot_ly(df, x = df[[2]], y = df[[1]], type = 'bar', orientation = 'h',name = tr("rate_cert"),
                     marker = list(color = '66c2a5'))
      
      fig <- fig %>% add_trace(x = df[[3]], name = tr("rate_cont"),
                               marker = list(color = 'fc8d62'))
      
      fig <- fig %>% add_trace(x = df[[4]], name = tr("rate_disc"),
                               marker = list(color = '8da0cb'))
      
      fig <- fig %>% layout(barmode = 'stack',
                            paper_bgcolor='rgba(0,0,0,0)',
                            plot_bgcolor='rgba(0,0,0,0)',
                            xaxis = list(title = ""),
                            yaxis = list(categoryorder = "array",
                                         categoryarray = df$trades),
                            margin = list(pad = 15),
                            legend=list(
                              traceorder = "normal",
                              orientation="h",
                              yanchor="button",
                              y=1.1,
                              xanchor="left",
                              x=0
                            )
                           )#layout
    } #if condition
  })

  # creating value boxes to all indicators  -------------------------------
  
  # To make text box react to barchart 
  clicl_select <- reactive({
    clk <- event_data("plotly_click")
    
    if (input$direc == "1"){
      if (is.null(clk)) {
        geoselect = tr("canada_lbl") 
      } else if (clk$y %in% referenceTrade()){  #if user switch "compare by", then reset 
        geoselect = tr("canada_lbl")
      } else {
        geoselect = clk$y
      }
      
    } else {
      if (is.null(clk)) {
        trade_select = tr("all_trades_lbl")
      } else if (clk$y %in% referenceGeo()){   #if user switch "compare by", then reset 
        trade_select = tr("all_trades_lbl")
      } else {
        trade_select = clk$y
      }
    }
  })
  
  # value box for selected region and trade
  output$ibox_regTrade <- renderValueBox({
    req(input$direc)
    if (input$direc == "1"){
      valueBox(
        value = tags$p(clicl_select(), style = "font-size: 150%"), 
        tr("region_txt") )
    } else {
      valueBox(
        value = tags$p(clicl_select(), style = "font-size: 150%"), 
        tr("trade_txt") )
    }
    
  })
  

  # reactive function to create a new data frame for  
  # indicators rendered by values box, whose values are
  # reactive to "click"
  
  dfFilter <- reactive({
    
    full2 <- reactive_full_data()
    
    req(input$direc)
    if (input$direc == "1"){
      df <- full2 %>%
        subset(
          REF_DATE  ==  input$year &
          GEO    ==  clicl_select() &
          dim_sex   ==  input$Sex &
          dim_trad  ==  input$trade,
          select    =   c(dim_inds,Pathway_inds,VALUE))
    } else {
      df <- full2 %>%
        subset(
          REF_DATE  ==  input$year &
          dim_geo   ==  input$geo &
          dim_sex   ==  input$Sex &
          trades    ==  clicl_select(),
          select    =   c(dim_inds,Pathway_inds,VALUE))
    }
  })
  

  
  # value box for cohort size
  output$ibox_Size <- renderValueBox({
    df2 <- dfFilter()  
    if(reactive_vars$language == "en"){
      df2$VALUE[df2$dim_inds == 1] <- df2$VALUE[df2$dim_inds == 1] %>% 
        formatC(format="f", big.mark = ",", digits=0)
    } else {
      df2$VALUE[df2$dim_inds == 1] <- df2$VALUE[df2$dim_inds == 1] %>% 
        formatC(format="f", big.mark = " ", digits=0)      
    }
    valueBox(
      df2$VALUE[df2$dim_inds == 1], tr("coh_size_lbl"))
  })
  
  # value box for program duration 
  output$ibox_durpgm <- renderValueBox({
    df2 <- dfFilter()
    valueBox(
      df2$VALUE[df2$dim_inds == 2], tr("dur_lbl"))
  })
  
  # value box for median age at reg
  output$ibox_ageReg <- renderValueBox({
    df2 <- dfFilter()
    valueBox(
      df2$VALUE[df2$dim_inds == 3], tr("age_reg"))
  })
  
  # value box for median age at certification
  output$ibox_ageCert <- renderValueBox({
    df2 <- dfFilter()
    valueBox(
      df2$VALUE[df2$dim_inds == as.numeric(input$times) +5], tr("age_cert"))
  })
  
  # value box for median time to certification
  output$ibox_timeCert <- renderValueBox({
    df2 <- dfFilter()
    valueBox(
      df2$VALUE[df2$dim_inds == as.numeric(input$times) + 3], tr("time_cert"))
  })
  
  # value box for median time to discontinuation
  output$ibox_timeDisc <- renderValueBox({
    df2 <- dfFilter()
    valueBox(
      df2$VALUE[df2$dim_inds == as.numeric(input$times) + 4], tr("time_disct"))
  })
  
} #server func
