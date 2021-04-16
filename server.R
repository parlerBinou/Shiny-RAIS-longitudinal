dictionary_content <- read.csv('translation_fr.csv',encoding = "latin1")
translation <- dlply(dictionary_content ,.(key), function(s) key = as.list(s))

# extract list of trades and geos to map values in the dataset
refGeoEn <- dictionary_content %>%
  slice(6:18) %>% # Geo_mem rows
  pull("en") 

refTradesEn <- dictionary_content %>%
  slice(22:58) %>% # trades_mem rows
  pull("en") 

server <- function(input, output, session) {
  
# Translation -------------------------------------------------------------

  reactive_vars <- reactiveValues()
  reactive_vars$language <- "fr"
  
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
    rename_at(c("Pathway indicators","Selected trades"), ~ c("Pathway_inds","trades"))%>%
    within(STATUS[STATUS == ".." | is.na(STATUS) ] <- " ") %>%
    within(SYMBOL[is.na(SYMBOL)] <- " ") %>%
    mutate(flag = paste(STATUS,SYMBOL))
  
  full[, dims] <- sapply(full[, dims], as.numeric)
  
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
    req(input$direc,reactive_vars$language)
    if (reactive_vars$language == "en"){
      if(input$direc == "1" ){
        pickerInput(
          inputId = "trade",
          label = tr("trade_dim"),
          choices = list(
            "Trade group" = tr("trade_grp_mem"),
            "Selected Red Seal trades"  = tr("rs_trade_mem"),
            "Selected non-Red Seal trades"= tr("nrs_trade_mem")
          ),
          selected = tr("all_trades_lbl")
        ) 
      } else {
        pickerInput(
          inputId = "tradeList",
          label = tr("trade_dim"),
          choices = list(
            "Trade group" = tr("trade_grp_mem"),
            "Selected Red Seal trades"  = tr("rs_trade_mem"),
            "Selected non-Red Seal trades"= tr("nrs_trade_mem")
          ),
          multiple = TRUE,
          selected = tr("trade_grp_mem")
        )
      }
    } else{
      if(input$direc == "1" ){
        pickerInput(
          inputId = "trade",
          label = tr("trade_dim"),
          choices = list(
            "Groupe des métiers" = tr("trade_grp_mem"),
            "Métiers Sceau Rouge sélectionnés"  = tr("rs_trade_mem"),
            "Métiers non-Sceau Rouge sélectionnés"= tr("nrs_trade_mem")
          ),
          selected = tr("all_trades_lbl")
        ) 
      } else {
        pickerInput(
          inputId = "tradeList",
          label = tr("trade_dim"),
          choices = list(
            "Groupe des métiers" = tr("trade_grp_mem"),
            "Métiers Sceau Rouge sélectionnés"  = tr("rs_trade_mem"),
            "Métiers non-Sceau Rouge sélectionnés"= tr("nrs_trade_mem")
          ),
          multiple = TRUE,
          selected = tr("trade_grp_mem")
        )
      }
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
    
    req(input$direc,full)
    
    if (input$direc == "1"){
      req(input$year,input$Sex,input$trade,input$times,refTradesEn,reactive_vars$language)
      df <- full %>%
        subset(
          REF_DATE  ==  input$year &
          dim_sex   ==  input$Sex &
          trades   %in% mapvalues(input$trade,tr("trade_mem"),refTradesEn,warn_missing = FALSE) &
          dim_inds %in% c(input$times,as.numeric(input$times) +1,as.numeric(input$times) +2)
        )
      
      dfFlag <- df%>%
        subset(select = c(GEO,Pathway_inds,flag))
          
      df <- df %>%
        subset(select = c(GEO,Pathway_inds,VALUE)) %>%
        mutate(VALUE = VALUE/100) %>%
        recast(GEO ~ Pathway_inds, id.var = c("GEO", "Pathway_inds"))
      
      # update language of geo label in dataset
      df[[1]] <- mapvalues(df[[1]],refGeoEn,tr("geo_mem"),warn_missing = FALSE)
      dfFlag$GEO <- mapvalues(dfFlag$GEO,refGeoEn,tr("geo_mem"),warn_missing = FALSE)
      
      # to keep the order of geography
      df <- df[order(factor(df$GEO, levels = referenceGeo())),]
      dfFlag <- dfFlag[order(factor(dfFlag$GEO, levels = referenceGeo())),]
      
      
      # check if there are data points left after the filtering.
      validate(need(all(is.na(select(df,-1))) == FALSE, message = tr("mesg_val") ))
      
      df[is.na(df)] <- 0
      
      print(dfFlag$flag[dfFlag$Pathway_inds == colnames(df)[2]])
      
      fig <- plot_ly( x = df[[2]], y = df[[1]], type = 'bar', orientation = 'h', name = tr("rate_cert"),
                     marker = list(color = '66c2a5'),
                     text = dfFlag$flag[dfFlag$Pathway_inds == colnames(df)[2]],
                     hovertemplate = paste("%{y}","%{x:%}", "<b>%{text}</b>")
                     )
      
      fig <- fig %>% add_trace(x = df[[3]], name = tr("rate_cont"),
                               marker = list(color = 'fc8d62'),
                               text = dfFlag$flag[dfFlag$Pathway_inds == colnames(df)[3]],
                               hovertemplate = paste("%{y}","%{x:%}", "<b>%{text}</b>")
                               )
      
      fig <- fig %>% add_trace(x = df[[4]], name = tr("rate_disc"),
                               marker = list(color = '8da0cb'),
                               text = dfFlag$flag[dfFlag$Pathway_inds == colnames(df)[4]],
                               hovertemplate = paste("%{y}","%{x:%}", "<b>%{text}</b>")
                               )
      
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
          trades   %in%   mapvalues(input$tradeList,tr("trade_mem"),refTradesEn,warn_missing = FALSE) &
          dim_inds %in%   c(input$times,as.numeric(input$times) +1,as.numeric(input$times) +2)
        )
          
      dfFlag <- df%>%
        subset(select = c(trades,Pathway_inds,flag))    
          
      df <- df %>%
        subset(select = c(trades,Pathway_inds,VALUE)) %>%
        mutate(VALUE  = VALUE/100) %>%
        recast(trades ~ Pathway_inds, id.var = c("trades", "Pathway_inds")) 

      
      # update language of trade label in dataset
      df[[1]] <- mapvalues(df[[1]],refTradesEn,tr("trade_mem"),warn_missing = FALSE)
      dfFlag$trades <- mapvalues(dfFlag$trades,refTradesEn,tr("trade_mem"),warn_missing = FALSE)
      
      # to keep the order of trade
      df <- df[order(factor(df$trades, levels = referenceTrade() )),] 
      dfFlag <- dfFlag[order(factor(dfFlag$trades, levels = referenceTrade())),]
      
      # check if there are data points left after the filtering.
      validate(need(all(is.na(select(df,-1))) == FALSE, message = tr("mesg_val") ))
      
      df[is.na(df)] <- 0
      
      fig <- plot_ly(df, x = df[[2]], y = df[[1]], type = 'bar', orientation = 'h',name = tr("rate_cert"),
                     marker = list(color = '66c2a5'),
                     text = dfFlag$flag[dfFlag$Pathway_inds == colnames(df)[2]],
                     hovertemplate = paste("%{y}","%{x:%}", "<b>%{text}</b>")
                     )
      
      fig <- fig %>% add_trace(x = df[[3]], name = tr("rate_cont"),
                               marker = list(color = 'fc8d62'),
                               text = dfFlag$flag[dfFlag$Pathway_inds == colnames(df)[3]],
                               hovertemplate = paste("%{y}","%{x:%}", "<b>%{text}</b>")
                               )
      
      fig <- fig %>% add_trace(x = df[[4]], name = tr("rate_disc"),
                               marker = list(color = '8da0cb'),
                               text = dfFlag$flag[dfFlag$Pathway_inds == colnames(df)[2]],
                               hovertemplate = paste("%{y}","%{x:%}", "<b>%{text}</b>")
                               )
      
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
    
    req(input$direc)
    if (input$direc == "1"){
      
      df <- full %>%
        subset(
          REF_DATE   ==    input$year &
          dim_sex    ==    input$Sex &
          trades    %in%   mapvalues(input$trade,tr("trade_mem"),refTradesEn,warn_missing = FALSE) )
      df$GEO = mapvalues(df$GEO,refGeoEn,tr("geo_mem"),warn_missing = FALSE)
      df <- df%>%
        subset(
          GEO    ==  clicl_select() ,
          select =   c(dim_inds,Pathway_inds,VALUE,SYMBOL))
      
    } else {
      df <- full %>%
        subset(
          REF_DATE  ==  input$year &
          dim_geo   ==  input$geo &
          dim_sex   ==  input$Sex )
      df$trades = mapvalues(df$trades,refTradesEn,tr("trade_mem"),warn_missing = FALSE)
      df <- df %>%
        subset(
          trades    ==  clicl_select(),
          select    =   c(dim_inds,Pathway_inds,VALUE,SYMBOL)
        )
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
      paste(
        df2$VALUE[df2$dim_inds == as.numeric(input$times) +5],
        df2$SYMBOL[df2$dim_inds == as.numeric(input$times) +5]), 
      tr("age_cert"))
  })
  
  # value box for median time to certification
  output$ibox_timeCert <- renderValueBox({
    df2 <- dfFilter()
    valueBox(
      paste(
        df2$VALUE[df2$dim_inds == as.numeric(input$times) + 3], 
        df2$SYMBOL[df2$dim_inds == as.numeric(input$times) +3]),       
      tr("time_cert"))
  })
  
  # value box for median time to discontinuation
  output$ibox_timeDisc <- renderValueBox({
    df2 <- dfFilter()
    valueBox(
      paste(
        df2$VALUE[df2$dim_inds == as.numeric(input$times) + 4], 
        df2$SYMBOL[df2$dim_inds == as.numeric(input$times) +4]),       
        tr("time_disct"))
  })
  
} #server func
