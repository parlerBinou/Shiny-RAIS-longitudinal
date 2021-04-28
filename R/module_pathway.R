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
      width = 2
    ), 
    
    mainPanel(
      fluidRow(
        column(offset = 1, width = 5,valueBoxOutput(NS(id,"ibox_regTrade"),width = 8)),
        column(width = 5, valueBoxOutput(NS(id,"ibox_provOfYear"),width = 8))
        
      ),
      
      fluidRow(
        column(offset = 1, width = 11,valueBoxOutput(NS(id,"ibox_tradeReg"),width = 8))
      ),
      
      fluidRow(
        column(offset = 1, width = 4, valueBoxOutput(NS(id,"ibox_Size"),width = 12)),
        column(width  = 4,valueBoxOutput(NS(id,"ibox_ageReg"),width = 12)),
        column( width = 3,valueBoxOutput(NS(id,"ibox_timeCert"),width = 12))
      ),
      
      fluidRow(
        column(offset = 1,width  = 4,valueBoxOutput(NS(id,"ibox_durpgm"),width = 12)),
        column(width = 4,valueBoxOutput(NS(id,"ibox_ageCert"),width = 12)),
        column(width  = 3,valueBoxOutput(NS(id,"ibox_timeDisc"),width = 12))
      ),
      
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
    dictionary <- read.csv('dictionary/dict_pathway.csv',encoding = "latin1")
    translation <- dlply(dictionary ,.(key), function(s) key = as.list(s))
    
    # define not in function
    '%!in%' <- function(x,y)!('%in%'(x,y))
    
    # define trasnlation  function
    tr <- function(text){ 
      ls <-lapply(text,function(s) translation[[s]][[language]])
      return(ls[[1]])
    }
    
    # extract list of trades and geos to map values in the dataset
    refGeoEn <- dictionary %>%
      slice(6:18) %>% # Geo_mem rows
      pull("en") 
    
    refTradesEn <- dictionary %>%
      slice(22:58) %>% # trades_mem rows
      pull("en") 
    
    #  Data processing----------------------------------------------------------
    url = "data/pathway.csv"
    
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
      referenceGeo <- dictionary %>%
        slice(6:18) %>% # Geo_mem rows
        pull(language) %>%
        rev.default() 
    })
    
    referenceTrade <-reactive({ 
      referenceTrade <-dictionary %>%
        slice(22:58) %>% # trades_mem rows
        pull(language) %>%
        rev.default()
    })  
    
    #  sidebar widgets----------------------------------------------------------
    # the most recent year (cohort) is used to define the range of
    # cohorts in the dropdown menu.
    last_year <- full %>%
      pull(REF_DATE) %>%
      max()
    
    
    # slider for "reference period"
    output$year_control <- renderUI({
      req(input$direc)
      if (input$direc == "1"){
        # slide time range
        sliderTextInput(
          inputId = NS(id,"yearRange"),
          label = tr("year"), 
          choices = c(2008:last_year),
          selected = c(2008, last_year-6)
        )
      } else {
        # slide time point
        selectizeInput(
          inputId = NS(id,"year"),
          label = tr("year"),
          choices = c(last_year:2008),
          selected = last_year-6
        )
      }
    })
    
    
    # dropdown menu for "Sex"
    output$gender_control <- renderUI({
      selectInput(
        inputId = NS(id,"Sex"),
        label = tr("sex_dim"),
        choices = setNames(1:3, tr("sex_mem")),
        selected = 1
      )
    })
    
    # dropdown to select time point
    output$times_control <- renderUI({
      selectInput(
        inputId = NS(id,"times"),
        label = tr("times_dim"),
        choices = setNames(c(4,10,16), tr("times_mem")),
        selected = 4
      )
    })
    
    
    
    # dropdown to select trade/trade list
    output$trade_control <- renderUI({
      req(input$direc)
      if (language == "en"){
        if(input$direc != "3" ){
          pickerInput(
            inputId = NS(id,"trade"),
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
            inputId = NS(id,"tradeList"),
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
        if(input$direc != "3" ){
          pickerInput(
            inputId = NS(id,"trade"),
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
            inputId = NS(id,"tradeList"),
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
      req(input$direc != "2" )
      selectInput(
        inputId = NS(id,"geo"),
        label = tr("geo_dim"),
        choices = setNames(1:13, tr("geo_mem")),
        selected = 1
      )
    })
    
    # radio Buttons for "Compare by"
    output$direc_control <- renderUI({
      radioButtons(
        inputId = NS(id,"direc"),
        label = tr("comp_dim"),
        choices = setNames(1:3, tr("comp_mem") ),
        selected = 1
      )
    })
    
    #  create plotly chat-------------------------------------------------------
    output$outBarChart<- renderPlotly({
      
      req(input$direc,full)
      
      if (input$direc == "1"){
        req(input$yearRange,input$Sex,input$geo,input$trade,input$times)
        
        df <- full %>%
          subset(
            REF_DATE %in% c(input$yearRange[1]:input$yearRange[2] ) &
              dim_sex   ==  input$Sex &
              dim_geo   ==  input$geo &
              trades    ==  mapvalues(input$trade,tr("trade_mem"),refTradesEn,warn_missing = FALSE) &
              dim_inds %in% c(input$times,as.numeric(input$times) +1,as.numeric(input$times) +2)
          )
        
        dfFlag <- df%>%
          subset(select = c(REF_DATE,Pathway_inds,flag))
        
        df <- df %>%
          subset(select = c(REF_DATE,Pathway_inds,VALUE)) %>%
          mutate(VALUE = VALUE/100,REF_DATE = paste(REF_DATE)) %>%
          recast(REF_DATE ~ Pathway_inds, id.var = c("REF_DATE", "Pathway_inds"))
        
        
        # check if there are data points left after the filtering.
        validate(need(all(is.na(select(df,-1))) == FALSE, message = tr("mesg_val") ))
        
        df[is.na(df)] <- 0
        
        fig <- plot_ly(y = df[[1]], x = df[[2]], name = tr("rate_cert"), type = 'bar', orientation = 'h', 
                       marker = list(color = '66c2a5'),
                       text = dfFlag$flag[dfFlag$Pathway_inds == colnames(df)[2]],
                       hovertemplate = paste("%{y}",":", "%{x:%}<sup>%{text}</sup>")                     
        )
        
        fig <- fig %>% add_trace(x = df[[3]], name = tr("rate_cont"), 
                                 marker = list(color = 'fc8d62'),
                                 text = dfFlag$flag[dfFlag$Pathway_inds == colnames(df)[3]],
                                 hovertemplate = paste("%{y}",":", "%{x:%}<sup>%{text}</sup>"))
        
        fig <- fig %>% add_trace(x = df[[4]], name = tr("rate_disc"), 
                                 marker = list(color = '8da0cb'),
                                 text = dfFlag$flag[dfFlag$Pathway_inds == colnames(df)[4]],
                                 hovertemplate = paste("%{y}",":", "%{x:%}<sup>%{text}</sup>")
        )
        
        fig <- fig %>% layout(
          barmode = 'stack',
          paper_bgcolor='rgba(0,0,0,0)',
          plot_bgcolor='rgba(0,0,0,0)',
          xaxis = list(title = "",visible = FALSE),
          legend=list(
            traceorder = "normal",
            orientation="h",
            yanchor="button",
            y=1.1,
            xanchor="left",
            x=0
          ) # legend list
        ) #layout
      }
      
      
      else if (input$direc == "2"){
        req(input$year,input$Sex,input$trade,input$times)
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
        
        
        fig <- plot_ly( x = df[[2]], y = df[[1]], type = 'bar', orientation = 'h', name = tr("rate_cert"),
                        marker = list(color = '66c2a5'),
                        text = dfFlag$flag[dfFlag$Pathway_inds == colnames(df)[2]],
                        hovertemplate = paste("%{y}","%{x:%}<sup>%{text}</sup>")
        )
        
        fig <- fig %>% add_trace(x = df[[3]], name = tr("rate_cont"),
                                 marker = list(color = 'fc8d62'),
                                 text = dfFlag$flag[dfFlag$Pathway_inds == colnames(df)[3]],
                                 hovertemplate = paste("%{y}","%{x:%}<sup>%{text}</sup>")
        )
        
        fig <- fig %>% add_trace(x = df[[4]], name = tr("rate_disc"),
                                 marker = list(color = '8da0cb'),
                                 text = dfFlag$flag[dfFlag$Pathway_inds == colnames(df)[4]],
                                 hovertemplate = paste("%{y}","%{x:%}<sup>%{text}</sup>")
        )
        
        fig <- fig %>% layout(barmode = 'stack',
                              paper_bgcolor='rgba(0,0,0,0)',
                              plot_bgcolor='rgba(0,0,0,0)',
                              xaxis = list(title = "",visible = FALSE),
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
                       hovertemplate = paste("%{y}","%{x:%}<sup>%{text}</sup>")
        )
        
        fig <- fig %>% add_trace(x = df[[3]], name = tr("rate_cont"),
                                 marker = list(color = 'fc8d62'),
                                 text = dfFlag$flag[dfFlag$Pathway_inds == colnames(df)[3]],
                                 hovertemplate = paste("%{y}","%{x:%}<sup>%{text}</sup>")
        )
        
        fig <- fig %>% add_trace(x = df[[4]], name = tr("rate_disc"),
                                 marker = list(color = '8da0cb'),
                                 text = dfFlag$flag[dfFlag$Pathway_inds == colnames(df)[2]],
                                 hovertemplate = paste("%{y}","%{x:%}<sup>%{text}</sup>")
        )
        
        fig <- fig %>% layout(barmode = 'stack',
                              paper_bgcolor='rgba(0,0,0,0)',
                              plot_bgcolor='rgba(0,0,0,0)',
                              xaxis = list(title = "",visible = FALSE),
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
    # To make text box react to barchart 
    clicl_select <- reactive({
      
      if (input$direc == "1"){
        clk <- event_data("plotly_click")
        
        if (is.null(clk)) {
          yearR_select = last_year-6
        } else if (clk$y %!in% c(2008:last_year)){
          yearR_select = last_year-6
        } else{
          yearR_select = clk$y
        }
        return(yearR_select)
      }
      
      else if (input$direc == "2"){
        clk <- event_data("plotly_click")
        
        if (is.null(clk)) {
          geoselect = tr("canada_lbl") 
        } else if (clk$y %!in% referenceGeo()){  #if user switch "compare by", then reset 
          geoselect = tr("canada_lbl")
        } else {
          geoselect = clk$y
        }
        return(geoselect)
        
      } else {
        clk <- event_data("plotly_click")
        
        if (is.null(clk)) {
          trade_select = tr("all_trades_lbl")
        } else if (clk$y %!in% referenceTrade()){   #if user switch "compare by", then reset 
          trade_select = tr("all_trades_lbl")
        } else {
          trade_select = clk$y
        }
        return(trade_select)
      }
    })

    # value box for selected region and trade
    output$ibox_regTrade <- renderValueBox({
      req(input$direc)
      if (input$direc == "1"){
        valueBox(
          value = tags$p(clicl_select(), style = "font-size: 150%"), 
          tr("year_txt") )
      }
      else if (input$direc == "2"){
        valueBox(
          value = tags$p(clicl_select(), style = "font-size: 150%"), 
          tr("region_txt") )
      } else {
        valueBox(
          value = tags$p(clicl_select(), style = "font-size: 150%"), 
          tr("trade_txt") )
      }
      
    })
    
    # value box for selected trade and region
    output$ibox_tradeReg <- renderValueBox({
      req(input$direc)
      
      if (input$direc == "1"){
        valueBox(
          value = tags$p(input$trade, style = "font-size: 150%"), 
          tr("trade_txt") )
      } 
      
      else if (input$direc == "2"){
        valueBox(
          value = tags$p(input$trade, style = "font-size: 150%"), 
          tr("trade_txt") )
      } else {
        valueBox(
          value = tags$p(mapvalues(input$geo, c(1:13), tr("geo_mem"),warn_missing = FALSE),
                         style = "font-size: 150%"), 
          tr("region_txt") )
      }
    })
    
    output$ibox_provOfYear <- renderValueBox({
      req(input$direc)
      if (input$direc == "1"){
        valueBox(
          value = tags$p(mapvalues(input$geo, c(1:13), tr("geo_mem"),warn_missing = FALSE),
                         style = "font-size: 150%"), 
          tr("region_txt") )
      } else{
        valueBox("","")
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
            REF_DATE  ==  clicl_select() &
              dim_sex   ==  input$Sex &
              dim_geo   ==  input$geo &
              trades    ==  mapvalues(input$trade,tr("trade_mem"),refTradesEn,warn_missing = FALSE),
            select    =   c(dim_inds,Pathway_inds,VALUE,SYMBOL)
          )
        return(df)
      }    
      
      if (input$direc == "2"){
        
        df <- full %>%
          subset(
            REF_DATE   ==    input$year &
              dim_sex    ==    input$Sex &
              trades    %in%   mapvalues(input$trade,tr("trade_mem"),refTradesEn,warn_missing = FALSE) )
        df$GEO = mapvalues(df$GEO,refGeoEn,tr("geo_mem"),warn_missing = FALSE)
        df <- df%>%
          subset(
            GEO    ==  clicl_select() ,
            select =   c(dim_inds,Pathway_inds,VALUE,SYMBOL)
          )
        return(df)
        
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
        return(df)
        
      }
    })
    
    
    
    # value box for cohort size
    output$ibox_Size <- renderValueBox({
      df2 <- dfFilter()  
      valueBox(
        format_number(df2$VALUE[df2$dim_inds == 1], locale = language), 
        tr("coh_size_lbl"))
    })
    
    # value box for program duration 
    output$ibox_durpgm <- renderValueBox({
      df2 <- dfFilter()
      valueBox(
        format_number(df2$VALUE[df2$dim_inds == 2], locale = language), 
        tr("dur_lbl"))
    })
    
    # value box for median age at reg
    output$ibox_ageReg <- renderValueBox({
      df2 <- dfFilter()
      valueBox(
        format_number(df2$VALUE[df2$dim_inds == 3], locale = language), 
        tr("age_reg"))
    })
    
    # value box for median age at certification
    output$ibox_ageCert <- renderValueBox({
      df2 <- dfFilter()
      valueBox(
        HTML(paste0(format_number(df2$VALUE[df2$dim_inds == as.numeric(input$times) +5],locale = language), 
                    "<sup>",
                    df2$SYMBOL[df2$dim_inds == as.numeric(input$times) +5],
                    "</sup>",collapse = NULL)), 
        tr("age_cert"))
    })
    
    # value box for median time to certification
    output$ibox_timeCert <- renderValueBox({
      df2 <- dfFilter()
      valueBox(
        HTML(paste0(format_number(df2$VALUE[df2$dim_inds == as.numeric(input$times) +3],locale = language), 
                    "<sup>",
                    df2$SYMBOL[df2$dim_inds == as.numeric(input$times) +3],       
                    "</sup>",collapse = NULL)), 
        tr("time_cert"))
    })
    
    # value box for median time to discontinuation
    output$ibox_timeDisc <- renderValueBox({
      df2 <- dfFilter()
      valueBox(
        HTML(paste0(format_number(df2$VALUE[df2$dim_inds == as.numeric(input$times) +4],locale = language), 
                    "<sup>",
                    df2$SYMBOL[df2$dim_inds == as.numeric(input$times) +4],       
                    "</sup>",collapse = NULL)),         
        tr("time_disct"))
    })
    
  }) # module func
}
