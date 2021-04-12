

server <- function(input, output, session) {
  
# Translation -------------------------------------------------------------
  dictionary <- read.csv("translation_fr.csv",encoding = "latin1")
  
  reactive_vars <- reactiveValues()
  reactive_vars$language <- "en"
  
  output$label_language <- renderText({
    reactive_vars$language
  })
  
  # Change button label when click
  observeEvent(input$btn_language, {
    if (reactive_vars$language == "en") {
      reactive_vars$language <- "fr"
    } else {
      reactive_vars$language <- "en"
    }
  })
  
  # the dictionary reacts to the selected language
  selected_dict <- reactive({
    dict <- dictionary %>%
      pull(reactive_vars$language)
    dict
  })
  
  # general title
#  output$generalTitle <- renderText({
#    selected_dict()[1]
#  })

# Pathway tab -------------------------------------------------------------
  #  Data processing-------------------------------------------------------
  url = "3710019301_databaseLoadingData.csv"
  
  dims <- c("dim_geo", "dim_sex", "dim_trad", "dim_inds")
  full <- readr::read_csv(url) %>%
    separate(col=COORDINATE, into=dims) %>%
    as.data.frame() 
    
  full <-merge(full,dictionary,by.x ="GEO",by.y = "en" ) %>% 
    merge(dictionary,by.x ="Selected trades",by.y = "en" ) %>%
    rename_at(c("Pathway indicators","GEO","Selected trades","fr.x","fr.y"),
              ~ c("Pathway_inds","GEO_en","trades_en","GEO_fr","trades_fr")) 
  
  full[, dims] <- sapply(full[, dims], as.numeric)
 
  # extract geo and trade from dict to maintain order
  referenceGeo <- reactive({ 
    req(reactive_vars$language)
    referenceGeo <- dictionary %>%
      slice(7:19) %>%
      pull(reactive_vars$language) %>%
      rev.default() 
  })
  
  referenceTrade <-reactive({ 
    req(reactive_vars$language)
    referenceTrade <-dictionary %>%
      slice(23:59) %>%
      pull(reactive_vars$language) %>%
      rev.default()
  })  
  
  # bulding sider bar widgets --------------------------------------------- 
  
  # pathway tab title
  output$pathwayTab <- renderText({
    selected_dict()[81]
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
      label = selected_dict()[2],
      choices = c(last_year:2008),
      selected = last_year-6
    )
  })
  
  # dropdown menu for "Sex"
  output$gender_control <- renderUI({
    selectInput(
      inputId = "Sex",
      label = selected_dict()[4],
      choices = setNames(1:3, selected_dict()[20:22]),
      selected = 1
    )
  })
  
  # dropdown to select time range
  output$times_control <- renderUI({
    selectInput(
      inputId = "times",
      label = selected_dict()[82],
      choices = setNames(c(4,10,16), selected_dict()[83:85]),
      selected = 4
    )
  })
  
  # dropdown to select trade/trade list
  output$trade_control <- renderUI({
    req(input$direc)
    if(input$direc == "1" ){
      selectInput(
        inputId = "trade",
        label = selected_dict()[5],
        choices = setNames(1:37, selected_dict()[23:59]),
        selected = 1
      ) 
    } else {
      selectInput(
        inputId = "tradeList",
        label = selected_dict()[5],
        choices = setNames(1:37, selected_dict()[23:59]),
        multiple = TRUE,
        selected = c(1,2,3,29,30,37)
      )
    }
  })
  
  # dropdown menu for "Geography"
  output$geo_control <- renderUI({
    req(input$direc == "2" )
    selectInput(
      inputId = "geo",
      label = selected_dict()[3],
      choices = setNames(1:13, selected_dict()[7:19]),
      selected = 1
    )
  })
  
  # radio Buttons for "Compare by"
  output$direc_control <- renderUI({
    radioButtons(
      inputId = "direc",
      label = selected_dict()[86],
      choices = setNames(1:2,selected_dict()[87:88] ),
      selected = 1
    )
  })
  
  # creating bar 100% stack barchat  --------------------------------------
  
  output$outBarChart<- renderPlotly({
    
    req(reactive_vars$language)

    # let raw data react to selected language
    if(reactive_vars$language == "en"){
      full <- full %>%
        rename_at(c("GEO_en","trades_en"),~c("GEO","trades"))
      
    } else{
      full <- full %>%
        rename_at(c("GEO_fr","trades_fr"),~c("GEO","trades"))
    }
    
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
      validate(need(all(is.na(select(df,-1))) == FALSE, message = selected_dict()[89]))
      
      fig <- plot_ly(df, x = df[[2]], y = df[[1]], type = 'bar', orientation = 'h', name = selected_dict()[95],
                     marker = list(color = 'rgb(11,83,148)'))
      
      fig <- fig %>% add_trace(x = df[[3]], name = selected_dict()[96],
                               marker = list(color = 'rgb(61,133,198)'))
      
      fig <- fig %>% add_trace(x = df[[4]], name = selected_dict()[97],
                               marker = list(color = 'rgb(111,168,220)'))
      
      fig <- fig %>% layout(barmode = 'stack',
                            paper_bgcolor='rgba(0,0,0,0)',
                            plot_bgcolor='rgba(0,0,0,0)',
                            xaxis = list(title = ""),
                            yaxis = list(categoryorder = "array",
                                         categoryarray = df$GEO),
                            legend=list(
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
      validate(need(all(is.na(select(df,-1))) == FALSE, message = selected_dict()[89]))
      
      fig <- plot_ly(df, x = df[[2]], y = df[[1]], type = 'bar', orientation = 'h', name = selected_dict()[95],
                     marker = list(color = 'rgb(11,83,148)'))
      
      fig <- fig %>% add_trace(x = df[[3]], name = selected_dict()[96],
                               marker = list(color = 'rgb(61,133,198)'))
      
      fig <- fig %>% add_trace(x = df[[4]], name = selected_dict()[97],
                               marker = list(color = 'rgb(111,168,220)'))
      
      fig <- fig %>% layout(barmode = 'stack',
                            paper_bgcolor='rgba(0,0,0,0)',
                            plot_bgcolor='rgba(0,0,0,0)',
                            xaxis = list(title = ""),
                            yaxis = list(categoryorder = "array",
                                         categoryarray = df$trades),
                            legend=list(
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
        geoselect = selected_dict()[7] #Canada
      } else if (clk$y %in% referenceTrade()){#if user switch "compare by", then reset 
        geoselect = selected_dict()[7] #Canada
      } else {
        geoselect = clk$y
      }
      
    } else {
      if (is.null(clk)) {
        trade_select = selected_dict()[23]#All trades
      } else if (clk$y %in% referenceGeo()){ #if user switch "compare by", then reset 
        trade_select = selected_dict()[23]#All trades 
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
        selected_dict()[93])
    } else {
      valueBox(
        value = tags$p(clicl_select(), style = "font-size: 150%"), 
        selected_dict()[94])
    }
    
  })
  

  # reactive function to create a new data frame for  
  # indicators rendered by values box, whose values are
  # reactive to "click"
  
  dfFilter <- reactive({
    req(reactive_vars$language)
    if(reactive_vars$language == "en"){
      full2 <- full %>%
         rename_at(c("GEO_en","trades_en"),~c("GEO","trades"))
    } else{
      full2 <- full %>%
        rename_at(c("GEO_fr","trades_fr"),~c("GEO","trades"))
    }
    
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
      df2$VALUE[df2$dim_inds == 1], selected_dict()[60])
  })
  
  # value box for program duration 
  output$ibox_durpgm <- renderValueBox({
    df2 <- dfFilter()
    valueBox(
      df2$VALUE[df2$dim_inds == 2], selected_dict()[61])
  })
  
  # value box for median age at reg
  output$ibox_ageReg <- renderValueBox({
    df2 <- dfFilter()
    valueBox(
      df2$VALUE[df2$dim_inds == 3], selected_dict()[62])
  })
  
  # value box for median age at certification
  output$ibox_ageCert <- renderValueBox({
    df2 <- dfFilter()
    valueBox(
      df2$VALUE[df2$dim_inds == as.numeric(input$times) +5], selected_dict()[90])
  })
  
  # value box for median time to certification
  output$ibox_timeCert <- renderValueBox({
    df2 <- dfFilter()
    valueBox(
      df2$VALUE[df2$dim_inds == as.numeric(input$times) + 3], selected_dict()[91])
  })
  
  # value box for median time to discontinuation
  output$ibox_timeDisc <- renderValueBox({
    df2 <- dfFilter()
    valueBox(
      df2$VALUE[df2$dim_inds == as.numeric(input$times) + 4], selected_dict()[92])
  })
  
} #server func
