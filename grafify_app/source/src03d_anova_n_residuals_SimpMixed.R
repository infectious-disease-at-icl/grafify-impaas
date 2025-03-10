#collapse replicates in random factor
avgFile1 <- reactive({
  ######## -start file1() for XYNum & RelevelFile1() for everything else
  observe(input$MorS)
  if(Xnum() == TRUE & CatGp() == FALSE){df <- file1()}
  if(Xnum() == FALSE & CatGp() == TRUE) {df <- RelevelFile1()}
  if(Xnum() == FALSE & input$addVarsOpt == "No") {df <- RelevelFile1.1()}
  if(Xnum() == TRUE & CatGp() == TRUE) {df <- RelevelFile1.2()}
  ####### -end file1() for XYNum & RelevelFile1() for everything else 
  #req(RelevelFile1())
  #df <- RelevelFile1() #do not depend on DoRelevel
  #get colnames and assign
  ns <- colnames(df)
  x1 = ns[ns == input$varsOne]
  y = ns[ns == input$varsTwo]
  #get second variable
  if(input$addVarsOpt == "Yes"){
  x2 = ns[ns == input$varsFour]}
  #get random factor
  if(input$MorS == "Mixed"){
  r1 = ns[ns == input$varsSix]}
  #get table summaries  
  if(input$MorS == "Mixed" &
     input$addVarsOpt == "Yes"){
    avgdf <- table_summary(df,
                           y,
                           c(x1, x2, r1))
    colnames(avgdf)[4] <- y
    ltab <- length(colnames(avgdf))
    n1 <- ltab-3
    n2 <- ltab-2
    avgdf <- avgdf[, c(1:n1, n2:ltab)]
    }
  if(input$MorS == "Mixed" &
     input$addVarsOpt == "No"){
    avgdf <- table_summary(df,
                           y,
                           c(x1, r1))
    colnames(avgdf)[3] <- y
    ltab <- length(colnames(avgdf))
    n1 <- ltab-3
    n2 <- ltab-2
    avgdf <- avgdf[, c(1:n1, n2:ltab)]
    }
  if(input$MorS == "Simple" &
     input$addVarsOpt == "No"){
    avgdf <- df[, c(x1, y)]}
  if(input$MorS == "Simple" &
     input$addVarsOpt == "Yes"){
    avgdf <- df[, c(x1, x2, y)]}
  avgdf
})

output$avgFile_out <- render_gt({ 
  avgdf <- avgFile1() 
  gt(avgdf) %>% 
    opt_interactive() %>% 
    tab_options(
      table.width = pct(100)) %>% 
    fmt_auto()
  })

#reactive for simple linear model
simMod <- reactive({
  #observe({input$DoRelevel}) #were groups dropped on X-axis? 
  #req(file1())
  #req(RelevelFile1())
  ##get the updated or original dataframe
  #df <- RelevelFile1() #if(input$DoRelevel == "Yes") df <- RelevelFile1()  
  ##if(input$DoRelevel == "No") df <- file1()
  #
  #pass avgtable to reactive
  df <- avgFile1()
  
  ns <- colnames(df)
  x = ns[ns == input$varsOne]
  y = ns[ns == input$varsTwo]
  #get formula like form of predictors
  pred <- dep()
  #get Y value log10 or log2
  if(input$logTrans == "log10") {y <- paste("log10(", y, ")", sep = "")} 
  if(input$logTrans == "log2") {y <- paste("log2(", y, ")", sep = "")}
  if(input$logTrans == "") {y <- y}
  fml <- as.formula(paste(y, pred, sep = "~"))
  obj1 <- lm(formula = fml, 
             data = df)
  obj1$call$formula <- fml #update formula in lm object
  obj1
})
#text output of simple linear model 
#for RAW R output
output$newmodOut <- renderPrint({ simMod() })

#reactive for mixed model 
mixMod <- reactive({
  #observe({input$DoRelevel}) #were groups dropped in X-variable
  ##get original or updated dataframe
  #df <- RelevelFile1() #if(input$DoRelevel == "Yes") df <- RelevelFile1()
  ##if(input$DoRelevel == "No") df <- file1()
  
  #pass avgtable to reactive
  df <- avgFile1()
  
  ns <- colnames(df)
  x = ns[ns == input$varsOne]
  y = ns[ns == input$varsTwo]
  RandIn = ns[ns == input$varsSix]
  dep <- dep() #get formula like predictors
  #get random factor as lmer() 
  rand <- paste("+ (1|",
                input$varsSix, 
                ")")
  pred <- paste(dep, rand) #formula with random factor
  #get Y value with log10 and log2
  if(input$logTrans == "log10") {y <- paste("log10(", y, ")", sep = "")} 
  if(input$logTrans == "log2"){ y <- paste("log2(", y, ")", sep = "")}
  if(input$logTrans == "") {y <- y}
  fml <- as.formula(paste(y, pred, sep = "~")) #lmer formula
  obj1 <- lmer(formula = fml, 
               data = df)
  obj1@call$formula <- fml
  obj1 <- lmerTest::as_lmerModLmerTest(obj1) #set as lmerMod for P values
})
#UI Output of mixed model
output$anomodel <- renderPrint({
  mixMod()
})
#reactive that responds to Mixed vs Simple
decideModel <- eventReactive(input$analyseData, {
  if(input$MorS == "Simple") mod <- simMod()
  if(input$MorS == "Mixed") mod <- mixMod()
  mod
})
#reactive for summary of lm() or lmer()
ModsummEvent <- eventReactive(input$analyseData, {
  summary(decideModel())
})
#UI output for Linear Model panel
output$ModSummary <- renderPrint({ 
  ModsummEvent() })
#UI output of ANOVA table
output$AnovaTab1 <- render_gt({
  file7<-decideModel() #get linear model
  if(input$MorS == "Mixed") #if mixed use anova()
    obj2<-anova(file7, 
                type = "II",
                ddf = "Kenward-Roger")
  
  if(input$MorS == "Simple") #if simple use car::Anova()
    obj2 <- car::Anova(file7,
                       type = 2,
                       test.statistic = "F")
  obj2_n <- colnames(obj2)
  obj2$"ANOVA" <- rownames(obj2)
  obj2 <- obj2[, c("ANOVA", obj2_n)]
  n <- length(colnames(obj2))
  gt::gt(data = obj2)  %>% 
    tab_style(
      style = cell_fill(color = "#D0F4F5"),
      locations = cells_body(
        rows = `Pr(>F)` <= 0.05
      )
    ) %>%
    # Format the specified columns to scientific notation with 3 significant digits
    #fmt_scientific(
    #  columns = 1:n,
    #  decimals = 2
    #) %>%
    cols_align(
      align = "center",
      columns = everything()) %>%
    opt_interactive() %>% 
    fmt_auto()
  
  #brks <- seq(0, 0.05, 1)
  #clrs <- colorRampPalette(c("#6baed6", "white"))(length(brks) + 1)
  #formatSignif(DT::datatable(obj2,
  #                           options = list(columnDefs = list(list(className = 'dt-left', 
  #                                                                 targets = 1:n)))),
  #             columns = c(1:n),
  #             digits = 3) %>% 
  #  formatStyle(columns = "Pr(>F)",
  #              target = "row",
  #              backgroundColor = styleInterval(brks, 
  #                                              clrs))
})

#UI output of ANOVA residuals
#QQ plot 
output$ModPlot <- renderPlot({
  file8 <- decideModel()
  obj3 <- plot_qqmodel(Model = file8, 
                       symsize = input$sym_size, 
                       s_alpha = input$sym_alpha,
                       fontsize = (input$font_size-3))+
    #theme_grafify(aspect.ratio = 1)+
    labs(title = "QQ plot of model residuals")
    obj3
})
#Density plot
output$ModPlotDist <- renderPlot({
  file8 <- decideModel()
  Residuals = residuals(file8)
  n <- length(Residuals)
  df <- data.frame(Residuals,
                   Q = rep("A", times = n))
  obj3 <- plot_density(df,
                       Residuals,
                       Q,
                       fontsize = (input$font_size-3))+
    guides(fill = "none",
           colour = "none")+
    #theme_grafify(aspect.ratio = 1)+
    labs(title = "Density plot of model residuals")
  obj3
})
#faceted plot by random factor
RandFplotreact <- eventReactive(input$analyseData, {
  pf <- PlotSingCol()
  if(input$MorS == "Mixed")
    pf2 <- pf + facet_wrap(vars(!!input$varsSix))+
      #theme_grafify(aspect.ratio = 1)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo),
           subtitle = expr("(Faceted by"~!!input$varsSix~")"))
  pf2
})
#UI output of faceted plot
output$RandFplot <- renderPlot({
  RandFplotreact()
})


avg_RandFplotreact <- eventReactive(input$analyseData, {
  pf <- whichplotChosenGraph()
  df <- avgFile1()
  pf$data <- df
  #avgpf <- pf %+% df
  if(input$MorS == "Mixed"){
    avgpf <- pf + 
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo),
           subtitle = expr("(Means of replicates within levels of"~!!input$varsSix~"plotted)"))}
  avgpf
})

output$avgRandFplot <- renderPlot({
  avg_RandFplotreact()
})
