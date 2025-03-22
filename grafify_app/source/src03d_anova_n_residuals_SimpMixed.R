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
  ############# - for shapes for RF plot on ANOVA tab
  #get shapes factor
  if(input$ShapesOpt == "Yes"){
    sh1 = ns[ns == input$varsThree]}
  
  #############
  
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
  
  
  ############# - for shapes for RF plot on ANOVA tab
  if(input$MorS == "Mixed" &
     input$addVarsOpt == "Yes" &
     input$ShapesOpt == "Yes"){
    avgdf <- table_summary(df,
                           y,
                           c(x1, x2, r1, sh1))
    colnames(avgdf)[5] <- y
    ltab <- length(colnames(avgdf))
    n1 <- ltab-3
    n2 <- ltab-2
    avgdf <- avgdf[, c(1:n1, n2:ltab)]
  }
  if(input$MorS == "Mixed" &
     input$addVarsOpt == "No"&
     input$ShapesOpt == "Yes"){
    avgdf <- table_summary(df,
                           y,
                           c(x1, r1, sh1))
    colnames(avgdf)[4] <- y
    ltab <- length(colnames(avgdf))
    n1 <- ltab-3
    n2 <- ltab-2
    avgdf <- avgdf[, c(1:n1, n2:ltab)]
  }
  if(input$MorS == "Simple" &
     input$addVarsOpt == "No"&
     input$ShapesOpt == "Yes"){
    avgdf <- df[, c(x1, sh1, y)]}
  if(input$MorS == "Simple" &
     input$addVarsOpt == "Yes"&
     input$ShapesOpt == "Yes"){
    avgdf <- df[, c(x1, x2, sh1, y)]}
  
  ############ 
  avgdf
})

output$avgFile_out <- render_gt({
  req(c(file1(), avgFile1()))
  ifelse(input$AvgRF == "Yes",
  avgdf <- avgFile1(), avgdf <- file1())
  gt(avgdf) %>% 
    opt_interactive() %>% 
    tab_options(
      table.width = pct(100)) %>% 
    fmt_auto()
  })

#reactive for simple linear model
simMod <- reactive({
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
  
  ########## Yes/No to AvgRF
  ifelse(input$AvgRF == "Yes",
         df <- avgFile1(), df <- file1())
  #pass avgtable to reactive
  #df <- avgFile1()
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
                       type = 2) #F statistic is default for lm()
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
  p <- whichplotChosenGraph() #take base graph without faceting
  #or the single colour graph, if chosen
  if(input$addVarsOpt == "Yes" &  
     Xnum() == FALSE & 
     CatGp() == TRUE){singColnum <- CatGplevels()}
  if(input$addVarsOpt == "No" & 
     Xnum() == FALSE) {singColnum <- Xlevels()}
  ifelse (input$colPick == "No" ,
          p <- p,
          p <- p +
            scale_fill_manual(values = rep(input$colPick2, 
                                           times = singColnum)))
  #add random factor as the facet
  if(input$MorS == "Mixed"){
  p <- p + facet_wrap(vars(!!input$varsSix))+
      #theme_grafify(aspect.ratio = 1)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo),
           subtitle = expr("(Faceted by the Random factor:"~!!input$varsSix~")"))}
  p
})
#UI output of faceted plot
output$RandFplot <- renderPlot({
  RandFplotreact()
})


avg_RandFplotreact <- eventReactive(input$analyseData, {
  pf <- whichplotChosenGraph()
  #or the single colour graph, if chosen
  if(input$addVarsOpt == "Yes" &  
     Xnum() == FALSE & 
     CatGp() == TRUE){singColnum <- CatGplevels()}
  if(input$addVarsOpt == "No" & 
     Xnum() == FALSE) {singColnum <- Xlevels()}
  ifelse (input$colPick == "No" ,
          pf <- pf,
          pf <- pf +
            scale_fill_manual(values = rep(input$colPick2, 
                                           times = singColnum)))
  
  ############# Yes/No to AvgRF
  #ifelse(input$AvgRF == "Yes",
  #       df <- avgFile1(), df <- file1())
  #############
  df <- avgFile1()
  pf$data <- df
  #avgpf <- pf %+% df
  #if(input$AvgRF == "Yes" & 
  #   input$MorS == "Mixed"){
    avgpf <- pf + 
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo),
           subtitle = expr("(Means of replicates within levels of random factor:"~!!input$varsSix~"plotted)"))
    #}
  
  ############# Yes/No to AvgRF
  #if(input$AvgRF == "No" & 
  #         input$MorS == "Mixed"){
  #       avgpf <- pf + aes(shape = factor(!!input$varsSix))+ 
  #         guides(shape = guide_legend(title = input$varsSix))
  #         labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo),
  #              subtitle = expr("(Shapes mapped to "~!!input$varsSix~")"))}
  #############
  avgpf
})

output$avgRandFplot <- renderPlot({
  avg_RandFplotreact()
})
