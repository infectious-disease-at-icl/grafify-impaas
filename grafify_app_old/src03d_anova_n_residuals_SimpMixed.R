#reactive for simple linear model
simMod <- reactive({
  observe({input$DoRelevel}) #were groups dropped on X-axis? 
  req(file1())
  req(RelevelFile1())
  #get the updated or original dataframe
  if(input$DoRelevel == "Yes") df <- RelevelFile1()  
  if(input$DoRelevel == "No") df <- file1()
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
  observe({input$DoRelevel}) #were groups dropped in X-variable
  #get original or updated dataframe
  if(input$DoRelevel == "Yes") df <- RelevelFile1()
  if(input$DoRelevel == "No") df <- file1()
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
output$AnovaTab1 <- DT::renderDataTable({
  file7<-decideModel() #get linear model
  if(input$MorS == "Mixed") #if mixed use anova()
    obj2<-anova(file7, 
                type = "II",
                ddf = "Kenward-Roger")
  
  if(input$MorS == "Simple") #if simple use car::Anova()
    obj2 <- car::Anova(file7,
                       type = 2,
                       test.statistic = "F")
  n <- length(colnames(obj2))
  formatSignif(DT::datatable(obj2,
                             options = list(columnDefs = list(list(className = 'dt-left', 
                                                                   targets = 1:n)))),
               columns = c(1:n),
               digits = 3)
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
      labs(title = "Plots faceted by random factor")
  pf2
})
#UI output of faceted plot
output$RandFplot <- renderPlot({
  RandFplotreact()
})
