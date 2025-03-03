#get variables as a or a*b for lmer and emmeans specs
dep <- reactive({
  observe({c(input$logTransX, input$addVarsOpt)})
  #initiate NULL vectors
  g4 <- NULL
  dep1 <- NULL
  x = input$varsOne
  #get Grouping variable
  if(input$addVarsOpt == "Yes") g4 <- input$varsFour
  #get predictor when X is categorical 1way ANOVA
  if(CatGp() == 0 & Xnum() == FALSE) dep1 <- x
  #get predictor when X is categorical 2way ANOVA
  if(CatGp() == TRUE & Xnum() == FALSE) dep1 <- paste(x, g4, sep = "*")
  #get predictor when X is numeric w/o log
  if(Xnum() == TRUE & Ynum() == TRUE & CatGp() %in% c(TRUE, FALSE) &
     !input$logTransX %in% c("log10", "log2")) {dep1 <- paste(x, g4, sep = "*")}
  #get predictor when X is numeric w/ log10
  if(Xnum() == TRUE & Ynum() == TRUE & CatGp() %in% c(TRUE, FALSE) &
     input$logTransX == "log10") {dep1 <- paste(paste0("log10(", x, ")"), g4, sep = "*")}
  #get predictor when X is numeric w/o log2
  if(Xnum() == TRUE & Ynum() == TRUE & CatGp() %in% c(TRUE, FALSE) &
     input$logTransX == "log2") {dep1 <- paste(paste0("log2(", x, ")"), g4, sep = "*")}
  dep1
})

#UI output of dep() output is correct (dev only)
output$depReactive <- renderText({ paste(as.formula(formula(decideModel()))) })

#series of reactives to make emmeans formula
#reactive for emmeans specs Pairs
emmPairs <- reactive({
  observe(c(input$emm_type))
  emmDep <- dep()
  if(input$emm_type == "Pairwise")
    fml <- as.formula(paste("pairwise", "~", emmDep)) 
})
#UI output (dev only)
output$emmPairFML <- renderText({ paste(emmFml()) })
#reactive for emmeans specs Levelwise1
emmLev1 <- reactive({
  observe(c(input$emm_type, input$addVarsOpt))
  if(input$emm_type == "Levelwise 1" & 
     input$addVarsOpt == "Yes")
    emmDep <- paste(input$varsOne, input$varsFour, 
                    sep = "|")
  fml <- as.formula(sprintf('%s ~ %s', 
                            "pairwise", emmDep))
})
#reactive for emmeans specs Levelwise2
emmLev2 <- reactive({
  observe(c(input$emm_type, input$addVarsOpt))
  if(input$emm_type == "Levelwise 2" & 
     input$addVarsOpt == "Yes")
    emmDep <- paste(input$varsFour, input$varsOne, 
                    sep = "|")
  fml <- as.formula(sprintf('%s ~ %s', 
                            "pairwise", emmDep))
})
#reactive for emmeans specs trt.vs.ctrl
emmRef <- reactive({
  observe(c(input$emm_type, input$addVarsOpt))
  if(input$emm_type == "Compare to reference" & 
     input$addVarsOpt == "No")
    emmDep <- paste(input$varsOne)
  fml <- as.formula(sprintf('%s ~ %s', 
                            "trt.vs.ctrl", emmDep))
})
#UI output SelectInput for Ref level with trt.vs.ctrl
output$emmRefType <- renderUI({
  observe({input$emm_type})
  if(input$emm_type == "Compare to reference")
    numericInput(#session = "graphType", 
      inputId = "emm_Reftype",
      label = tags$strong("Choose reference level (only numbers 1, 2.. allowed)"), 
      value = 1, min = 1, step = 1, max = 100)
})
#reactive to get ref level to give emmeans
#nRefval <- reactive({ 
#  n <- input$emm_Reftype
#  n
#})

#reactive for emmeans fml of all types 
emmFml <- reactive({
  #formula output based on choice of comparison
  observe(input$emm_type) 
  if(input$emm_type == "Pairwise") fml <- emmPairs()
  if(input$emm_type == "Levelwise 1") fml <- emmLev1()
  if(input$emm_type == "Levelwise 2") fml <- emmLev2()
  if(input$emm_type == "Compare to reference") fml <- emmRef()
  fml
})

#reactive for main emmeans 
Comp1 <- reactive({
  #get linear model
  M1<-decideModel()
  #req(nRefval)
  #nRef <- nRefval()
  nRef <- input$emm_Reftype #from user input
  fml <- emmFml() #emmeans formula
  M2<-emmeans(M1, 
              specs = fml,
              type = "response",
              adjust="fdr",
              ref = nRef
  )
})

#UI output of emmeans $emmeans part
output$Comp1 <- DT::renderDataTable({
  req(file1())
  if(input$DoRelevel == "Yes") df <- RelevelFile1()
  if(input$DoRelevel == "No") df <- file1()
  file4 <- df
  t1 <- Comp1()
  t2 <- data.frame(t1[[1]])  #extract $emmeans from emmeans output
  n <- length(colnames(t2))  #get dim of table
  colnames(t2)[1] <- colnames(file4)[colnames(file4)==input$varsOne]
  observe(input$addVarsOpt)  #variable width for 1w or 2w ANOVA
  if(input$addVarsOpt == "Yes") from = 3
  if(input$addVarsOpt == "No") from = 2
  formatSignif(DT::datatable(t2,
                             options = list(columnDefs = list(list(className = 'dt-left', 
                                                                   targets = 1:n)))),
               columns = c(from:n),
               digits = 3)
})

#UI output of emmans $contrasts
output$Comp2 <- DT::renderDataTable({
  t3 <- Comp1()
  t4 <- data.frame(t3[[2]])  #extract $contrasts from emmeans output
  n <- length(colnames(t4))  #get dim of table
  observe(input$addVarsOpt)  #get column values for DT formatting
  if(input$addVarsOpt == "Yes" & 
     input$emm_type != "Pairwise") from = 3
  if(input$addVarsOpt == "No") from = 2
  if(input$addVarsOpt == "Yes" & 
     input$emm_type == "Pairwise") from = 2
  formatSignif(DT::datatable(t4,
                             options = list(columnDefs = list(list(className = 'dt-left', 
                                                                   targets = 1:n)))),
               columns = c(from:n),
               digits = 3)
})

#updating choices of emmeans for users if only 2 levels in X-axis variable
nlev <- reactive({
  req(file1())
  if(input$DoRelevel == "Yes") df <- RelevelFile1()
  if(input$DoRelevel == "No") df <- file1()
  #df <- file1()
  varLevels = length(levels(as.factor(df[[input$varsOne]])))
})

#observer for t.tests
observe({
  req(input$varsOne)
  if(input$addVarsOpt == "No" & 
     nlev() == 2)
    updateSelectInput(#session = "graphType", 
      inputId = "emm_type",
      #label = tags$strong("Choose graph type"),
      choices = c("Pairwise"))
})

#observer for 1w ANOVAs
observe({
  req(input$varsOne)
  if(input$addVarsOpt == "No" & 
     nlev() > 2)
    updateSelectInput(#session = "graphType", 
      inputId = "emm_type",
      #label = tags$strong("Choose graph type"),
      choices = c("Pairwise",
                  "Compare to reference"))
})

#observer for 2w ANOVAs
observe({
  req(input$varsOne)
  if(input$addVarsOpt == "Yes")
    updateSelectInput(#session = "graphType", 
      inputId = "emm_type",
      #label = tags$strong("Choose graph type"),
      choices = c("Pairwise",
                  "Levelwise 1",
                  "Levelwise 2"))
})