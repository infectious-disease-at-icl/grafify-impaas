#reactives for AvgRF plot on ANOVA panel
AvgRFwhichplotChosenGraph <- eventReactive(input$analyseData, {
  #befafter graph w & w/o facets
  if (input$graphType == "Before-after plot"  &
      input$addVarsOpt == "No")
    p <- plotBefAfterAvg_react()

  #Density & Histogram
  if (input$graphType == "Density plot")
    p <- plotDensity_react()
  if (input$graphType == "Histogram plot" )
    p <- plotHistogram_react()
  
    #3dBox graphs w & w/o facets
    if (input$graphType == "Boxplot" &
        input$addVarsOpt == "No")
      p <- plot3dAvgBox_react()
    #3dViolin graphs w & w/o facets
    if (input$graphType == "Violin plot" &
        input$addVarsOpt == "No")
      p <- plot3dAvgViolin_react()
    #3dBar graphs w & w/o facets
    if (input$graphType == "Bar graph" &
        input$addVarsOpt == "No")
      p <- plot3dAvgBar_react()
    #3dpoint graphs w & w/o facets
    if (input$graphType == "Point & Errorbar" &
        input$addVarsOpt == "No")
      p <- plot3dAvgPoint_react()
    #4dbox w/ w/o shapes (w/o facets)
    if (input$graphType == "Boxplot" &
        input$addVarsOpt == "Yes")
      p <- plot4dAvgShapesBox_react()
    #4dbar w/ w/o shapes (wo/ facets)
    if (input$graphType == "Bar graph" &
        input$addVarsOpt == "Yes")
      p <- plot4dAvgShapesBar_react()
    #4dviolin w/ w/o shapes (w/o facets)
    if (input$graphType == "Violin plot" &
        input$addVarsOpt == "Yes")
      p <- plot4dAvgShapesViolin_react()
    #4dbar w/ w/o shapes (wo/ facets)
    if (input$graphType == "Point & Errorbar" &
        input$addVarsOpt == "Yes")
      p <- plot4dAvgShapesPoint_react()

  #output reactive graph p
  p
})

#add single colour on chosen graph
AvgRFPlotSingCol <- eventReactive(input$analyseData, {
  #ifelse(input$facetingOpt == "Yes",
  #       p <- AvgRFwhichplotChosenGraph() +
  #         facet_grid(FacVars(),
  #                    scales = input$facet_scales),
  #       p <- AvgRFwhichplotChosenGraph())
  
  ########ignore faceting 
  observe(input$MorS)
  if(input$MorS == "Mixed" &
     RFLevs() >= 25){
    p <- PlotSingCol()}
  
  if(input$MorS == "Mixed" &
     RFLevs() < 25){
  p <- AvgRFwhichplotChosenGraph()+ 
    labs(subtitle = expr("Shapes mapped to"~!!input$varsSix))}
  
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
  p
})


######### RandFact levels

#used for warning if >25 levels
RFLevs <- eventReactive(input$analyseData, {
  req(avgFile1())
  observe(input$MorS)
  if(input$MorS == "Simple"){nRFLev <- "A"}
  if(input$MorS == "Mixed"){ 
    df <- avgFile1()
    nRFLev <- length(levels(factor(df[[input$varsSix]])))}
  nRFLev
})

output$RFLev_txt <- renderText({
  if (RFLevs() >= 25){
    txt <- paste(
      "Random Factor has more than 25 levels, which are too many to map to symbol shapes."
    )}
  if (RFLevs() < 25){
    txt <- paste(
      "Levels within Random Factor are mapped to symbol shapes."
    )}
  if (RFLevs() == "A"){
    txt <- paste(
      "Simple linear model chosen (no Random Factor used)."
    )}
  txt
})

############# 4d shapes versions ##### point
plot4dAvgShapesPoint_react <- reactive({
  df <- RelevelFile1() #if(input$DoRelevel == "Yes") df <- RelevelFile1()
  #if(input$DoRelevel == "No") df <- file1()
  if(input$logTrans %in% c("log10", "log2")) plot_4dshapesAvgPoint <- 
      plot_4d_point_sd(data = df,
                       xcol = !!input$varsOne, 
                       ycol = !!input$varsTwo,
                       points = !!input$varsFour,
                       shapes = !!input$varsSix,
                       ErrorType = input$error_type,
                       ewid = input$ewid,
                       symsize = input$sym_size,
                       s_alpha = input$sym_alpha,
                       all_jitter = input$sym_jitter,
                       all_alpha = input$pointAllalpha, 
                       all_size = input$pointAllsize, 
                       all_shape = input$pointAllshape, 
                       TextXAngle = input$text_angle,
                       fontsize = input$font_size,
                       ColSeq = input$colSeq,
                       ColRev = input$colRev,
                       ColPal = input$colpal,
                       LogYTrans = input$logTrans)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~" (log Y-axis)"))
  if(input$logTrans == "") plot_4dshapesAvgPoint <- 
      plot_4d_point_sd(data = df,
                       xcol = !!input$varsOne, 
                       ycol = !!input$varsTwo,
                       points = !!input$varsFour,
                       shapes = !!input$varsSix,
                       ErrorType = input$error_type,
                       ewid = input$ewid,
                       symsize = input$sym_size,
                       s_alpha = input$sym_alpha,
                       all_jitter = input$sym_jitter,
                       all_alpha = input$pointAllalpha, 
                       all_size = input$pointAllsize, 
                       all_shape = input$pointAllshape, 
                       TextXAngle = input$text_angle,
                       fontsize = input$font_size,
                       ColSeq = input$colSeq,
                       ColRev = input$colRev,
                       ColPal = input$colpal)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo))
  plot_4dshapesAvgPoint
})

output$bar4dshapesAvgPoint_out <- renderPlot({ plot4dAvgShapesPoint_react() })

#4d shapes box
plot4dAvgShapesBox_react <- reactive({
  df <- RelevelFile1() #if(input$DoRelevel == "Yes") df <- RelevelFile1()
  #if(input$DoRelevel == "No") df <- file1()
  if(input$logTrans %in% c("log10", "log2")) plot_4dshapesAvgBox <- 
      plot_4d_scatterbox(data = df,
                         symsize = input$sym_size,
                         TextXAngle = input$text_angle,
                         s_alpha = input$sym_alpha,
                         b_alpha = input$box_alpha,
                         jitter = input$sym_jitter,
                         fontsize = input$font_size,
                         ColSeq = input$colSeq,
                         ColRev = input$colRev,
                         ColPal = input$colpal,
                         shapes = !!input$varsSix,
                         boxes = !!input$varsFour,
                         xcol = !!input$varsOne, 
                         ycol = !!input$varsTwo,
                         LogYTrans = input$logTrans)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~" (log Y-axis)"))
  if(input$logTrans == "") plot_4dshapesAvgBox <- 
      plot_4d_scatterbox(data = df,
                         symsize = input$sym_size,
                         TextXAngle = input$text_angle,
                         fontsize = input$font_size,
                         s_alpha = input$sym_alpha,
                         b_alpha = input$box_alpha,
                         jitter = input$sym_jitter,
                         ColSeq = input$colSeq,
                         ColRev = input$colRev,
                         ColPal = input$colpal,
                         shapes = !!input$varsSix,
                         boxes = !!input$varsFour,
                         xcol = !!input$varsOne, 
                         ycol = !!input$varsTwo)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo))
  plot_4dshapesAvgBox
})

output$box4dshapesAvgPlot_out <- renderPlot({ plot4dAvgShapesBox_react() })


#4d shapes bar
plot4dAvgShapesBar_react <- reactive({
  df <- RelevelFile1() #if(input$DoRelevel == "Yes") df <- RelevelFile1()
  #if(input$DoRelevel == "No") df <- file1()
  if(input$logTrans %in% c("log10", "log2")) plot_4dshapesAvgBar <- 
      plot_4d_scatterbar(data = df,
                         xcol = !!input$varsOne, 
                         ycol = !!input$varsTwo,
                         bars = !!input$varsFour,
                         shapes = !!input$varsSix,
                         ErrorType = input$error_type,
                         ewid = input$ewid,
                         symsize = input$sym_size,
                         s_alpha = input$sym_alpha,
                         b_alpha = input$box_alpha,
                         jitter = input$sym_jitter,
                         TextXAngle = input$text_angle,
                         fontsize = input$font_size,
                         ColSeq = input$colSeq,
                         ColRev = input$colRev,
                         ColPal = input$colpal,
                         LogYTrans = input$logTrans)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~" (log Y-axis)"))
  if(input$logTrans == "") plot_4dshapesAvgBar <- 
      plot_4d_scatterbar(data = df,
                         xcol = !!input$varsOne, 
                         ycol = !!input$varsTwo,
                         bars = !!input$varsFour,
                         shapes = !!input$varsSix,
                         ErrorType = input$error_type,
                         ewid = input$ewid,
                         symsize = input$sym_size,
                         s_alpha = input$sym_alpha,
                         b_alpha = input$box_alpha,
                         jitter = input$sym_jitter,
                         TextXAngle = input$text_angle,
                         fontsize = input$font_size,
                         ColSeq = input$colSeq,
                         ColRev = input$colRev,
                         ColPal = input$colpal)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo))
  plot_4dshapesAvgBar
})

output$bar4dshapesAvgPlot_out <- renderPlot({ plot4dAvgShapesBar_react() })

#4d shapes violin
plot4dAvgShapesViolin_react <- reactive({
  df <- RelevelFile1() #if(input$DoRelevel == "Yes") df <- RelevelFile1()
  #if(input$DoRelevel == "No") df <- file1()
  if(input$logTrans %in% c("log10", "log2")) plot_4dshapesAvgviolin <- 
      plot_4d_scatterviolin(data = df,
                            symsize = input$sym_size,
                            TextXAngle = input$text_angle,
                            s_alpha = input$sym_alpha,
                            b_alpha = input$box_alpha,
                            v_alpha = input$vio_alpha,
                            jitter = input$sym_jitter,
                            fontsize = input$font_size,
                            ColPal = input$colpal,
                            shapes = !!input$varsSix,
                            boxes = !!input$varsFour,
                            xcol = !!input$varsOne, 
                            ycol = !!input$varsTwo,
                            LogYTrans = input$logTrans)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~" (log Y-axis)"))
  if(input$logTrans == "") plot_4dshapesAvgviolin <- 
      plot_4d_scatterviolin(data = df,
                            symsize = input$sym_size,
                            TextXAngle = input$text_angle,
                            fontsize = input$font_size,
                            s_alpha = input$sym_alpha,
                            b_alpha = input$box_alpha,
                            v_alpha = input$vio_alpha,
                            jitter = input$sym_jitter,
                            ColSeq = input$colSeq,
                            ColRev = input$colRev,
                            ColPal = input$colpal,
                            shapes = !!input$varsSix,
                            boxes = !!input$varsFour,
                            xcol = !!input$varsOne, 
                            ycol = !!input$varsTwo)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo))
  plot_4dshapesAvgviolin
})

output$violin4dshapesAvgPlot_out <- renderPlot({ plot4dAvgShapesViolin_react() })


############# 3d shapes versions ##### point
#3d shapes point
plot3dAvgPoint_react <- reactive({
  df <- RelevelFile1.1() #if(input$DoRelevel == "Yes") df <- RelevelFile1()
  #if(input$DoRelevel == "No") df <- file1()
  if(input$logTrans %in% c("log10", "log2")) plot_3dpoint <- 
      plot_3d_point_sd(data = df,
                       symsize = input$sym_size,
                       TextXAngle = input$text_angle,
                       s_alpha = input$sym_alpha,
                       ErrorType = input$error_type,
                       all_jitter = input$sym_jitter,
                       all_alpha = input$pointAllalpha, 
                       all_size = input$pointAllsize, 
                       #all_shape = input$pointAllshape, 
                       ewid = input$ewid, 
                       ColSeq = input$colSeq,
                       ColRev = input$colRev,
                       fontsize = input$font_size,
                       ColPal = input$colpal,
                       shapes = !!input$varsSix,
                       xcol = !!input$varsOne, 
                       ycol = !!input$varsTwo,
                       LogYTrans = input$logTrans)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~" (log Y-axis)"))
  if(input$logTrans == "") plot_3dpoint <- 
      plot_3d_point_sd(data = df,
                       symsize = input$sym_size,
                       TextXAngle = input$text_angle,
                       fontsize = input$font_size,
                       s_alpha = input$sym_alpha,
                       ErrorType = input$error_type,
                       all_jitter = input$sym_jitter,
                       all_alpha = input$pointAllalpha, 
                       all_size = input$pointAllsize, 
                       all_shape = input$pointAllshape, 
                       ewid = input$ewid, 
                       ColSeq = input$colSeq,
                       ColRev = input$colRev,
                       ColPal = input$colpal,
                       shapes = !!input$varsSix,
                       xcol = !!input$varsOne, 
                       ycol = !!input$varsTwo)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo))
  plot_3dpoint
})

output$point3dPlotAvg_out <- renderPlot({ plot3dAvgPoint_react() })

#3d shapes box
plot3dAvgBox_react <- reactive({
  df <- RelevelFile1.1() #if(input$DoRelevel == "Yes") df <- RelevelFile1()
  #if(input$DoRelevel == "No") df <- file1()
  if(input$logTrans %in% c("log10", "log2")) plot_3dbox <- 
      plot_3d_scatterbox(data = df,
                         symsize = input$sym_size,
                         TextXAngle = input$text_angle,
                         s_alpha = input$sym_alpha,
                         b_alpha = input$box_alpha,
                         jitter = input$sym_jitter,
                         fontsize = input$font_size,
                         ColSeq = input$colSeq,
                         ColRev = input$colRev,
                         ColPal = input$colpal,
                         shapes = !!input$varsSix,
                         xcol = !!input$varsOne, 
                         ycol = !!input$varsTwo,
                         LogYTrans = input$logTrans)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~" (log Y-axis)"))
  if(input$logTrans == "") plot_3dbox <- 
      plot_3d_scatterbox(data = df,
                         symsize = input$sym_size,
                         TextXAngle = input$text_angle,
                         fontsize = input$font_size,
                         s_alpha = input$sym_alpha,
                         b_alpha = input$box_alpha,
                         jitter = input$sym_jitter,
                         ColSeq = input$colSeq,
                         ColRev = input$colRev,
                         ColPal = input$colpal,
                         shapes = !!input$varsSix,
                         xcol = !!input$varsOne, 
                         ycol = !!input$varsTwo)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo))
  plot_3dbox
})

output$box3dPlotAvg_out <- renderPlot({ plot3dAvgBox_react() })

#3d shapes bar
plot3dAvgBar_react <- reactive({
  df <- RelevelFile1.1() #if(input$DoRelevel == "Yes") df <- RelevelFile1()
  #if(input$DoRelevel == "No") df <- file1()
  if(input$logTrans %in% c("log10", "log2")) plot_3dbar <- 
      plot_3d_scatterbar(data = df,
                         xcol = !!input$varsOne, 
                         ycol = !!input$varsTwo,
                         shapes = !!input$varsSix,
                         ErrorType = input$error_type,
                         ewid = input$ewid,
                         symsize = input$sym_size,
                         s_alpha = input$sym_alpha,
                         b_alpha = input$box_alpha,
                         jitter = input$sym_jitter,
                         fontsize = input$font_size,
                         TextXAngle = input$text_angle,
                         ColPal = input$colpal,
                         ColSeq = input$colSeq,
                         ColRev = input$colRev,
                         LogYTrans = input$logTrans)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~" (log Y-axis)"))
  if(input$logTrans == "") plot_3dbar <- 
      plot_3d_scatterbar(data = df,
                         xcol = !!input$varsOne, 
                         ycol = !!input$varsTwo,
                         shapes = !!input$varsSix,
                         ErrorType = input$error_type,
                         ewid = input$ewid,
                         symsize = input$sym_size,
                         s_alpha = input$sym_alpha,
                         b_alpha = input$box_alpha,
                         jitter = input$sym_jitter,
                         fontsize = input$font_size,
                         TextXAngle = input$text_angle,
                         ColPal = input$colpal,
                         ColSeq = input$colSeq,
                         ColRev = input$colRev)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo))
  plot_3dbar
})

output$bar3dPlotAvg_out <- renderPlot({ plot3dAvgBar_react() })

#3d shapes violin
plot3dAvgViolin_react <- reactive({
  df <- RelevelFile1.1() #if(input$DoRelevel == "Yes") df <- RelevelFile1()
  #if(input$DoRelevel == "No") df <- file1()
  if(input$logTrans %in% c("log10", "log2")) plot_3dviolin <- 
      plot_3d_scatterviolin(data = df,
                            symsize = input$sym_size,
                            TextXAngle = input$text_angle,
                            s_alpha = input$sym_alpha,
                            b_alpha = input$box_alpha,
                            v_alpha = input$vio_alpha,
                            jitter = input$sym_jitter,
                            fontsize = input$font_size,
                            ColSeq = input$colSeq,
                            ColRev = input$colRev,
                            ColPal = input$colpal,
                            shapes = !!input$varsSix,
                            xcol = !!input$varsOne, 
                            ycol = !!input$varsTwo,
                            LogYTrans = input$logTrans)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~" (log Y-axis)"))
  if(input$logTrans == "") plot_3dviolin <- 
      plot_3d_scatterviolin(data = df,
                            symsize = input$sym_size,
                            TextXAngle = input$text_angle,
                            fontsize = input$font_size,
                            s_alpha = input$sym_alpha,
                            b_alpha = input$box_alpha,
                            v_alpha = input$vio_alpha,
                            jitter = input$sym_jitter,
                            ColSeq = input$colSeq,
                            ColRev = input$colRev,
                            ColPal = input$colpal,
                            shapes = !!input$varsSix,
                            xcol = !!input$varsOne, 
                            ycol = !!input$varsTwo)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo))
  plot_3dviolin
})

output$violin3dPlotAvg_out <- renderPlot({ plot3dAvgViolin_react() })

######### before-after shapes
plotBefAfterAvg_react <- reactive({
  df <- RelevelFile1.1() #if(input$DoRelevel == "Yes") df <- RelevelFile1()
  #if(input$DoRelevel == "No") df <- file1()
  if(input$logTrans %in% c("log10", "log2")) plot_befafter <- 
      plot_befafter_shapes(data = df,
                        symsize = input$sym_size,
                        TextXAngle = input$text_angle,
                        fontsize = input$font_size,
                        s_alpha = input$sym_alpha,
                        b_alpha = input$box_alpha,
                        jitter = input$sym_jitter,
                        ColSeq = input$colSeq,
                        ColRev = input$colRev,
                        ColPal = input$colpal,
                        Boxplot = TRUE,
                        match = !!input$varsThree,
                        xcol = !!input$varsOne,
                        ycol = !!input$varsTwo,
                        LogYTrans = input$logTrans)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~" (log Y-axis)"))
  if(input$logTrans == "") plot_befafter <- 
      plot_befafter_shapes(data = df,
                        symsize = input$sym_size,
                        TextXAngle = input$text_angle,
                        fontsize = input$font_size,
                        s_alpha = input$sym_alpha,
                        b_alpha = input$box_alpha,
                        jitter = input$sym_jitter,
                        ColSeq = input$colSeq,
                        ColRev = input$colRev,
                        ColPal = input$colpal,
                        Boxplot = TRUE,
                        match = !!input$varsThree,
                        xcol = !!input$varsOne,
                        ycol = !!input$varsTwo)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo))
  plot_befafter
})


