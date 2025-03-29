#reactives for AvgRF plot on ANOVA panel
AvgRFwhichplotChosenGraph <- eventReactive(input$analyseData, {
  #boxplot w & w/o facets
  if (input$graphType == "Boxplot" &
      input$addVarsOpt == "No" )
    p <- plotBox_react()
  #bar graph w & w/o facets
  if (input$graphType == "Bar graph" &
      input$addVarsOpt == "No")
    p <- plotBar_react()
  #violin graph w & w/o facets
  if (input$graphType == "Violin plot"  &
      input$addVarsOpt == "No" )
    p <- plotViolin_react()
  #pointSD
  if (input$graphType == "Point & Errorbar" &
      input$addVarsOpt == "No" )
    p <- plotPointSD_react()
  
  #befafter graph w & w/o facets
  if (input$graphType == "Before-after plot"  &
      input$addVarsOpt == "No")
    p <- plotBefAfter_react()
  
  if (input$graphType == "Boxplot" &
      input$addVarsOpt == "Yes")
    p <- plot4dBox_react()
  if (input$graphType == "Bar graph" &
      input$addVarsOpt == "Yes")
    p <- plot4dBar_react()
  if (input$graphType == "Violin plot" &
      input$addVarsOpt == "Yes")
    p <- plot4dViolin_react()
  if (input$graphType == "Point & Errorbar" &
      input$addVarsOpt == "Yes")
    p <- plot4dPoint_react()
  
  #XY numeric catGroup
  if (input$graphType == "Numeric XY 1" &
      input$addVarsOpt == "Yes")
    p <- plot_XYCat_react()
  if (input$graphType == "Numeric XY 2" &
      input$addVarsOpt == "Yes")
    p <- plot_XYNum_react()
  
  #Density & Histogram
  if (input$graphType == "Density plot")
    p <- plotDensity_react()
  if (input$graphType == "Histogram plot" )
    p <- plotHistogram_react()
  
  #  #3dBox graphs w & w/o facets
  #  if (input$graphType == "Boxplot" &
  #      input$addVarsOpt == "No" &
  #      input$ShapesOpt == "Yes")
  #    p <- plot3dBox_react()
  #  #3dViolin graphs w & w/o facets
  #  if (input$graphType == "Violin plot" &
  #      input$addVarsOpt == "No" &
  #      input$ShapesOpt == "Yes")
  #    p <- plot3dViolin_react()
  #  #3dBar graphs w & w/o facets
  #  if (input$graphType == "Bar graph" &
  #      input$addVarsOpt == "No" &
  #      input$ShapesOpt == "Yes")
  #    p <- plot3dBar_react()
  #  #3dpoint graphs w & w/o facets
  #  if (input$graphType == "Point & Errorbar" &
  #      input$addVarsOpt == "No" &
  #      input$ShapesOpt == "Yes")
  #    p <- plot3dPoint_react()
  #  #4dbox w/ w/o shapes (w/o facets)
  #  if (input$graphType == "Boxplot" &
  #      input$addVarsOpt == "Yes" &
  #      input$ShapesOpt == "Yes")
  #    p <- plot4dShapesBox_react()
  #  #4dbar w/ w/o shapes (wo/ facets)
  #  if (input$graphType == "Bar graph" &
  #      input$addVarsOpt == "Yes" &
  #      input$ShapesOpt == "Yes")
  #    p <- plot4dShapesBar_react()
  #  #4dviolin w/ w/o shapes (w/o facets)
  #  if (input$graphType == "Violin plot" &
  #      input$addVarsOpt == "Yes" &
  #      input$ShapesOpt == "Yes")
  #    p <- plot4dShapesViolin_react()
  #  #4dbar w/ w/o shapes (wo/ facets)
  #  if (input$graphType == "Point & Errorbar" &
  #      input$addVarsOpt == "Yes" &
  #      input$ShapesOpt == "Yes" )
  #    p <- plot4dShapesPoint_react()

  #output reactive graph p
  p
})

#add single colour on chosen graph
AvgRFPlotSingCol <- eventReactive(input$analyseData, {
  ifelse(input$facetingOpt == "Yes",
         p <- AvgRFwhichplotChosenGraph() +
           facet_grid(FacVars(),
                      scales = input$facet_scales),
         p <- AvgRFwhichplotChosenGraph())
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