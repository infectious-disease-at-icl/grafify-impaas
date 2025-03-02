source("src14b_plot_density_histo.R", local = TRUE)

plotDensity_react <- reactive({
  if(input$DoRelevel == "Yes") df <- RelevelFile1()
  if(input$DoRelevel == "No") df <- file1()
  if(input$logTrans %in% c("log10", "log2")) plotDensity <- 
      plot_density_log(data = df,
                      TextXAngle = input$text_angle,
                      c_alpha = input$sym_alpha,
                      fontsize = input$font_size,
                      ColSeq = input$colSeq,
                      ColRev = input$colRev,
                      ColPal = input$colpal,
                      group = !!input$varsOne, 
                      ycol = !!input$varsTwo,
                      LogYTrans = input$logTrans)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~" (log X-axis)"))
  if(input$logTrans == "") plotDensity <- 
      plot_density_log(data = df,
                      TextXAngle = input$text_angle,
                      fontsize = input$font_size,
                      c_alpha = input$sym_alpha,
                      ColSeq = input$colSeq,
                      ColRev = input$colRev,
                      ColPal = input$colpal,
                      group = !!input$varsOne, 
                      ycol = !!input$varsTwo)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo))
  plotDensity
})

fac_plotDensity_react <- reactive({
  p <- plotDensity_react() +
    facet_grid(FacVars())
  p
})

output$densityPlot_out <- renderPlot({ plotDensity_react() })

output$SaveViolin <- downloadHandler(
  filename = function(file) {
    "density_plot.pdf"
  },
  content = function(file) {
    ggsave(file, plot = plotDensity_react(), 
           width = input$g_wid, 
           height = input$g_ht, 
           units = "cm")
  }
)

###Histogram
plotHistogram_react <- reactive({
  if(input$DoRelevel == "Yes") df <- RelevelFile1()
  if(input$DoRelevel == "No") df <- file1()
  if(input$logTrans %in% c("log10", "log2")) plotHistogram <- 
      plot_histogram_log(data = df,
                         TextXAngle = input$text_angle,
                         BinSize = input$Binsize,
                         c_alpha = input$sym_alpha,
                         fontsize = input$font_size,
                         ColSeq = input$colSeq,
                         ColRev = input$colRev,
                         ColPal = input$colpal,
                         group = !!input$varsOne, 
                         ycol = !!input$varsTwo,
                         LogYTrans = input$logTrans)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~" (log X-axis)"))
  if(input$logTrans == "") plotHistogram <- 
      plot_histogram_log(data = df,
                         TextXAngle = input$text_angle,
                         BinSize = input$Binsize,
                         fontsize = input$font_size,
                         c_alpha = input$sym_alpha,
                         ColSeq = input$colSeq,
                         ColRev = input$colRev,
                         ColPal = input$colpal,
                         group = !!input$varsOne, 
                         ycol = !!input$varsTwo)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo))
  plotHistogram
})

fac_plotHistogram_react <- reactive({
  p <- plotHistogram_react() +
    facet_grid(FacVars())
  p
})

output$histogramPlot_out <- renderPlot({ plotHistogram_react() })

output$SaveViolin <- downloadHandler(
  filename = function(file) {
    "histogram_plot.pdf"
  },
  content = function(file) {
    ggsave(file, plot = plotHistogram_react(), 
           width = input$g_wid, 
           height = input$g_ht, 
           units = "cm")
  }
)
