plot4dShapesBox_react <- reactive({
  df <- RelevelFile1() #if(input$DoRelevel == "Yes") df <- RelevelFile1()
  #if(input$DoRelevel == "No") df <- file1()
  if(input$logTrans %in% c("log10", "log2")) plot_4dShapesBox <- 
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
                         shapes = !!input$varsThree,
                         boxes = !!input$varsFour,
                         xcol = !!input$varsOne, 
                         ycol = !!input$varsTwo,
                         LogYTrans = input$logTrans)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~"grouped by"~!!input$varsFour~" (log Y-axis)"))
  if(input$logTrans == "") plot_4dShapesBox <- 
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
                         shapes = !!input$varsThree,
                         boxes = !!input$varsFour,
                         xcol = !!input$varsOne, 
                         ycol = !!input$varsTwo)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~"grouped by"~!!input$varsFour))
  plot_4dShapesBox
})

fac_plot4dShapesBox_react <- reactive({
  p <- plot4dShapesBox_react() +
    facet_grid(FacVars())
  p
})

output$box4dShapesPlot_out <- renderPlot({ plot4dShapesBox_react() })

output$SaveViolin <- downloadHandler(
  filename = function(file) {
    "box4dShapes_plot.pdf"
  },
  content = function(file) {
    ggsave(file, plot = plot4dShapesBox_react(), 
           width = input$g_wid, 
           height = input$g_ht, 
           units = "cm")
  }
)
