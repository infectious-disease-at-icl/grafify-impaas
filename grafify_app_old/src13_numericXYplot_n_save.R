plot_XYCat_react <- reactive({
  observe(input$XYBox)
  if(input$XYBox == "Yes") box = TRUE
  if(input$XYBox == "No") box = FALSE
  if(input$logTrans %in% c("log10", "log2") &
     input$logTransX == "") XYCatGroup <- 
      plot_xy_CatGroup(data = file1(),
                       symsize = input$sym_size,
                       TextXAngle = input$text_angle,
                       s_alpha = input$sym_alpha,
                       fontsize = input$font_size,
                       Boxplot = box,
                       ColPal = input$colpal,
                       ColSeq = input$colSeq,
                       ColRev = input$colRev,
                       CatGroup = !!input$varsFour,
                       xcol = !!input$varsOne, 
                       ycol = !!input$varsTwo,
                       LogYTrans = input$logTrans)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~" (log Y-axis)"))
  if(input$logTrans == "" &
     input$logTransX == "") XYCatGroup <- 
      plot_xy_CatGroup(data = file1(),
                       symsize = input$sym_size,
                       TextXAngle = input$text_angle,
                       Boxplot = box,
                       fontsize = input$font_size,
                       s_alpha = input$sym_alpha,
                       ColSeq = input$colSeq,
                       ColPal = input$colpal,
                       ColRev = input$colRev,
                       CatGroup = !!input$varsFour,
                       xcol = !!input$varsOne, 
                       ycol = !!input$varsTwo)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo))
  if(input$logTransX %in% c("log10", "log2") &
     input$logTrans == "") XYCatGroup <- 
      plot_xy_CatGroup(data = file1(),
                       symsize = input$sym_size,
                       TextXAngle = input$text_angle,
                       Boxplot = box,
                       fontsize = input$font_size,
                       s_alpha = input$sym_alpha,
                       ColSeq = input$colSeq,
                       ColPal = input$colpal,
                       ColRev = input$colRev,
                       CatGroup = !!input$varsFour,
                       LogXTrans = input$logTransX,
                       xcol = !!input$varsOne, 
                       ycol = !!input$varsTwo)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~" (log X-axis)"))
  if(input$logTransX %in% c("log10", "log2") &
     input$logTrans %in% c("log10", "log2")) XYCatGroup <- 
      plot_xy_CatGroup(data = file1(),
                       symsize = input$sym_size,
                       TextXAngle = input$text_angle,
                       Boxplot = box,
                       fontsize = input$font_size,
                       s_alpha = input$sym_alpha,
                       ColSeq = input$colSeq,
                       ColPal = input$colpal,
                       ColRev = input$colRev,
                       CatGroup = !!input$varsFour,
                       LogXTrans = input$logTransX,
                       LogYTrans = input$logTrans,
                       xcol = !!input$varsOne, 
                       ycol = !!input$varsTwo)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~" (log X & Y-axis)"))
  XYCatGroup
})

fac_plot_XYCat_react <- reactive({
  p <- plot_XYCat_react() +
    facet_grid(FacVars())
  p
})

output$XYCatGroup_out <- renderPlot({ plot_XYCat_react() })

output$SaveViolin <- downloadHandler(
  filename = function(file) {
    "XYCatGroup_plot.pdf"
  },
  content = function(file) {
    ggsave(file, plot = plot_XYCat_react(), 
           width = input$g_wid, 
           height = input$g_ht, 
           units = "cm")
  }
)

plot_XYNum_react <- reactive({
  observe(input$XYBox)
  if(input$XYBox == "Yes") box = TRUE
  if(input$XYBox == "No") box = FALSE
  if(input$logTrans %in% c("log10", "log2") &
     input$logTransX == "") XYNumGroup <- 
      plot_xy_NumGroup(data = file1(),
                       symsize = input$sym_size,
                       TextXAngle = input$text_angle,
                       s_alpha = input$sym_alpha,
                       fontsize = input$font_size,
                       Boxplot = box,
                       ColPal = input$colpal,
                       ColRev = input$colRev,
                       NumGroup = !!input$varsFour,
                       xcol = !!input$varsOne, 
                       ycol = !!input$varsTwo,
                       LogYTrans = input$logTrans)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~" (log Y-axis)"))
  if(input$logTrans == "" &
     input$logTransX == "") XYNumGroup <- 
      plot_xy_NumGroup(data = file1(),
                       symsize = input$sym_size,
                       TextXAngle = input$text_angle,
                       Boxplot = box,
                       fontsize = input$font_size,
                       s_alpha = input$sym_alpha,
                       ColPal = input$colpal,
                       ColRev = input$colRev,
                       NumGroup = !!input$varsFour,
                       xcol = !!input$varsOne, 
                       ycol = !!input$varsTwo)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo))
  if(input$logTransX %in% c("log10", "log2") &
     input$logTrans == "") XYNumGroup <- 
      plot_xy_NumGroup(data = file1(),
                       symsize = input$sym_size,
                       TextXAngle = input$text_angle,
                       Boxplot = box,
                       fontsize = input$font_size,
                       s_alpha = input$sym_alpha,
                       ColPal = input$colpal,
                       ColRev = input$colRev,
                       NumGroup = !!input$varsFour,
                       LogXTrans = input$logTransX,
                       xcol = !!input$varsOne, 
                       ycol = !!input$varsTwo)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~" (log X-axis)"))
  if(input$logTransX %in% c("log10", "log2") &
     input$logTrans %in% c("log10", "log2")) XYNumGroup <- 
      plot_xy_NumGroup(data = file1(),
                       symsize = input$sym_size,
                       TextXAngle = input$text_angle,
                       Boxplot = box,
                       fontsize = input$font_size,
                       s_alpha = input$sym_alpha,
                       ColRev = input$colRev,
                       ColPal = input$colpal,
                       NumGroup = !!input$varsFour,
                       LogXTrans = input$logTransX,
                       LogYTrans = input$logTrans,
                       xcol = !!input$varsOne, 
                       ycol = !!input$varsTwo)+
      labs(title = expr("Plot of"~!!input$varsOne~"vs"~!!input$varsTwo~" (log X & Y-axis)"))
  XYNumGroup
})

fac_plot_XYNum_react <- reactive({
  p <- plot_XYNum_react() +
    facet_grid(FacVars())
  p
})

output$XYNumNumGp_out <- renderPlot({ plot_XYNum_react() })

output$SaveViolin <- downloadHandler(
  filename = function(file) {
    "XYNumNumGrp_plot.pdf"
  },
  content = function(file) {
    ggsave(file, plot = plot_XYNum_react(), 
           width = input$g_wid, 
           height = input$g_ht, 
           units = "cm")
  }
)

