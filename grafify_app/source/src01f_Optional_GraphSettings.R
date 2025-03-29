#optional choices for graphs

#UI output for plot_point_sd transparency
output$pointAllalpha <- renderUI({
  if (input$graphType == "Point & Errorbar")
    numericInput(
      "pointAllalpha",
      label = tooltip(
        trigger = list(
          tags$strong("Opacity of all symbols except the mean (0-1)"),
          bs_icon("info-circle")
        ),
        "Reduce to below 1 to make boxes or bars transparent. When set to 0, symbols will disappear."
      ),
      value = 0.4, step = 0.1,
      min = 0,
      max = 1
    )
})

#UI output for plot_point_sd sizes
output$pointAllsize <- renderUI({
  if (input$graphType == "Point & Errorbar")
    numericInput(
      "pointAllsize",
      label = tooltip(
        trigger = list(
          tags$strong("Adjust size of all symbols except the mean"),
          bs_icon("info-circle")
        ),
        "Increase or decrease size of data points."
      ),
      value = 2.5, step = 0.5,
      min = 0,
      max = 10
    )
})
#UI output for plot_point_sd shaapes
output$pointAllshape <- renderUI({
  if (input$graphType == "Point & Errorbar" & 
      input$ShapesOpt == "No")
    numericInput(
      "pointAllshape",
      label = tooltip(
        trigger = list(
          tags$strong("Change shape of all symbols except the mean (1-25)"),
          bs_icon("info-circle")
        ),
        "Change the shape of data points."
      ),
      value = 1, step = 1,
      min = 1,
      max = 25
    )
})
#UI output for plot_point_sd & bars errorbar width
output$ewid <- renderUI({
  if (input$graphType %in% c("Point & Errorbar", "Bar graph"))
    numericInput(
      "ewid",
      label = tooltip(
        trigger = list(
          tags$strong("Change the width of errorbar"),
          bs_icon("info-circle")
        ),
        "Change the width of the horizontal lines at ends of error bars."
      ),
      value = 0.1, step = 0.1,
      min = 0,
      max = 1
    )
})
#UI output for histogram bindwidth
output$Binsize <- renderUI({
  if (input$graphType %in% c("Histogram plot"))
    numericInput(
      "Binsize",
      label = tooltip(
        trigger = list(
          tags$strong("Change the binsize for histograms"),
          bs_icon("info-circle")
        ),
        "The default is 30, increase or decreaes this for binning values into groups for histograms."
      ),
      value = 30, step = 1,
      min = 5,
      max = 100
    )
})

#UI output for ColSeq options TRUE/FALSE
output$colSeqOut <- renderUI({
  selectizeInput(
    "colSeq",
    label = tooltip(
      trigger = list(tags$strong("Colour sequence"), bs_icon("info-circle")),
      "Colours for categorical variables are picked sequentially from the chosen colour palette (Default). If set to FALSE, the most distant colours are chosen."
    ),
    choices = c(TRUE, FALSE),
    selected = TRUE,
    options = list(dropdownParent = 'body'),
    multiple = FALSE
  )
})

#UI output for ColRev options TRUE/FALSE
output$colRevOut <- renderUI({
  selectizeInput(
    "colRev",
    label = tooltip(
      trigger = list(
        tags$strong("Reverse swatch order?"),
        bs_icon("info-circle")
      ),
      "Whether or not to pick colours from palette swatches in default or reverse order. Set to TRUE for the order to be reversed."
    ),
    choices = c(TRUE, FALSE),
    selected = FALSE,
    options = list(dropdownParent = 'body'),
    multiple = FALSE
  )
})

#UI output for errorbar type SD/SEM/CI95
#changed to dropdown list for consistency on Jan 19
output$error_type <- renderUI({
  if (input$graphType %in% c("Bar graph", "Point & Errorbar"))
    selectizeInput(
      "error_type",
      label = tooltip(
        trigger = list(tags$strong("Error Bars"), bs_icon("info-circle")),
        "Pick type of errorbars on Bar or Point & Errorbar graphs."
      ),
      choices = c("CI95", "SD", "SEM"),
      options = list(dropdownParent = 'body'),
      selected =  "SD", 
    )
})
#UI output for Violin transparency
output$vio_alpha <- renderUI({
  if (input$graphType == "Violin plot")
    #violin alpha
    numericInput(
      "vio_alpha",
      label = tooltip(
        trigger = list(tags$strong("Violin opacity (0-1)"), bs_icon("info-circle")),
        "Reduce to below 1 to make violins transparent. When set to 0, violins will appear white in colour."
      ),
      value = 1,
      max = 1, min = 0, step = 0.1
    )
})

#facet scales option
output$facet_scaleOut <- renderUI({
  if (input$facetingOpt == "Yes")
    selectizeInput(
      "facet_scales",
      label = tooltip(
        trigger = list(
          tags$h3("4.2"),
          tags$strong("Axes options for facet panels"),
          bs_icon("info-circle")
        ),
        "Should X and/or Y axes have the same scales on facet panel graphs? Pick which axes can vary freely."
      ),
      choices = c("Fixed XY" = "fixed",
                  "Free Y" = "free_y",
                  "Free X" = "free_x"), 
      selected = "Fixed XY",
      options = list(dropdownParent = 'body'),
      multiple = FALSE
      )
})


## Output for density counts
output$dens_counts <- renderUI({
  if (input$graphType %in% c("Density plot"))
    selectizeInput(
      "dens_count_type",
      label = tooltip(
        trigger = list(tags$strong("Plot type"), 
                       bs_icon("info-circle")),
        "Pick whether to plot smooth density, counts or normalised counts."
      ),
      choices = c("Density", "Counts", "Normalised counts"),
      options = list(dropdownParent = 'body'),
      selected =  "Counts", 
      multiple = FALSE
    )
})

## Output for histogram counts
output$hist_counts <- renderUI({
  if (input$graphType %in% c("Histogram plot"))
    selectizeInput(
      "hist_count_type",
      label = tooltip(
        trigger = list(tags$strong("Plot type"), 
                       bs_icon("info-circle")),
        "Pick whether to plot counts or normalised counts."
      ),
      choices = c("Counts", "Normalised counts"),
      options = list(dropdownParent = 'body'),
      selected =  "Counts", 
      multiple = FALSE
    )
})


## Output for box alpha, conditional
output$out_box_alpha <- renderUI({
  if (input$graphType %in% c("Boxplot",
                             "Bar graph", 
                             "Violin plot",
                             "Before after plot"))
    numericInput(
      "box_alpha",
      #box transparency option
      label = tooltip(
        trigger = list(tags$strong("Box or Bar opacity (0-1)"), bs_icon("info-circle")),
        "Reduce to below 1 to make boxes or bars transparent. When set to 0, boxes and bars will appear white in colour."
      ),
      value = 1,
      min = 0,
      max = 1,
      step = 0.1
    )
})

