mainPanel2.1 <- list(layout_columns(col_widths = 12, #used in src_02_headers_help
                                  card("",
                                       tagList(#h4("Types of graphs with grafify"),
                                         tags$img(src="shiny1_all_graphs.png", 
                                                  align="left", 
                                                  width="90%"),
                                         tags$ul(
                                           tags$li("The types of graphs available depend on the type of variables (cateogrical or numeric)."),
                                           tags$li("For categorical X-axis variables, the following graphs are possible with all data points shown by default and different wasy of depicting the central tendancy (e.g., median or mean) and dispersion (e.g., whiskers, SD, SEM or CI95 errorbars):"),
                                           tags$ol(
                                           tags$li("Box and whiskers"), 
                                           tags$li("Violin plot"), 
                                           tags$li("Bar graph"), 
                                           tags$li("Point and errorbar"), 
                                           tags$li("Before-after plot (needs a Matching variable in Boxes 5 and 5.1)"),
                                           tags$li("Density or Histogram plots"),
                                           tags$li("Numeric XY1 and Numeric XY2 (a Grouping factor is required in Boxes 3 and 3.1)"),
                                           tags$li("Faceting variables chosen in Boxes 4 and 4.1 will add panels to the corresponding graph type.")
                                         )
                                  )))
))

mainPanel2.2 <- list(layout_columns(col_widths = 12, #used in src_02_headers_help
                                  card("",
                                       tagList(#h4("Types of graphs with grafify"),
                                         tags$ul(
                                           tags$li("With a categorical X-axis variable graphs can have 2 or more groups along the X-axis, i.e., 1-way ANOVA or Student's t-test type designs."),
                                         tags$li("When a Grouping factor is chosen in ", tags$strong("Box 3"), " i.e., data are from two-way ANOVA type experimental designs, the same types of graphs will be available, but will be grouped along the X-axis.")),
                                         tags$img(src="shiny2_graphs_types.png",
                                                  align = "left",
                                                  width = "90%"),
                                         tags$ul(
                                           tags$li("A Shapes variable, typically to denote randomised blocks or matching or repeated measures, allows data symbols to match levels within the Shapes variables (only up to 25 shapes are availables for symbols)."),
                                           tags$li("Typically, data from matched experiments or subjects can be shown using this option."),
                                           tags$li("This variable is required for Before-after plots.")),
                                         tags$img(src="shiny3_graph_types.png",
                                                  align="left",
                                                  width="90%"),
                                         tags$ul(
                                           tags$li("If the X-axis variable is numeric, a Grouping factor is required."),
                                           tags$li("The type of X-axis variable and Grouping factor (categorical or numeric) is detected automatically and the graphs option updated to 'Numeric XY 1' (i.e., Grouping factor is categorical) or 'Numeric XY 2' (i.e., Grouping factor is numeric)."),
                                           tags$li("The colour palettes will also be accordingly updated. Note that Shapes variable, if chosen, will be ignored."),
                                           tags$li("A boxplot can also be shown by choosing 'Yes' in ", tags$strong("Box 7.1."))),
                                         tags$img(src="shiny4_graph_types.png",
                                                  align="left",
                                                  width="60%")
                                       )))
)

mainPanel2.3 <- list(layout_columns(col_widths = 12, #used in src_02_headers_help
                                    card("",
                                         tagList(#h4("Types of graphs with grafify"),
                                           tags$h6("Colour palettes for categroical and numeric variables will be made available in ", tags$strong("Box 8"), " based on the types of variables chosen."),
                                           tags$img(src="shiny6_colpalettes.png",
                                                    align = "left",
                                                    width = "90%")
                                           ))))