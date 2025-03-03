mainPanel2 <- list(layout_columns(col_widths = 8, #not in use
                                   card("",
                                        tagList(#h4("Types of graphs with grafify"),
                                                tags$img(src="shiny1_all_graphs.png", 
                                                         align="left", 
                                                         width="80%"),
                                                tags$h6("The types of graphs available depend on the type of variables (cateogrical or numeric). For categorical X-axis variables, the types of graphs can be chosen from ", tags$strong("Box 6"), ". Boxes/bars/point errorbar/violins and data distribution can be plotted."),
                                                tags$h6("When a Grouping factor is chosen in ", tags$strong("Box 3"), " i.e., data are from two-way ANOVA type experimental designs, the same types of graphs will be available, but will look as follows."),
                                                tags$img(src="shiny2_graphs_types.png",
                                                         align = "left",
                                                         width = "80%"),
                                                tags$h6("A Shapes variable, typically to denote randomised blocks or matching or repeated measures, allows data symbols to match levels within the Shapes variables (only up to 25 shapes are availables for symbols). Typically, data from matched experiments or subjects can be shown using this option. This variable is required for Before-after plots."),
                                                tags$img(src="shiny3_graph_types.png",
                                                         align="left",
                                                         width="80%"),
                                                tags$h6("If the X-axis variable is numeric, a Grouping factor is required. The type of X-axis variable and Grouping factor (categorical or numeric) is detected automatically and the graphs option updated to 'Numeric XY 1' (i.e., Grouping factor is categorical) or 'Numeric XY 2' (i.e., Grouping factor is numeric). The colour palettes will also be accordingly updated. Note that Shapes variable, if chosen, will be ignored. A boxplot can also be shown by choosing 'Yes' in ", tags$strong("Box 7.1.")),
                                                tags$img(src="shiny4_graph_types.png",
                                                         align="left",
                                                         width="50%")
                                        )))
)

mainPanel2.1 <- list(layout_columns(col_widths = 12, #used in src_02_headers_help
                                  card("",
                                       tagList(#h4("Types of graphs with grafify"),
                                         tags$img(src="shiny1_all_graphs.png", 
                                                  align="left", 
                                                  width="80%"),
                                         tags$h6("The types of graphs available depend on the type of variables (cateogrical or numeric). For categorical X-axis variables, the following graphs are possible with all data points shown by default and different wasy of depicting the central tendancy (e.g., median or mean) and dispersion (e.g., whiskers, SD, SEM or CI95 errorbars): Box and whiskers, Violin plot, Bar graph, Point and errorbar, Before-after plot.  Density or Histogram plots of data can also be plotted."),
                                           tags$h6("When both the X- and Y-axis variables are numeric, the graph types are called Numeric XY (a Grouping variable is required).")
                                         )
                                  )))

mainPanel2.2 <- list(layout_columns(col_widths = 12, #used in src_02_headers_help
                                  card("",
                                       tagList(#h4("Types of graphs with grafify"),
                                         tags$h6("With a categorical X-axis variable graphs can have 2 or more groups along the X-axis, i.e., 1-way ANOVA or Student's t-test type designs."),
                                         tags$h6("When a Grouping factor is chosen in ", tags$strong("Box 3"), " i.e., data are from two-way ANOVA type experimental designs, the same types of graphs will be available, but will be grouped along the X-axis."),
                                         tags$img(src="shiny2_graphs_types.png",
                                                  align = "left",
                                                  width = "90%"),
                                         tags$h6("A Shapes variable, typically to denote randomised blocks or matching or repeated measures, allows data symbols to match levels within the Shapes variables (only up to 25 shapes are availables for symbols). Typically, data from matched experiments or subjects can be shown using this option. This variable is required for Before-after plots."),
                                         tags$img(src="shiny3_graph_types.png",
                                                  align="left",
                                                  width="90%"),
                                         tags$h6("If the X-axis variable is numeric, a Grouping factor is required. The type of X-axis variable and Grouping factor (categorical or numeric) is detected automatically and the graphs option updated to 'Numeric XY 1' (i.e., Grouping factor is categorical) or 'Numeric XY 2' (i.e., Grouping factor is numeric). The colour palettes will also be accordingly updated. Note that Shapes variable, if chosen, will be ignored. A boxplot can also be shown by choosing 'Yes' in ", tags$strong("Box 7.1.")),
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