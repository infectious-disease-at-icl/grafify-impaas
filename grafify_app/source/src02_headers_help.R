output$dataHelpOpen <- renderText({ #in use
  HTML(paste0(tags$div(
    tags$h4("Quick Instructions", (
      tags$h6("(If using `grafify` for the first time, please read detailed instructions through the link on navigation bar.)")
    )),
    tags$ul(
      tags$li(
        "Upload a 'long' format table saved as a csv or Excel file, and click 'Start'. Or click 'Start' without file upload to use example data. If you are not familiar with a long-format table, see ",
        tags$strong("Instructions"),
        "."
      ),
      tags$li(
        "Then select options in dropdown",
        tags$strong(" Boxes 1-10 "),
        "for Graphs and Analyses. Remember to click appropriate action buttons to see graphs and analyses results ",
        tags$strong(
          "(e.g., 'Variables chosen', 'grafify my data', 'Analyse my data')"
        ),
        "."
      ),
      tags$li(
        tags$strong("Boxes 1 and 2"),
        " are for choosing X- and Y-axis variables."
      ),
      tags$li(
        tags$strong("Box 3"),
        "is for an optional Grouping variable (e.g., 2way-ANOVAs), but it is required if the X-axis variable is also numeric."
      ),
      tags$li(
        "All variables selected on this page will be used on Graphs and in the ANOVA analyses."
      ),
      tags$li(
        "After selecting variables go to the Graphs tab and choose options in ",
        tags$strong("Boxes 4-8"),
        "."
      )
    )
  )))
})

output$graphsHelpOpen <- renderText({ #in use
  HTML(paste0(tags$div(
    tags$ul(
      tags$li(
        tags$strong("Box 4"),
        " is for choosing Faceting variables (any number), or choose 'No' before proceeding."
      ),
      tags$li(
        tags$strong("Box 5"),
        " lets you pick a Shapes or Matching variable (only 1) from dropdown list, or choose 'No'."
      ),
      tags$li("Then press ", tags$strong("'Variables chosen'"), "."),
      tags$li(
        tags$strong("Box 6"),
        "lists the types of Graphs based on variables you have chosen. Make a selection and press ",
        tags$strong("'grafify my data'"),
        "to see the Graph."
      ),
      tags$li(
        tags$strong("Boxes 7.1 and 7.2"),
        "appears with options to reorder groups when the X-axis or Grouping variables are categorical."
      ),
      tags$li(
        tags$strong("Box 7.1"),
        "also appears when both X and Y are numeric variables, and offers a choice to show data grouped into box and whiskers plot with a connecting line."
      ),
      tags$li(
        tags$strong("Box 8"),
        "presents several optional settings that change the appearance of Graphs (e.g., colours, fonts etc). Click ",
        tags$strong("'grafify my data'"),
        "after changing any setting and see updated graphs. Download your graph as high-resolution PDFs by clicking ",
        tags$strong("'Save as PDF'"),
        "."
      ),
      tags$li(
        "Graphs and ANOVAs will not update automatically if you change variables. Please remember to press ",
        tags$strong("'Variables chosen'"),
        "after changing variables in ",
        tags$strong("(Boxes 1-5)"),
        " for updated Graph options and ANOVA results."
      ),
      tags$li(
        "Similarly, remember to press ",
        tags$strong("'grafify my data'"),
        "after changing options in  ",
        tags$strong("Box 8"),
        "."
      ),
      tags$li(
        "For analyses, go to the ANOVAs and Comparisons tab for ",
        tags$strong("Boxes 9-10"),
        "."
      )
    )
  )))
})
output$ANOVAsHelpOpen <- renderText({ #in use
  HTML(paste0(tags$div(
    tags$ul(
      tags$li(
        "ANOVA and post-hoc comparisons can be performed only after picking optional variables on Graphs tab and clicking ",
        tags$strong("'Variables chosen'"),
        "."
      ),
      tags$li(
        tags$strong("Box 9"),
        "allows you to choose the type of analysis (Simple/Mixed effects). Results will appear only after making a selection and clicking ",
        tags$strong("'Analyse my data'"),
        "."
      ),
      tags$li(
        "Graphs of model residuals will be shown as a QQ plot and a Density plot."
      ),
      tags$li(
        "If you choose 'Mixed', you need to also choose a random factor, e.g., experimental blocks, matching, pairing, repeated-measures. The random factor should be a categorical variable, otherwise the analyses will fail. A graph of your data faceted by the random factor will be displayed."
      ),
      tags$li(
        "After the analyses, if you change variables again in ",
        tags$strong("Boxes 1-3"),
        " remember to press ",
        tags$strong("'Variables chosen'"),
        "on Graphs tab and then click 'Analyase my data' to update ANOVAs results."
      ),
      tags$li(
        tags$strong("Box 10"),
        "for choosing the type of post-hoc comparisons."
      ),
      tags$li(
        "Check the model summary on the Linear Model tab, which should show the 'formula' with the correct variables and log-transformations, if used."
      )
    )
  )))
})

output$varHelpOpen <- renderText({ #NOT in use
  HTML(paste0(tags$div(
    tags$h4("Quick Instructions", (
      tags$h6("(Detailed instructions through the link on navigation bar.)")
    )),
    tags$ul(
      tags$li(
        "Upload a 'long' format table saved as a csv or Excel file, and click 'Start'."
      ),
      tags$li(
        "Pick X- and Y-axis variables. The Grouping variable is optional (e.g., 2way-ANOVAs), but it is required if the X-axis variable is also numeric."
      ),
      tags$li(
        "All variables selected on this page, and log-transformation from the Graphs tab, will be used on Graphs and in the ANOVA analyses."
      ),
      tags$li(
        "On the Graphs tab, set other options or choose 'No' and press 'Variables chosen'. Choose a Graph type and press 'grafify my data' to see the Graph."
      ),
      tags$li(
        "Box 8 on the Graphs Tab has options to change the appearance of graphs, which can be downloaded as high-resolution PDFs."
      ),
      tags$li(
        "ANOVA and post-hoc comparisons can be done only after picking optional variables on Graphs tab and clicking 'Variables chosen'."
      ),
      tags$li(
        "On the ANOVAs and Comparisons tab, pick the type of analysis (Simple/Mixed effects) and then click 'Analyse my data'"
      ),
      tags$li(
        "Graphs and ANOVAs will not update automatically if you change variables. Please remember to press appropriate action buttons to get updated Graphs and ANOVA results."
      ),
      tags$li(
        "Check the model summary on the Linear Model tab, which should show the 'formula' with the correct variables and log-transformations, if used."
      )
    )
  )))
})

output$pickvariables <- renderText({ #NOT in use
  HTML(paste0(
    tags$div(tags$h6("Info box"), tags$ul(tags$li(
      tags$em("Further help available by hovering over i icons.")
    ))),
    
    
    tags$div(tags$h5("grafify"), tags$ul(tags$li(
      tags$h6(
        "You can use grafify online to plot graphs, and easily perform ANOVAs and post-hoc comparisons just like the R package."
      )
    ), tags$li(
      tags$h6(
        "The main advantages of grafify are the use of ggplot2 and various colourblind-friendly palettes, and easy access to linear models and linear mixed effects analyses for ANOVAs. These are more powerful and appropriate when experiments are designed as randomised blocks or have repeated measures."
      )
    ))),
    
    tags$div(
      tags$h5("Data table"),
      tags$ul(
        tags$li(
          "To get started, upload a csv or Excel file containing your table in long-format. This means each column should contain one categorical or one numeric variable that you want to plot/use in analyses."
        ),
        tags$li(
          "Then press 'Start'. If all goes OK, your table should appear below. You should also see dropdown menus that let you select variables from your table."
        ),
        tags$li(
          "If you would like to use example data, click 'Start' directly and proceed to choosing variables."
        )
      ),
      layout_columns(col_widths = 12, card("", tagList(
        #h4("Types of graphs with grafify"),
        tags$img(
          src = "shiny7_long_table.png",
          align = "left",
          width = "70%"
        ),
        tags$h6(
          "The data table should be long-format with one variable (either categorical or numeric) per column. Note that columns with numbers are automatically assumed to be numeric. If you have a column to indicate Experiment 1, 2, 3, and so on, enter them as Exp_1, Exp_2 etc. so that R 'reads' them as categorical variables. The random variable for mixed effects analyses should be a categorical variable."
        )
      )))
    ),
    
    tags$div(
      tags$h5("Variables and Graphs"),
      tags$ul(
        tags$li(
          "At least 2 variables must be chosen - one each for the X and Y axes of Graphs. A third Grouping variable (e.g., for 2-way ANOVA designs) is optional when the X-axis variable is categorical."
        ),
        tags$li(
          "The X and Y axis variables can both be numeric. In this case, the Grouping variable is mandatory, and this variable can be categorical or numeric."
        ),
        tags$li(
          "Many types of Graphs are available, which will depend on type of data. Their appearance can be changed with options in ",
          tags$strong("Box 8"),
          " (see below)."
        ),
        tags$li(
          "After choosing variables on the Data tab, go to the Graphs tab for additional options. You can choose a Faceting and Shapes (or Matching) variable. Remember to press 'Variables chosen' after choosing these variables."
        ),
        tags$li(
          "The Shapes or Matching variable is useful to indicate pairing within data (e.g., time course, experimental blocks, paired subjects). This is required for Before-after plots and to map an additional 'Shape' variable to data points."
        ),
        tags$li(
          "A dropdown menu of Graph types should appear. Pick one, and then click 'grafify my data' to see your Graph. Graphs do not automatically update if you change appearance or variables - please click 'grafify my data' to update the graph."
        ),
        tags$li(
          "R and ggplot2 use alphabetically ordering based on group (levels) within categorical variables. You can reorder groups along the X-axis or plot only those you are interested in."
        ),
        tags$li(
          tags$strong(
            "When reordering groups, if you drop some levels within the factor (i.e., there were 10 groups in your data, but you plot only 3), those levels will not be used in the ANOVA analyses (i.e., only those 3 groups will be analysed in the ANOVA). Check that the Graph contains all the data you want to analyse."
          )
        ),
        tags$li(
          "If you change graph appearance in ",
          tags$strong("Box 8"),
          "remember to click 'grafify my data' again."
        ),
        tags$li(
          "The graph is available to download as a PDF file. You will need to change the height and width to fit the graph within the PDF document."
        ),
        tags$li(
          "Typically, at fontsize of 18 pts (default) with a angled text on X-axis, start with a height of 12 cm and increase width based on number of groups (e.g., 20cm for ~10 groups plus legend). Avoid reducing the fontsize on the graph, instead increase the dimensions of the PDF.Even if the graph looks 'squished' on the screen, it will look good when the dimension of the PDF file are large enough to accommodate it.  Increase the height if you also used a faceting variable, angled labels on the X-axis or have long list of legends. You do not need to press 'grafify my data' when you change dimensions, click 'Save as PDF' directly."
        ),
        tags$li(
          "Changing height or width does not change appearance of the graph online."
        )
      )
    ),
    
    tags$div(
      tags$h5("Types of Graphs"),
      mainPanel2, #from src_01g_Help_n_Images
      tags$ul(
        tags$li(
          tags$strong("Box & whiskers: "),
          tags$em(
            "All data points are shown as scattered circles with boxes depicting the interquartile range (IQR; 25th - 75th percentiles), and whiskers depicting 1.6xIQR, and a line representing the median (50th percentile). The colours of boxes and circles will match, and can be changed. If a Shapes variable is slected, the shape of data points will change (shown in black). Up to 25 different shapes are allowed."
          )
        ),
        tags$li(
          tags$strong("Violin plus box & whiskers: "),
          tags$em(
            "Everything as with box & whiskers, plus violins which show overall distribution of data. By default, the colours of symbols and vioins will match and boxes will look white (box opacity = 0), which can be changed."
          )
        ),
        tags$li(
          tags$strong("Bar graph: "),
          tags$em(
            "Bars show the mean of data and circles represent all data points. The default error bars will denote SD (standard deviation), which can be changed to SEM (standard error of the mean) or CI95 (95% confidence interval calculated from the t-distribution)."
          )
        ),
        tags$li(
          tags$strong("Point & errorbars: "),
          tags$em(
            "A square represents the mean of the data and circles show all data points. The default error bars will denote SD (standard deviation), which can be changed to SEM (standard error of the mean) or CI95 (95% confidence interval calculated from the t-distribution)."
          )
        ),
        tags$li(
          tags$strong("Density or Histogram plot : "),
          tags$em(
            "A smooth density plot of data (Y-axis variable) grouped by the X-axis (categorical variable only) or histogram plot. If Histogram plot is chosen, the binsize can be changed."
          )
        ),
        tags$li(
          tags$strong("Before-after plot: "),
          tags$em(
            "This is a box and whiskers plot, but an additional Shapes variable is needed that indicates matching or pairing within the data. Data points will be joined by grey lines to show change 'before and after treatment'."
          )
        ),
        tags$li(
          tags$strong("Numeric XY 1 and 2: "),
          tags$em(
            "These are used when the X-axis is also numeric (e.g., time, concentrations). A Grouping variable is also required, which can be categorical or numeric."
          )
        ),
        tags$li(
          tags$strong("Graph appearance: "),
          tags$ul(
            tags$li(
              tags$em(
                tags$strong("Colours: "),
                "12 colourblind-friendly colour schemes are available for categorical X-axis variables, and 5 for numeric X-axis variables. Sometimes, when there are lots of groups along the X-axis, too many colours can be distracting and unnecessary (because group labels are available). In this case, a single colour can be used for all groups."
              )
            ),
            tags$li(
              tags$em(
                tags$strong("Fonts and labels: "),
                "The default fontsize is 18 points, which can be changed as required. For publications/reports, I suggest keeping it to 18-20 which appears ~8 pts when scaled down to an average sized figure. The angle of labels on the X-axis can be changed to avoid overlap. The default is 45 to avoid overlap of long labels, which can be 0 for short labels."
              )
            ),
            tags$li(
              tags$em(
                tags$strong("Log axes: "),
                "log10 or log2 transformations are possible for the Y-axis. X-axis can also be transformed if a numeric variable is used. Remember that transformation is also used in the ANOVA analyses. If you want the analyses without transformation, repeat after deselecting these choices."
              )
            ),
            tags$li(
              tags$em(
                tags$strong("Appearance of symbols/bars/boxes/violins: "),
                "Sizes of data points can be changed (range of 1-10 allowed). Opacity of all items can be modified as well (range 0-1, where 0 = fully transparent (invisible symbols or white bars/boxes/violins) and 1 = opaque."
              )
            ),
            tags$li(
              tags$em(
                tags$strong("Data scatter: "),
                "By default, the data are scattered to avoid overlap when data points are aligned. This can be changed with a number between 0-1 (0 = aligned data points with no scatter)."
              )
            )
          )
        ),
      )
    ),
    
    
    tags$div(
      tags$h5("ANOVAs (linear models) & comparisons"),
      tags$ul(
        tags$li(
          "The X-axis variable, and the Grouping variable if chosen, are used as predictor or independent variables in ANOVAs. For categorical X and Grouping variables, these are also called 'Fixed Factors'."
        ),
        tags$li(
          "The Y-axis variable (which should be a numeric variable) is the dependent or response variable."
        ),
        tags$li(
          "Only the variables selected on the Data page are used in ANOVA analyses. If levels of the X-axis variable are dropped from the graph, they will also be lost in ANOVAs."
        ),
        tags$li(
          "Optional settings need to be added/declined on the Graphs tab, and the 'Variables chosen' button pressed before proceeding to ANOVAs tab for analyses. This allows you to 'eye-ball' your data before the analyses. The Faceting and Shapes variable on graphs do not affect the analyses."
        ),
        tags$li(
          tags$strong(
            "If a log-transformation is selected, it will be used for graphs and in the analyses."
          )
        ),
        tags$li(
          "If you change variables for graphs or analyses, remember to click relevant action buttons. Graphs and analyses do not automatically update when variables are changed."
        ),
        tags$li(
          "ANOVAs can be simple or mixed effects linear models. The random variable can be selected on the ANOVA tab. This is typically a categorical variable that shows hierarchy within data, e.g., experimental blocks, subject matching, repeated measures over time."
        ),
        tags$li(
          "A QQ plot (quantile-quantile) plot of model residuals is useful to see whether there is significant deviation from approximately normal distribution. If residuals are highly skewed, the linear model may not be a good fit to the model and therefore unreliable. You could consider transforming the response variable, e.g., log, squares, square-roots."
        ),
        tags$li(
          "Options for post-hoc comparisons will appear on the ANOVA tab based on the type of analyses."
        ),
        tags$li(
          "Info buttons on the ANOVA tab provide more information on the ANOVA table, post-hoc comparisons, estimated marginal means (EMMEANS)."
        ),
        tags$li(
          "The Linear Model tab provides the summary of the linear model fitted to data in R."
        )
      )
    )
  ))
})

output$tab_header <- renderText({ #in use
  if (input$startBtn > 0) {
    HTML(paste0("<h4>", "Your data table", "</h3>"))
  }
})

output$qq_head <- renderText({ #in use
  if (input$startBtn > 0) {
    HTML(paste0("<h4>", "QQ plot of residuals from linear model.", "</h3>"))
  }
})


output$anova_head <- renderText({ #in use
  if (input$startBtn > 0) {
    HTML(
      paste0(
        "<h4>",
        "ANOVA table",
        "</h3>",
        "<h6>",
        "Type II SS. Kenward-Roger method for degrees of freedom.",
        "</h6>"
      )
    )
  }
  ifelse(input$logTrans %in% c("log10", "log2"),
         HTML(
           paste0(
             "<h4>",
             "ANOVA table",
             "</h3>",
             "<h6>",
             "Note: you used log-transformation.",
             "</h4>",
             "<h6>",
             "Type II SS. Kenward-Roger method for degrees of freedom.",
             "</h6>"
           )
         ),
         HTML(
           paste0(
             "<h4>",
             "ANOVA table",
             "</h3>",
             "<h6>",
             "Type II SS. Kenward-Roger method for degrees of freedom.",
             "</h6>"
           )
         ))
})

output$emmeans_head <- renderText({ #in use
  if (input$startBtn > 0) {
    HTML(
      paste0(
        "<h4>",
        "Estimated Marginal Means.",
        "</h3>",
        "<h6>",
        "Kenward-Roger method for degrees of freedom.",
        "</h6>"
      )
    )
  }
  ifelse(input$logTrans %in% c("log10", "log2"),
         HTML(
           paste0(
             "<h4>",
             "Estimated Marginal Means.",
             "</h3>",
             "<h6>",
             "Note: emmeans are back-transformed to the original scale.",
             "</h4>",
             "<h6>",
             "Kenward-Roger method for degrees of freedom. Confidence level used: 0.95 ",
             "</h6>"
           )
         ),
         HTML(
           paste0(
             "<h4>",
             "Estimated Marginal Means.",
             "</h3>",
             "<h6>",
             "Kenward-Roger method for degrees of freedom. Confidence level used: 0.95 ",
             "</h6>"
           )
         ))
})

output$contrasts_head <- renderText({ #in use
  if (input$startBtn > 0) {
    HTML(
      paste0(
        "<h4>",
        "Post-hoc comparisons",
        "</h3>",
        "<h6>",
        "P values are FDR adjusted. Kenward-Roger method for degrees of freedom.",
        "</h6>"
      )
    )
  }
  ifelse(input$logTrans %in% c("log10", "log2"),
         HTML(
           paste0(
             "<h4>",
             "Post-hoc comparisons",
             "</h3>",
             "<h6>",
             "Note: the ratio is on the original scale.",
             "</h4>",
             "<h6>",
             "P values are FDR adjusted. Kenward-Roger method for degrees of freedom.",
             "</h6>"
           )
         ),
         HTML(
           paste0(
             "<h4>",
             "Post-hoc comparisons",
             "</h3>",
             "<h6>",
             "P values are FDR adjusted. Kenward-Roger method for degrees of freedom.",
             "</h6>"
           )
         ))
})

output$faq <- renderText({ #NOT in use
  HTML(paste0(tags$div(
    tags$h2("To be written!"),
    tags$ul(
      tags$li(
        "You can use grafify online to plot graphs, perform ANOVA and post-hoc comparisons just like the R package."
      ),
      tags$li("Graphs and ANOVA results will appear in tabs below."),
      tags$li(
        "Start by uploading a csv or Excel file with long-format data (i.e., one variable in one column)"
      ),
      tags$li("Then press 'Start'."),
      tags$li("Further instructions on the Data and variables tab below."),
    )
  )))
})


output$Instr_Data <- renderText({ #in use
  HTML(paste0(tags$div(
    #tags$h5("Data table"),
    tags$ul(
      tags$li(
        "To get started, upload a csv or Excel file containing your table in long-format. This means each column should contain one categorical or one numeric variable that you want to plot/use in analyses."
      ),
      tags$li(tags$strong("Important: column names should not have space between words. Please use an `_` (underscore) instead. You may have to edit the name later when you prepare figures in Powerpoint or other software you use.")),
      tags$li("See image below of an example of", tags$strong("long"), " table that ", tags$strong("is the required"), "format for `grafify`."),
      tags$br(),
      layout_columns(col_widths = 12, card("", tagList(
        #h4("Types of graphs with grafify"),
        tags$img(
          src = "shiny7_long_table.png",
          align = "left",
          width = "50%"
        ),
        tags$h6(
          "The data table should be long-format with one variable (either categorical or numeric) per column. Note that columns with numbers are automatically assumed to be numeric. If you have a column to indicate Experiment 1, 2, 3, and so on, enter them as Exp_1, Exp_2 etc. so that R 'reads' them as categorical variables. The random variable for mixed effects analyses should be a categorical variable. It is easy to add another variable as another column."
        )))),
      tags$br(),
      tags$li("See image below of an example of", tags$strong("wide"), " table format that", tags$strong("is not compatible"), "with `grafify`."),
      tags$br(),
      layout_columns(col_widths = 12, card("", tagList(
        tags$img(
          src = "shiny8_wide_table.png",
          align = "left",
          width = "60%"
        ),
        tags$h6(
          "In this case, data spread across multiple columns, and the hierarchy in data is apparent only after scanning across columns. Adding another variable makes the table even more 'wider'."
        )
      ))),
      tags$br(),
      tags$li(
        "Then press", tags$strong("Start"), ". If all goes OK, your table should appear below. You should also see dropdown menus that let you select variables from your table."
      ),
      tags$li(
        "If you would like to use example data, click", tags$strong("Start"), " directly and proceed to choosing variables."
      ),
      tags$li(
        "If you upload an Excel file, a dropdown list of sheets within the file will appear. Pick a sheet and press 'Start'."
      ),
      tags$li(
        "Pick variables accordingly to plot on X-axis ", tags$strong("Box 1"), ", Y-axis ", tags$strong("Box 2"), " and a grouping variable, if required in ", tags$strong("Boxes 3 and 3.1")
      ),
      tags$br()
    )
  )))
})

output$Instr_Graphs <- renderText({ #in use
  HTML(paste0(
    tags$div(tags$h5("Types of Graphs"), 
             mainPanel2.1), #from src01g_Help_n_Images
    
    tags$div(
      tags$h5("Requirement for plotting data:"),
      tags$ul(
        tags$li(
          "At least 2 variables must be chosen: one each for the X and Y axes of Graphs"
        ),
        tags$li(
          "A third Grouping variable (e.g., for 2-way ANOVA designs) is optional when the X-axis variable is categorical."
        ),
        tags$li(
          "If the X and Y axis variables can both also be numeric. In this case, the Grouping variable is mandatory, and this variable can be categorical or numeric."
        ),
        tags$li(
          tags$strong(
            "Note: all variables chosen in Boxes 1-3, and log-transformation used from Box 8, will be used in data analyses. If any levels within categorical variables are dropped from graphs in Boxes 7.1 and 7.2, they will not be used in analyses."
          )
        ),
        tags$li(
          "The types of Graphs available will depend on type of data. Their appearance can be changed with options in ",
          tags$strong("Box 8"),
          " (see below)."
        )
      )
    ),
    
    tags$div(tags$h6("Types of Graphs"), 
             mainPanel2.2), #from src01g_Help_n_Images
    
    tags$div(
      tags$ul(
        tags$li(
          "After choosing variables on the Data tab, go to the Graphs tab for additional options. You can choose additional variables from data to generate faceted plots (Faceting variable) or map the shapes of data symbols or match data for before-after plots (Shapes or Matching) variable."
        ),
        tags$li(
          "Remember to press 'Variables chosen' after choosing these variables."
        ),
        tags$li(
          "If all variables are correctly chosen (or 'No' is chosen) in ",
          tags$strong("Boxes 1-6"),
          "a dropdown menu of Graph types should appear. Pick one, and then click 'grafify my data' to see your Graph."
        ),
        tags$li(
          "Graphs do not automatically update if you change appearance or variables - please click 'grafify my data' to update the graph."
        ),
        tags$li(
          "R and ggplot2 use alphabetically ordering based on group (levels) within categorical variables (e.g., if groups are labelled WT and KO, the KO group will appear before WT along the X-axis)."
        ),
        tags$li(
          tags$strong(
            " You can reorder groups or plot only those you are interested in by using options in ",
            tags$strong("Boxes 7.1 and 7.2"),
            ". When reordering groups, if you drop some levels within the factor (i.e., there were 10 groups in your data, but you plot only 3), those levels will not be used in the ANOVA analyses (i.e., only those 3 groups will be analysed in the ANOVA). Check that the Graph contains all the data you want to analyse."
          )
        ),
        tags$li(
          "Appearance of graphs can be changed using options in ",
          tags$strong("Box 8"),
          ". See below for options available."
        ),
        tags$li(
          "If you change graph appearance in ",
          tags$strong("Box 8"),
          "remember to click 'grafify my data' again."
        ),
        tags$li(
          "The graph is available to download as a PDF file. You will need to change the height and width to fit the graph within the PDF document."
        ),
        tags$li(
          "Typically, at fontsize of 18 pts (default) with a angled text on X-axis, start with a height of 12 cm and increase width based on number of groups (e.g., 20cm for ~10 groups plus legend). Avoid reducing the fontsize on the graph, instead increase the dimensions of the PDF.Even if the graph looks 'squished' on the screen, it will look good when the dimension of the PDF file are large enough to accommodate it.  Increase the height if you also used a faceting variable, angled labels on the X-axis or have long list of legends. You do not need to press 'grafify my data' when you change dimensions, click 'Save as PDF' directly."
        ),
        tags$li(
          "Changing height or width does not change appearance of the graph online."
        )
      )
    ),
    
    tags$div(
      tags$h5("Descriptions of graphs types:"),
      #mainPanel2.1,
      tags$ul(
        tags$li(
          tags$strong("Box & whiskers: "),
          tags$em(
            "All data points are shown as scattered circles with boxes depicting the interquartile range (IQR; 25th - 75th percentiles), and whiskers depicting 1.6xIQR, and a line representing the median (50th percentile). The colours of boxes and circles will match, and can be changed. If a Shapes variable is slected, the shape of data points will change (shown in black). Up to 25 different shapes are allowed."
          )
        ),
        tags$li(
          tags$strong("Violin plus box & whiskers: "),
          tags$em(
            "Everything as with box & whiskers, plus violins which show overall distribution of data. By default, the colours of symbols and vioins will match and boxes will look white (box opacity = 0), which can be changed."
          )
        ),
        tags$li(
          tags$strong("Bar graph: "),
          tags$em(
            "Bars show the mean of data and circles represent all data points. The default error bars will denote SD (standard deviation), which can be changed to SEM (standard error of the mean) or CI95 (95% confidence interval calculated from the t-distribution)."
          )
        ),
        tags$li(
          tags$strong("Point & errorbars: "),
          tags$em(
            "A square represents the mean of the data and circles show all data points. The default error bars will denote SD (standard deviation), which can be changed to SEM (standard error of the mean) or CI95 (95% confidence interval calculated from the t-distribution)."
          )
        ),
        tags$li(
          tags$strong("Density or Histogram plot : "),
          tags$em(
            "A smooth density plot of data (Y-axis variable) grouped by the X-axis (categorical variable only) or histogram plot. If Histogram plot is chosen, the binsize can be changed."
          )
        ),
        tags$li(
          tags$strong("Before-after plot: "),
          tags$em(
            "This is a box and whiskers plot, but an additional Shapes variable is needed that indicates matching or pairing within the data. Data points will be joined by grey lines to show change 'before and after treatment'."
          )
        ),
        tags$li(
          tags$strong("Numeric XY 1 and 2: "),
          tags$em(
            "These are used when the X-axis is also numeric (e.g., time, concentrations). A Grouping variable is also required, which can be categorical or numeric."
          )
        )
      )
    ),
    
    tags$div(
      tags$h5("Changing graph appearance: "),
      tags$ul(
        tags$li("Options for changing graph appearance are available in ", tags$strong("Box 8"), ". Depending on graph type, options for error bars and binsizes appear close to ", tags$strong("Box 6"),"."),
        tags$li(
          tags$em(
            tags$strong("Colours: "),
            "12 colourblind-friendly colour schemes are available for categorical X-axis variables, and 5 for numeric X-axis variables. Colours are plotting sequentially from grafify colourblind-friendly palettes and mapped to groups (levels) along the X-axis variable - the colours will map to boxes/bars/vioins/points and data symbols. Sometimes, when there are lots of groups along the X-axis, too many colours can be distracting and unnecessary (because group labels are available). In this case, a single colour can be used for all groups."
          )
        ),
        
        tags$li(mainPanel2.3), #from src01g_Help_n_Images
        
        tags$li(
          tags$em(
            tags$strong("Fonts and labels: "),
            "The default fontsize is 18 points, which can be changed as required. For publications/reports, I suggest keeping it to 18-20 which appears ~8 pts when scaled down to an average sized figure. The angle of labels on the X-axis can be changed to avoid overlap. The default is 45 to avoid overlap of long labels, which can be 0 for short labels."
          )
        ),
        tags$li(
          tags$em(
            tags$strong("Log axes: "),
            "log10 or log2 transformations are possible for the Y-axis. X-axis can also be transformed if a numeric variable is used. Remember that transformation is also used in the ANOVA analyses. If you want the analyses without transformation, repeat after deselecting these choices."
          )
        ),
        tags$li(
          tags$em(
            tags$strong("Appearance of symbols/bars/boxes/violins: "),
            "Sizes of data points can be changed (range of 1-10 allowed). Opacity of all items can be modified as well (range 0-1, where 0 = fully transparent (invisible symbols or white bars/boxes/violins) and 1 = opaque. For 'Bar graphs' and 'Point and errorbar', the type of error bar (SD, SEM or CI95) and width of error bars (0-1) can be adjusted."
          )
        ),
        tags$li(
          tags$em(
            tags$strong("Binsizes: "),
            "For Histogram plots, binsizes can be changed from the default of 30."
          )
        ),
        tags$li(
          tags$em(
            tags$strong("Data scatter: "),
            "By default, the data are scattered to avoid overlap when data points are aligned. This can be changed with a number between 0-1 (0 = aligned data points with no scatter)."
          )
        )
      )
    )
  ))
})


output$Instr_ANOVA <- renderText({ #in use
  HTML(paste0(tags$div(
    #tags$h5("ANOVAs (linear models) & comparisons"),
    tags$ul(
      tags$li(
        "The X-axis variable and the Grouping variable, if chosen, are used as predictor or independent variables in ANOVAs. Variables chosen in ", tags$strong("Boxes 1 and 3.1"), " for the X-axis and the optional Grouping variables are also called ", tags$strong("Fixed factors.")),
        tags$li("In the example dataset below, ", tags$strong("Genotype"), "and ", tags$strong("Treatment"), "are ", tags$strong("Fixed factors"), " and could be chosen to be plotted along the X-axis in ", tags$strong("Box 1"), "."
      ),
      tags$li(
        "The Y-axis variable (which should be a numeric variable) is the dependent or response variable. In the example below, 'Cell_viability_percent' should be chosen as the Y-axis variable in ", tags$strong("Box 2"), "."
      ),
      tags$br(),
      layout_columns(col_widths = 12, card("", tagList(
        #h4("Types of graphs with grafify"),
        tags$img(
          src = "shiny7_long_table.png",
          align = "left",
          width = "50%"
        ),
        tags$ul(
          tags$li("In this example data set, one ", tags$strong("Fixed factor"), " is ", tags$strong("Treatment"), ", which has ", tags$strong("two groups or levels"), "'Untreated' and 'Drug'."),
          tags$li("There is also a second ", tags$strong("Fixed factor"), ", which is", tags$strong("Genotype"), " with ", tags$strong("two levels"), ", i.e., WT & KO."),
          tags$li("These experiments were performed as randomised blocks, wherein 'Untreated' and 'Drug' samples were compared side-by-side for WT and KO cells. This experimental set up is indicated by the variable", tags$strong("Experiment_num"), "which in this case and similar experimental designs is the ", tags$strong("Random factor (Box 9.1)", "for linear mixed effects linear analysis.")),
          tags$li("Sometimes, to increase the precision of the measurement of 'Cell_viability_percent' within each independent experimental repeat, we may use one or more technical replicates. This is indicated in the variable ", tags$strong("Tech_Rep_num"), ", e.g., Expt1 has data for 'Cell_viability_percent' from TechRep1 and TechRep2.", tags$strong("As these kinds of technical replicates are not statistically independent,  these should be averaged before proceeding to avoid pseudoreplication.")),
          tags$li(
            "`Technical_rep_num` should be averaged per experiment, otherwise this could lead to ", tags$strong("pseudoreplication"), " in the data. "
          ),
          tags$br(),
          layout_columns(col_widths = 12, card("", tagList(
            #h4("Types of graphs with grafify"),
            tags$img(
              src = "shiny7_long_table_techs.png",
              align = "left",
              width = "70%"
            )))),
            tags$li("For simplicity, `grafify` will ", tags$strong("average replicates within levels of the Random factor chosen in Box 9.1"), "grouped by Fixed factors selected in Boxes 1-3 before fitting a random intercepts mixed effects model. This is done to avoid pseudoreplication, which poses a higher risk than loosing information on variability of technical replicates within experiments."),
            tags$li("To ensure this is performed correctly, in this example and similar experimental data prepared as the table above, the ", tags$strong("Random factor chosen in Box 9.1"), "should be ", tags$strong("Experiment_num"), ". Choosing 'Technical_rep_num' as Random variable will ", tags$strong("give the wrong result"), "."),
            tags$li("Ideally, you should average technical replicates and only use means from independent experiments in the data set uploaded to `grafify`."),
            tags$li("Alternatively, more advanced mixed effects models can be used in R, which are not available in `grafify` online.")
        )
        ))),
      tags$li(
        "Only the variables selected on the 'Data & Variables' tab are used in ANOVA analyses, i.e, ", tags$strong("only variables in Box 1-3 are used in the linear model analyses."), "Variables chosen for Faceting or Shapes, i.e., Boxes 4-5 are ignored."), 
        tags$li(tags$strong("Important: If levels of the X-axis variable are dropped from the graph, they will also be lost in ANOVAs."), "The data used in the analyses is seen at the bottom of the 'ANOVAs (linear models) and Comparisons' tab."
      ),
      tags$li(
        "Optional settings need to be added/declined on the Graphs tab, and the ", tags$strong("'Variables chosen'"), " button pressed before proceeding to ANOVAs tab for analyses. This allows you to 'eye-ball' your data before the analyses. "
      ),
      tags$li(
        tags$strong(
          "If a log-transformation is selected in Box 8, it will be used for graphs and in the analyses."
        )
      ),
      tags$li(
        "If you change variables for graphs or analyses, i.e., if you change variables chosen in Boxes 1-3 and 7, remember to click ", tags$strong("'Variables chosen'"), "afterwards. Graphs and analyses do not automatically update when these options are changed."
      ),
      tags$li(
        "Analyses can be Simple or Mixed effects linear models. The random variable can be selected on the ANOVA tab in ", tags$strong("Box 9.1"), ". Only one Random variable can be chosen in `grafify` online. This is typically a categorical variable that shows hierarchy within data, e.g., experimental blocks, subject matching, repeated-measures over time. It is best to ensure that only statistically independent data points are included in the table. ", tags$strong("Using linear mixed effects analysis  does not automatically fix issues such as pseudoreplication. It is up to the user to ensure that data are correctly entered.")
      ),
      tags$li(
        "A QQ plot (quantile-quantile) plot of model residuals is useful to see whether there is significant deviation from approximately normal distribution. If residuals are highly skewed, the linear model may not be a good fit to the model and therefore unreliable. You could consider transforming the response variable, e.g., log, squares, square-roots."
      ),
      tags$li("For Mixed effects analysis, a graph of the same type chosen in Box 6 will be shown, but this time faceted by the Random factor. This allows you to see the effect of experimental blocks. A second graph will show data averaged over levels of the Random factor. These data appear at the bottom of the page. "),
      tags$li(
        "Options for post-hoc comparisons will appear on the ANOVA tab based on the type of analyses."
      ),
      tags$li(
        "Info buttons on the ANOVA tab provide more information on the ANOVA table, post-hoc comparisons, estimated marginal means (EMMEANS)."
      ),
      tags$li(
        "The Linear Model tab provides the summary of the linear model fitted to data in R."
      ),
      tags$br()
    )
  )))
})
