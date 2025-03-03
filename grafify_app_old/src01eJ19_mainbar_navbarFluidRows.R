#main panel of outputs and inputs.
source("src01e_menu_links.R", local = TRUE)
mainPanel1 <- list(#width = 6,
  #tags$br(),
  #tags$br(),
  tabsetPanel(
    id = "1",
    tags$br(),
    tabPanel(
      tags$h5("Data & Variables"),
      width = 12,
      value = 1,
      mainPanel(
        width = 12,
        fluidRow(column(
          12, accordion(
            open = TRUE,
            #starting help on data
            accordion_panel(
              "Readme: Getting started (can be minimised)",
              htmlOutput("dataHelpOpen")
            )
          )
        )),
        tags$br(),
        card(
          card_header(tags$h5("Data & variables")),
          value = 1,
          #tab panel for data & variables value 1
          fluidRow(column(3, 
            fluidRow(column(12, 
                        htmlOutput("varsel1")),
                            #X-var varsOne
                 column(12, 
                        htmlOutput("varsel2")),
                            #Y-var varsTwo
                 column(12,
                              #conditional panel to show after start button
                  conditionalPanel(
                                condition = "output.started == 'Now pick variables.'",
                                htmlOutput("addVarsOut"),
                                selectizeInput(
                                  "addVarsOpt",
                                  #Add grouping var Yes/No button
                                  label =
                                    list(
                                      tooltip(
                                        trigger = list(
                                          tags$h3("3"),
                                          tags$strong("Add a Grouping variable (Yes/No)?"),
                                          bs_icon("info-circle")
                                        ),
                                        "An optional grouping variable for 2way ANOVA designs. A Grouping variable (either categoric or numeric) is required when both X and Y variables are numeric."
                                      )
                                    ),
                                  choices = c("Choose Yes/No" =
                                                "", "Yes", "No"),
                                  options = list(dropdownParent = 'body')
                                )
                              )
                            ),
                            column(12, uiOutput("addMoreVars")))), #Grouping var varsFour
                 column(8, 
                        htmlOutput("tab_header"), #Table title
                   DT::dataTableOutput("mytable2")
                 ))
    )
    )),
    tabPanel(
      tags$h5("Graphs"),
      width = 12,
      value = 2,
      #tags$br(),
      fluidRow(
        column(12, accordion(
          open = TRUE,
          accordion_panel(
            "Readme: Graphs (can be minimised)",
            #Graphs tab title
            htmlOutput("graphsHelpOpen")
          )
        )),
        tags$br(),
          column(12, #full card for side column 8
               card(
                 card_header(
                   tags$h5("Select graph options"),
                   class = "d-flex justify-content-between",
                   tooltip(
                     bs_icon("info-circle"),
                     "Please select Yes (and then a variable) or No and then press 'Variables chosen'."
                   )
                 ),
                 fluidRow(
                   column(
                     3,
                     selectizeInput(
                       "facetingOpt",
                       #Add faceting variable yes/no
                       #width = "250px",
                       label = tooltip(
                         trigger = list(
                           tags$h3("4"),
                           tags$strong("Add a faceting variable to graph (Yes/No)?"),
                           bs_icon("info-circle")
                         ),
                         "A categorical variable to facet plots by, or choose No before proceeding."
                       ),
                       choices = c("Choose Yes/No" = "", c("Yes", "No")),
                       options = list(dropdownParent = 'body')
                     )
                   ),
                   column(
                     3,
                     uiOutput("addFacets"),
                     #Add faceting variable varsFive
                     #textOutput("FacVars")
                     ),
                     column(
                       3,
                       selectizeInput(
                         "ShapesOpt",
                         #Add shapes yes/no
                         #width = "250px",
                         label = tooltip(
                           trigger = list(
                             tags$h3("5"),
                             tags$strong("Add a variable for matched symbols/shapes?"),
                             bs_icon("info-circle")
                           ),
                           "A categorical variable to map to shape of symbols or a matching variable for Before-after plots, or choose No before proceeding."
                         ),
                         choices = c("Choose Yes/No" = "", c("Yes", "No")),
                         options = list(dropdownParent = 'body')
                       )
                     ),
                     column(3, 
                            uiOutput("addShapes") #Shapes variable varsThree
                            ),
                   column(4,
                     actionBttn(
                       "varsDone",
                       #Done with variables
                       label = tooltip(
                         trigger = list(tags$strong("Variables Chosen"), bs_icon("check2-circle")),
                         "The options available will depend on the type and number of variables you chose before clicking Done. You can choose different variables again and click 'Variables chosen' to update graph options."
                       ),
                       size = "sm",
                       color = "royal",
                       style = "unite")
                     ))),
                   column(12,
                          card(card_header(tags$h5("Graph choice & X-axis options")),
                               fluidRow(column(3, 
                     selectizeInput(
                       "graphType",
                       #GraphType dropdown list
                       #width = "150px",
                       label = tooltip(
                         trigger = list(
                           tags$h3("6"),
                           tags$strong("Choose a graph type."),
                           bs_icon("info-circle")
                         ),
                         "After clicking 'Variables Chosen', choose a graph type and then click 'grafify my data' to visualise the graph."
                       ),
                       choices = c(
                         "Boxplot",
                         "Bar graph",
                         "Violin plot",
                         "Point & Errorbar",
                         "Before-after plot",
                         "Density plot",
                         "Histogram plot",
                         "Numeric XY 1",
                         "Numeric XY 2"
                       ),
                       selected = "Boxplot",
                       options = list(dropdownParent = 'body')
                     )
                   ),
                   #conditional panel if XY is numeric & boxplot wanted?
                   column(3, 
                          card("",
                          textOutput("ShapeLevs"),
                          textOutput("XforRelevel"))),
                   column(3,
                     conditionalPanel(
                       "input.graphType == 'Numeric XY 1' || input.graphType == 'Numeric XY 2'",
                       selectizeInput(
                         "XYBox",
                         #show numeric boxplot yes/no
                         label = tooltip(
                           trigger = list(
                             tags$h3("7"),
                             tags$strong("Show box and whiskers (Yes/No)?"),
                             bs_icon("info-circle")
                           ),
                           "Click 'Yes' and press 'grafify my data' if you would like box and whiskers plot grouped along X-axis."
                         ),
                         choices = c("Yes", "No"),
                         selected = "No",
                         multiple = FALSE,
                         options = list(dropdownParent = 'body'),
                       )
                   ),
                   #Option to reorder X-axis groups yes/no
                   conditionalPanel(
                       "output.XforRelevel == 'The X-axis variable is categorical. You can reorder groups.'",
                       selectizeInput(
                         "DoRelevel",
                         #width = "150px",
                         label = tooltip(
                           trigger = list(
                             tags$h3("7"),
                             tags$strong("Reorder X-axis groups."),
                             bs_icon("info-circle")
                           ),
                           "Click Yes if you would like to reorder the groups along the X-axis. NOTE: if you drop groups from graphs, they will not be used in the analyses."
                         ),
                         choices = c("Choose one" =
                                       "", "Yes", "No"),
                         options = list(dropdownParent = 'body'),
                         selected = "No"
                       )
                   )),
                   #conditional panel if reordering chosen
                   column(3,
                     conditionalPanel(
                       "input.DoRelevel == 'Yes'",
                       htmlOutput("selVarsReLevel"),
                       #reorder X-axis groups
                       textOutput("newRelevel")
                     )      #Text of new order of X-axis
                   ),
                   column(3,
                          conditionalPanel(
                            "input.graphType == 'Histogram plot'",
                            uiOutput("Binsize")
                          ),
                          conditionalPanel(
                            "input.graphType == 'Bar graph' ||
                     input.graphType == 'Point & Errorbar'",
                            #card(card_header("Error bar type"),
                            uiOutput("error_type"), #errorbar type
                            uiOutput("ewid")
                            )
                          ),
                   )
               ))),
        #Graphs tab help
        #textOutput("OutCatGp"),
        fluidRow(column(
          4, card(max_height = "800px", HTML(paste(
            tags$h3("8"),
            tags$h5(
              "Graph Options",
              #sidebar title
              class = "d-flex justify-content-between",
              tooltip(
                bs_icon("info-circle"),
                "Select options here and click 'grafify my data' to update the graph."
              )
            )
          )), fluidRow(
            column(
              12,
              accordion(
                open = TRUE,
                #Colour options accordion, open default
                accordion_panel(
                  "Colour Options",
                  #graph_colours
                  selectizeInput(
                    "colpal",
                    #colour palette choices for categorical X
                    label = tooltip(
                      trigger = list(tags$strong("Colour Scheme"), bs_icon("info-circle")),
                      "Pick one of several colourblind-friendly grafify palettes for categorical or numeric variables."
                    ),
                    choices = c(
                      "okabe_ito",
                      "bright",
                      "contrast",
                      "dark",
                      "fishy",
                      "kelly",
                      "light",
                      "muted",
                      "pale",
                      "r4",
                      "safe",
                      "vibrant",
                      "blue_conti",
                      "yellow_conti",
                      "grey_conti",
                      "PrGn_div",
                      "OrBl_div"
                    ),
                    selected = "okabe_ito",
                    options = list(dropdownParent = 'body'),
                    multiple = FALSE
                  ),
                  uiOutput("colSeqOut"),
                  #ColSeq option
                  uiOutput("colRevOut"),
                  #ColRev option
                  conditionalPanel(
                    #Conditional panel for SingleColour plots
                    "input.graphType == 'Boxplot' ||
         input.graphType == 'Bar graph' ||
         input.graphType == 'Point & Errorbar' ||
         input.graphType == 'Violin plot' ||
         input.graphType == 'Before-after plot' ||
         input.graphType == 'Density plot' ||
         input.graphType == 'Histogram plot'",
                    selectizeInput(
                      inputId = "colPick",
                      #Single colour yes/no
                      label =  tooltip(
                        trigger = list(
                          tags$strong("Graph with a single colour (Yes/No)?"),
                          bs_icon("info-circle")
                        ),
                        "If you have lots of groups along the X-axis, you may want to consider a single colour for all groups instead of multiple colours. Choose Yes and a hexcode from the colour picker below for 'single colour' graphs."
                      ),
                      choices = c("Choose one" = "", c("Yes", "No")),
                      selected = "No",
                      options = list(dropdownParent = 'body', container = "body"),
                      multiple = FALSE
                    ),
                    colourInput(
                      "colPick2",
                      #single colour picker
                      label = tooltip(
                        trigger = list(tags$strong("Single colour picker"), bs_icon("info-circle")),
                        "Pick a hexcode for single colour graphs."
                      ),
                      showColour = "both",
                      closeOnClick = TRUE,
                      value =  "#E69F00"
                    )
                  )
                )
              ),
              accordion(
                open = TRUE,
                accordion_panel(
                  "Axes options",
                  #Log Y axis option
                  #graph_log Y
                  pickerInput(
                    "logTrans",
                    label = tooltip(
                      trigger = list(tags$strong("Log(Y) axis"), bs_icon("info-circle")),
                      "Use a log-scale for the Y-axis. Log-transformed data will be used in analyses."
                    ),
                    choices = c("", "log10", "log2"),
                    selected = "",
                    #selectize = TRUE,
                    options = list(dropdownParent = 'body'),
                    multiple = FALSE
                  ),
                  #uiOutput("logTransX"),
                  pickerInput(
                    "logTransX",
                    #Log X axis option
                    label = tooltip(
                      trigger = list(tags$strong("Log(X) axis"), bs_icon("info-circle")),
                      "If X-axis variable is also numeric, it can be converted to a log10 or log2 scale. Log-transformed data will be used in analyses."
                    ),
                    choices = c("", "log10", "log2"),
                    selected = "",
                    multiple = FALSE
                  )
                )
              )
            ), column(12, accordion(
              open = TRUE,
              accordion_panel(
                "Symbols & Labels options",
                #Symbol options
                #card(card_header("Symbols & Labels options"),
                #graph_font
                numericInput(
                  "font_size",
                  #fontsize option
                  label = tooltip(
                    trigger = list(tags$strong("Font size"), bs_icon("info-circle")),
                    "Change text fontsize on graphs."
                  ),
                  min = 1,
                  step = 1,
                  max = 30,
                  value =  18
                ),
                #graph_angle
                numericInput(
                  "text_angle",
                  #textsize option
                  label = tooltip(
                    trigger = list(tags$strong("Angled X-axis labels"), bs_icon("info-circle")),
                    "Change angle of labels to avoid overlapping text."
                  ),
                  min = 0,
                  max = 90,
                  value = 45
                ),
                #graph_symsize
                numericInput(
                  "sym_size",
                  #symbol size option
                  label = tooltip(
                    trigger = list(tags$strong("Symbol size"), bs_icon("info-circle")),
                    "Pick a value between 1-10 for sizes of data symbols on graphs."
                  ),
                  min = 1,
                  max = 10, step = 1,
                  value = 3
                ),
                #sym_jitter
                numericInput(
                  "sym_jitter",
                  #symbol scatter option
                  label = tooltip(
                    trigger = list(tags$strong("Symbol scatter (0-1)"), bs_icon("info-circle")),
                    "Use 0 to align data symbols with no scatter."
                  ),
                  value = .1,
                  step = 0.1,
                  min = 0,
                  max = 1
                ),
                numericInput(
                  "sym_alpha",
                  #symbol transparency option
                  label = tooltip(
                    trigger = list(tags$strong("Symbol opacity (0-1)"), bs_icon("info-circle")),
                    "Reduce the value below 1 to control transparency of symbols when data points overlap."
                  ),
                  value = .8,
                  step = 0.1,
                  min = 0,
                  max = 1
                ),
                uiOutput("pointAllalpha"),
                #all points transparency option
                uiOutput("pointAllsize"),
                uiOutput("pointAllshape"),
                conditionalPanel(
                  "input.graphType == 'Bar graph' || input.graphType == 'Violin plot' || input.graphType == 'Boxplot'",
                  accordion(
                    open = TRUE,
                    accordion_panel(
                      "Transparency options",
                      #box_alpha
                      numericInput(
                        "box_alpha",
                        #box transparency option
                        label = tooltip(
                          trigger = list(tags$strong("Box or Bar opacity (0-1)"), bs_icon("info-circle")),
                          "Reduce to below 1 to make boxes or bars transparent. When set to 0, boxes and bars will appear white in colour."
                        ),
                        value = 1,
                        min = 0,
                        max = 1
                      ),
                      uiOutput("vio_alpha")
                    )
                  )
                ),
                #conditionalPanel(
                #  "input.graphType == 'Bar graph' ||
                #     input.graphType == 'Point & Errorbar'",
                #  #card(card_header("Error bar type"),
                #  accordion(
                #    open = TRUE,
                #    accordion_panel("Error Bar options", uiOutput("error_type"), #errorbar type#
                #                    uiOutput("ewid"))
                #  )
                #)
                )
            ))
          ))
        ), column(
          8,
          card(
            min_height = "800px",
            card_header(
              "",
              class = "d-flex justify-content-between",
              actionBttn(
                inputId = "makegraph",
                label = "grafify my data",
                width = "10px",
                size = "sm",
                color = "royal",
                style = "unite",
                #block = TRUE,
                icon = bs_icon("power")
              ),
              tooltip(
                bs_icon("info-circle"),
                "Height and width options below only affect the PDF for download. They do not change appearance of graph here."
              )
            ),
            plotOutput("plotChosenGraph", #graph output
                       #width = "60vw",
                       height = "60vh"),
            #textOutput("FacVars"),
            #plotOutput("plotgraph2"),
            #tags$br(),
            fluidRow(
              column(3, #PDF height
                     numericInput(
                       "g_ht",
                       label = tooltip(trigger = list(
                         tags$strong("Graph Height (cm)"), bs_icon("info-circle")
                       ), "Adjust height of PDF file."),
                       value = 12
                     )),
              column(3, numericInput(
                "g_wid",
                #PDF width
                label = tooltip(trigger = list(
                  tags$strong("Graph Width (cm)"), bs_icon("info-circle")
                ), "Adjust width of PDF file."),
                value = 15
              )),
              #tags$br(),
              #column so that next button is not on the same line and less wide
              column(10),
              column(
                3,
                downloadBttn(
                  "SaveGraph",
                  size = "sm",
                  #download button
                  label = "Save as PDF",
                  color = "royal",
                  style = "unite",
                  block = TRUE,
                  icon = bs_icon("download")
                )
              ),
              column(6, 
                     actionButton(
                       "PDFSizeHelper",
                       #Posthoc comparisons help button
                       label = "More Info on sizing PDFs for download",
                       icon = icon("info-circle"),
                       class = "btn-info",
                       style = "unite"
                     ),)
            )
          )
        ))
      )
    ),
    tabPanel(
      tags$h5("ANOVAs (linear models) and Comparisons"),
      width = 12,
      value = 3,
      mainPanel(
        width = 12,
        #main graph card
        fluidRow(column(
          12,
          accordion(
            open = TRUE,
            accordion_panel(
              "Readme: ANOVAs and Comparisons (can be minimised)",
              htmlOutput("ANOVAsHelpOpen"),
              tags$br()
            )
          )),
          tags$br(),
          column(10,
          card(
            #title = tags$h5("ANOVA & Comparisons"),
            #ANOVA help
            #textOutput("depReactive"),
            #textOutput("emmPairFML"),
            fluidRow(
              column(
                3,
                #card(card_header(tags$h6("Type of linear model")),
                selectizeInput(
                  "MorS",
                  multiple = FALSE,
                  #Type of ANOVA simple/mixed
                  label = tooltip(
                    trigger = list(
                      tags$h3("9"),
                      tags$strong("Choose simple or mixed effects model."),
                      bs_icon("info-circle")
                    ),
                    "A simple linear model is an ordinary 1-way or 2-way ANOVA. For a mixed effects linear models, in addition to fixed factors",
                    tags$strong("Boxes 1-3"),
                    "a random factor is required (e.g., experimental blocks, matching or repeated-measures)."
                  ),
                  choices = c("Choose one" =
                                "", c("Simple", "Mixed")),
                  options = list(dropdownParent = 'body')
                )
              ),
              column(4, htmlOutput("varsel6")),
              #Random factor choice varsSix
              #empty columns for formatting
              column(12),
              column(
                3,
                #card(card_header(tags$h6("Click to anlayse data")),
                actionBttn(
                  inputId = "analyseData",
                  #ANOVA action button
                  label = tags$strong("Analyse my data"),
                  size = "sm",
                  color = "royal",
                  style = "unite",
                  icon = bs_icon("power")
                )
              )
            )
          ))
        ),
        fluidRow(
          column(10, card(
            card_header(
              "Residuals plots",
              #Residuals plots card
              tooltip(
                bs_icon("info-circle"),
                "These are plots of residuals of your linear model. If residuals are very skewed away from the line in QQ plot or far from 0 in the Density plot, it is likely that there is a major deviation from normal distribution. Therefore, the linear model may be a poor fit for your data. Proceed with caution and consider using data-transformations (e.g., log-data)."
              )
            ),
            #htmlOutput("qq_head"),
            fluidRow(
              column(5, plotOutput(
                "ModPlot", #QQ plot of residuals
                width = "25vw", height = "25vw"
              )),
              column(5, plotOutput(
                "ModPlotDist", #Density plot of residuals
                width = "25vw", height = "25vw"
              )),
              column(
                10,
                #conditional panel for faceted plot
                conditionalPanel(condition = "input.MorS == 'Mixed'", 
                                 plotOutput("RandFplot",
                                            height = "40vw"))
              )
            )
          ))          #Plot faceted by random factor
          ,
          column(
            10,
            #card(card_header("",
            actionButton(
              "AnTbleHelpBtn",
              #ANOVA table help button
              label = "More Info on ANOVA Table",
              icon = icon("info-circle"),
              class = "btn-info",
              style = "unite"
            ),
            tags$br(),
            htmlOutput("anova_head"),
            #ANOVA table header
            DT::dataTableOutput("AnovaTab1"),
            #ANOVA table
            tags$br(),
            tags$br(),
            tags$br()
          ),
          column(
            10,
            #card(card_header("",
            actionButton(
              "EmmeansHelpBtn",
              #EMMEANS table help button
              label = "More Info on EMMEANS table",
              icon = icon("info-circle"),
              class = "btn-info",
              style = "unite"
            ),
            tags$br(),
            htmlOutput("emmeans_head"),
            #EMMEANS table header
            DT::dataTableOutput("Comp1"),
            #EMMEANS table
            tags$br(),
            tags$br(),
            tags$br()
          ),
          column(
            6,
            #card(
            actionButton(
              "ContrHelper",
              #Posthoc comparisons help button
              label = "More Info on Comparisons",
              icon = icon("info-circle"),
              class = "btn-info",
              style = "unite"
            ),
            selectizeInput(
              "emm_type",
              #choose contrast type
              label = list(tags$h3("10"), "Type of comparisons"),
              choices = c("Pairwise", "Compare to reference", "Levelwise 1", "Levelwise 2"),
              selected = c("Pairwise"),
              options = list(dropdownParent = 'body'),
              multiple = FALSE
            ),
            uiOutput("emmRefType"),
            #Post-hoc comparison output
            tags$br()
          ),
          column(
            10,
            #card(card_header("",
            actionButton(
              "ConstrasHelpBtn",
              #Contrasts help button
              label = "More Info on Post-hoc comparisons",
              icon = icon("info-circle"),
              class = "btn-info",
              style = "unite"
            ),
            tags$br(),
            htmlOutput("contrasts_head"),
            #Posthoc comparisons help button
            tags$br(),
            DT::dataTableOutput("Comp2"),
            #Posthoc comparisons
            tags$br()),
          column(10, 
            tags$br(),
            card(card_header(tags$h5(
              "Linear Model",
              #sidebar title
              class = "d-flex justify-content-between",
              tooltip(
                bs_icon("info-circle"),
                "Raw output from R of the linear model fitted to data."
              )
              )),
              #Linear model panel
              tags$h6(
                "This is the summary of the fitted model. It shows model parameters, such as residual SE, coefficients etc. For simple or ordinar linear models, grafify uses the lm() function from base R. For mixed effects models, grafify uses the lmer() function from the lme4 and lmerTest packages. "
              ),
             verbatimTextOutput("ModSummary"))                 #Linear model from R
            )
          )
        )
      )
    )
#    tabPanel(
#      tags$h5("Linear Model"),
#      width = 12,
#      value = 4,
#      mainPanel(width = 12, fluidRow(column(
#        12,
#        card(
#          title = tags$h5("Linear model"),
#          value = 4,
#          #Linear model panel
#          tags$br(),
#          tags$h6(
#            "This is the summary of the fitted model. It shows model parameters, such as #residual SE, coefficients etc. For simple or ordinar linear models, grafify uses the lm() #function from base R. For mixed effects models, grafify uses the lmer() function from the lme4 #and lmerTest packages. "
#          ),
#          fluidRow(column(10, verbatimTextOutput("ModSummary")))                 #Linear model #from R
#        )
#      )))
#    )
  )
